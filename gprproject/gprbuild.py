from __future__ import annotations
from subprocess import run
from shutil import copytree
from .os import which, add_search_path, which_project
import json
import os
import re


class GPRError(Exception):
    pass


def get_compiler_info(target: str | None) -> dict[str, str]:
    """Retrieve information as returned by gprconfig for the curent Ada compiler.

    :param: target
    :return: a dict with all the information returned by gprconfig --mi-show-compilers
        command
    """
    gprconfig_exe = which("gprconfig")
    if not gprconfig_exe:
        raise GPRError("no gprconfig found")
    gprconfig_cmd = [gprconfig_exe, "--config=ada", "--mi-show-compilers"]
    if target:
        gprconfig_cmd.append(f"--target={target}")

    process = run(gprconfig_cmd, capture_output=True)
    if process.returncode != 0:
        raise GPRError(f"error while trying to capture output of '{gprconfig_cmd}'")
    try:
        gprconfig_output = process.stdout.decode("utf-8").strip()
    except Exception:
        raise GPRError(f"utf-8 output expected")

    result = {}
    for line in gprconfig_output.splitlines():
        if line.startswith("*  1 "):
            key, value = line.replace("*  1 ", "", 1).strip().split(":", 1)
            result[key] = value
    return result


class GPRTool:
    """Manage gpr tools invocation and configuration."""

    def __init__(
        self,
        project_file: str,
        object_dir: str | None = None,
        target: str | None = None,
        integrated: bool = False,
        variables: dict[str, str] | None = None,
        jobs: int = 0,
        variants_var: str | None = None,
        variants_values: list[str] | None = None,
        gnatcov: bool = False,
        symcc: bool = False,
        prefix: str | None = None,
        gpr_paths: list[str] | None = None,
        add_prefix_to_gpr_paths: bool = False,
    ) -> None:
        """Instantiate gpr tools instance.

        :param project_file: path to the project file
        :param object_dir: root path in which project should be built
        :param target: gpr target
        :param integrated: whether installation prefix should be platform specific
        :param variables: scenario variables for the project
        :param jobs: level of parallelism for gpr tools that support it
        :param gnatcov: if True add gnatcov instrumentation
        :param symcc: if True add symcc instrumentation
        """
        project_full_path = os.path.abspath(project_file)
        self.project_file = os.path.basename(project_full_path)
        self.project_name = self.project_file[:-4]
        self.source_dir = os.path.dirname(project_full_path)
        self.object_dir = (
            os.path.abspath(object_dir) if object_dir is not None else os.getcwd()
        )
        if variables:
            self.variables = {k: v for k, v in variables.items()}
        else:
            self.variables = {}

        # Compute the canonical target
        self.original_target = target

        # Compute canonical target
        gprconfig_cmd = [which("gprconfig"), "--config=ada", "--mi-show-compilers"]
        if self.original_target:
            gprconfig_cmd.append(f"--target={self.original_target}")
        gprconfig_output = self.capture(gprconfig_cmd)
        self.target = re.findall(r" 1 normalized_target:(\S*)", gprconfig_output)[0]

        # Compute default prefix
        if prefix:
            self.prefix = os.path.abspath(prefix)
        else:
            self.prefix = re.findall(r" 1 path:(.*)", gprconfig_output)[0].strip()
            if self.prefix.endswith(os.sep):
                self.prefix = os.path.dirname(os.path.dirname(self.prefix))
            else:
                self.prefix = os.path.dirname(self.prefix)
        self.integrated = integrated

        self.jobs = jobs

        # variants
        self.variants_var = variants_var
        if self.variants_var:
            self.variants_values: list[str] = list(variants_values)
        else:
            self.variants_values = ["_"]

        self.gnatcov = gnatcov
        self.symcc = symcc
        self.gpr_paths: list[str] = []
        if gpr_paths:
            self.gpr_paths = list(gpr_paths)
        else:
            self.gpr_paths = []

        if add_prefix_to_gpr_paths:
            self.gpr_paths.append(os.path.join(self.prefix, "share", "gpr"))
            self.gpr_paths.append(
                os.path.join(self.prefix, self.target, "share", "gpr")
            )

    def run(self, args: list[str], **kwargs) -> int:
        """Execute a GPR tool.

        :param args: arguments to the tool including the command name as first
            element
        :param kwargs: named parameters passed down to subprocess.run
        """
        cmd_name = args[0]
        cmd = [which(args[0])] + args[1:]

        for path in self.gpr_paths:
            add_search_path("GPR_PROJECT_PATH", path)

        # Handle gnatcov
        if self.gnatcov:
            gnatcov_prefix = os.path.join(self.object_dir, "gnatcov_rts")
            if cmd_name == "gprbuild":
                if not os.path.isdir(gnatcov_prefix):
                    print("Compile GNATCOV runtime")
                    status = run(
                        [which("gnatcov"), "setup", f"--prefix={gnatcov_prefix}"]
                    )
                    # assert status == 0, "gnatcov runtime compilation failure"
            if cmd_name in ("gprbuild", "gprinstall"):
                cmd += ["--src-subdirs=gnatcov-instr", "--implicit-with=gnatcov_rts"]
            if not which_project("gnatcov_rts.gpr"):
                add_search_path(
                    "GPR_PROJECT_PATH", os.path.join(gnatcov_prefix, "share", "gpr")
                )

            if args[0] == "gprbuild":
                status = self.run(
                    [
                        "gnatcov",
                        "instrument",
                        "--full-slugs",
                        "--no-subprojects",
                        "--level=stmt+decision",
                        "--restricted-to-languages=Ada",
                    ]
                )
                if status != 0:
                    return status

        # Handle basic parameters such out of tree build
        if self.source_dir != self.object_dir:
            cmd.append("--relocate-build-tree")
            cmd.append(f"-P{os.path.join(self.source_dir, self.project_file)}")
        else:
            cmd.append(f"-P{self.project_file}")

        # Handle symcc instrumentation
        if self.symcc:
            cmd.append("--RTS=symcc")
            if cmd_name == "gprbuild":
                cmd += ["-cargs", "-fpass-plugin=libsymcc.so", "-gargs"]

        # Pass jobs
        if cmd_name == "gprbuild":
            cmd.append(f"-j{self.jobs}")

        # Pass canonical target
        cmd.append(f"--target={self.target}")

        for key, value in self.variables.items():
            cmd.append(f"-X{key}={value}")

        if cmd_name == "gprinstall":
            if self.integrated:
                final_prefix = os.path.join(self.prefix, self.target)
            else:
                final_prefix = self.prefix

            cmd += [
                "-p",
                "-f",
                f"--prefix={final_prefix}",
                f"--sources-subdir=include/{self.project_name}",
                f"--install-name={self.project_name}",
            ]
            if self.gnatcov:
                # In gnatcov mode, by default copy the gnatcov runtime in the same
                # location as the final library to ensure that it is visible whenever
                # the lib is through GPR_PROJECT_PATH.
                copytree(
                    os.path.join(self.object_dir, "gnatcov_rts"),
                    final_prefix,
                    dirs_exist_ok=True,
                )

        status = 0
        for variants_value in self.variants_values:
            final_cmd = list(cmd)
            if self.variants_var:
                final_cmd.append(f"-X{self.variants_var}={variants_value}")
                if cmd_name == "gprinstall":
                    final_cmd += [
                        f"--build-name={variants_value}",
                        f"--build-var={self.variants_var}",
                    ]
            print(final_cmd)
            status = run(final_cmd, **kwargs).returncode
            if status != 0:
                return status
        return status

    def build(self, args: list[str], **kwargs):
        return self.run(["gprbuild"] + args, **kwargs)

    def install(self, args: list[str], **kwargs):
        return self.run(["gprinstall"] + args, **kwargs)

    def clean(self, args: list[str], **kwargs):
        return self.run(["gprclean"] + args, **kwargs)

    def uninstall(self, args: list[str], **kwargs):
        return self.run(["gprinstall", "--uninstall"] + args, **kwargs)

    def capture(self, cmd: list[str]) -> str:
        """Capture output of a given tool and decode it to utf-8.

        :param cmd: the command to run
        """
        process = run([which(cmd[0])] + cmd[1:], capture_output=True)
        if process.returncode != 0:
            raise GPRError(f"error while trying to capture output of '{cmd}'")
        try:
            output = process.stdout.decode("utf-8").strip()
        except Exception:
            raise GPRError(f"utf-8 output expected")

        return output

    def set_variable(self, name: str, value: str) -> None:
        self.variables[name] = value

    def save(self):
        json_file = os.path.join(self.object_dir, self.project_name + ".json")
        data = {
            "project_file": os.path.join(self.source_dir, self.project_file),
            "object_dir": self.object_dir,
            "target": self.original_target,
            "integrated": self.integrated,
            "variables": self.variables,
            "jobs": self.jobs,
            "variants_var": self.variants_var,
            "variants_values": self.variants_values if self.variants_var else None,
            "gnatcov": self.gnatcov,
            "symcc": self.symcc,
            "prefix": self.prefix,
            "gpr_paths": self.gpr_paths,
        }
        with open(json_file, "w") as fd:
            json.dump(data, fd, indent=2)

    @classmethod
    def load(cls, project_file: str, object_dir: str | None = None) -> GPRTool:
        object_dir = (
            os.path.abspath(object_dir) if object_dir is not None else os.getcwd()
        )
        project_name = os.path.basename(project_file)[:-4]
        json_file = os.path.join(object_dir, project_name + ".json")

        with open(json_file) as fd:
            data = json.load(fd)

        return GPRTool(
            project_file=data["project_file"],
            object_dir=object_dir,
            target=data["target"],
            integrated=data["integrated"],
            variables=data["variables"],
            jobs=data["jobs"],
            variants_var=data["variants_var"],
            variants_values=data["variants_values"],
            gnatcov=data["gnatcov"],
            symcc=data["symcc"],
            prefix=data["prefix"],
            gpr_paths=data["gpr_paths"],
        )


if __name__ == "__main__":
    d = GPRTool(project_file="toto.gpr")
