#!/usr/bin/env python3
from .gprbuild import GPRTool
from subprocess import CalledProcessError
import argparse
import logging
import os
import re


class BuilderApp:

    constants_project_file = None
    constants = None

    def __init__(self):
        """Create the main CLI argument parser."""
        self.main = argparse.ArgumentParser(description=self.description)
        self.parser = self.main.add_subparsers(
            title="commands",
            description="available commands (do ./setup.py CMD --help "
            "for help on command CMD)",
        )

        self.subparsers = {}

        # Build command
        self.build_cmd = self.parser.add_parser("build", help="build")
        self.build_cmd.add_argument(
            "--gpr-opts",
            nargs=argparse.REMAINDER,
            default=[],
            help="pass remaining arguments to gprbuild",
        )
        self.build_cmd.add_argument(
            "--add-gpr-path", help="prepend a path to look for GPR files"
        )
        self.build_cmd.add_argument(
            "--jobs", "-j", help="gprbuild parallelism", default="0"
        )
        self.build_cmd.add_argument("--target", help="target", default=None)
        self.build_cmd.add_argument(
            "--prefix", help="installation prefix", default=None
        )
        self.build_cmd.add_argument(
            "--integrated",
            help="installation in platform specific subdir",
            default=False,
            action="store_true",
        )
        self.build_cmd.add_argument(
            "--install",
            help="proceed with install automatically after the build",
            default=False,
            action="store_true",
        )
        instr_group = self.build_cmd.add_mutually_exclusive_group()
        instr_group.add_argument(
            "--gnatcov",
            default=False,
            action="store_true",
            help="build project with gnatcov instrumentation",
        )
        instr_group.add_argument(
            "--symcc",
            default=False,
            action="store_true",
            help="build project with symcc intrumentation (works only with LLVM)",
        )

        self.build_cmd.add_argument(
            "--configure-only",
            default=False,
            action="store_true",
            help=(
                "only perform configuration (i.e: update of project "
                "constants and creation of json file). Can be used to integrate "
                "with Alire"
            ),
        )
        self.build_cmd.add_argument(
            "--enable-constant-updates",
            default=False,
            action="store_true",
            help=(
                "Update constants in GPR files in order to pass conviently the"
                " result of the configuration to tools such as IDE and Alire."
            ),
        )
        self.build_cmd.set_defaults(command=self.build)

        # Clean command
        self.clean_cmd = self.parser.add_parser("clean", help="clean")
        self.clean_cmd.add_argument(
            "--gpr-opts",
            default=[],
            nargs=argparse.REMAINDER,
            help="pass remaining arguments to gprclean",
        )
        self.clean_cmd.set_defaults(command=self.clean)

        # Install command
        self.install_cmd = self.parser.add_parser("install", help="install")

        self.install_cmd.add_argument(
            "--gpr-opts",
            default=[],
            nargs=argparse.REMAINDER,
            help="pass remaining arguments to gprinstall",
        )
        self.install_cmd.add_argument(
            "--prefix", help="installation prefix", default=None
        )
        self.install_cmd.set_defaults(command=self.install)

        # Uninstall command
        self.uninstall_cmd = self.parser.add_parser("uninstall", help="uninstall")
        self.uninstall_cmd.add_argument(
            "--gpr-opts",
            default=[],
            nargs=argparse.REMAINDER,
            help="pass remaining arguments to gpruninstall",
        )
        self.uninstall_cmd.add_argument(
            "--prefix", help="un-installation prefix", default=None
        )
        self.uninstall_cmd.set_defaults(command=self.uninstall)

        # Set basic logging
        logging.basicConfig(level=logging.DEBUG, format="%(message)s")

    def adjust_config(self, gpr, args: argparse.Namespace) -> None:
        pass

    def adjust_project_constants(self, gpr) -> None:
        if self.constants_project_file is not None and self.constants:
            with open(self.constants_project_file, "r") as fd:
                content = fd.read().splitlines()

            with open(self.constants_project_file, "w") as fd:
                result = []
                for line in content:
                    m = re.match(
                        r"(\s*)(" + "|".join(self.constants) + r")_DEFAULT(\s*):=", line
                    )
                    if m:
                        variable = m.group(2)
                        result.append(
                            f'{m.group(1)}{variable}_DEFAULT{m.group(3)}:= "{gpr.variables[variable]}";'
                        )
                    else:
                        result.append(line)
                fd.write("\n".join(result) + "\n")

    def build(self, args):
        gpr = GPRTool(
            project_file=self.project_file,
            target=args.target,
            integrated=args.integrated,
            jobs=args.jobs,
            gnatcov=args.gnatcov,
            symcc=args.symcc,
            prefix=args.prefix,
            gpr_paths=args.add_gpr_path,
            add_prefix_to_gpr_paths=args.prefix is not None,
        )
        self.adjust_config(gpr, args)

        if args.enable_constant_updates:
            self.adjust_project_constants(gpr)
        gpr.save()

        for var, value in gpr.variables.items():
            print(f'{var:<32} := "{value}";')

        if not args.configure_only:
            status = gpr.build([])
            if status == 0 and args.install:
                status = gpr.install([])
            return status
        else:
            return 0

    def clean(self, args):
        gpr = GPRTool.load(self.project_file)
        return gpr.clean([])

    def install(self, args):
        gpr = GPRTool.load(self.project_file)
        if args.prefix:
            gpr.prefix = os.path.abspath(args.prefix)
        return gpr.install([])

    def uninstall(self, args):
        gpr = GPRTool.load(self.project_file)
        if args.prefix:
            gpr.prefix = os.path.abspath(args.prefix)
        return gpr.uninstall([])

    def add_arguments(self, parser: argparse.ArgumentParser) -> None:
        """Function to be used by end user to add switch to the build command.

        :param parser: the argparse subparser of the build command
        """
        pass

    def run(self) -> int:
        """Execute a command (build, install, uninstall or clean).

        :return: the process return code
        """
        # Allow user to amend switches for the build command
        build_cmd_group = self.build_cmd.add_argument_group(
            title="project specific options"
        )
        self.add_arguments(build_cmd_group)

        # Parse arguments and dispatch to the right implementation
        args = self.main.parse_args()

        # Check that a command has been passed
        if "command" not in args:
            logging.error("Missing command")
            self.main.print_help()
            return 1

        try:
            return args.command(args)
        except CalledProcessError as e:
            logging.error("process failed with status: %s", e.returncode)
            return 1
        except AssertionError as e:
            logging.error("requirement missing: %s", e)
            return 1
