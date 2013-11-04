#!/usr/bin/env python
import re
import sys
import os
import subprocess
import copy

pkg_name = "Orm"

store_connections = True
# If True, connections are stored in detached objects (and need to be passed
# explicitly to Get). If False, the connections are queried from a pool
# automatically otherwise

database_connection = "Session_Type"
get_db_connection = ".DB"
# These two variables indicate the type used for the database connection.
# database_connection is what is stored inside lists, and get_db_connection
# is the suffix to append to get the actual
# GNATCOLL.SQL.Exec.Database_Connection
# Only relevant if store_connections is True

max_depth = 3
# Maximum valid value for Select_Related
# No need in putting this too high, since that will generate make queries much
# bigger, thus invalidating any saving we might have in doing only one of them

database_pkg = "Database"
# Name of the package that contains the database description

debug = False
# Insert additional debug code in the generated source


def exec_or_fail(*args, **kwargs):
    """Same parameters as process.Process, but prints an error message in
       case of error, and then raises an exception
    """

    try:
        sub = subprocess.Popen(*args, **kwargs)
        sub.wait()
        if sub.returncode != 0:
            print "Error: could not execute %s" % args[0]
            raise subprocess.CalledProcessError(sub.returncode, args[0])

        return sub

    except OSError:
        print "Error: could not execute %s" % args[0]
        raise subprocess.CalledProcessError(sub.returncode, args[0])


def save_dir(fn):
    """Temporarily change the current directory while running a function,
      and restore it when the function exits. This is a decorator:
         @save_dir
         def my_function ():
            ...
    """

    def do_work(*args, **kwargs):
        saved = os.getcwd()
        try:
            result = apply(fn, args, kwargs)
        finally:
            os.chdir(saved)
        return result

    do_work.__name__ = fn.__name__  # Reset name
    do_work.__doc__ = fn.__doc__
    return do_work


def unlink_if_exist(files):
    """Unlink, if they exist, the file(s).
       No error is propagated if the files do not exist
    """

    if not isinstance(files, list):
        files = [files]
    for f in files:
        try:
            os.unlink(f)
        except OSError:
            pass


def max_length(iter):
    """Return the length of the longuest element in iter"""
    longuest = 0
    for f in iter:
        longuest = max(longuest, len(f))
    return longuest


def splitstr(str, maxlen):
    """Split str into lines at most maxlen characters wide"""
    line = ""
    result = []
    for w in str.split():
        if len(line) + len(w) + 1 > maxlen:
            result.append(line)
            line = w
        else:
            line += " " + w

    if line != "":
        result.append(line)

    return result


######################################
## Pretty-printer
######################################

class Subprogram(object):

    def __init__(self, name, params, local_vars, body, returns,
                 comment, overriding, abstract, inline):
        self.name = name
        self.params = params
        self.local_vars = local_vars
        self.body = body
        self.returns = returns
        self.comment = comment
        self.overriding = overriding
        self.abstract = abstract
        self.inline = inline


def _subprogram_sorter(sub1, sub2):
    """cmd for two subprograms.
       This groups property setters/getters together
    """
    n1 = sub1.name.replace("set_", "")
    n2 = sub2.name.replace("set_", "")
    return cmp(n1, n2)


class Pretty_Printer(object):
    """This class is responsible for doing the actual output of code, properly
       formatted
    """

    def __init__(self, out, casing=[]):
        """CASING stored the casing exceptions. This is a list of names with
           their expected formating. All names not in that list will be
           capitalized
        """

        casing.extend(["out", "in", "access", "constant", "aliased"])
        self.out = out
        self.casing = dict()
        for c in casing:
            self.casing[c.lower()] = c
        self.pkg_name = None

    def start_package(self, pkg_name):
        """Terminate the current file, do the output, and start a new file"""
        self.terminate_package(need_dba=True)
        self.pkg_name = pkg_name.title()
        self.spec_withs = []     # List of packages to with+use in specs
        self.body_withs = []     # List of packages to with+use in bodies
        self.global_vars = []    # Global variables and constants
        self.body_cst = []       # Constants to put in the body, same as params
        self.unique_body_cst = []
        self.private_before = ""        # Private part of the spec
        self.private_after = ""        # Private part of the spec
        self.body_code = ""      # Goes in body before subprograms
        self.sections = []       # Sections in the specs. Contains tuples:
                                 #    0 => section name
                                 #    1 => types declaration for the section
                                 #    2 => list of subprograms:
                                 #     (name, params, body, returns, comment)
        self.sections.append(("", "", [], ""))  # Default subprograms

    def add_with(self, pkg, specs=True, do_use=True):
        """Add a with+use clause for pkg. pkg can be a list of packages.
           Automatic casing is performed. If specs is True, the withs are
           appended to the specs, otherwise to the body"""
        if type(pkg) == str:
            pkg = [pkg]

        for p in pkg:
            if specs:
                if (p, do_use) not in self.spec_withs:
                    self.spec_withs.append((p, do_use))
            else:
                if (p, do_use) not in self.body_withs:
                    self.body_withs.append((p, do_use))

    def add_constants(self, cst):
        """cst has the same format as the parameters of a subprogram"""
        if cst:
            self.body_cst.append(cst)

    def add_unique_constants(self, cst):
        """Same as add_constants, does nothing if the constant already exists
        """
        if cst:
            for c in cst:
                if c not in self.unique_body_cst:
                    self.unique_body_cst.append(c)

    def add_property(self, schema, row, field, getter, setter, type,
                     getter_local_vars=[], setter_local_vars=[],
                     comment=None, abstract=False, section=""):
        """Define a property for self (ie a setter and getter strongly linked
           to each other
        """
        get_name = schema.subprogram_name_from_field(field)
        if isinstance(field, str):
            set_name = "set_%s" % field
        else:
            set_name = "set_%s" % field.name

        if getter:
            self.add_subprogram(
                get_name, body=getter,
                params=[("self", "detached_%s" % row)],
                local_vars=getter_local_vars,
                returns=type,
                comment=comment, abstract=abstract, section=section)

        if setter:
            self.add_subprogram(
                set_name, body=setter,
                local_vars=setter_local_vars,
                params=[("self", "detached_%s" % row), ("value", type)],
                comment=comment, abstract=abstract, section=section)

    def add_subprogram(self, name, body, params=[], local_vars=[],
                       returns=None, comment=None, overriding=False,
                       abstract=False, inline=False,
                       section=""):
        """Add a new subprogram. local_vars and params are lists of
          (name, type, default) tuples.
           The body will be pretty-printed automatically, and the required
           pragma Unreferenced are also added automatically.
           If the section is "body" the subprogram will not be visible in the
           specs
        """
        news = Subprogram(name, params, local_vars, body, returns, comment,
                          overriding, abstract, inline)

        for index, s in enumerate(self.sections):
            if s[0] == section:
                self.sections[index] = (section, s[1], s[2] + [news], s[3])
                return
        self.sections.append((section, "", [news], ""))

    def add_section(self, name, types, comment=""):
        """Add a new section in the specs, which contains TYPES, as well as
           any subprogram added to that section later on.
           COMMENT is a general comment for the section
        """
        for index, s in enumerate(self.sections):
            if s[0] == name:
                self.sections[index] = (s[0], s[1] + "\n" + types, s[2], s[3])
                return

        self.sections.append((name, types, [], comment))

    def add_private_before(self, str):
        """Add custom contents to the private section, before the subprograms
           in that section"""
        self.private_before += str

    def add_private_after(self, str):
        """Add custom contents to the private section, after the subprograms
           in that section"""
        self.private_after += str

    def _title(self, name):
        def _title_sep(sep, str):
            if sep == ".":
                return [" ".join(_title_sep(" ", n)) for n in str.split(sep)]
            elif sep == " ":
                return ["'".join(_title_sep("'", n)) for n in str.split(sep)]
            else:
                return [self.casing.get(n.lower(), n.title())
                        for n in str.split(sep)]

        return ".".join(_title_sep(".", name))

    def _output_withs(self, list):
        if list:
            self.out.write("pragma Warnings (Off);\n")
            l = max_length(list)
            for w, do_use in sorted(list):
                if do_use:
                    self.out.write("with %-*s use %s;\n" % (l + 1, w + ";", w))
                else:
                    self.out.write("with %-*s;\n" % (l, w))
            self.out.write("pragma Warnings (On);\n")

    def _format_decl(self, list):
        """List has the same format as params and local_vars for a subprogram
        """
        l = max_length([p[0] for p in list])
        result = []
        for p in list:
            if len(p) == 3:
                name, type, default = p
                result.append(
                    "%-*s : %s := %s"
                    % (l, self._title(name), self._title(type), default))
            elif len(p) == 2:
                name, type = p
                result.append(
                    "%-*s : %s" % (l, self._title(name), self._title(type)))
            else:
                result.append(p[0])

        return result

    def _format_parameters(self, params):
        result = self._format_decl(params)
        if result:
            return "\n     (" + ";\n      ".join(result) + ")"
        else:
            return ""

    def _format_local_vars(self, local_vars):
        result = self._format_decl(local_vars)
        if result:
            return "      " + ";\n      ".join(result) + ";\n"
        else:
            return ""

    def _get_subprogram_proto(self, subp):
        p = self._format_parameters(subp.params)

        if subp.overriding:
            prefix = "overriding "
        else:
            prefix = ""

        if subp.returns:
            func = "   %sfunction %s%s\n     return %s" % \
              (prefix, self._title(subp.name), p, self._title(subp.returns))
        else:
            func = "   %sprocedure %s%s" % (prefix, self._title(subp.name), p)

        if subp.abstract:
            func = func + " is abstract"

        # Would we fit on a single line ?
        if len(re.sub("\s+", " ", func)) < 79:
            return "  " + re.sub("\s+", " ", func)
        else:
            return func

    def _output_section_spec(self, section):
        name, types, subprograms, comment = section
        if name and name != "body" and name != "private":
            self.out.write("\n   " + "-" * (len(name) + 6) + "\n")
            self.out.write("   -- %s --\n" % self._title(name))
            self.out.write("   " + "-" * (len(name) + 6) + "\n")

            if comment:
                self.out.write("   " + comment.strip() + "\n")

            self.out.write("\n")

        if types:
            self._output_indented(types.rstrip(), indent=3)
            self.out.write("\n")

        # Sort subprograms within the section
        subprograms.sort(cmp=_subprogram_sorter)

        comment = ""

        for index, p in enumerate(subprograms):
            self.out.write(self._get_subprogram_proto(p) + ";\n")

            if p.inline:
                self.out.write("   pragma Inline (%s);\n" % p.name);

            if p.comment:
                comment += " " + p.comment

            # Share the comment with the next subprogram if it has the same
            # name

            if index + 1 < len(subprograms) \
               and _subprogram_sorter(subprograms[index + 1], p) != 0:
                if comment:
                    self.out.write("   -- ")
                    self.out.write("\n   --  ".join(splitstr(comment, 72)))
                    self.out.write("\n")
                comment = ""
                self.out.write("\n")

    def _output_subprogram_specs(self):
        for section in self.sections:
            if section[0] == "body":
                pass
            elif section[0] != "private":
                self._output_section_spec(section)

    def _output_indented(self, code, indent=3):
        body = code.strip()
        if not body:
            return

        # Add newlines where needed, but preserve existing blank lines
        body = re.sub(";(?!\s*\n)", ";\n", body)
        body = re.sub("(?<!and )then(?!\s*\n)", "then\n", body)
        body = re.sub("(?<!or )else(?!\s*\n)", "else\n", body)
        body = re.sub("declare", "\ndeclare", body)
        body = re.sub("\n\s*\n+", "\n\n", body)

        parent_count = 0

        for l in body.splitlines():
            l = l.strip()
            if l.startswith("end") \
               or l.startswith("elsif")  \
               or l.startswith("else")  \
               or l.startswith("exception")  \
               or l.startswith("begin"):
                indent -= 3

            old_parent = parent_count
            parent_count = parent_count + l.count("(") - l.count(")")

            if not l:
                pass
            elif l[0] == '(':
                self.out.write(" " * (indent + 2))
                if parent_count > old_parent:
                    indent += (parent_count - old_parent) * 3
            elif not old_parent:
                self.out.write(" " * indent)
                if parent_count > old_parent:
                    indent += (parent_count - old_parent) * 3
            else:
                if parent_count > old_parent:
                    indent += (parent_count - old_parent) * 3
                self.out.write(" " * indent)

            if old_parent > parent_count:
                indent -= (old_parent - parent_count) * 3

            self.out.write(l)
            self.out.write("\n")

            if(l.endswith("then") and not l.endswith("and then")) \
               or l.endswith("loop") \
               or(l.endswith("else") and not l.endswith("or else"))\
               or l.endswith("begin") \
               or l.endswith("exception") \
               or l.endswith("declare"):
                indent += 3

    def _output_subprogram_bodies(self):
        for section in self.sections:
            if section[0] == "body":
                self._output_section_spec(section)

        subp = []
        for name, types, subprograms, comment in self.sections:
            subp.extend(subprograms)

        subp.sort(key=lambda x: x.name)

        for p in subp:
            if not p.abstract:
                self.out.write("\n   " + "-" * (len(p.name) + 6) + "\n")
                self.out.write("   -- %s --\n" % self._title(p.name))
                self.out.write("   " + "-" * (len(p.name) + 6) + "\n\n")

                proto = self._get_subprogram_proto(p)
                lines = proto.splitlines()
                if not p.local_vars and len(lines[-1]) < 79 - 3:
                    self.out.write(proto + " is\n")
                else:
                    self.out.write(proto + "\n   is\n")

                if p.local_vars:
                    local = self._format_local_vars(p.local_vars)
                else:
                    local = ""

                unreferenced = []
                for param in p.params:
                    if not re.search(
                       r'\b%s\b' % param[0], p.body + local, re.IGNORECASE):
                        unreferenced.append(self._title(param[0]))

                if unreferenced:
                    self.out.write("      pragma Unreferenced (%s);\n"
                                   % (", ".join(unreferenced)))

                if p.local_vars:
                    self.out.write(local)

                self.out.write("   begin\n")
                self._output_indented(p.body, indent=6)
                self.out.write("   end %s;\n" % (self._title(p.name)))

    def terminate_package(self, need_dba=False):
        """Terminate the current file, and do the output"""
        if not self.pkg_name:
            return

        ## Specs

        self._output_withs(self.spec_withs)
        self.out.write("pragma Style_Checks (Off);\n\n")
        self.out.write("package %s is\n" % self.pkg_name)

        if need_dba:
            self.out.write("   package DBA renames %s;\n" % database_pkg)
            self.out.write(
                "   subtype Related_Depth is Integer range 0 .. %d;\n"
                % max_depth)

        self._output_subprogram_specs()
        self.out.write("\nprivate\n")
        self.out.write(self.private_before)
        self.out.write("\n")
        private = [s for s in self.sections if s[0] == "private"]
        for p in private:
            self._output_section_spec(p)
        self.out.write(self.private_after)
        self.out.write("\n")

        self.out.write("end %s;\n\n" % self.pkg_name)

        ## Bodies
        self._output_withs(self.body_withs)
        self.out.write("pragma Style_Checks (Off);\n\n")
        self.out.write("package body %s is\n" % self.pkg_name)

        if need_dba:
            self.out.write(
                "   pragma Warnings (Off);\n")
            #"   pragma Warnings (Off, \"*is not referenced\");\n")

            self.out.write("   use Sessions.Pointers;\n")
            if debug:
                self.out.write(
                    '   Me : constant Trace_Handle := Create("ORM");')

        for c in sorted(self.unique_body_cst):
            self.out.write(
                "\n   " + ";\n   ".join(self._format_decl([c])) + ";")

        self.out.write("\n")
        for c in self.body_cst:
            self.out.write(
                "\n   " + ";\n   ".join(self._format_decl(c)) + ";")

        self.out.write(self.body_code)

        if need_dba:
            self.out.write("\n\n")
            #self.out.write("   pragma Warnings (On, \"*is not referenced\");")
            self.out.write("   pragma Warnings (On);")
        self.out.write("\n")

        self._output_subprogram_bodies()
        self.out.write("end %s;\n\n" % self.pkg_name)

        self.pkg_name = None


######################################
## Schema
######################################

class Cannot_Parse_Schema(Exception):
    pass


class Schema(object):
    """A class that represents a database schema"""

    #############
    ## __init__
    #############

    def __init__(self, setup, tables, pretty, all_tables, omit):
        """Connect to the database and get the schema.
           SETUP is an object which contains the database description.
           OMIT is the list of fields to omit in the binding"""

        self.pretty = pretty

        self.details = get_db_schema(setup, omit=omit, requires_pk=True,
                                     all_tables=all_tables)
        if not self.details:
            raise Cannot_Parse_Schema

        self.sql_tables = None

    #############
    ## withs_for
    #############

    def withs_for(self, table):
        """Create the list of withs needed for table"""

        self.pretty.add_with(
          ["GNATCOLL.SQL", "GNATCOLL.SQL.Exec", "GNATCOLL.Tribooleans",
           "GNATCOLL.SQL.Orm", "GNATCOLL.SQL.Orm.Impl",
           "GNATCOLL.SQL.Sessions",
           "Ada.Strings.Unbounded", "GNAT.Strings", database_pkg.title(),
           "GNAT.Calendar", "Ada.Calendar",
            "Ada.Finalization"])
        self.pretty.add_with("Ada.Unchecked_Deallocation", specs=False,
                              do_use=False)
        self.pretty.add_with("Ada.Containers", specs=False)
        self.pretty.add_with("System.Address_Image", do_use=False)

        if not store_connections:
            self.pretty.add_with("Sessions", specs=False)
        if debug:
            self.pretty.add_with("GNATCOLL.Traces")
            self.pretty.add_with("System.Address_Image", do_use=False)

    #################
    ## params_create
    #################

    def params_create(self, table):
        """Return the list of parameters to Create, for a given table
           TABLE is an instance of Table"""
        return [(f.name, f.type.ada_param, f.type.default_param)
                for f in table.fields]

    ##########################
    ## Compute the name of the subprogram to use for a field
    ## If the field has the same name as one of the tables or rows, we would
    ## get conflicts and couldn't compile the code, so we have to generate
    ## a different name for the subprogram
    ##########################

    def subprogram_name_from_field(self, field):
        """FIELD is an instance of Field, or a string"""
        if isinstance(field, str):
            base = field
        else:
            base = field.name

        f = base.lower()
        for table in self.details.itervalues():
            if f == table.row.lower():
                return "Get_" + base

        return base

    ######################
    ## call_create_params
    ######################

    def call_create_params(self, table):
        """Parameters when calling Create for TABLE"""

        params = []
        for pk in self.details[table].pk:
            params.append("%s => %s" % (pk.name.title(), pk.name.title()))
        return params

    #########
    ## equal
    #########

    def equal(self, table):
        """Return the comparison operators when comparing Op1 and Op2
           TABLE is instance of Table
        """

        params = []
        for pk in table.pk:
            params.append("%s'(Op1.%s) = Op2.%s"
                           % (pk.type.ada_param,
                              self.subprogram_name_from_field(pk),
                              self.subprogram_name_from_field(pk)))
        return params

    #################
    ## params_get_pk
    #################

    def params_get_pk(self, table):
        """Return the list of parameters for Get.
           TABLE is an instance of Table"""

        if not table.pk:
            return []

        params = []

        for f in table.pk:
            params.append((f.name, f.type.ada_param))

        return params

    ########################
    ## detached_data_fields
    #######################

    def detached_data_fields(self, table):
        """TABLE is instance of Table"""

        longuest = max_length([f.name for f in table.fields])
        data = []

        for f in table.fields:
            if f.show:
                data.append("       ORM_%-*s : %s := %s" % \
                  (longuest + 3,
                   f.name, f.type.ada_field, f.default_for_field()))

        for f in table.fk:
            if f.foreign.show:
                name = subprogram_from_fk(f)
                f2 = table.get_field(name)
                if f2 is None or f2.show:
                    data.append(
                        "       ORM_FK_%-*s : Detached_%s_Access := null"
                        % (longuest, name, f.foreign.row))

        return ";\n".join(sorted(data))

    ##################
    ## unchecked_free
    ##################

    def unchecked_free(self, table, all_tables):
        """List of Unchecked_Free instantiations needed for TABLE"""

        d = self.details[table]
        l = set()

        for f in d.fk:
            if f.show():
                l.add(
                ("procedure Unchecked_Free is new Ada.Unchecked_Deallocation\n"
                 + "     (Detached_%(row)s'Class, Detached_%(row)s_Access)"
                  % {"row": f.foreign.row},))

        # The call to set() is to uniquify the elements in the list
        return list(l)

    ###############
    ## free_fields
    ###############

    def free_fields(self, table):
        """The commands to free the fields of the manager"""

        free_fields = []
        for f in table.fk:
            if f.foreign.show:
                subp = subprogram_from_fk(f)
                try:
                    free_fields.append(
                        "Unchecked_Free (Self.ORM_FK_%s);" % subp)
                except KeyError:
                    print "free_fields: Invalid field: %s.%s" % (table, subp)

        for f in table.fields:
            free_fields.append(f.free_field("Self"))

        if free_fields:
            return "\n".join(free_fields)
        else:
            return ""


######################
## subprogram_from_fk
######################

def subprogram_from_fk(fk):
    """Return the name of the primitive operation, for a given Foreign Key
       Handling of multi-key foreign keys:
         subscription(nb,type) REFERENCES contract(nb,type)
       Should result in
         function Contract(Self : Subscription) return Contract;
       When the FK has a single key, we use that name for the name of the
       subprogram, which leads to a more natural use
    """

    if len(fk.pairs) == 1:
        return fk.pairs[0][0].name
    else:
        return fk.foreign.name.title()


######################################
## generator for Internal_Query
######################################

def internal_query(pretty, table, schema):
    joins = ""
    lj = "Table"
    body = ""
    fk_list = []
    local_vars = [("Table",
                   "T_Numbered_%s(Aliases(Base))" % table.name)]

    field_list = ["Table.%s" % f.name for f in table.fields]
    fklocal = ""

    for index, fk in enumerate(table.fk):
        if fk.foreign.show:
            reft = "FK%d" % (index + 1, )

            fklocal += "\n%s : T_Numbered_%s(Aliases(Aliases(Base + %d)));" \
                % (reft, fk.foreign.name, index + 1)

            # Join criteria. No need to repeat for null FK, since we already
            # use a LEFT JOIN.

            if not fk.can_be_null():
                for f in fk.pairs:
                    joins += "\nand Table.%(field)s = %(reft)s.%(reff)s" % \
                        {"field": f[0].name, "reft": reft, "reff": f[1].name}

            # Add possible LEFT JOIN

            if fk.can_be_null():
                criteria = []
                for f in fk.pairs:
                    criteria.append(
                       "Table.%(field)s=%(reft)s.%(reff)s"
                       % {"reft": reft, "field": f[0].name, "reff": f[1].name})

                lj = """Left_Join(%(lj)s, %(reft)s, %(criteria)s)""" % \
                      {"lj": lj, "criteria": " and ".join(criteria),
                       "reft": reft}

            # Add fields for FK table

            fk_body = ("C2 := No_Criteria;"
                       + "Do_Query_%(ref)s(Fields, T, C2,"
                       + "Aliases(Base + %(index)d),\n"
                       + "Aliases, Depth - 1, Follow_LJ);") % \
                      {"ref": fk.foreign.name, "index": index + 1}

            fk_body += "if Depth > 1 then Criteria := Criteria and C2; end if;"

            if fk.can_be_null():
                fk_body = "if Follow_LJ then %s end if;" % fk_body
            else:
                fk_body += "From := From & T;"
            fk_list.append(fk_body)

    if joins:
        joins = "Criteria := Criteria%s;" % joins

    if fk_list:
        body = "if Depth > 0 then declare %s begin %s" % (fklocal, joins)

        if lj == "Table":
            body += "From := +Table;"
        else:
            body += \
             "if Follow_LJ then From := +%s; else From := +Table; end if;" % lj

        body += "%s end; end if;" % "\n\n".join(fk_list)
        local_vars.extend([("C2", "SQL_Criteria"),
                           ("T",  "SQL_Table_List")])
    else:
        fk_list = "From := +Table;"

    pkfield_list = ["Table.%s" % f.name for f in table.fields if f.is_pk()]

    if table.pk != []:
        body = """if PK_Only then
       Fields := Fields & %(pkfields)s;
    else
       Fields := Fields & %(fields)s;
    end if;
    From := Empty_Table_List;""" + body
    else:
        body = """Fields := Fields & %(fields)s; """ + body

    body = body % {"fields":  "\n& ".join(field_list),
                   "pkfields": "\n& ".join(pkfield_list)}

    if debug:
        tmp = \
          'Trace(Me, "Do_Query_%s, Base=(" & Base\'Img & Aliases(Base)\'Img' \
          % table.name
        tmp += '& ")"'
        for index, fk in enumerate(table.fk):
            if fk.foreign.show:
                tmp += '\n & " FK%(i)d=(" & Aliases(Base + %(i)d)\'Img' \
                  + '& Aliases(Aliases(Base + %(i)d))\'Img & ")"' \
                  % {"i": index + 1}

        tmp += ");"

        body = tmp + body

    if table.pk != []:
        params = [("PK_Only", "Boolean", "False")]
        internal_body = """Do_Query_%(cap)s(Fields, From, Criteria,
           0, Alias_%(cap)s, Depth, Follow_LJ, PK_Only);"""
    else:
        params = []
        internal_body = """if PK_Only then
        raise Program_Error with "Table %(cap)s has no primary key";
    end if;
    Do_Query_%(cap)s(Fields, From, Criteria,
       0, Alias_%(cap)s, Depth, Follow_LJ);"""

    pretty.add_subprogram(
        name="internal_query_%s" % table.name,
        params=[("fields",    "in out SQL_Field_List"),
                  ("from",      "out SQL_Table_List"),
                  ("criteria",  "in out SQL_Criteria"),
                  ("depth",     "natural"),
                  ("Follow_LJ", "Boolean"),
                  ("PK_Only", "Boolean", "False")],
        section="Managers(implementation details)",
        body=internal_body % {"cap": table.name})

    pretty.add_subprogram(
        name="do_query_%s" % table.name,
        params=[("fields",    "in out SQL_Field_List"),
                  ("from",      "out SQL_Table_List"),
                  ("criteria",  "in out SQL_Criteria"),
                  ("Base",      "Natural"),
                  ("Aliases",   "Alias_Array"),
                  ("depth",     "natural"),
                  ("Follow_LJ", "Boolean")] + params,
        local_vars=local_vars,
        section="body",
        body=body)


def create(pretty, table, schema):
    """Generate a Create subprogram to create managers"""
    body = ""
    for f in table.fields:
        body += "if %s /= %s then " % (f.name, f.type.default_param)
        if f.type.sql_type.lower() == "boolean":
            body += " C := C and DBA.%s.%s = To_Boolean(%s);" \
                % (table.name, f.name, f.name)
        else:
            body += " C := C and DBA.%s.%s = %s;" \
                    % (table.name, f.name, f.name)
        body += "end if;"

    body += "Copy(Self.Filter(C), Into => Result); return Result;"
    # body += "return %s_Managers(Self.Filter(C));" % table.name

    pretty.add_subprogram(
       name="filter",
       params=[("self", "%s_Managers'Class" % table.name)]
                + schema.params_create(table),
       body=body,
       local_vars=[("C", "SQL_Criteria", "No_Criteria"),
                  ("Result", "%s_Managers" % table.name)],
       section="Manager: %s" % table.name,
       returns="%s_Managers" % table.name)


def from_cache_params(schema, table, with_self=""):
    """Return the parameters passed to the Hash_%row function"""
    if with_self:
        tmp = ", ".join(["%s.%s" % (with_self, schema.subprogram_name_from_field(p))
                         for p in table.pk])
    else:
        tmp = ", ".join(["%s" % p.name for p in table.pk])
    return tmp


def from_cache_hash(schema, table, with_self=""):
    if table.has_cache:
        return "(%d, %s)" % (table.base_key,
                             from_cache_params(schema, table, with_self))
    else:
        return "(%d, No_Primary_Key)" % table.base_key


def detach(pretty, table, schema, translate):
    """Generate Detach subprograms
       TABLE is an instance of Table
    """

    # We return a Detached_*'Class, rather than a Detached_*_Access for the
    # following reason: returning an access type would be tempting for the user
    # to store directly in a persistent data structure. But the pointer really
    # belongs to the session, that will free it when no longer needed, so there
    # is a risk of Storage_Error.
    # Instead, we return the 'Class. A Detached_* is supposed to be a smart
    # pointer anyway, so any modification to it will affect all objects that
    # represent the same element.
    # This choice makes it slightly less convenient to manipulate the result of
    # Detach since basically we need a declare block.
    #
    # In a Detached_*, we store new pointer types, not the ones stored in the
    # session, since the latter might have a shorter lifetime.

    if table.pk != [] and table.has_cache:
        tr = {"row": translate["row"],
              "fromcache": from_cache_params(schema, table, with_self="Self")}

        body = ("return Detached_%(row)s'Class (Session.From_Cache ("
                    + "%(fromcache)s, No_Detached_%(row)s));") % {
                "row": translate["row"],
                "fromcache": from_cache_hash(schema, table, with_self="")}


        pretty.add_subprogram(
            name="from_cache",
            params=[("session", "Session_Type")] + schema.params_get_pk(table),
            section="Elements: %(cap)s" % translate,
            returns="Detached_%(row)s'Class" % translate,
            comment="""Lookup in the session whether there is already an element
with this primary key. If not, the returned value will be a null element
(test with Is_Null)""",
            body=body)

        local = [("R", "constant Detached_%(row)s'Class" % translate,
                  "From_Cache (Self.Data.Session, %s)" %
                  from_cache_params(schema, table, with_self="Self"))]
        body = """if R.Is_Null then
              return Detach_No_Lookup (Self, Self.Data.Session);
          else
              return R;
          end if;"""

    else:
        local = []
        body = "return Detach_No_Lookup (Self, Self.Data.Session);"
        tr = {}

    pretty.add_subprogram(
       name="detach",
       params=[("self", "%(row)s'Class" % translate)],
       section="Elements: %(cap)s" % translate,
       returns="Detached_%(row)s'Class" % translate,
       local_vars=local,
       body=body % tr)

    # Internal version of Detach

    long = max_length([f.name for f in table.fields])
    aggregate = []
    decl = [("Default", "Detached_%(row)s" % translate),
            ("Result", "Detached_%(row)s'Class" % translate,
             "Detached_%(row)s'Class (Session.Factory (Self, Default))"
             % translate)]
    setters = ""

    for f in table.fields:
        if f.show:
            aggregate.append(f.free_field("Tmp"))
            conv = f.field_from_db("Self")
            aggregate.append("Tmp.ORM_%-*s := %s;" % (long + 3, f.name, conv))

    tests = []

    # Only keep foreign keys for tables for which we generate orm.* packages
    fk = [f for f in table.fk if f.foreign.show]

    for index, f in enumerate(fk):
        subp = subprogram_from_fk(f)
        ref = f.foreign.name
        d = dict(name=subp, ref=ref, cap=subp.title(), pkg=pkg_name,
                     table=table.name, index=index, row=f.foreign.row)

        if f.show():
            decl.append(("FK_" + subp, "Detached_%(row)s_Access" % d))
            aggregate.append("Tmp.ORM_FK_%-*s := FK_%s;" % (long, subp, subp))

        # Have to create new access type, see above
        str = """FK_%(name)s := new Detached_%(row)s'Class'(
  I_%(ref)s.Internal_Element
  (Self, Upto_%(table)s_%(index)d (Self.Depth, LJ)).Detach);"""

        if f.show():
            if f.can_be_null():
                tests.append("         if LJ then\n" + str % d
                             + "\n         end if;\n")
            else:
                tests.append(str % d)

    if tests:
        tests = "if Self.Depth > 0 then\n%s\n      end if;" % "\n".join(tests)
    else:
        tests = ""

    if table.fk:
        decl.append(("LJ", "constant Boolean", "Self.Data.Follow_LJ"))

    tests += setters

    pretty.add_subprogram(
       name="detach_no_lookup",
       comment="Same as Detach, but does not check the session cache",
       section="body",
       params=[("self", "%(row)s'Class" % translate),
                 ("session", database_connection)],
       returns="Detached_%(row)s'Class" % translate,
       local_vars=decl +
         [("Tmp", "%(row)s_Data" % translate)],
       body="""
  Tmp := %(row)s_Data (Result.Get);
  if Tmp = null then
     Tmp := new %(row)s_DDR;
     Set (Result, Tmp);
  end if;

  %(tests)s %(aggregate)s
  %(traces)sSession.Persist (Result);
  return Result;
 """ % {"cap":       table.name,
        "tests":     tests,
        "row":       translate["row"],
        "traces":    debug_msg(table, "Creating", "Result.all"),
        "aggregate": "\n            ".join(sorted(aggregate))})


##########################
## Prepare the sections in the specs
## so that they appear in the right order
##########################

def order_sections(schema, pretty, all_tables):
    for table in all_tables.itervalues():
        if table.is_abstract:
            pretty.add_section(
               "Elements: %s" % table.name, "",
               "--  Interfaces corresponding to abstract tables in the schema")

    # First the elements declarations grouped, since their primitive ops
    # might reference each other

    pretty.add_section("Types", "",
                        """
    --  Detached_* elements extract the value from the list and store them
    --  locally. As a result, they remain valid even if the list is modified,
    --  but require more memory to store.
    --
    --  Other elements are only valid while the list from which they are
    --  created is not modified(see Element below). As soon as you iterate the
    --  list this element becomes invalid.
    --
    --  Direct lists are stored in memory, and can be traversed in any order.
    --  Forward lists can only be iterated forward. With some database backends
    --  this is much more efficient since only the current element needs to be
    --  stored in memory(and retrieved from the server).""")

    # Then all their primitive operations, before we freeze the types

    for table in all_tables.itervalues():
        pretty.add_section("Elements: %s" % table.name, "")

    # Then instanciate the generic packages, which freezes the element types.
    # We can then define the managers

    pretty.add_section("Managers(implementation details)", "")
    pretty.add_section("Managers", "")

    # And now we can create the primitive ops for the managers and lists,
    # which might reference each other

    for table in all_tables.itervalues():
        if not table.is_abstract:
            pretty.add_section("Manager: %s" % table.name, "")


#########################
## debug trace
#########################

def debug_msg(table, msg, self="Self"):
    if debug:
        return 'Debug_%s(%s, "%s");' % (table.row, self, msg)
    return ""


def add_debug_msg(pretty, table):
    if debug and not table.is_abstract:
        tmp = "& ".join(["Self.Data.%s'Img" % p.name for p in table.pk])
        if tmp:
            tmp = "& " + tmp
        body = """Trace(Me, Msg
           & "(%s, pk=" %s & ", "
           & System.Address_Image(Self.Data.all'Address)
           & ")");"""
        pretty.add_subprogram(
           name='Debug_%s' % table.row,
           params=[("Self", "Detached_%s'Class" % table.row),
                  ("msg", "String", '""')],
           body=body,
           section="debug")


##########################
## generate_orb_one_table
##########################

def generate_orb_one_table(name, schema, pretty, all_tables):
    """Generate Ada glue code for a specific table.
       out is an instance of file, writable"""

    table = schema.details[name]   # instance of dbgraph.Table

    add_debug_msg(pretty, table)

    schema.withs_for(name)

    tagged = ""
    if table.superClass:
        tagged = " and %s" % table.superClass.row

    translate = dict(
       cap=table.name,
       row=table.row,
       pkg_name=pkg_name,
       detached_data_fields=schema.detached_data_fields(table),
       tagged=tagged,
       field_count=len(table.fields) + len(table.fk),
       free_fields=schema.free_fields(table),
       equal="\n         and ".join(schema.equal(table)))

    if table.is_abstract:
        pretty.add_section(
           "Elements: %s" % table.name,
           """type %(row)s is interface;""" % translate)

    else:
        pretty.add_section("Types", """

   type %(row)s is new Orm_Element%(tagged)s with null record;
   type %(row)s_DDR is new Detached_Data (%(field_count)s) with private;
   type Detached_%(row)s is  --  Get() returns a %(row)s_DDR
      new Sessions.Detached_Element%(tagged)s with private;
   type Detached_%(row)s_Access is access all Detached_%(row)s'Class;
   No_Detached_%(row)s : constant Detached_%(row)s;
   No_%(row)s : constant %(row)s;""" % translate)

        pretty.add_section("Managers", """

   type I_%(cap)s_Managers is abstract new Manager with null record;
   package I_%(cap)s is new Generic_Managers
    (I_%(cap)s_Managers, %(row)s, Related_Depth, DBA.%(cap)s,
      Internal_Query_%(cap)s);
   subtype %(cap)s_Managers is I_%(cap)s.Manager;
   subtype %(cap)s_Stmt is I_%(cap)s.ORM_Prepared_Statement;

   All_%(cap)s : constant %(cap)s_Managers := I_%(cap)s.All_Managers;
   subtype %(row)s_List is I_%(cap)s.List;
   subtype Direct_%(row)s_List is I_%(cap)s.Direct_List;
   Empty_%(row)s_List : constant %(row)s_List := I_%(cap)s.Empty_List;
   Empty_Direct_%(row)s_List : constant Direct_%(row)s_List :=
     I_%(cap)s.Empty_Direct_List;
 """
                           % translate)

        detach(pretty, table, schema, translate)
        internal_query(pretty, table, schema)
        create(pretty, table, schema)

        pretty.add_constants(
          [("F_%s_%s" % (name, f.name), "constant", "%d" % index)
            for index, f in enumerate(table.fields) if f.show])
        pretty.add_unique_constants(
          [("""procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      ( %(row)s_DDR, %(row)s_Data)"""
            % translate,)]
           + schema.unchecked_free(name, all_tables))

        # Constants Counts_%s are the number of fields needed for %s and its
        # FK at each depth

        if table.revert_fk:
            counts = table.fields_count_array(False, max_depth=max_depth)
            lj_counts = table.fields_count_array(True,  max_depth=max_depth)
            counts = ["(%d,%d)" % (counts[c], lj_counts[c])
                      for c in range(len(counts))]
            pretty.add_constants(
              [("Counts_%s" % table.name,
                 "constant Counts", "(%s)" % ",".join(counts))])

        # Constants Upto_%s_%s is the index of the first fields for the second
        # %s when retrieving the fields for the first %s

        for index, fk in enumerate(table.fk):
            counts = table.fields_count_array(False, max_depth, fk)
            lj_counts = table.fields_count_array(True, max_depth, fk)
            counts = ["(%d,%d)" % (counts[c], lj_counts[c])
                      for c in range(len(counts))]
            pretty.add_constants(
              [("Upto_%s_%d" % (table.name, index),
                 "constant Counts", "(%s)" % ",".join(counts))])

        aliases = table.compute_table_aliases(max_depth=max_depth)

        if len(aliases) == 1:
            aliases = "0 => %d" % aliases[0]
        else:
            aliases = ",".join([str(a) for a in aliases])

        pretty.add_constants(
          [("Alias_%s" % table.name, "constant Alias_Array",
            "(%s)" % aliases)])

        if table.pk:
            when_not_in_cache = ("""
               declare
                 M : %(name)s_Managers := All_%(cap)s.Filter
                   (""" % {"name": table.name,
                           "cap": pretty._title(name)}
                 + ",".join(schema.call_create_params(name))
                 + """);
                 L : I_%(cap)s.List;
               begin
                 M.Select_Related
                   (Depth, Follow_Left_Join => Follow_Left_Join);
                 M.Limit (1);
                 L := M.Get(Session);
                 if not L.Has_Row then
                    return No_Detached_%(row)s;
                 else
                    declare
                       E : constant %(row)s := L.Element;
                    begin
                       --  Workaround bug in gnat which is missing a call
                       --  to Finalize if we do not reset the list (K321-012)
                       L := I_%(cap)s.Empty_List;
                       return E.Detach_No_Lookup (Session);
                    end;
                 end if;
               end;""" % translate)

            if table.has_cache:
                local = [("R",
                          "constant Detached_%s'Class" % table.row,
                          "From_Cache (Session, %s)" %
                          from_cache_params(schema, table, with_self=""))]
                body = ("""if not R.Is_Null then return R; else """ +
                        when_not_in_cache + """end if;""")
            else:
                local = []
                body = when_not_in_cache

            pretty.add_subprogram(
               name="get_%(row)s" % translate,
               params=[("Session", database_connection)]
                  + schema.params_get_pk(table)
                  + [("Depth",            "Related_Depth", "0"),
                     ("Follow_Left_Join", "Boolean", "False")],
               local_vars=local,
               body=body,
               returns="detached_%(row)s'Class" % translate,
               section="Manager: %(cap)s" % translate)

        if table.pk:
            pretty.add_subprogram(
               name='"="',
               params=[("op1", "%(row)s" % translate),
                       ("op2", "%(row)s" % translate)],
               body="return %(equal)s;" % translate,
               returns="boolean",
               section="Elements: %(cap)s" % translate)

            pretty.add_subprogram(
               name='"="',
               params=[("op1", "Detached_%(row)s" % translate),
                      ("op2", "Detached_%(row)s" % translate)],
               body="""if Op1.Get = null then return Op2.Get = null;
               elsif Op2.Get = null then return False; else
               return %(equal)s; end if;""" % translate,
               section="Elements: %(cap)s" % translate,
               returns="boolean",
               comment="""
                  Compares two elements using only the primary keys. All other
                  fields are ignored""")

        pretty.add_subprogram(
            name='new_%(row)s' % translate,
            returns="Detached_%(row)s'Class" % translate,
            section="Elements: %(cap)s" % translate,
            local_vars=[("Result", "Detached_%(row)s" % translate),
                        ("Data", "constant %(row)s_Data" % translate,
                         "new %(row)s_DDR" % translate)],
            body="Set (Result, Data); return Result;",
            comment="""
            Create a new element, but no attribute is set. Use Set_* to
            modify any attribute for which you need a value""")

        has_pk = []
        where = []
        assign = []
        getpk = ""
        execute = "R.Fetch"

        for index, f in enumerate(table.fields):
            if f.is_pk():
                has_pk.append(
                    "D.ORM_%s = %s" % (f.name, f.default_for_field()))
                where.append("DBA.%s.%s = %s" %
                             (table.name, f.name,
                              f.to_return("D")))
                if getpk != "":
                    getpk = "null; --  Can't retrieve multi-key PK"
                    execute = "Execute"
                elif f.type.sql_type != "integer":
                    getpk = "null;  --  Can only retrieve integer PK"
                    execute = "Execute"
                else:
                    getpk = ("D.ORM_%s := R.Last_Id (Self.Session.DB," \
                        + " DBA.%s.%s);") % (f.name, table.name, f.name)

            elif f.is_fk():
                tr = {"index": index + 1,
                      "table": table.name,
                      "row": translate["row"],
                      "name": f.name,
                      "default": f.default_for_field(),
                      "fk": f.fk.name,
                      "fkrow": f.fk.table.row,
                      "fkall": f.fk.to_return("D2"),
                      "value": f.to_return("D")}

                if translate["row"] == f.fk.table.row:
                    self_check = """
                      if Detached_%(row)s'Class (Self) =
                         D.ORM_FK_%(name)s.all
                      then
                         raise Self_Referencing with
                            "%(row)s is self referencing";
                      end if;
                      """ % tr
                else:
                    self_check = ""

                tr["self_check"] = self_check

                if f.fk.table.show:
                    assign.append(
                        """if Mask (%(index)d) then
                        if D.ORM_%(name)s /= %(default)s then
                          A := A & (DBA.%(table)s.%(name)s = %(value)s);
                        else
                           %(self_check)s
                           declare
                              D2 : constant %(fkrow)s_Data :=
                                 %(fkrow)s_data (D.ORM_FK_%(name)s.Get);
                           begin
                              if D2.ORM_%(fk)s = %(default)s then
                                 Self.Session.Insert_Or_Update
                                    (D.ORM_FK_%(name)s.all);
                              end if;

                              A := A & (DBA.%(table)s.%(name)s = %(fkall)s);
                           end;
                        end if;
                     end if;""" % tr)
                else:
                    assign.append(
                        """if Mask (%(index)d) then
                          A := A & (DBA.%(table)s.%(name)s = %(value)s);
                     end if;""" % tr)

            else:
                assign.append(
                    """if Mask (%d) then
                      A := A & (DBA.%s.%s = %s);
                    end if;""" %
                    (index + 1, table.name, f.name, f.to_return("D")))

        tr = {"table": table.name,
              "setassign": "\n".join(assign),
              "getpk": getpk,
              "execute": execute,
              "where": " AND ".join(where)}

        local_vars = [("D", "constant %(row)s_Data" % translate,
                       "%(row)s_Data (Self.Get)" % translate),
                      ("Q", "SQL_Query"),
                      ("A", "SQL_Assignment", "No_Assignment")]

        if has_pk:
            local_vars.append(("Missing_PK", "constant Boolean",
                               " or else ".join(has_pk)))

        if execute == "R.Fetch":
            local_vars.append(("R", "Forward_Cursor"))

        if has_pk:
            insert_or_update = """
                %(setassign)s
                if Missing_PK then
                   Q := SQL_Insert (A);
                else
                   Q := SQL_Update (DBA.%(table)s, A, %(where)s);
                end if;
                %(execute)s (Self.Session.DB, Q);

                if Missing_PK and then Success (Self.Session.DB) then
                   PK_Modified := True;
                   %(getpk)s
                end if;
                """ % tr
        else:
            insert_or_update = """
                %(setassign)s
                Q := SQL_Insert (A);
                %(execute)s (Self.Session.DB, Q);""" % tr

        pretty.add_subprogram(
            name="insert_or_update",
            params=[("self", "in out detached_%(row)s" % translate),
                    ("PK_Modified", "in out Boolean"),
                    ("Mask", "Dirty_Mask")],
            overriding=True,
            section="internal",
            local_vars=local_vars,
            body=insert_or_update)

        if has_pk:
            local_vars=[("D", "constant %(row)s_Data" % translate,
                         "%(row)s_Data (Self.Get)" % translate)]
            delete_body = ("Execute (Self.Session.DB,"
               + " SQL_Delete (DBA.%(table)s, %(where)s));") % tr
        else:
            local_vars = []
            delete_body = ('raise Program_Error with'
               + ' "Table %(cap)s has no primary key";') % translate

        pretty.add_subprogram(
            name="internal_delete",
            params=[("self", "detached_%(row)s" % translate)],
            overriding=True,
            section="internal",
            local_vars=local_vars,
            body=delete_body)

        on_add = ""
        for f in table.fk:
            if f.foreign.show:
                name = subprogram_from_fk(f)
                on_add += """if D.ORM_FK_%s /= null then
                   Self.Session.Persist (D.ORM_FK_%s.all);
                   end if;""" % (name, name)
        if on_add:
            pretty.add_subprogram(
                name="on_persist",
                overriding=True,
                params=[("self", "Detached_%(row)s" % translate)],
                local_vars=[("D", "constant %(row)s_Data" % translate,
                             "%(row)s_Data (Self.Get)" % translate)],
                section="internal",
                body="if Persist_Cascade (Self.Session) then "
                    + on_add + " end if;")

        translate["traces"] = debug_msg(table, "Free")
        pretty.add_subprogram(
           name="free",
           params=[("self", "in out %(row)s_DDR" % translate)],
           section="internal",
           overriding=True,
           body="""%(free_fields)s Free (Detached_Data (Self));""" % translate)

        if not table.is_abstract:
            if table.has_cache:
                unset = " or else ".join(
                    ["Self.ORM_%s = %s" % (p.name, p.type.default_record)
                     for p in table.pk])
                tmp = ", ".join([p.to_return("Self") for p in table.pk])
                body = ('if %s then' % unset
                 + '  return (%d, No_Primary_Key);' % (table.base_key, )
                 + ' else '
                 + '  return (%d, %s);' % (table.base_key, tmp)
                 + " end if;")

            else:
                body = """
  --  Not cachable, since the PK is not a single integer field
  return (%s, No_Primary_Key);""" % table.base_key

            pretty.add_subprogram(
               name="key",
               params=[("self", "%(row)s_DDR" % translate)],
               returns="Element_Key",
               section="internal",
               overriding=True,
               body=body)


    # Prepare the getters
    # Prepare the setters for simple values. These are only available for
    # the Detached_* types, since we need a place where to store the new
    # values

    for index, f in enumerate(table.fields):
        if f.show:
            pretty.add_subprogram(
               name=schema.subprogram_name_from_field(f),
               params=[("self", "%(row)s" % translate)],
               returns=f.type.ada_return,
               section="Elements: %(cap)s" % translate,
               abstract=table.is_abstract,
               comment=f.comment,
               body="return %s;" % f.ada_from_db(cursor="Self"))

            if not table.is_abstract:
                getter = "return %s;" % \
                        f.to_return("%s_Data (Self.Get)" % f.table.row)

                if f.is_pk():
                    # We must not change primary keys
                    setter = None
                    local = []
                else:
                    free = f.free_field("D")
                    if f.is_fk() and f.fk.table.show:
                        free += "Unchecked_Free (D.ORM_FK_%s);" \
                                % f.name

                    local = [("D", "constant %(row)s_Data" % translate,
                              "%(row)s_Data (Self.Get)" % translate)]
                    setter = """%sD.ORM_%s := %s;
                    Self.Set_Modified (%d);
                    """ % (free, f.name, f.to_field("Value"),
                           index + 1)

                pretty.add_property(
                    schema=schema,
                    row="%(row)s" % translate,
                    field=f,
                    getter=getter,
                    setter=setter,
                    setter_local_vars=local,
                    type=f.type.ada_return,
                    abstract=table.is_abstract,
                    section="Elements: %(cap)s" % translate)

    # Prepare the getters for foreign keys

    for index, fk_field in enumerate(table.fk):
        table_name = subprogram_from_fk(fk_field)

        # Do not generate code if we do not generate code for the external
        # table
        if fk_field.show():
            params = []
            detached_params = []
            reset_fk = []
            is_same = []
            free_fk = []

            for ffrom, fto in fk_field.pairs:
                params.append("%s => Self.%s"
                               % (fto.name,
                                  schema.subprogram_name_from_field(ffrom)))
                detached_params.append("%s => %s"
                               % (fto.name,
                                  ffrom.to_return("D")))
                reset_fk.append(
                    "D.ORM_%s := %s;" %
                    (ffrom.name,
                     fto.to_field("Value.%s"
                        % schema.subprogram_name_from_field(fto))))
                free_fk.append(ffrom.free_field("D"))
                is_same.append(
                    "%s = %s" % (fto.to_return("D"), ffrom.to_return("D")))

            body = """if Current (Self.Current) /= Self.Index then
               raise Cursor_Has_Moved;
            end if;

            if Self.Depth > 0"""

            if fk_field.can_be_null():
                body += " and then Self.Data.Follow_LJ"

            body += """ then
               return I_%(ref)s.Internal_Element
                 (Self,
                  Upto_%(cap)s_%(index)d (Self.Depth, Self.Data.Follow_LJ));
            else
               if not Dynamic_Fetching then
                  raise Field_Not_Available with
                     "Dynamic fetching disabled for %(name)s";
               end if;

               return All_%(ref)s.Filter (%(pk)s)
                  .Limit (1).Get (Self.Data.Session).Element;
            end if;
 """ % {"pkg_name": pkg_name, "name": table_name, "index": index,
        "ref": fk_field.foreign.name, "cap": table.name,
        "pk": ",".join(params)}

            pretty.add_subprogram(
               name=schema.subprogram_name_from_field(table_name),
               params=[("self", "%(row)s" % translate)],
               returns="%s'Class" % fk_field.foreign.row,
               abstract=table.is_abstract,
               section="Elements: %(cap)s" % translate,
               body=body)

            if not table.is_abstract:
                tr = {"pkg_name": pkg_name, "name": table_name,
                      "row": fk_field.foreign.row,
                      "is_same": " and ".join(is_same),
                      "pk": ",".join(detached_params)}

                if store_connections:
                    params = []
                else:
                    params = [("Session", database_connection)]

                getter = "if D.ORM_FK_%(name)s = null then " % tr

                if tr["row"] == translate["row"]:
                    getter += """
                     if %(is_same)s then
                        --  ??? Avoid reference cycle. Perhaps we could simply
                        --  avoid the cache for all foreign keys, and only
                        --  rely on the session cache for the elements instead
                        --  ??? Or use a weak reference
                        return Detached_%(row)s'Class (Self);
                     end if;
                     """ % tr

                getter += """if not Dynamic_Fetching then
                     raise Field_Not_Available with
                        "Dynamic fetching disabled for %(name)s";
                  end if;""" % tr

                if store_connections:
                    getter += """S := Session (Self);
                      if S = No_Session then
                          raise Field_Not_Available with
                              "Element is detached from any session";
                      end if;
                      D.ORM_FK_%(name)s := new Detached_%(row)s'Class'
                         (Get_%(row)s (S, %(pk)s));
                      """ % tr

                else:
                    getter += """
                       D.ORM_FK_%(name)s := new Detached_%(row)s'Class'
                       (Get_%(row)s (Session, %(pk)s));""" % tr

                getter += """end if; return D.ORM_FK_%(name)s.all;""" % tr

                setlocal = [("D", "constant %(row)s_Data" % translate,
                             "%(row)s_Data (Self.Get)" % translate)]
                free = ffrom.free_field("D") \
                    + "".join(free_fk) \
                    + "Unchecked_Free (D.ORM_FK_%s);" % table_name \
                    + "\n".join(reset_fk)
                setter = ""
                for idx, f2 in enumerate(table.fields):
                    if f2 == ffrom:
                        setter = \
                    """%sD.ORM_FK_%s := new Detached_%s'Class'(Value);

                    Self.Set_Modified (%d);
                    if Persist_Cascade (Self.Session) then
                       Self.Session.Persist (D.ORM_FK_%s.all);
                    end if;
                    """ % (free, table_name, tr["row"], idx + 1, table_name)

                pretty.add_property(
                    schema=schema,
                    row="%(row)s" % translate,
                    field=table_name,
                    getter=getter,
                    setter=setter,
                    setter_local_vars=setlocal,
                    getter_local_vars=[
                        ("D", "constant %(row)s_Data" % translate,
                         "%(row)s_Data (Self.Get)" % translate),
                        ("S", "Session_Type")],
                    type="Detached_%s'Class" % fk_field.foreign.row,
                    abstract=table.is_abstract,
                    section="Elements: %(cap)s" % translate)

#                if not table.is_abstract:
#                    # From a table's manager, get a manager for the tables
#                    # that are related via a FK.
#
#                    pretty.add_subprogram(
#                       name=schema.subprogram_name_from_field(table_name),
#                       params=[("self",
#                                "I_%(row)s_Managers'Class" % translate)],
#                       returns="%s_Managers" % fk_field.foreign.row,
#                       section="Manager: %(cap)s" % translate,
#                       local_vars=[("Q", "constant SQL_Query",
#                                      "I_%s.Build_Query(Self, +DBA.%s.%s)"
#                                    % (table.name,
#                                       table.name,
#                                       table_name))],
#                       body="""return All_%s.Filter
#        (SQL_In(DBA.%s.%s, Q));""" % (
#                          fk_field.foreign.name,
#                          fk_field.foreign.name,
#                          fk.pairs[0][1].name))


    # Generate revert-FK getters for elements

    for fk in table.revert_fk:      # fk.foreign is always table
        foreign = fk.pairs[0][0].table  # revert the FK relationship
        if fk.revert and fk.foreign.show and foreign.show:
            pretty.add_subprogram(
               name=schema.subprogram_name_from_field(fk.revert),
               params=[("self", "%(row)s'Class" % translate)],
               returns="%s_Managers" % foreign.name,
               section="Manager: %(cap)s" % translate,
               abstract=table.is_abstract,
               body="return All_%s.Filter(%s => Self.%s);" \
                  % (foreign.name,
                     fk.pairs[0][0].name,
                     fk.pairs[0][1].name))

            if not table.is_abstract:
                pretty.add_subprogram(
                   name=schema.subprogram_name_from_field(fk.revert),
                   params=[("self", "Detached_%(row)s'Class" % translate)],
                   returns="%s_Managers" % foreign.name,
                   section="Manager: %(cap)s" % translate,
                   abstract=table.is_abstract,
                   body="return All_%s.Filter (%s => Self.%s);" \
                      % (foreign.name,
                         fk.pairs[0][0].name,
                         fk.pairs[0][1].name))

            pretty.add_subprogram(
               name=schema.subprogram_name_from_field(fk.revert),
               params=[("self", "I_%(cap)s_Managers'Class" % translate)],
               returns="%s_Managers" % foreign.name,
               section="Manager: %(cap)s" % translate,
               abstract=table.is_abstract,
               local_vars=[("Q", "constant SQL_Query",
                              "I_%s.Build_Query(Self, +DBA.%s.%s)"
                            % (table.name, table.name, fk.pairs[0][1].name))],
               body="""return All_%s.Filter
        (SQL_In(DBA.%s.%s, Q));""" % (
                  foreign.name,
                  foreign.name,
                  fk.pairs[0][0].name))

    if not table.is_abstract:
        pretty.add_private_before("""
    type %(row)s_DDR is new Detached_Data (%(field_count)s) with record
%(detached_data_fields)s;
    end record;
    type %(row)s_Data is access all %(row)s_DDR;
    """ % translate)
        pretty.add_private_after("""
    type Detached_%(row)s
       is new Sessions.Detached_Element%(tagged)s with null record;
    No_%(row)s : constant %(row)s :=(No_Orm_Element with null record);
    No_Detached_%(row)s : constant Detached_%(row)s :=
      (Sessions.Detached_Element with null record);
 """ % translate)


#################
## generate_orm
#################

def generate_orm(setup, pkg_name, tables=[], omit=[], out=sys.stdout):
    """Generate ORB packages for a set of tables.
       SETUP is an object that contains the database description(.db_name,...)
       Do not generate data for the fields in OMIT("table.field").
    """

    pretty = Pretty_Printer(
       out, casing=["Follow_LJ", "GNAT", "GNATCOLL", "SQL",
                    "SQL_Table_List", "SQL_Query",
                    "SQL_Field_List", "SQL_Field"])
    schema = Schema(setup, tables, pretty, all_tables=tables, omit=omit)
    if not tables:
        tables = schema.details
    else:
        t = dict()
        for p in tables:
            t[p] = schema.details[p.lower()]
        tables = t

    pretty.start_package(pkg_name)

    # First generate abstract tables, since other fields will inherit from
    # them

    order_sections(schema, pretty, tables)

    pretty.add_subprogram(
        name="Str_Or_Empty",
        params=[("str", "access String")],
        returns="String",
        body='if Str = null then return ""; else return Str.all; end if;',
        section="body")

    table_image_body = "case Table is"

    for t in sorted(tables):
        table_image_body += ' when %d => return "%s";' % (
            tables[t].base_key, tables[t].name)
        if tables[t].is_abstract:
            generate_orb_one_table(t, schema, pretty, tables)

    for t in sorted(tables):
        if not tables[t].is_abstract:
            generate_orb_one_table(t, schema, pretty, tables)

    table_image_body += " when others => return Table'Image; end case;"
    #pretty.add_subprogram(
    #    name="Image",
    #    params=[("Table", "Integer")],
    #    returns="String",
    #    body=table_image_body,
    #    section="Internal")

    pretty.terminate_package(need_dba=True)


class DBSetup(object):
    """This class describes how to get the schema from a database. This
       schema can either be read from a file, or from a live database. This
       class abstracts those accesses.
    """

    @staticmethod
    def from_file(filename):
        result = DBSetup()
        result.db_model = os.path.abspath(filename)
        return result

    @staticmethod
    def from_live_db(dbname, dbuser, dbhost="localhost",
                     dbpassword="", dbtype='sqlite'):
        result = DBSetup()
        result.db_name = dbname
        result.db_type = dbtype
        result.db_user = dbuser
        result.db_host = dbhost
        result.db_password = dbpassword

    def gnatcoll_db2ada(self, args=[]):
        """Executes gnatcoll_db2ada, passing the required args to access
           the database model, and extra arguments if needed.
        """
        if hasattr(self, "db_model"):
            p = exec_or_fail(
                ["gnatcoll_db2ada", "-dbmodel", self.db_model] + args,
                stdout=subprocess.PIPE)

        else:
            p = exec_or_fail(
              ["gnatcoll_db2ada", "-dbhost", self.db_host,
                "-dbname", self.db_name, "dbtype", self.db_type,
                "-dbuser", self.db_user,
                "-dbpasswd", self.db_password] + args,
              stdout=subprocess.PIPE)

        return p.stdout

    def get_schema(self):
        """Returns a textual description of the schema of the database"""
        if hasattr(self, "db_model"):
            return file(self.db_model).read()
        else:
            return self.gnatcoll_db2ada(["-text"]).read()


@save_dir
def create_orm(setup, pkg_name, indir, tables=[], omit=[]):
    """Creates INDIR/orm-* based on the database defined in the config.
       First remove any orm-*.ad? in that directory
       TABLES can be used to limit the generation to a subset of the tables.
       OMIT is used to remove some of the tables from the set.
       Return 1 in case of error, 0 for success (exit status for the shell)
    """

    for dirpath, dirnames, filenames in os.walk(indir):
        for f in filenames:
            if os.path.splitext(f)[1] in(".ads", ".adb") \
               and f.startswith("orm-"):
                unlink_if_exist(os.path.join(dirpath, f))

    os.chdir(indir)
    out = file("tmp_orm", "w")
    try:
        generate_orm(setup,
                     pkg_name=pkg_name,
                     tables=tables,
                     omit=omit,  # omit circular deps
                     out=out)
        out.close()
        exec_or_fail(["gnatchop", "-q", "-w", "-gnat05", "tmp_orm"])

    except Cannot_Parse_Schema:
        return 1

    unlink_if_exist("tmp_orm")
    return 0


class Field_Type(object):
    def __init__(self,
                 sql_type,        # SQL type
                 ada_return,      # As an Ada return value
                 ada_param,       # As an Ada subprogram parameter
                 default_param,   # default value for the Get parameter
                 ada_field,       # As an Ada record field
                 default_record,  # default value for the record
                 value_from_db,   # Ada value from db (%1=Cursor, %2=index)
                 to_return,       # convert from field type to return type
                 free_field,      # How to free the field
                 img,             # From a field type to a string
                 to_field):       # Convert from ada_param to ada_field
        self.sql_type = sql_type
        self.ada_return = ada_return
        self.ada_param = ada_param
        self.default_param = default_param
        self.ada_field = ada_field
        self.default_record = default_record
        self._value_from_db = value_from_db
        self._to_return = to_return  # %1 => value to convert
        self._to_field = to_field   # %1 => entity to convert
        self._img = img  # %1 => value to convert
        self._free_field = free_field

    __all_types = None

    @staticmethod
    def get(sql):
        if not Field_Type.__all_types:
            Field_Type.__all_types = dict(
               text=Field_Type(
                  # Map to Unbounded_String. This avoids having to do explicit
                  # memory management, and in particular we can take advantage
                  # of copy-on-write done for Unbounded_String rather than
                  # redo it ourselves (with complex support for multi-tasking)
                  "text", "String", "String", 'No_Update',
                  "GNAT.Strings.String_Access",
                  "null", "String_Value (%s, %s)",
                  "Str_Or_Empty (%s)", "Free (%s)", "%s",
                  "new String'(%s)"),
               integer=Field_Type(
                  "integer", "Integer", "Integer", -1, "Integer", -1,
                  "Integer_Value (%s, %s)", "%s", "", "%s'Img", "%s"),
               autoincrement=Field_Type(
                  "integer", "Integer", "Integer", -1, "Integer", -1,
                  "Integer_Value (%s, %s)", "%s", "", "%s'Img", "%s"),
               time=Field_Type(
                  "time", "Ada.Calendar.Time", "Ada.Calendar.Time", "No_Time",
                  "Ada.Calendar.Time", "No_Time",
                   "Time_Value (%s, %s)", "%s", "", "%s'Img", "%s"),
               float=Field_Type(
                  "float", "Float", "Float", "Float'First", "Float",
                   "Float'First", "Float_Value (%s, %s)", "%s", "",
                   "%s'Img", "%s"),
               boolean=Field_Type(
                  "boolean", "Boolean", "TriBoolean", "Indeterminate",
                   "Boolean", "False", "Boolean_Value (%s, %s)",
                   "%s", "", "%s'Img", "%s"),
               json=Field_Type(
                  "json", "String", "String", "No_Update",
                   "GNAT.Strings.String_Access",
                   "null", "String_Value (%s, %s)",
                   "Str_Or_Empty (%s)", "Free (%s)", "%s",
                   "new String'(%s)"),
               money=Field_Type(
                  "money", "GNATCOLL.SQL.T_Money",
                   "GNATCOLL.SQL.T_Money", "GNATCOLL.SQL.T_Money'First",
                   "GNATCOLL.SQL.T_Money", "GNATCOLL.SQL.T_Money'First",
                   "Money_Value (%s, %s)", "%s", "", "%s'Img", "%s"))

        sql = sql.lower()
        if sql in ("timestamp without time zone",
                   "timestamp with time zone",
                   "timestamp",
                   "date"):
            sql = "time"
        elif sql.startswith("character"):
            sql = "text"

        try:
            return Field_Type.__all_types[sql]
        except:
            sys.stderr.write("Unsupported SQL type: %s\n" % sql)
            raise


class Field(object):
    """A field in a database table"""

    def __init__(self, table, name, type, default, comment, null, show):
        """SHOW is whether we should generate subprograms for it or not"""
        self.table = table  # Instance of Table
        self.name = name.title()  # Normalized name
        self.type = type   # string, will be replaced with Field_Type
        self.comment = comment
        self.null = null
        self.show = show

        self._fk = None
        self.pk = False  # This field is a primary key

        self.__default = default  # See default_field () instead
        if default.find("::") != -1:
            self.__default = default[:default.find('::')]  # Remove type casts

    def __repr__(self):
        return "Field<%s.%s>" % (self.table.name, self.name)

    @property
    def fk(self):
        """If SELF is a foreign key, return a pointer to the field in the
           foreign table
        """
        if self._fk is None:
            for f in self.table.fk:
                for p in f.pairs:
                    if self == p[0]:
                        self._fk = p[1]
                        break
        return self._fk

    def field_from_db(self, cursor):
        """Retrieves a field value by reading the current row from CURSOR
           and checking the specific TABLE.FIELD
        """
        return self.to_field(entity=self.ada_from_db(cursor))

    def ada_from_db(self, cursor):
        """Retrieves an Ada value by reading the current row from CURSOR"""
        return self.type._value_from_db % (
            cursor, "F_%s_%s" % (self.table.name, self.name))

    def free_field(self, element):
        """Returns the string used to free ELEMENT.FIELD if needed"""
        if self.type._free_field:
            return self.type._free_field % (
                "%s.ORM_%s" % (element, self.name),) + ";"
        else:
            return ""

    def to_field(self, entity):
        """Converts ENTITY to the proper type to use in a record"""
        return self.type._to_field % entity

    def to_return(self, data):
        """Return the value stored in memory to the proper return type"""
        return self.type._to_return % ("%s.ORM_%s" % (data, self.name))

    def image(self, data=""):
        """Converts a value to a string, for display"""
        if data == "":
            return self.type._img % ("%s" % self.name)
        else:
            return self.type._img % ("%s.ORM_%s" % (data, self.name))

    def is_fk(self):
        """Whether SELF is a foreign key for its table, ie a field referencing
           another table
        """
        return self.fk is not None

    def is_pk(self):
        """Whether SELF is a primary key for its table"""
        for f in self.table.pk:
            if f == self:
                return True
        return False

    def default_for_field(self):
        """Return the default value to use for a record field"""
        if self.__default:
            if self.__default.lower().find("now()") != -1 \
               or self.__default.lower() == "now":
                return "Clock"

            elif self.__default.lower().find("'now'") != -1:
                return "Clock"

            elif self.__default == "t":
                return "True"

            elif self.__default == "f":
                return "False"

            elif not self.__default.startswith("nextval"):
                if self.__default == "''":
                    return "Null_Unbounded_String"
                elif self.__default[0] == "'":
                    return 'To_Unbounded_String ("' \
                            + self.__default[1:-1] + '")'
                else:
                    return self.__default.title()

        return self.type.default_record

    def resolve_fk(self, all_tables):
        """Lookup the type of field if it was unknown.
        """

        if self.type.startswith("FK "):
            # Look for the actual type, recursively since the PK of
            # the foreign table might itself be a FK
            pk = self.type
            while isinstance(pk, str) and pk.startswith("FK "):
                descr = Foreign_Key.parse_fk_descr(self, self.name, pk)
                if len(all_tables[descr.foreign_name].pk) > 1:
                    print ("Error: '%s.%s' references '%s', which has more"
                           + " than one PK") % (
                               self.table.name, self.name, descr.foreign_name)
                pk = all_tables[descr.foreign_name].pk[0].type

            if isinstance(pk, str):
                self.type = Field_Type.get(pk)
            else:
                self.type = pk
        else:
            self.type = Field_Type.get(self.type)


class Foreign_Key(object):
    def __init__(self, from_name, foreign_name, pairs, revert):
        """FOREIGN_NAME is the name of the foreign table we are referencing
           PAIRS is a list of tuples (from, to), where from is the name of a
                 field in FROM_NAME, and to is the name of a field in FOREIGN.
        """
        self.from_name = from_name     # A string, will be deleted
        self.foreign_name = foreign_name  # A string, will be deleted
        self.foreign = None   # A Table, will be resolved later
        self.pairs = pairs  # Initially  strings, but afterward Fields
        self.revert = revert  # Empty string if should be ignored

    def resolve_fk(self, all_tables):
        """Once we know all tables, complete the definition of foreign keys
           by pointing to the right table instances and completing the pairs
        """

        table = all_tables[self.from_name.lower()]
        self.foreign = all_tables[self.foreign_name.lower()]
        del self.foreign_name
        del self.from_name

        for index, p in enumerate(self.pairs):
            if p[1] is None:   # references the PK of the foreign table
                self.pairs[index] = (table.get_field(p[0]), self.foreign.pk[0])
            else:
                self.pairs[index] = (table.get_field(p[0]),
                                     self.foreign.get_field(p[1]))
            if self.pairs[index][0] is None:
                print "Couldn't resolve field %s.%s" % (table, p[0])

    def can_be_null(self):
        """Whether the foreign key can be NULL.
           If it must always be set, False is returned
        """
        for p in self.pairs:
            if p[0].null:
                return True
        return False

    def show(self):
        """Whether we should display this foreign key"""
        if not self.foreign.show:
            return False
        for p in self.pairs:
            if not p[0].show:
                return False
        return True

    @staticmethod
    def parse_fk_descr(table, field_name, descr):
        """Parse a description of a foreign key in the definition of TABLE.
           DESCR contains text like "FK table(revert)".
        """
        pairs = [(field_name, None)]
        revert = re.search("\((.*)\)", descr)
        if revert:
            return Foreign_Key(table.name,
                                descr[3:revert.start(1) - 1],
                                pairs,
                                revert.group(1))
        else:
            return Foreign_Key(table.name,
               descr[3:], pairs, table.name + "_" + field_name + "_id")


class Table(object):
    """Describes a database table"""

    base_key = 0

    def __init__(self, name, row, show=True, is_abstract=False,
                 superClass=None):
        """FIELDS is a dictionary of Field, indexed by the name of the field
           PK is a list of Field that make up the primary key
           FK is a list of Foreign_Key
           ROW is the name of a row instance ("book" if table is "books")
           SHOW is true if we should do some output for self
        """
        self.name = name.title()  # string
        self.fields = []      # list of Field
        self.fk = []      # list of Foreign_Key
        self.pk = []      # list of Field
        self.row = row.title()     # string
        self.is_abstract = is_abstract
        self.show = show
        self.superClass = superClass  # a Str, then will be instance of Table
        self.revert_fk = []   # the FK from other tables that point to self

        self.base_key = Table.base_key
        Table.base_key += 1000000

        self.has_cache = True
        # Whether these items can be cached in the database. When they can't,
        # for instance because their PK is not a single integer field. In this
        # case, we generate a dummy Key function for the table, which always
        # indicate there is no PK, and the element will not be saved in the
        # session cache as a result. We also need to make sure that no other
        # part of the generated file uses From_Cache.

    def __repr__(self):
        return "Table<%s>" % self.name

    def fields_count(self, depth, follow_lj, until=""):
        """Remove the count of fields that in SELF and all its fk-related
           tables up to DEPTH. If FOLLOW_LF is False, ignore those fk that can
           be null.  Stops when we find a FK equal to UNTIL, if the latter is
           specified.
        """
        result = len(self.fields)
        if depth > 0:
            for fk in self.fk:
                if fk == until:
                    break
                if follow_lj or not fk.can_be_null():
                    result += fk.foreign.fields_count(depth - 1, follow_lj)
        return result

    def fields_count_array(self, follow_lj, max_depth, until=""):
        """Same as fields_count, but returns a list with all possible fields
           count at any depth. MAX_DEPTH is used to avoid infinite recursion
           (a table that references itself for instance)
        """
        result = []
        depth = 0
        while depth <= max_depth:
            result.append(self.fields_count(depth, follow_lj, until=until))
            depth += 1
        return result

    def get_field(self, name):
        """Retrieve field, by name"""
        name = name.lower()
        for f in self.fields:
            if f.name.lower() == name:
                return f
        return None

    def compute_table_aliases(self, max_depth):
        """See description of ORM.Alias_Array"""

        class Outscope:  # so that internal can modify alias
            pass

        Outscope.seen = set()  # tables already in the query
        Outscope.alias = 0      # current alias

        def internal(table, alias, start_index, depth):
            """Compute array for table TABLE, where START_INDEX is its first
               entry in the resulting array. ALIAS is the alias to use for
               TABLE
            """

            local = [alias]

            # First compute the aliases for all FK (breadth first search), so
            # that smaller numbers are used for small depth

            if depth < max_depth and table.fk:
                aliases = []
                for fk in table.fk:
                    if fk.foreign.name in Outscope.seen:
                        aliases.append(Outscope.alias)
                        Outscope.alias += 1
                    else:
                        aliases.append(-1)
                        Outscope.seen.add(fk.foreign.name)

                local += [-2] * len(table.fk)   # reserve space

                for index, fk in enumerate(table.fk):
                    local[index + 1] = start_index + len(local)  # Jump
                    local += internal(
                        fk.foreign, aliases[index], local[index + 1],
                        depth + 1)

            return local

        Outscope.seen.add(self.name)
        return internal(self, alias=-1, start_index=0, depth=0)

    def resolve_fk(self, all_tables):
        """Lookup the type of fields that were defined as "FK ..."
           ALL_TABLES must contain the set of all tables in the database.
           Return False in case of error.

           This is called immediately after parsing the database schema.
        """

        for f in self.fk:
            f.resolve_fk(all_tables)
            f.foreign.revert_fk.append(f)  # revert FK relationships

        for f in self.fields:
            f.resolve_fk(all_tables)

        if self.superClass:  # Add fields inherited from abstract parent
            self.superClass = all_tables[self.superClass]

            for f in self.superClass.fields:
                f = copy.copy(f)
                f.table = self
                self.fields.append(f)
                if f.pk:
                    self.pk.append(f)

        if len(self.pk) != 1:
            self.has_cache = False
        elif self.pk[0].type.ada_param != "Integer":
            self.has_cache = False


def get_db_schema(setup, requires_pk=False, all_tables=[], omit=[]):
    """Parse the schema of the database (database access is described in
       setup, which is an instance of configfile.ConfigFile).
       If requires_pk is True, a warning is raised for all tables that have
       no PRIMARY KEY. Views never generate a warning.
       OMIT is the list of fields to omit (in the form "table.field").
       ALL_TABLES, if specified, lists the tables for which we do output.
    """

    schematxt = setup.get_schema()
    tables = dict()  # Index is lower cases table name
    table = None      # Instance of Table

    for line in schematxt.splitlines():
        if line.startswith("|") and not line.startswith("|--"):
            fields = line.split("|")
            fields[1] = fields[1].strip()
            fields[2] = fields[2].strip()

            if fields[1].startswith("VIEW") \
               or fields[1].startswith("TABLE") \
               or fields[1].startswith("ABSTRACT TABLE"):

                m = re.search("\((.*)\)", fields[1])
                if m:
                    superclass = m.group(1)
                else:
                    superclass = None

                if len(fields) > 3 and fields[3].strip():
                    table_row = fields[3].strip()
                else:
                    table_row = fields[2]

                table = Table(name=fields[2],
                               row=table_row,
                               superClass=superclass,
                               show=all_tables == []
                                    or fields[2] in all_tables,
                               is_abstract=fields[1].startswith("ABSTRACT"))
                tables[table.name.lower()] = table

            elif table and len(fields) > 3:
                # Ignores VIEWs
                fields[3] = fields[3].strip()

                if fields[1] == "FK:":
                    pairs = []
                    to = fields[4].strip().split(" ")

                    for index, f in enumerate(fields[3].split(" ")):
                        pairs.append((f, to[index]))

                    table.fk.append(
                        Foreign_Key(table.name, fields[2], pairs, ''))

                elif fields[1] == "INDEX:":
                    # Skip, no influence here
                    pass

                else:
                    if fields[2].startswith("FK "):
                        table.fk.append(
                           Foreign_Key.parse_fk_descr(
                               table, fields[1], fields[2]))

                    null = fields[3].find("NOT NULL") == -1 \
                            and fields[3].find("PK") == -1

                    field = Field(table=table,
                                   name=fields[1],
                                   type=fields[2],
                                   default=fields[4].strip(),
                                   comment=fields[5].strip(),
                                   null=null,
                                   show="%s.%s" % (table.name, fields[1])
                                        not in omit)
                    table.fields.append(field)
                    if fields[3] == "PK":
                        field.pk = True
                        table.pk.append(field)

    # Now resolve all foreign keys

    for t in tables.itervalues():
        t.resolve_fk(tables)

    return tables


##########
# Graphs #
##########

class Graph(object):
    default_color = "white"
    bg_color = "palegoldenrod"
    font = "tahoma"

    @staticmethod
    def create(setup, output_file, clusters=dict()):
        # CLUSTERS can be used to group tables. Grouping is done either
        # just be settings a specific color for their title bars, but you can
        # also force graphviz to group them visually by calling the groups with
        # names starting with "cluster_".
        # The keys in this dict are the names of the group. Each value is a
        # tuple, whose first element is the color to use for that group, and the
        # remaining elements are the names of the database tables belonging to
        # that group. For instance:
        #    clusters ["group1"] = ("color", "table1", "table2")

        tables = get_db_schema(setup)

        abs_output_file = os.path.abspath(output_file)
        output = file(abs_output_file, "w")
        output.write("""
digraph g {
  graph [
    rankdir = "TD",
    //rankdir = "LR",
    concentrate = true,     // Allow edge concentration
    //page="11.69291,16.53",  // Output on multiple pages (A3), inches
    size="15.5,20.5",       // Force output on single page (inches)
    rotate=90,              // Landscape mode
    nodesep=0,
    ranksep=0,
    margin=0
  ];

  node [
    fontsize = 8,
    fontname = "%(font)s",
    shape = plaintext
  ];

  edge [ ];

  "legend" [label=<<TABLE CELLBORDER="0" CELLSPACING="0">
<TR><TD>Legend</TD></TR>
""" % {"font": Graph.font})

        for c in sorted(clusters.keys()):
            output.write(
              '<TR><TD BGCOLOR="%s">%s</TD></TR>\n' % (clusters[c][0], c))

        output.write("""</TABLE>>, layer=0]""")

        colors = dict()

        for c in clusters.keys():
            t = clusters[c][1:]
            notfound = [ta for ta in t if ta not in tables]
            if notfound:
                print \
                   "Tables referenced in clusters, but no longer exists: %s" \
                   % " ".join(notfound)
            output.write(""" subgraph "%s" { %s }""" % (c, " ".join(t)))
            for col in t:
                colors[col.lower()] = clusters[c][0]

        for table in tables.itervalues():
            attrs = ""
            for field in table.fields:
                trans = {"bg":   Graph.bg_color,
                         "name": field.name}  # + " : " + field.type.sql_type}

                if field.is_fk():
                    if field.is_pk():
                        html = '<FONT FACE="bold" COLOR="blue">%(name)s</FONT>'
                    else:
                        html = '<FONT COLOR="blue">%(name)s</FONT>'
                else:
                    if field.is_pk():
                        html = '<FONT FACE="bold">%(name)s</FONT>'
                    else:
                        html = '%(name)s'

                attrs += ('<TR><TD BGCOLOR="%(bg)s">'
                          + html + "</TD></TR>") % trans

            output.write("""
 "%s" [label=<<TABLE BORDER="1" CELLBORDER="0" CELLSPACING="0"><TR>
                     <TD BGCOLOR="%s">%s</TD></TR>%s</TABLE>>]\n"""
                 % (table.name.lower(),
                    colors.get(table.name.lower(), Graph.default_color),
                 table.name, attrs))

        for table in tables.itervalues():
            for f in table.fk:
                output.write('"%s" -> "%s";\n' % (
                    table.name.lower(), f.foreign.name.lower()))

        output.write("}")
        output.close()

        abs_ps = abs_output_file.replace(".dot", ".ps")
        ps = output_file.replace(".dot", ".ps")
        try:
            sub = subprocess.Popen(
                ["dot", "-Tps", "-o", abs_ps, abs_output_file])
            status = sub.wait()
        except OSError:
            status = 1

        if status != 0:
            print "Created '%s'" % output_file
            print "Use 'dot -Tps -o %s %s' to convert to PS" % (
                ps, output_file)
        else:
            print "Created '%s'" % abs_ps
        print "Use 'ps2pdf -sPAGESIZE=a3' to convert to PDF"


########
# Main #
########

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print "Usage: dborm -ada   dbschema.txt [pkg_name] [db_pkg]" \
            + " [output_dir] [tables]"
        print "   or: dborm -graph dbschema.txt [clusters]"
        print ""
        print "Where dbschema.txt contains the description of the database"
        print "as described in the Gnat Components Collection documentation."
        print ""
        print "Clusters describes the grouping to use for tables in the"
        print "graph output. These are any number of arguments of the form:"
        print "    name:bg:table1,table2,table3,..."
        sys.exit(0)

    db = DBSetup.from_file(filename=sys.argv[2])

    if sys.argv[1] == "-ada":
        pkg = "%s.%s" % (pkg_name, "Queries")
        output_dir = "."

        if len(sys.argv) >= 4:
            pkg = sys.argv[3]
        if len(sys.argv) >= 5:
            database_pkg = sys.argv[4]
        if len(sys.argv) >= 6:
            output_dir = sys.argv[5]
        if len(sys.argv) >= 7 and sys.argv[6] != '':
            tables = sys.argv[6].split(",")
        else:
            tables = []

        sys.exit(create_orm(db, indir=output_dir, omit=[], pkg_name=pkg,
                            tables=tables))

    elif sys.argv[1] == "-graph":
        clusters = dict()
        for s in sys.argv[3:]:
            name, bg, tables = s.split(":")
            clusters[name] = [bg] + tables.split(",")
        Graph.create(db, output_file="schema.dot", clusters=clusters)
