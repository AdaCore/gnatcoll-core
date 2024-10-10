# -*- coding: utf-8 -*-
#
# GNATColl documentation build configuration file, created by
# sphinx-quickstart on Wed Dec  7 11:02:44 2011.

import time

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom ones.
extensions = [
    "sphinx.ext.autodoc",
    "sphinx.ext.coverage",
    "sphinx.ext.ifconfig",
    "sphinx_rtd_theme",
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ["_templates"]

# The suffix of source filenames.
source_suffix = ".rst"

# The master toctree document.
master_doc = "index"


def get_copyright():
    return "2007-%s, AdaCore" % time.strftime("%Y")


# General information about the project.
project = "GNATColl"
copyright = get_copyright()


def get_version():
    """Extract the version from configure.in"""
    with open("../core/VERSION") as f:
        return f.readline().strip()


version = get_version()

release = version

exclude_patterns = ["_build"]

pygments_style = None

# HTML options
html_theme = "sphinx_rtd_theme"
html_theme_options = {
    # Use AdaCore blue in the Table Of Content
    "style_nav_header_background": "#12284c",
}
html_logo = "adacore-logo-white.png"
html_favicon = "favicon.ico"
html_static_path = ["_static"]
html_show_sourcelink = False
html_show_sphinx = False

htmlhelp_basename = "GNATColldoc"

# Latex options
latex_documents = [
    ("index", "GNATColl.tex", "GNATColl Documentation", "AdaCore", "manual"),
]

# Man pages options
man_pages = [("index", "gnatcoll", "GNATColl Documentation", ["AdaCore"], 1)]

# Epub options
epub_title = "GNATColl"
epub_author = "AdaCore"
epub_publisher = "AdaCore"
epub_copyright = copyright
