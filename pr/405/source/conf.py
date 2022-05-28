# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))


# -- Project information -----------------------------------------------------

project = 'Fortran-lang.org website'
copyright = '2022, Fortran Community'
author = 'Fortran Community'

# The full version, including alpha/beta/rc tags
release = '1.0.0'


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    "ablog",
    "myst_parser",
    "sphinx_design",
    "sphinx_copybutton",
    "sphinx.ext.intersphinx",
]
myst_enable_extensions = [
    "colon_fence",
    "deflist",
    "substitution",
    "html_image",
]


# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# The language for content autogenerated by Sphinx. Refer to documentation
# for a list of supported languages.
#
# This is also used if you do content translation via gettext catalogs.
# Usually you set "language" from the command line for these cases.
language = 'None'

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = []


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = "pydata_sphinx_theme"

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']


html_theme_options = {
    "favicons" : [
        {
            "rel": "icon",
            "href": "images/favicon.ico",
        },
    ],
    "page_sidebar_items": [],
    "footer_items": ["copyright"],
    "navbar_align": "content",
    "navbar_start": ["navbar-logo"],
    "navbar_center": ["navbar-nav"],
    "navbar_end": ["navbar-icon-links"],
    "icon_links": [
        {
            "name": "Discourse",
            "url": "https://fortran-lang.discourse.group/",
            "icon": "fab fa-discourse",
        },
        {
            "name": "Twitter",
            "url": "https://twitter.com/fortranlang",
            "icon": "fab fa-twitter",
        },
        {
            "name": "GitHub",
            "url": "https://github.com/fortran-lang",
            "icon": "fab fa-github",
        },
        {
            "name": "RSS",
            "url": "https://fortran-lang.org/news.xml",
            "icon": "fas fa-rss",
        },
    ]
}


html_title = "Fortran Programming Language"
html_logo = "_static/images/fortran-logo-256x256.png"
html_baseurl = "https://henilp105.github.io/fortran-lang.org/"

master_doc = 'index'

panels_add_bootstrap_css = False

blog_path = "news"
blog_post_pattern = "_posts/*/*"
post_redirect_refresh = 1
post_auto_image = 1
post_auto_excerpt = 2

def hide_h1_on_index_pages(app, pagename, templatename, context, doctree):
    if pagename in ["index", "learn", "compilers", "community", "packages"]:
        app.add_css_file("css/hide_h1.css")

def setup(app):
    app.connect('html-page-context', hide_h1_on_index_pages)