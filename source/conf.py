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
import yaml
from pathlib import Path
from collections import Counter
import json
import requests
import datetime
#print("learn section")
conf = yaml.safe_load(Path('_data/learning.yml').read_text())
#print(conf['books'])
fortran_index = yaml.safe_load(Path('_data/package_index.yml').read_text())

fortran_index_tags = []
fortran_index_tags_50 = []
fortran_index_categories = []
fortran_index_libraries = []
fortran_index_data_types = []
fortran_index_strings = []
fortran_index_programming = []
fortran_index_graphics = []
fortran_index_interfaces = []
fortran_index_examples = []
fortran_index_scientific = []
fortran_index_io = []
fortran_index_numerical = []

for i in fortran_index:
    try:
        for j in str(i['tags']).split():
            fortran_index_tags.append(j)
    except KeyError:
        print("")
    if "libraries" in i['categories'].split():
        fortran_index_libraries.append(i)
    if "data-types" in i['categories'].split():
        fortran_index_data_types.append(i)
    if "strings" in i['categories'].split():
        fortran_index_strings.append(i)
    if "programming" in i['categories'].split():
        fortran_index_programming.append(i)
    if "graphics" in i['categories'].split():
        fortran_index_graphics.append(i)
    if "interfaces" in i['categories'].split():
        fortran_index_interfaces.append(i)
    if "examples" in i['categories'].split():
        fortran_index_examples.append(i)
    if "scientific" in i['categories'].split():
        fortran_index_scientific.append(i)
    if "io" in i['categories'].split():
        fortran_index_io.append(i)
    if "numerical" in i['categories'].split():
        fortran_index_numerical.append(i)

fortran_tags = {
  "fortran_tags": "tags"
}
fortran_index_tags = Counter(fortran_index_tags)
a = sorted(fortran_index_tags.items(), key=lambda x: x[1],reverse=True)
for i in a:
    if i[0]=="None":
        a.remove(i)

#print(fortran_index_libraries)
for k in range(50):
    fortran_index_tags_50.append(a[k][0])

for i in fortran_index:
    for j in i['categories'].split():
        fortran_index_categories.append(j)

fortran_index_categories  = list(set(fortran_index_categories))

def github_info(list):
  for i in list:
    try:
        info = requests.get('https://api.github.com/repos/'+i['github']).text
        d = json.loads(info)
        if type(d['forks_count']) is type(None):
            d['forks_count'] = 0
        if type(d['open_issues_count']) is type(None):
            d['open_issues_count'] = 0
        if type(d['stargazers_count']) is type(None):
            d['stargazers_count'] = 0
        try:
            if str(d['license']['name']) =='null':
                print('hello')
                d['license']['name'] = 'null'
            i['license'] = d['license']['name']
        except TypeError:
            print("")
            d['license'] = 'null'
        #print(d['forks_count'],d['open_issues_count'],d['stargazers_count'])
        i['forks'] = d['forks_count']
        i['issues'] = d['open_issues_count']
        i['stars'] = d['stargazers_count']
        info = requests.get('https://api.github.com/repos/'+i['github']+'/commits/'+d['default_branch']).text
        d = json.loads(info)
        monthinteger = int(d['commit']['author']['date'][5:7])
        month = datetime.date(1900, monthinteger, 1).strftime('%B')
        i['last_commit'] = month+" "+d['commit']['author']['date'][:4]
        info = requests.get('https://api.github.com/repos/'+i['github']+'/releases/latest').text
        d = json.loads(info)
        #print(d)
        try:
            i['release'] = d['tag_name']
        except KeyError:
            print("")
    except KeyError:
        print("")

github_info(fortran_index_data_types)
github_info(fortran_index_numerical)
github_info(fortran_index_io)
github_info(fortran_index_scientific)
github_info(fortran_index_examples)
github_info(fortran_index_interfaces)
github_info(fortran_index_graphics)
github_info(fortran_index_programming)
github_info(fortran_index_strings)
github_info(fortran_index_libraries)
print(fortran_index_data_types)
fortran_tags['numerical'] =  fortran_index_numerical
fortran_tags['io'] =  fortran_index_io
fortran_tags['scientific'] =  fortran_index_scientific
fortran_tags['examples'] =  fortran_index_examples
fortran_tags['interfaces'] =  fortran_index_interfaces
fortran_tags['graphics'] =  fortran_index_graphics
fortran_tags['programming'] =  fortran_index_programming
fortran_tags['strings'] =  fortran_index_strings
fortran_tags['data_types'] =  fortran_index_data_types
fortran_tags['libraries'] =  fortran_index_libraries
fortran_tags['tags'] =  fortran_index_tags_50
conf['reference_books'] = conf['reference-books']
conf['reference_courses'] = conf['reference-courses']
conf['reference_links'] = conf['reference-links']

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
    "sphinx_jinja",
    'matplotlib.sphinxext.plot_directive',
    'IPython.sphinxext.ipython_directive',
    'IPython.sphinxext.ipython_console_highlighting',
    'sphinx.ext.mathjax',
    'sphinx.ext.autodoc',
    'sphinx.ext.doctest',
    'sphinx.ext.inheritance_diagram',
    'numpydoc',
    'sphinx_charts.charts',
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
html_additional_pages = {'index': 'index.html'}
suppress_warnings = ["myst.header"]

jinja_contexts = {
    'conf':conf,
    'fortran_index':fortran_tags,
}


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = "pydata_sphinx_theme"

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']

def Sort_Tuple(tup):
    lst = len(tup)
    for i in range(0, lst):

        for j in range(0, lst-i-1):
            if (tup[j][0] > tup[j + 1][0]):
                temp = tup[j]
                tup[j]= tup[j + 1]
                tup[j + 1]= temp
    return tup

def plot_graphs(graph):
  a=[]
  login=[]
  contributions=[]
  info = requests.get('https://api.github.com/repos/fortran-lang/'+graph+'/contributors').text
  d = json.loads(info)
  for i in range(len(d)):
    a.append((d[i]['login'],d[i]['contributions']))
  Sort_Tuple(a)
  for i in a:
    login.append(i[0])
    contributions.append(i[1])
  test_chart = {"data": [
      {
        "x": login,
        "y": contributions,
      }
    ],
    "layout": {
      "margin": {
        "t": 15,
        "b": 30,
        "r": 15,
        "l": 35
      }
    }
  }
  print(test_chart)
  with open("charts/"+graph+".json", "w") as f:
    json.dump(test_chart, f)
graphs =["fortran-lang.org","fpm","stdlib"]
for i in graphs:
  plot_graphs(i)

html_theme_options = {
    "favicons" : [
        {
            "rel": "icon",
            "href": "images/favicon.ico",
        },
    ],
    "show_prev_next": True,
    "page_sidebar_items": [],
    "footer_items": ["copyright"],
    "navbar_align": "right",
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

html_sidebars = {
    "news": [
        "tagcloud.html",
        "archives.html",
        "recentposts.html",
    ],
    "news/**": [
        "postcard.html",
        "recentposts.html",
        "archives.html",
    ],
   "learn/index": [],
    "compilers": [],
    "packages": [],
    "community": [],
    "packages/**": [],
}
html_title = "Fortran Programming Language"
html_logo = "_static/images/fortran-logo-256x256.png"
html_baseurl = "https://henilp105.github.io/fortran-lang.org/"

master_doc = 'index'

blog_path = "news"
blog_post_pattern = "news/*"
post_redirect_refresh = 1
post_auto_image = 1
post_auto_excerpt = 2

def hide_h1_on_index_pages(app, pagename, templatename, context, doctree):
    if pagename in ["index", "learn", "compilers", "community", "packages"]:
        app.add_css_file("css/hide_h1.css")

def setup(app):
    app.connect('html-page-context', hide_h1_on_index_pages)