:sd_hide_title:

.. |br| raw:: html

     <br>

Packages - The Fortran Programming Language
###########################################


.. div:: sd-text-center sd-fs-2 sd-font-weight-bold sd-text-primary

    Fortran Packages

.. div:: sd-text-center sd-fs-3 

    A rich ecosystem of high-performance code


.. div:: sd-fs-3 sd-font-weight-bold sd-text-primary

    Find a Package

.. raw:: html
    
    <form class="bd-search d-flex align-items-center" align="center" action="search.html" method="get">  <input type="search" class="form-control" name="q" id="search-input"
               placeholder="Search for a package" aria-label="Search" autocomplete="off" style='margin: auto;text-align: center;width:40em;'> </form>
    <br>
    <table><col width="500em" />
    <tr><td>

.. div:: sd-fs-3 

    Package index

.. raw:: html
    
    </td><td>


.. div::  sd-fs-3 

    Featured topics


.. raw:: html
    
    </td></tr><tr><td>

The fortran-lang package index is community-maintained and 
lists open source Fortran-related projects.
This includes large-scale scientific applications,
function libraries, Fortran interfaces, and developer tools.
|br|
See  `here <https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/PACKAGES.md>`_ for how to get your project listed.  |br|
Use the box above to search the package index by keyword, package name, or author username.

.. raw:: html
    
    </td><td> 

.. jinja:: fortran_index

    {% for j in tags %}
    `{{j}}, <{{"search.html?q="+j}}>`_ {% endfor %}


.. raw:: html
    
    </td></tr></table>

.. div:: sd-fs-3 sd-font-weight-bold sd-text-primary

    Browse Packages by Category



`Data types and containers <packages/data-types.html>`_
-------------------------------------------------------

Libraries for advanced data types and container classes

`Interface libraries <packages/interfaces.html>`_
-------------------------------------------------

Libraries that interface with other systems, languages, or devices

`Libraries <packages/libraries.html>`_
--------------------------------------

Fortran libraries for general programming tasks

`Input, output and parsing  <packages/io.html>`_
------------------------------------------------

Libraries for reading, writing and parsing files and inputs

`Graphics, plotting and user interfaces <packages/graphics.html>`_
------------------------------------------------------------------

Libraries for plotting data, handling images and generating user interfaces

`Examples and templates <packages/examples.html>`_
--------------------------------------------------

Demonstration codes and templates for Fortran

`Numerical projects <packages/numerical.html>`_
-----------------------------------------------

Fortran libraries for linear algebra, optimization, root-finding etc.

`Programming utilities <packages/programming.html>`_
----------------------------------------------------

Error handling, logging, documentation and testing

`Characters and strings <packages/strings.html>`_
-------------------------------------------------

Libraries for manipulating characters and strings

`Scientific Codes <packages/scientific.html>`_
----------------------------------------------

Applications and libraries for applied mathematical and scientific problems


.. raw:: html
    
    See<a href="https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/PACKAGES.md" target="_blank"><i class="devicon-github-plain colored"></i> here</a> for how to get your project listed.