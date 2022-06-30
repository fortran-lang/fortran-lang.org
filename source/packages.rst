.. raw:: html
    
    <p style="text-align: center;font-size:36px;"><b>Fortran Packages</b></p>
    <p style="text-align: center;font-size:24px;">A rich ecosystem of high-performance code</p>

--------------

Find a Package
-------------

.. raw:: html
    
    <form class="bd-search d-flex align-items-center" align="center" action="search.html" method="get">  <input type="search" class="form-control" name="q" id="search-input"
               placeholder="Search for a package" aria-label="Search" autocomplete="off" style='margin: auto;text-align: center;width:40em;'> </form>
    <br>
    <table><col width="500em" />
    <tr><td><h3><i data-feather="list"></i>
          Package Index</h3></td><td><h3>Featured topics</h3>
        <div id="package-topics"></div></td></tr><tr><td>
          The fortran-lang package index is community-maintained and 
          lists open source Fortran-related projects.
          This includes large-scale scientific applications,
          function libraries, Fortran interfaces, and developer tools.
        <br>
        See
	        <a href="https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/PACKAGES.md" target="_blank">
          <igithub-plain colored"></i> here</a> for how to get your project listed.
     class="devicon-    <br>
          Use the box above to search the package index by keyword, package name, or author username.
        </td>
        <td> 

.. jinja:: fortran_index

    {% for j in tags %}
    `{{j}}, <{{"search.html?q="+j}}>`_ {% endfor %}


.. raw:: html
    
    </td></tr></table>


Browse Packages by Category
--------------------------
* `IO <packages/io.html>`_
* `FPM <packages/fpm.html>`_
* `data-types <packages/data-types.html>`_
* `graphics <packages/graphics.html>`_
* `examples <packages/examples.html>`_
* `interfaces <packages/interfaces.html>`_
* `numerical <packages/numerical.html>`_
* `programming <packages/programming.html>`_
* `strings <packages/strings.html>`_
* `scientific <packages/scientific.html>`_
* `libraries <packages/libraries.html>`_

