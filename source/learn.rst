:sd_hide_title: true

Learn
=====

.. raw:: html
    
    <p style="text-align: center;font-size:36px;"><b>Learn Fortran</b></p>
    <p style="text-align: center;font-size:24px;">Learning resources for beginners and experts alike</p>
   
         <h2>Getting Started</h2>
         <table cellpadding="10">
            <tr>
               <td>
                  <h3><i class="fas fa-info-circle"></i>
                     New to Fortran</h3>
                  <p>
                     Try the quickstart Fortran tutorial, to
                     get an overview of the language syntax and capabilities.
                  </p>
                  <a  href="https://henilp105.github.io/fortran-lang.org/learn/quickstart/index.html">
                     <i class="fas fa-book"></i>
                     <b> Quickstart tutorial</b>
                  </a><br>
               </td>
    
      <td>

         <h3><i class="fas fa-question-circle"></i>
            Looking for help</h3>
         <p>
            Ask a question in the Fortran-lang discourse - a forum
            for friendly discussion of all things Fortran.

         </p>
         <a class="button center blue" href="https://fortran-lang.discourse.group/" target="_blank">
            <b> <i class="fab fa-discourse"></i>
               Fortran-lang Discourse
         </a></b>
      </td>
      </tr>
      </table>
       <h2 id="book-index">Mini-book Tutorials</h2>
      <h3><i class="fas fa-cubes"></i> Getting started</h3>


.. jinja:: conf

    {% for j in books %}
    {% if j.category == 'Getting started' %}
    `{{j.title}} <{{"https://henilp105.github.io/fortran-lang.org"+j.link+"/index.html"}}>`_ 
     {{j.description}}
    {% endif %}
    {% endfor %}

.. raw:: html

     <h3><i class="fas fa-book"></i> Fortran Documentation</h3>
     
.. jinja:: conf

    {% for j in books %}
    {% if j.category == 'Fortran Documentation' %}
    `{{j.title}} <{{"https://henilp105.github.io/fortran-lang.org"+j.link+"/index.html"}}>`_ 
      {{j.description}}
    {% endif %}
    {% endfor %}


Other Resources
---------------

On the web
~~~~~~~~~~

.. jinja:: conf
    
    {% for j in reference_links %}
    
 
   * `{{j.name}} <{{j.url}}>`_ {{j.description}}
   
    {% endfor %}


Online Courses
~~~~~~~~~~~~~~

.. jinja:: conf
    
    {% for j in reference_courses %}
    
 
   * `{{j.name}} <{{j.url}}>`_ {{j.description}}
   
    {% endfor %}


In print
~~~~~~~~

.. jinja:: conf

    {% for j in reference_books %}
    
   * {{j.author}} {{j.year}} `{{j.title}} <{{j.url}}>`_ {{j.edition}} {{j.location}}{{j.publisher}}
   
    {% endfor %}


.. toctree::

   learn/quickstart/index
   learn/best_practices/index
   learn/os_setup/index
   learn/building_programs/index
   learn/intrinsics/index
