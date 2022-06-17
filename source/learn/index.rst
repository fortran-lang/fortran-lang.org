.. rst-class:: center
Learn Fortran
####

Learning resources for beginners and experts alike
#####

Getting Started
####
.. jinja:: conf

    {% for j in books %}

   * `{{j.title}} <{{j.link}}>`_ {{j.description}}

   
    {% endfor %}


Other Resources
####

On the web
####

.. jinja:: conf
    
    {% for j in reference_links %}
    
 
   * `{{j.name}} <{{j.url}}>`_ {{j.description}}
   
    {% endfor %}


Online Courses
####

.. jinja:: conf
    
    {% for j in reference_courses %}
    
 
   * `{{j.name}} <{{j.url}}>`_ {{j.description}}
   
    {% endfor %}


In print
####

.. jinja:: conf

    {% for j in reference_books %}
    
   * {{j.author}} {{j.year}} `{{j.title}} <{{j.url}}>`_ {{j.edition}} {{j.location}}{{j.publisher}}
   
    {% endfor %}
