:sd_hide_title: true

Learn
=====

.. |br| raw:: html

     <br>

.. div:: sd-text-center sd-fs-2 sd-font-weight-bold sd-text-primary

    Learn Fortran

.. div:: sd-text-center sd-fs-3 

    Learning resources for beginners and experts alike
   
.. div:: sd-fs-3 sd-font-weight-bold sd-text-primary

    Getting Started

.. grid:: 1 1 2 2
    :gutter: 1

    .. grid-item::

        .. grid:: 1 1 1 1
            :gutter: 1

            .. grid-item-card::  :octicon:`info;1em;sd-text-info` New to Fortran
                :shadow: none

                Try the quickstart Fortran tutorial, to get an overview of the language syntax and capabilities. |br|

                 .. button-link:: learn/quickstart/index.html
                     :color: primary
                     :expand:

                        :octicon:`book;1em;sd-text-info` Quickstart tutorial
                        
                


    .. grid-item::

        .. grid:: 1 1 1 1
            :gutter: 1

            .. grid-item-card::  :octicon:`bug;1em;sd-text-info` Looking for help
                :shadow: none


                Ask a question in the Fortran-lang discourse - a forum for friendly discussion of all things Fortran.
                |br|
                 .. button-link:: https://fortran-lang.discourse.group/s
                     :color: primary
                     :expand:

                       :octicon:`check-circle;1em;sd-text-info` Fortran-lang Discourse
            


.. div:: sd-fs-3 sd-font-weight-bold sd-text-primary

    Mini-book Tutorials

.. jinja:: conf

    {% for j in categories %}
    .. div:: sd-fs-4 sd-font-weight-bold sd-text-primary

        {{j.name}}
        
    .. grid:: 2
    {% for b in books %}
    {% if b.category == j.name %}

        .. grid-item-card::
            :columns: 6
            :shadow: none
             
            .. div:: sd-fs-5 sd-font-weight-bold sd-text-primary

                :octicon:`book;1em;sd-text-info` `{{b.title}} <{{b.link[1:]+"/"}}>`_
            {{b.description}}
    {% endif %}
    {% endfor %}
    {% endfor %}

.. div:: sd-fs-3 sd-font-weight-bold sd-text-primary

    Other Resources

.. div:: sd-fs-4 sd-font-weight-bold sd-text-primary

    On the web

.. jinja:: conf

    {% for j in reference_links %}


   * `{{j.name}} <{{j.url}}>`_ {{j.description}}

    {% endfor %}



.. div:: sd-fs-4 sd-font-weight-bold sd-text-primary

    Online Courses

.. jinja:: conf

    {% for j in reference_courses %}


   * `{{j.name}} <{{j.url}}>`_ {{j.description}}

    {% endfor %}

.. div:: sd-fs-4 sd-font-weight-bold sd-text-primary

    In print

.. jinja:: conf

    {% for j in reference_books %}

   * {{j.author}} {{j.year}} `{{j.title}} <{{j.url}}>`_ {{j.edition}} {{j.location}}. {{j.publisher}}

    {% endfor %}


.. toctree::
   :hidden:
   
   learn/quickstart/index
   learn/best_practices/index
   learn/os_setup/index
   learn/building_programs/index
   learn/intrinsics/index