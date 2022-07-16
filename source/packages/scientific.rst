.. div:: sd-text-center sd-fs-2 sd-font-weight-bold sd-text-primary

    Featured Open Source Projects

.. div:: sd-text-center sd-fs-3 

    A rich ecosystem of high-performance code

.. |license| image:: https://img.shields.io/badge/license-grey
.. |stars| image:: https://img.shields.io/badge/stars-grey
.. |forks| image:: https://img.shields.io/badge/forks-grey
.. |lastcommit| image:: https://img.shields.io/badge/last%20commit-grey
.. |issues| image:: https://img.shields.io/badge/issues-grey
.. |release| image:: https://img.shields.io/badge/Release-grey


Applications and libraries for applied mathematical and scientific problems
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. jinja:: fortran_index

    {% for j in scientific|sort(attribute='name') %}
    {% if j.github is defined %}
    `{{j.name}} <{{"https://github.com/"+j.github}}>`_   
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    {% elif j.gitlab is defined%}
    `{{j.name}} <{{"https://gitlab.com/"+j.gitlab}}>`_   
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    {% else %}
    `{{j.name}} <{{j.url}}>`_   
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    {% endif %}
    {{j.description}} 

    Tags: {{j.tags}} 

    |release| : {{j.release}}   |license| : {{j.license}}  |stars| : {{j.stars}}  |forks| :{{j.forks}}  |lastcommit| : {{j.last_commit}}  |issues| : {{j.issues}}  

    {% endfor %}


------------

.. raw:: html
    
    See<a href="https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/PACKAGES.md" target="_blank"><i class="devicon-github-plain colored"></i> here</a> for how to get your project listed.