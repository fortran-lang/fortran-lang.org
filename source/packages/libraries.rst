.. raw:: html
    
    <p style="text-align: center;font-size:36px;"><b>Featured Open Source Projects</b></p>
    <p style="text-align: center;font-size:24px;">A rich ecosystem of high-performance code</p>

------------

.. |license| image:: https://img.shields.io/badge/license-grey
.. |version| image:: https://img.shields.io/badge/version-grey


Fortran libraries for general programming tasks
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. jinja:: fortran_index

    {% for j in libraries|sort(attribute='name') %}
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

    {% if j.license is defined %}|license| : {{j.license}}{% endif %}
    {% if j.version is defined %}|version| : {{j.version}}{% endif %}
    {% endfor %}


------------

.. raw:: html
    
    See<a href="https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/PACKAGES.md" target="_blank"><i class="devicon-github-plain colored"></i> here</a> for how to get your project listed.