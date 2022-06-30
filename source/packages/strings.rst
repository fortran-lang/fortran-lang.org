.. raw:: html
    
    <p style="text-align: center;font-size:36px;"><b>Featured Open Source Projects</b></p>
    <p style="text-align: center;font-size:24px;">A rich ecosystem of high-performance code</p>

------------

Libraries for manipulating characters and strings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. jinja:: fortran_index

    {% for j in strings|sort(attribute='name') %}
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
    {% endfor %}


------------

.. raw:: html
    
    See<a href="https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/PACKAGES.md" target="_blank"><i class="devicon-github-plain colored"></i> here</a> for how to get your project listed.