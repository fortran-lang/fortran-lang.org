:sd_hide_title:

.. |br| raw:: html

     <br>

.. raw:: html

     <link rel="stylesheet" href="https://unpkg.com/octicons@4.4.0/build/font/octicons.css">
     <link rel="stylesheet" href="https://unpkg.com/github-activity-feed@latest/dist/github-activity.min.css">
      
     <script type="text/javascript" src="https://unpkg.com/mustache@4.2.0/mustache.min.js"></script>
     <script type="text/javascript" src="https://unpkg.com/github-activity-feed@latest/dist/github-activity.min.js"></script>


Community - The Fortran Programming Language
###########################################

.. div:: sd-text-center sd-fs-2 sd-font-weight-bold sd-text-primary

    Fortran-lang Community

.. div:: sd-text-center sd-fs-3 

    Collaboration for the advancement of Fortran
  

.. div:: sd-text-left sd-fs-2 sd-text-primary

    Fortran-lang Community Projects

.. grid:: 2

    .. grid-item-card::
        :columns: 5
        :shadow: none

        .. div:: sd-text-left sd-fs-4 

            Fortran Standard Library (stdlib)

        A community-driven project for a de facto 'standard' library for Fortran. The stdlib project is both a specification and a reference implementation, developed in cooperation with the Fortran Standards Committee.

        `GitHub <https://github.com/fortran-lang/stdlib>`_  `Documentation <https://stdlib.fortran-lang.org/>`_  `Contributing <https://github.com/fortran-lang/stdlib/blob/HEAD/WORKFLOW.md>`_

        .. div:: sd-text-left sd-fs-4 

            Fortran Package Manager (fpm)

        A prototype project to develop a common build system for Fortran packages and their dependencies.

        `GitHub <https://github.com/fortran-lang/fpm>`_  `Documentation <https://github.com/fortran-lang/fpm/blob/HEAD/PACKAGING.md>`_  `Contributing <ttps://github.com/fortran-lang/fpm/blob/HEAD/CONTRIBUTING.md>`_

        .. div:: sd-text-left sd-fs-4 

            fortran-lang.org

        This website is open source and contributions are welcome!.

        `GitHub <https://github.com/fortran-lang/fortran-lang.org>`_  `Contributing <ttps://github.com/fortran-lang/fortran-lang.org/blob/HEAD/CONTRIBUTING.md>`_


    .. grid-item-card::
        :columns: 7
        :shadow: none

        .. raw:: html

            <div id="fortran-lang-gh-feed-sphinx" style="height: 500px;" ></div>
            
            <script> GitHubActivity.feed({
            username: "fortran-lang",
            selector: "#fortran-lang-gh-feed-sphinx",
            limit: 20 // optional
            });
        
            </script>


.. div:: sd-text-left sd-fs-2 sd-text-primary

    Get Involved

.. grid:: 1 1 2 2
    :gutter: 1

    .. grid-item::

        .. grid:: 1 1 1 1
            :gutter: 1

            .. grid-item-card::
                :shadow: none
                
                .. div:: sd-text-left sd-fs-4 

                    Join the Discussion
                The easiest way to join the community and contribute is by
                commenting on issues and pull requests in the project
                repositories.

                Whether Fortran beginner or seasoned veteran, your feedback and comments are most
                welcome in guiding the future of Fortran-lang.

            .. grid-item-card::
                :shadow: none
                
                .. div:: sd-text-left sd-fs-4 

                    Contributor Guide

                Want to contribute code and content?
                Check out the contributor guides in each repository for information
                on the project workflow and recommended practices.
                Contributor guide for stdlib   

                `Contributor guide for stdlib <https://github.com/fortran-lang/stdlib/blob/HEAD/WORKFLOW.md>`_ |br|
                `Contributor guide for fpm <https://github.com/fortran-lang/fpm/blob/HEAD/CONTRIBUTING.md>`_ |br|
                `Contributor guide for fortran-lang.org <https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/CONTRIBUTING.md>`_ 


    .. grid-item::

        .. grid:: 1 1 1 1
            :gutter: 1
            
            .. grid-item-card:: 
                :shadow: none
                
                .. div:: sd-text-left sd-fs-4 

                    Build and Test

                Get more involved with each project by cloning, building and testing
                it on your own machine(s) and with your own codes;
                if something doesn't work, create an issue to let us know!
                We value user feedback highly, be it a bug report, feature request, or
                suggestion for documentation.

            .. grid-item-card:: 
                :shadow: none
                
                .. div:: sd-text-left sd-fs-4 

                    Community Conduct

                As a community, we strive to make participation in our discussions and projects a friendly and
                harassment-free experience for everyone.
                See the full  `Code of Conduct <https://github.com/fortran-lang/stdlib/blob/HEAD/CODE_OF_CONDUCT.md>`_ 




.. div:: sd-text-left sd-fs-2 sd-text-primary

    Fortran-lang Contributors

We are grateful for every contribution made by all members of the community.

.. raw:: html

    <iframe src="https://contributor-graph.vercel.app/?chart=contributorOverTime&repo=fortran-lang/fortran-lang.org,fortran-lang/fpm,fortran-lang/stdlib,j3-fortran/fortran_proposals" 
    onload='javascript:(function(o){o.style.height=o.contentWindow.document.body.scrollHeight+"px";}(this));' style="height:700px;width:100%;border:none;overflow:hidden;"></iframe>

.. div:: sd-text-left sd-fs-3

    source: https://git-contributor.com/

.. div:: sd-text-left sd-fs-4 sd-text-primary

    Contributors:

.. jinja:: contributors

    .. grid:: 6
        :gutter: 1

    {% for j in contributor | batch(6, '&nbsp;') %}
        
        {% for i in j %}
        {% if i != '&nbsp;' %}
        .. grid-item-card:: 
            :shadow: none

            .. grid-item-card:: `{{i}} <{{"https://github.com/"+i}}>`_
                    :img-top: https://github.com/{{i}}.png?size=100
                    :shadow: none
                    :text-align: center
        {% endif %}
        {% endfor %}
    {% endfor %}