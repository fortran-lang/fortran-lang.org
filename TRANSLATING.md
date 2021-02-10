Translation and Internationalization
====================================


## Adding a new language.

Before starting to translate, make sure to setup *jeykll* as described in the
[README](./README.md).

To add a new language to the webpage add the language name to the *languages* array
in the [``_config.yml``](./_config.yml) file.
The first language in the arrays is the default language and is used in the main tree
at ``https://fortran-lang.org`` while additional trees are generated under
``https://fortran-lang.org/<lang>``.
You might have to restart *jekyll* to apply changes from the config file.


### Translating keywords

After adding a language to the config file, add its English name to the English subtree
at [``_i18n/en.yml``](./_i18n/en.yml) to the *langs* field.
To create a new tree from the added language copy the English language file

```
cp _i18n/en.yml _i18n/<lang>.yml
```

You can now start to fill the keyword translations in the new ``<lang>.yml`` file.


### Translating content

To translate the content of the main page ([``_i18n/en/index.html``](./i18n/en/index.html))
create a copy in the new language tree under ``_i18n/<lang>/index.html``.
Pages that do not have a translation will be used from the default language automatically,
therefore you can translate one page at a time.
