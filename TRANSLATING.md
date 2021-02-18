Translation and Internationalization
====================================


## Adding a new language.

Before starting to translate, make sure to setup *jeykll* as described in the
[README](./README.md).

To add a new language to the webpage add the language name to the *languages* array
in the [``_config.yml``](./_config.yml) file.
The language name should be represented by its two letter code, the full name of the
language will be provided in the localized configuration files at ``./_i18n/<lang>.yml``.
The first language in the arrays is the default language and is used in the main tree
at ``https://fortran-lang.org`` while additional trees are generated under
``https://fortran-lang.org/<lang>``.
You might have to restart *jekyll* to apply changes from the config file.

Add the new language in the [``_data/langs.yml``](./_data/langs.yml) file together with
its name, those entries will be used in the footer as navigation between the translations.


### Translating keywords

After adding a language to the config file, add its English name to the English subtree
at [``_i18n/en.yml``](./_i18n/en.yml) to the *langs* field.
To create a new tree from the added language copy the English language file

```
cp _i18n/en.yml _i18n/<lang>.yml
```

You can now start to fill the keyword translations in the new ``<lang>.yml`` file.
Make sure to keep the same structure as in the English language file, since missing
keywords will be dropped rather than filled in from the default language.


### Translating content

To translate the content of the main page create a copy of the included HTML or markdown
files from the English subtree in the new language tree under ``_i18n/<lang>/``.
Files that do not have a translation will be used from the default language automatically,
therefore you can translate one file at a time.


### Creating posts

Posts are limited to their language scope, to include the posts from the English page
the recommended strategy is to create symbolic links from the entries in
``_i18n/en/_posts`` to the localized variant ``_i18n/<lang>/_posts`` instead of
copying the complete posts.
