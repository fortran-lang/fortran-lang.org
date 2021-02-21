Translation and Internationalization
====================================

The fortran-lang.org webpage uses the
[jekyll-multiple-languages-plugin](https://github.com/kurtsson/jekyll-multiple-languages-plugin)
to support translations for multiple languages.
The plugin provides three main functionalities to allow the localization of content:

- `translate`, `t`:
  replace expression with content from language file in ``_i18n/<lang>.yml`` file,
  this functionality does not provide a fallback to the default language.
  The translate function does not expand liquid templates.
- `translate_file`, `tf`:
  include file from ``_i18n/<lang>/`` subtree, this functionality automatically
  falls back to the default language subtree if no translation is available.
  Liquid templating is possible here like usual.
- `translate_link`, `tl`
  localization of permalinks is currently not used.

The default behaviour of jekyll is mostly retained, new pages are added in the main
subtree as usual. To provide the possibility to localize page content all English
keywords or content blocks are included with ``{% t ... %}`` or ``{% tf ... %}`` in the
main tree while the actual content is provided in the English subtree at ``_i18n/en/``.
Therefore, it is best to have content and markup separated for all localized pages.

The content of the language configuration file is available as ``site.translations[lang]``.
Note that you cannot reliably access content of other language files by this mean.
The ``site.baseurl`` variable is localized as well, to access the actual root, *e.g.*
to get to the assets directory to include CSS use the ``site.baseurl_root`` variable instead.


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

Note that this allows to have posts visible only in certain language subtrees, which
can be useful if you want to announce local events or have translated versions of
the English posts.
