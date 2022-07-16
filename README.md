# fortran-lang.org website

This repository contains the source for the fortran-lang.io website. 
It's derived from https://github.com/neovim/neovim.github.io.

## Contributing

* [CONTRIBUTING](./CONTRIBUTING.md): getting started and general guidance on contributing to <https://fortran-lang.org>

* [MINIBOOKS](./MINIBOOKS.md): how to write and structure a mini-book tutorial for the [Learn](https://fortran-lang.org/learn) section

* [PACKAGES](./PACKAGES.md): adding an entry to the [Package index](https://fortran-lang.org/packages)

## Setup

### Build fortran-lang.org site (Sphinx Version)

Build the site by invoking

```
make html
```

The website will be built in `build/html` and can be previewed by opening the page with a browser (*e.g.* firefox, chromium or similar):

```
firefox file://$PWD/build/html/en/index.html
```

By default all languages will be built.
To limit the build to a single language subtree, *i.e.* English, use

```
make html LANGUAGES=en
```

### Update or add translations

The documentation uses the
[sphinx-intl](https://sphinx-intl.readthedocs.io/en/master/quickstart.html)
utility to generate websites for multiple languages.
It generates `*.po` files,
which contain the original sentences and a placeholder for translations.

To update translations run

```
make gettext
```

if you only want to update a single translation add `LANGUAGES=de` to the command.
This command will generate the message catalog (`*.pot`) and update the `*.po` files in the *locale* directory of the respective translations.
Then edit the `*.po` files,
e.g. `locale/de/LC_MESSAGES/index.po`.
In the `*.po` files are paragraphs like
```po
#: ../../pages/index.md:16
msgid "Package manager and build system for Fortran"
msgstr ""
```

The first line describes the file and line where to find the original text.

The second line is the original text.
**Don't edit this line, edit the original document instead**.

The third line is meant for the translation.

To continue a long string in another line,
simply close the string in the current line with `"`
and open another one in the line underneath. E.g.
```
msgstr "This is "
"one string"
```
*don't forget a space between 'is' and 'one'*

After adding or updating translations
build the documentation as described above.

### Build fortran-lang.org site (Jekyll Version)

This assumes that you already have a recent Ruby with RubyGems.

For example on Ubuntu 18.04, do:
```
sudo apt install ruby-dev
```

### Installing Ruby gems

Install Bundler, either systemwide:

```
sudo gem install bundler
```

or for the current user:

```
gem install --user-install bundler
export PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
```

In the latter case, you might want to add the `PATH` update to `.bashrc` or your shell's equivalent.

To install the dependencies of this project, use Bundler:

```
bundle config set path '.bundle'
bundle install
```

### Serving locally

Execute the following command:

```
bundle exec jekyll serve --watch
```

Open `http://localhost:4000` to view the website.


