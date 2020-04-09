# fortran-lang.org website

This repository contains the source for the fortran-lang.io website. 
It's derived from https://github.com/neovim/neovim.github.io.

## Contributing

Coming soon.

## Setup

This assumes that you already have a recent Ruby with RubyGems.

### Installing Ruby gems

Install Bundler, either systemwide:

```
sudo gem install bundler
```

or for the current user:

```
gem install --user-install
export PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
```

In the latter case, you might want to add the `PATH` update to `.bashrc` or your shell's equivalent.

To install the dependencies of this project, use Bundler:

```
bundle install --path .bundle
```

### Serving locally

Execute the following command:

```
bundle exec jekyll serve --watch
```

Open `http://localhost:4000` to view the website.
