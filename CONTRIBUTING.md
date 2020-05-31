# Contributing to fortran-lang.github.io

Fortran-lang.github.io is open-source and contributions are welcome!
The Fortran-lang site uses the Ruby-based [Jekyll static site generator](https://jekyllrb.com/).
To contribute you will therefore need to install Jekyll on your development computer.
See [README.md](README.md) for how to setup Jekyll and build the site.

* See [PACKAGES](./PACKAGES.md) for how to add an entry to the [Package index](https://fortran-lang.org/packages)

* See [MINIBOOKS](./MINIBOOKS.md) for how to write and structure a mini-book tutorial for the [Learn](https://fortran-lang.org/learn) section

## Workflow

Contributions to the site are made by pull request to the github repository: <https://github.com/fortran-lang/fortran-lang.github.io/>.

The workflow for doing so takes the following form:

1. Create/update a personal fork of fortran-lang.github.io
   - (See  [github help: syncing a fork](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/syncing-a-fork) )

2. Create a new branch in your fork
   - The branch name should concisely describe your contribution, _e.g._ `fix-spelling-homepage`, `update-compiler-info`

3. Perform your changes on the local branch

4. Push your modified branch to your local fork
   - _e.g._ `git push --set-upstream origin fix-spelling-homepage`

5. Create a pull request in the fortran-lang/fortran-lang.github.io from your modified fork branch
   - (See [github help: creating a pull request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request) )

__Note: Before opening a pull request you must build your changes locally using Jekyll (see [README.md](README.md)) to verify that your changes build correctly and render as you expect.__

Your pull request will be reviewed by other members of the community who may request changes.

__Note: You can continue to push changes to your fork branch after you open a pull request  - the pull request will update accordingly__

Once your pull request is approved, usually by at least two other community members, it will be merged into the fortran-lang.github.io master branch by the maintainers at which point it will be published to the fortran-lang.org site.

If required, the repository maintainers can build a public preview of your proposed changes which will be available to view at `fortran-lang.org/pr/<pr_id>/` where `<pr_id>` is the numeric identifier of your pull request.

This allows reviewers to directly view the generated result of your PR.
After a pull request has been merged and successfully rendered, the maintainers will delete the preview build.


## Style guide

### External links

It is recommended practice for off-site hyperlinks to open in a new tab.
On `Fortran-lang.org` all such links will automatically be suffixed with a new-tab icon;
this gives site users prior expectation that the link will lead them off-site while
keeping fortran-lang.org open in a previous tab.

__Example:__ Open link in new tab (HTML or markdown)
```html
<a href="https://fortran-lang.discourse.group/" target="_blank">Discourse</a>
```

### Internal site links

Hyperlinks that point to other parts of the fortran-lang.org website should be prefixed with `{{ site.baseurl }}` - this is important for generating pull request previews (see [here](https://byparker.com/blog/2014/clearing-up-confusion-around-baseurl/) for an explanation).

__Example:__ markdown link

```
[Fortran-lang news]({{site.baseurl}}/News)
```

__Example:__ html link

```
<a href="{{site.baseurl}}/Packages">Fortran packages</a>
```

### Icon packs

Icons are an easy way to improve page aesthetic by breaking-up otherwise monotonic text passages
and drawing attention to headings or key information.

Three icon packs are available for use on `fortran-lang.org`:

* [Font awesome](https://fontawesome.com/icons?d=gallery) (CC BY 4.0 License)

* [Feather](https://feathericons.com/) (MIT)

* [Devicon](https://konpa.github.io/devicon/) (MIT)


__Example:__ Font awesome
```html
<i class="fas fa-info-circle"></i>
```

__Example:__ Feather

```html
<i data-feather="globe"></i>
```

__Example:__ Devicon

```html
<i class="devicon-github-plain"></i>
```

Visit the respective websites to browse available icons.

__Note:__ font-awesome icons currently appear to vertically-align with text better -
we need to get feather icons to do the same.


### Page contents

It is sometimes useful to display a hyperlinked page contents for lengthy pages.
There are two ways to do this on `fortran-lang.org`.

__Option 1: Use the `book` layout__

The `book` layout is the layout used for mini-book tutorials;
it includes a non-scrolling sidebar which is automatically populated
by the `<h2>` headings on the current page.

__Option 2: __

If you just want a list of headings at the top of your page,
include the following snippet, which will be automatically
populated by the `<h2>` headings on the current page.

```html
<ul id="page-nav"></ul>
```

__Implementation:__
the functionality described above is implemented in the javascript file
[assets/js/page_nav.js](./assets/js/page_nav.js).