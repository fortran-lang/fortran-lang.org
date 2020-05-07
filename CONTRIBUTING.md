# Contributing to fortran-lang.github.io

Fortran-lang.github.io is open-source and contributions are welcome!
The Fortran-lang site uses the Ruby-based [Jekyll static site generator](https://jekyllrb.com/).
To contribute you will therefore need to install Jekyll on your development computer.
See [README.md](README.md) for how to setup Jekyll and build the site.

This document details information for contributors and is broken into the following sections:

1. [Contributor guide](#contributor-guide): information and guidelines for adding to and modifying existing site content

2. [Developer information](#developer-information): detail on the structure of the site backend

## Contributor guide

### Workflow

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


### Pull request previews

Once you open a pull request, a github action will execute and build your pull request branch to produce a public preview which will be available to view at `fortran-lang.org/pr/<pr_id>/` where `<pr_id>` is the numeric identifier of your pull request.

This allows reviewers to directly view the generated result of your PR.

__Note: Subsequent pushes to your pull request branch will trigger new builds of the pull request preview.__

__Note: to disable pull request preview builds, place the string '#no_preview' within the pull request description.__

After a pull request has been merged and successfully rendered, the preview build can be deleted by commenting on
the pull request with the following keyword: `#delete_preview`.

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


## Developer information

`Under development`