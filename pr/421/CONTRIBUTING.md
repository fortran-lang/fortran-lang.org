# Contributing to fortran-lang.org

Fortran-lang.org is open-source and contributions are welcome!

* See [PACKAGES](./PACKAGES.md) for how to add an entry to the [Package index](https://fortran-lang.org/packages)

* See [MINIBOOKS](./MINIBOOKS.md) for how to write and structure a mini-book tutorial for the [Learn](https://fortran-lang.org/learn) section


## Introduction

__How is the site written?__

The content of the website is primarily written in a combination of Markdown, HTML and YAML (for data).
This source is compiled to produce pure HTML which is what you see on the final website.

The website is _static_ which means that once built, the content on the site is the same for all users;
this is in contrast to many websites that are _dynamic_, meaning they can serve different content 
depending on the user and the inputs supplied by the user.

Structural components of the website are written in the Jekyll [Liquid](https://github.com/Shopify/liquid/wiki) templating language for static features, and JavaScript for dynamic features.


__Do I need to know HTML to contribute?__

The majority of the site content is written in [Markdown](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet), a simple markup language for formatting text - don't worry if you haven't used it before, it's very easy to pick up!


__How is the site built?__

The Fortran-lang site uses the Ruby-based [Jekyll static site generator](https://jekyllrb.com/)
to compile the Markdown and HTML files.
It is recommended for contributors to install Jekyll on your development computer so that changes 
can be previewed locally, however this is not mandatory since site previews can be generated during the
pull request process (see below for more information).
See [README.md](README.md) for how to setup Jekyll and build the site.

The GitHub repository default branch only ever contains the 'source code' for the website, not the final
compiled result; an automated service compiles this source code every time an update is pushed and stores
the compiled result on the [`gh-pages`](https://github.com/fortran-lang/fortran-lang.org/tree/gh-pages) branch
which is served up at <https://fortran-lang.org>.

Therefore, as a contributor you only need to upload changes to the site source code and not the compiled result, because
this is built automatically from the source code on the default branch.


## Workflow

Contributions to the site are made by pull request to the github repository: <https://github.com/fortran-lang/fortran-lang.org/>.

The workflow for doing so takes the following form:

1. Create/update a personal fork of fortran-lang.org
   - (See  [github help: syncing a fork](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/syncing-a-fork) )

2. Create a new branch in your fork
   - The branch name should concisely describe your contribution, _e.g._ `fix-spelling-homepage`, `update-compiler-info`

3. Perform your changes on the local branch

4. Push your modified branch to your fork
   - _e.g._ `git push --set-upstream origin fix-spelling-homepage`

5. Create a pull request in the fortran-lang/fortran-lang.org from your modified fork branch
   - (See [github help: creating a pull request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request) )

__Note: Before opening a pull request you must build your changes locally using Jekyll (see [README.md](README.md)) to verify that your changes build correctly and render as you expect.__

__Note: You can continue to push changes to your fork branch after you open a pull request  - the pull request will update accordingly__

Your pull request will be reviewed by other members of the community who may request changes.
GitHub provides an easy interface on its website to apply (or reject) any reviewer-suggested changes with a click of a button.
This avoids having to manually copy suggestions to your local copy and push back again.
If you use the "Commit suggestion" button, you will need to update the local copy on your computer using `git pull` if you
intend to push more edits from your computer.

Once your pull request is approved, usually by at least two other community members, it will be merged into the fortran-lang.org default branch by the maintainers at which point it will be published to the fortran-lang.org site.

If required, the repository maintainers can build a public preview of your proposed changes which will be available to view at `fortran-lang.org/pr/<pr_id>/` where `<pr_id>` is the numeric identifier of your pull request.

This allows reviewers to directly view the generated result of your PR.

__Note:__ if you push subsequent commits to your pull request branch, you must rebuild the pull request preview by commenting on
the pull request with '#build_preview'.

After a pull request has been merged and successfully rendered, the maintainers will delete the preview build.

__Note:__ if your pull request preview link doesn't work or doesn't update after re-building, try adding a random parameter to the end of the URL, _e.g._ `https://fortran-lang.org/pr/98?v=2` - the name and value of the parameter don't matter, but use different values for each update.
This will force the GitHub content delivery network to serve you an updated version instead of a cached version which is out-of-date.

## Style guide

### Markdown

- Place code excerpts in [code blocks](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet#code-and-syntax-highlighting),
  denoted by back ticks (```` ``` ````). Use inline code style (`` `code` ``) for inline code excerpts, programming language keywords, variables names and file names.

- Have no more than one sentence per source-code line, and break-up long sentences across multiples lines -
   this is important to avoid large git diffs and code review blocks on github.


### External links

It is recommended practice for off-site hyperlinks to open in a new tab.
On `Fortran-lang.org` all such links will automatically be suffixed with a new-tab icon;
this gives site users prior expectation that the link will lead them off-site while
keeping fortran-lang.org open in a previous tab.

__Example:__ Open link in new tab (HTML or markdown)
```html
<a href="https://fortran-lang.discourse.group/" target="_blank" rel="noopener">Discourse</a>
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


### Page contents

It is sometimes useful to display a hyperlinked page contents for lengthy pages.
There are two ways to do this on `fortran-lang.org`.

__Option 1: Use the `book` layout__

The `book` layout is the layout used for mini-book tutorials;
it includes a non-scrolling sidebar which is automatically populated
by the `<h2>` headings on the current page.

__Option 2:__

If you just want a list of headings at the top of your page,
include the following snippet, which will be automatically
populated by the `<h2>` headings on the current page.

```html
<ul id="page-nav"></ul>
```

__Implementation:__
the functionality described above is implemented in the javascript file
[assets/js/page_nav.js](./assets/js/page_nav.js).


## Tutorials

Guidelines for mini-book content.

### General

Use the `book` layout.

Follow the [Markdown guidelines](#markdown).

### Code style

Use two spaces for indentation, indenting bodies of units but keeping the `contains` statement at the same level as its `module` or `type`. Try to limit line length to 90 characters. These considerations should make the code more readable and easier to view on devices with smaller viewport widths.
```fortran
module m
  implicit none
  private
  public :: a_t

  type :: a_t
    integer :: val
  contains
    procedure :: func
  end type a_t

contains

  subroutine func(self)
    class(a_t), intent(in) :: self
    if (self%val > 0) then
      print *, self%val
    end if
  end function func

end module m
```

Each code block should have a base indentation level of 0, even if it would be indented if put into a larger context.
```fortran
integer :: i1  ! yes
  integer :: i2  ! no
```

Avoid vertically aligning `::` and inline comments since this adds maintenance burden and pushes the line length in most cases.

If a code block contains lines that are not valid Fortran, leave it as a language-less code block to avoid the syntax highlighter's red boxes.
```
module <module name>
...
end module <module name>
```

Feel free to omit spacing in expressions where it helps with readability, but generally include whitespace around operators.
```fortran
y1 = a * b
y2 = a*b + c*d  ! instead of a * b + c * d
y3 = a**2 + 1
y4 = (a*b + c*d) / 2
s3 = s1 // s2
```

Generally add a space after commas, except when indexing with short index values or variables names.
```fortran
a(:,1)
a2(1:10, 2:5)
b(i,j)
b2(long_i_name, long_j_name)
b3(i + 2, j)
call some_subroutine(a, b, an_option=.false.)
c = [1, 2, 3, 10]
d = [(i, i = 1, 10)]
do i = 1, 10
! ...
```

Other situations besides simple indexings where white space can be omitted:
* Aliasing in imports 
  ```fortran
  use, intrinsic :: iso_c_binding, only: sp=>c_float, dp=>c_double
  ```
* String concatentation
  ```fortran
  print *, 'hello '//'world'
  ```
* Accessing components (attributes) of derived types
  ```fortran
  p%x
  p%calc_something(a, b)
  ```
* Around `=` when passing keyword arguments
  ```fortran
  call sr(a, b, c=3)
  point = t_point(x=1., y=2.)
  character(len=:), allocatable :: s
  ```

Capitalize the first letter for inline comments except for trailing inline comments that only consist of one word or a short phrase.
```fortran
! Compute new values
y = m*x + b  ! meters
```

These code style recommendations are similar to those in [the DFTB+ style guide](https://dftbplus-develguide.readthedocs.io/en/latest/fortranstyle.html).

### Text

Use sentence case (as opposed to title case) for page and section titles.

Use *emphasis* (`*emphasis*`/`_emphasis_`, rendered as italic) for key words/phrases when they are first introduced, for emphasis, ...

Avoid use of **strong** (`**strong**`, rendered as bold) within paragraphs, since bold style is used for headings, drawing attention to examples (**Example:**), admonition/aside titles, etc.

Make use of the admonition/aside [includes](_includes) (*note*, *tip*, *important*) where appropriate.
* *note*: extra information, something that might appear in a footnote
* *tip*: information about best practices, practical tips
* *important*: warnings, things to avoid, etc.

Prefer including the [Oxford comma](https://en.wikipedia.org/wiki/Serial_comma). It usually makes things more clear.
> Fortran is fast, fun, and famed.
