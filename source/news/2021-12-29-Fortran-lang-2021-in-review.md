---
layout: post
title: "Fortran-lang: 2021 in review"
category: newsletter
date: 2021-12-29
author: Milan Curcic, Ondřej Čertík, Laurence Kedward, Sebastian Ehlert, Jérémie Vandenplas
---

With another year behind us, let's review the progress that the
Fortran-lang community has made.
If you're new to Fortran-lang, here's a quick intro:
We're an open source community that aims to develop modern Fortran tooling and
nurture a rich ecosystem of libraries, as well as to provide a friendly,
helpful, and inclusive space for newcomers and experienced Fortran programmers
to work together.
We started in late 2019 and have been going ever since.
If you're first discovering (or re-discovering) Fortran through this article,
welcome, and we hope it inspires you to try Fortran for one of your projects.
In this article we summarize new developments from 2021,
from flagship and new projects to community development and outreach.

<ul id="page-nav"></ul>

# Standard Library (stdlib)

To date, [33 people](https://github.com/fortran-lang/stdlib/graphs/contributors)
have contributed code to stdlib, and more than 100 people have participated in
discussions.
More than a dozen new modules have been added in 2021:

* `stdlib_array`: Provides `trueloc` and `falseloc` which allow you
  to index an array based on a logical condition in a functional style
* `stdlib_hash`: Provides many hash algorithms, 32- and 64-bit alike
* `stdlib_math`: Provides a number of common mathematical functions
* `stdlib_random`: Pseudo-random integer number generation
* `stdlib_selection`: Selection procedures for selecting elements from an array
  given a desired range
* `stdlib_sorting`: Sorting procedures based on Rust's sorting algorithm and
  introsort by David Musser
* `stdlib_specialfunctions`: Provides the Legendre function and its derivative
  in support of the Gaussian quadrature procedures
* `stdlib_stats_distribution_normal`: Functions to sample values from a normal
  distribution
* `stdlib_stats_distribution_uniform`: Functions to sample values from a uniform
  distribution
* `stdlib_string_type`: Provides a `string_type` derived type that alleviates
  some limitations of the variable-length `character` variables. `string_type`
  is compatible with all intrinsic procedures that operate on `character`.
* `stdlib_stringlist_type`: A derived type that is a 1-dimensional list of
  strings
* `stdlib_strings`: Provides a number of inquiry and manipulation procedures that
  complement the intrinsic set
* `stdlib_version`: Allows querying the version of the stdlib library build

which brings us to a total of 23 modules in stdlib.
You can read about these modules in more detail on the
[stdlib API docs website](https://stdlib.fortran-lang.org).

Besides the new modules, procedures, and derived types, there have been a few
key improvements in terms of stdlib delivery:

1. You can now use stdlib as a dependency in your fpm projects,
   see [here](https://github.com/fortran-lang/stdlib#build-with-fortran-langfpm).
   This significantly lowers the bar for getting started with stdlib.
2. We had our first stdlib release (0.1.0) on October 4. As of now we don't have
   a set release schedule, and plan to publish a new release when there is
   significant new functionality.
   As stdlib matures and becomes more widely used, we expect releases to become
   more frequent.
3. We now maintain a
  [change log](https://github.com/fortran-lang/stdlib/blob/HEAD/CHANGELOG.md) where every change to the API is documented.
  This is a useful document to reference when you want to know what's been
  added to the stdlib since the latest release.

If you haven't tried stdlib yet, please do and let us know what you think
and how we can improve it.
Our vision for stdlib is to provide basic utilities that most Fortran projects use,
as well as wider numerical capabilities with the scope of NumPy and SciPy.

## Fortran Package Manager (fpm)

Fortran Package Manager (fpm) is the package manager and build system for
Fortran.
Its key goal is to make developing, distributing, and reusing Fortran
libraries and applications as easy and as fun as possible.
In 2020, the big change for fpm was the transition from the prototype
implemented in Haskell to a pure Fortran implementation.
Fpm has since been used in increasingly more and ever larger Fortran projects.

To date, [22 people](https://github.com/fortran-lang/fpm/graphs/contributors)
have contributed code to fpm.
In 2021 fpm has advanced from v0.1.3 to v0.5.0.
Key additions this year include (release version in parentheses):

* CLI arguments for linker, archiver, and C compiler (0.5.0)
* Support for MPI and LFortran (0.4.0)
* Support for installed library modules via `external-modules` in the manifest
  (0.3.0)
* Automatic discovery of manifest files in parent directories (0.3.0)
* Support for reading package version from file (0.3.0)
* Support for include directories (0.2.0)
* Support for `--flag` CLI option to specify compiler flags (0.2.0)
* `fpm build --show-model` displays the internal representation of a package
  (0.1.4)
* Allow hyphen in new package names (0.1.4)
* `fpm new` now supports `--full` and `--bare` to specify level of scaffolding
  (0.1.4)

Check out also these fpm plugins:

* [fpm-search](https://github.com/brocolis/fpm-search):
  Adds the `fpm search` command for searching for registered fpm packages from
  the command line.
* [fpm-man](https://github.com/urbanjost/fpm-man):
  Adds the `fpm man` command for displaying man-style help pages about Fortran
  intrinsics and other language features.

At the time of writing, there are almost 200 projects now using fpm.
If you haven't tried fpm yet, please do!
It has truly been a game-changing tool for many of us.

We have many ideas that we want to pursue, such as:

* First-class integration with other package managers such as Spack and Conda
* First-class integration with build systems like CMake and Meson
* Improving [fpm's package registry](https://github.com/fortran-lang/fpm-registry), etc.

We are always looking for new contributors.
If any of these projects interest you, please join us.

## LFortran

<center>
<a href="https://lfortran.org">
<img src="{{ site.baseurl }}/assets/img/Fortran-lang-2021-in-review/lfortran-logo-300x300.png">
</a>
</center>

Though not technically a Fortran-lang project,
[LFortran](https://lfortran.org) has been growing close to the Fortran-lang
community and projects largely thanks to its creator and lead developer 
[Ondřej Čertík](https://github.com/certik) also being one of the founding
members of Fortran-lang.
LFortran has been developing rapidly this year and was released as a
[Minimum Viable Product (MVP)](https://lfortran.org/blog/2021/09/lfortran-minimum-viable-product-mvp/) in September.
LFortran currently parses all of Fortran 2018 and compiles a significant
subset of the language.
16 people have contributed code so far, and many more have reported bugs or participated in discussions.
If you haven't tried LFortran yet, please do and let us know how it can best
improve for your use case.
You can help speed up LFortran's development by contributing code and/or
documentation directly, or by donating funds to the project via
[NumFOCUS](https://numfocus.org/project/lfortran).

Follow LFortran on Twitter at [@lfortranorg](https://twitter.com/lfortranorg).

## fortran-lang.org, Discourse, and social media

A major addition to the Fortran website this year is the
[Fortran Best Practices mini-book](https://fortran-lang.org/learn/best_practices).
This is a port and an update to the well-known
[fortran90.org](https://www.fortran90.org) by
[Ondřej Čertík](https://github.com/certik).
It provides a plethora of tips on how to write simple and idiomatic Fortran
for numerical tasks, and how to avoid some common gotchas.
Give it a read and let us know how it can be improved.

The [Fortran Discourse](https://fortran-lang.discourse.group) is as active
as ever, with new users joining daily.
There are currently 338 active users out of a total of 537 registered users.
The Fortran Discourse is a great place to ask for help with Fortran code, post
a Fortran job opening, and discuss anything Fortran-related.

We also continue to release the monthly newsletter where we
document the progress month-by-month, as well as post about any notable events
in the Fortran world.
This newsletter is a great way to stay up to date with Fortran-lang.
If you're also on Twitter, follow our account
[@fortranlang](https://twitter.com/fortranlang) for daily bite-size news and
updates, as well as the new [@FortranTip](https://twitter.com/FortranTip)
account, managed by [Beliavsky](https://github.com/beliavsky), which brings
daily Fortran tips to your Twitter feed.

Finally, we meet on monthly video calls to discuss issues and topics related to all
Fortran-lang projects.
The calls are advertised on Fortran Discourse and are open to everyone.
Don't hesitate to join the call to meet other Fortran-lang participants and take part
in different discussions.
## New projects

In addition to the flagship projects summarized above, a few new projects
started or were adopted by Fortran-lang this year.

### fftpack

[fftpack](https://github.com/fortran-lang/fftpack) is a classic collection of
subroutines to perform the Fast Fourier Transform on real and complex data.
It is based on the classic
[FFTPACK library from Netlib](http://www.netlib.org/fftpack/).
fftpack was adopted by Fortran-lang in an effort to provide:

* A community-maintained FFT library
* Bug fixes to the existing codebase
* Modern interfaces on top of the existing API
* fpm package for easy use as a dependency.

### test-drive

[Test-drive](https://github.com/fortran-lang/test-drive) is a simple and 
easy-to-use testing framework developed by
[Sebastian Ehlert](https://github.com/awvwgk).
It follows a simple functional style to collect and run your tests in parallel
and print a minimal and clean diagnostic printout to the standard output. 
Test-drive is currently used by both stdlib and fpm for their own test suites.
Give test-drive a test drive for your next Fortran project!

### fpm-docs

[fpm-docs](https://github.com/fortran-lang/fpm-docs) is a brand new user
documentation website for fpm, developed by
[Sebastian Ehlert](https://github.com/awvwgk).
Its key aim is to provide community-maintained documentation across four
target audiences/use cases:

* **Tutorials**: for learning how to use fpm for Fortran development
* **How-to guides**: recipes for specific and concrete problems
* **Design documents**: resources that document the design of various aspects of
  fpm
* **References**: specification documents of fpm components

<center>
<a href="https://fpm.fortran-lang.org">
<img src="{{ site.baseurl }}/assets/img/Fortran-lang-2021-in-review/fpm-docs.png" width="100%">
</a>
</center>

As the fpm user docs are now hosted at
[fpm.fortran-lang.org](https://fpm.fortran-lang.org), the API docs are now
located at [fortran-lang.github.io/fpm/](https://fortran-lang.github.io/fpm/).

## Google Summer of Code 2021

2021 has been the first year for Fortran-lang to participate in the
[Google Summer of Code](https://summerofcode.withgoogle.com/) program.
Together with NumFOCUS and LFortran, Fortran-lang had six students who worked on a 
variety of projects:

* [Aman Godara](https://github.com/Aman-Godara) (Improving strings in stdlib)
* [Chetan Karwa](https://github.com/chetankarwa) (Linked lists in stdlib)
* [Gagandeep Singh](https://github.com/czgdp1807) (Arrays and allocatables in LFortran)
* [Jakub Jelínek](https://github.com/kubajj) (Compiler arguments in fpm)
* [Rohit Goswami](https://rgoswami.me/) (Compiling dftatom with LFortran)
* [Thirumalai Shaktivel](https://github.com/Thirumalai-Shaktivel) (AST generation in LFortran)

You can read in more detail about their projects
[here](https://summerofcode.withgoogle.com/archive/2021/organizations/6542461173760000).

A big thank you to all students, mentors, and administrators, for their great
work, and of course, to the Google Summer of Code program for making
possible for students to participate.

We plan to apply for Google Summer of Code in 2022 as well.
The program is no longer limited to students and anybody 18 or older can apply
to work on an open source project and get paid for it.
If you're interested participating in the program in 2022 with Fortran-lang,
don't hesitate to contact us and we'll guide you toward applying.

## Conferences and papers

### FortranCon 2021

Like in 2020, the Fortran event of this year was
[FortranCon 2021](https://tcevents.chem.uzh.ch/event/14/), the international
Fortran conference, held on September 23-24.
The keynote speaker this year was [Damian Rouson](https://github.com/rouson),
the head of the
[Computer Languages and Systems Software (CLaSS)](https://crd.lbl.gov/divisions/amcr/computer-science-amcr/class/)
group at the Lawrence Berkeley National Lab. 
Fortran-lang had a dedicated session (a Fortran-lang minisymposium) on the
second day of the conference, with the talks about specific Fortran-lang
projects as well as the Google Summer of Code student presentations.
FortranCon was hosted at the University of Zurich (UZH), and organized by
[Tiziano Müller](https://github.com/dev-zero) from UZH and
[Alfio Lazzaro](https://github.com/alazzaro) from Hewlett-Packard Enterprise.
You can watch all FortranCon 2021 talks [here](https://www.youtube.com/playlist?list=PLeKbr7eYHjt5UaV9zQtY24oEbne9_uFni).

### PackagingCon 2021

Another big event for Fortran-lang was
[PackagingCon 2021](https://packaging-con.org/), a conference dedicated to
package management software and its community of developers.
[Sebastian Ehlert](https://github.com/awvwgk) presented fpm--you can watch the
recording below:

<center>
<iframe width="560" height="315" src="https://www.youtube.com/embed/YG8zEM1lAVM" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
</center>

### Papers

This year two papers have been written about Fortran-lang projects.
The first paper documents the motivation and goals of
Fortran-lang: "Toward Modern Fortran Tooling and a Thriving Developer
Community" by Milan Curcic, Ondřej Čertík, Brad Richardson, Sebastian Ehlert,
Laurence Kedward, Arjen Markus, Ivan Pribec, and Jérémie Vandenplas
([https://arxiv.org/abs/2109.07382](https://arxiv.org/abs/2109.07382)).

<center>
<a href="https://arxiv.org/abs/2109.07382">
<img width="600" src="{{ site.baseurl }}/assets/img/Fortran-lang-2021-in-review/fortran-lang-paper.png">
</a>
</center>

If Fortran-lang has been helpful in your work and if you want to cite this paper,
here's the citation info:

```bibtex
@article{curcic2021toward,
  title={Toward Modern Fortran Tooling and a Thriving Developer Community},
  author={Curcic, Milan and
          {\v{C}}ert{\'\i}k, Ond{\v{r}}ej and
          Richardson, Brad and
          Ehlert, Sebastian and
          Kedward, Laurence and
          Markus, Arjen and
          Pribec, Ivan and
          Vandenplas, J{\'e}r{\'e}mie},
  journal={arXiv preprint arXiv:2109.07382},
  year={2021}
}
```

We have also submitted a paper draft to IEEE's
[Computing in Science & Engineering (CiSE)](https://ieeexplore.ieee.org/xpl/RecentIssue.jsp?punumber=5992)
journal, titled "The State of Fortran" and led by
[Laurence Kedward](https://github.com/lkedward).
For this paper, we have
[publicly invited on Discourse](https://fortran-lang.discourse.group/t/fortran-lang-community-paper/1232)
anybody in the Fortran-lang community to participate.
Once published, this will be another paper that you can cite.
Stay tuned for its publication.

## Summary

* 2021 is behind us as another productive year.
* Fortran-lang flagship projects such as
  [stdlib](https://github.com/fortran-lang/stdlib),
  [fpm](https://github.com/fortran-lang/fpm),
  [fortran-lang.org](https://fortran-lang.org), and
  [LFortran](https://lfortran.org) continue to grow and gain traction.
* New Fortran-lang projects include
  [fftpack](https://github.com/fortran-lang/fftpack),
  [test-drive](https://github.com/fortran-lang/test-drive), and
  [fpm-docs](https://github.com/fortran-lang/fpm-docs).
* Fortran-lang participated in the
  [Google Summer of Code](https://summerofcode.withgoogle.com/) program for
  the first time, and had 6 students working on projects across stdlib, fpm,
  and LFortran.
* We had several presentations of Fortran-lang projects at
  [FortranCon 2021](https://www.youtube.com/playlist?list=PLeKbr7eYHjt5UaV9zQtY24oEbne9_uFni) and
  [PackagingCon 2021](https://www.youtube.com/watch?v=YG8zEM1lAVM).
* New Fortran-lang [paper](https://arxiv.org/abs/2109.07382) is out and 
another is in review.

## Thanks

We thank all people who contributed to Fortran-lang projects and discussions
on GitHub, Fortran Discourse, Fortran-lang mailing list, Twitter, and 
elsewhere.
It wouldn't have been possible without you all.

```fortran
end program fortran_lang_2021
```
