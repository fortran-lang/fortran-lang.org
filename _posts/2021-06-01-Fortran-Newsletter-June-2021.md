---
layout: post
title: "Fortran newsletter: June 2021"
category: newsletter
author: Sebastian Ehlert, Milan Curcic
---

Welcome to the June 2021 edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

## fortran-lang.org

This month we've had several updates to the website:

* [#259](https://github.com/fortran-lang/fortran-lang.org/pull/259):
  MapTran3D, RPNcalc, Gemini3D and Blocktran were added to the package index
* [#253](https://github.com/fortran-lang/fortran-lang.org/pull/253):
  Fixed grammar in Easy to learn section

Ongoing work:

* [#201](https://github.com/fortran-lang/fortran-lang.org/pull/201) (WIP):
  Internationalization for fortran-lang
* [#246](https://github.com/fortran-lang/fortran-lang.org/pull/246) (WIP):
  Transferring fortran90.org “Fortran Best Practise” into a mini-book
* [#255](https://github.com/fortran-lang/fortran-lang.org/pull/255) (WIP):
  Quickstart edits
* [#261](https://github.com/fortran-lang/fortran-lang.org/pull/261) (WIP):
  Script for summarizing PRs

[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues) if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the [contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/master/CONTRIBUTING.md) for how to get started.

## Fortran Standard Library

Here's what's new in `stdlib`:

* [#355](https://github.com/fortran-lang/stdlib/pull/355):
  Implement clip function
* [#359](https://github.com/fortran-lang/stdlib/pull/359):
  Add general contributing guidelines to stdlib
* [#407](https://github.com/fortran-lang/stdlib/pull/407):
  Changed to\_title to to\_sentence and implemented correct to\_title

Work in progress:

* [#189](https://github.com/fortran-lang/stdlib/pull/189) (WIP):
  Initial implementation of sparse matrices.
* [#272](https://github.com/fortran-lang/stdlib/pull/272) (WIP),
  [#273](https://github.com/fortran-lang/stdlib/pull/273) (WIP),
  [#276](https://github.com/fortran-lang/stdlib/pull/276) (WIP),
  [#278](https://github.com/fortran-lang/stdlib/pull/278) (WIP):
  Implementation of the `stdlib_stats_distribution` modules.
  It provides probability distribution and statistical functions.
* [#311](https://github.com/fortran-lang/stdlib/pull/311) (WIP):
  Implementation of a module for handling lists of strings
* [#313](https://github.com/fortran-lang/stdlib/pull/313) (WIP):
  Legendre polynomials and Gaussian quadrature
* [#333](https://github.com/fortran-lang/stdlib/pull/333) (WIP):
  Provide abstract base class for a string object
* [#353](https://github.com/fortran-lang/stdlib/pull/353) (WIP):
  Initial checkin for a module for tolerant comparison of reals
* [#363](https://github.com/fortran-lang/stdlib/pull/363) (WIP):
  Add sort to stdlib\_string\_type module
* [#408](https://github.com/fortran-lang/stdlib/pull/408) (WIP):
  Addition of the stdlib\_sorting module


Please help improve stdlib by testing and [reviewing pull requests](https://github.com/fortran-lang/stdlib/issues?q=is%3Apr+is%3Aopen+label%3A%22reviewers+needed%22)!

The candidate for file system operations to be included in stdlib is being developed by
[@MarDiehl](https://github.com/MarDiehl) and [@arjenmarkus](https://github.com/arjenmarkus)
in [this repository](https://github.com/MarDiehl/stdlib_os).
Please try it out and let us know how it works, if there are any issues, or if the API can be improved.

## Fortran Package Manager

Work in progress:

* [First beta release](https://github.com/fortran-lang/fpm/milestone/1) (WIP):
  First feature-complete release of the Fortran implementation.
* [#364](https://github.com/fortran-lang/fpm/pull/364) (WIP):
  Plugin alpha version
* [#423](https://github.com/fortran-lang/fpm/pull/423) (WIP):
  Use default instead of master to reference the repository HEAD
* [#444](https://github.com/fortran-lang/fpm/pull/444) (WIP):
  Allow to find include files / modules in CPATH environment variable
* [#449](https://github.com/fortran-lang/fpm/pull/449) (WIP):
  Response files with ar on Windows
* [#450](https://github.com/fortran-lang/fpm/pull/450) (WIP):
  Remove coarray flag from intel debug settings
* [#451](https://github.com/fortran-lang/fpm/pull/451) (WIP):
  Refactor: use objects to represent compilers and archiver

`fpm` is still in early development and we need as much help as we can get.
Here's how you can help today:

* Use it and let us know what you think! Read the [fpm packaging guide](https://github.com/fortran-lang/fpm/blob/master/PACKAGING.md) to learn how to build your package with fpm, and the [manifest reference](https://github.com/fortran-lang/fpm/blob/master/manifest-reference.md) to learn what are all the things that you can specify in the fpm.toml file.
* Browse existing *fpm* packages on the [fortran-lang website](https://fortran-lang.org/packages/fpm)
* Browse the [open issues](https://github.com/fortran-lang/fpm/issues) and see if you can help implement any fixes or features.
* Adapt your Fortran package for fpm and submit it to the [Registry](https://github.com/fortran-lang/fpm-registry).
* Improve the documentation.

The short-term goal of fpm is to make development and installation of Fortran packages with dependencies easier.
Its long term goal is to build a rich and decentralized ecosystem of Fortran packages and create a healthy
environment in which new open source Fortran projects are created and published with ease.

## Compilers

### Classic Flang


### LLVM Flang


### LFortran


## Events

* We had our 12th Fortran Monthly call on May 20.
You can watch the recording below:

<iframe width="560" height="315" src="https://www.youtube.com/embed/06hVFA8ApG4" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

* Google Summer of Code program has announced the allocation of students to each project.
  Fortran-lang received six studens (one through [NumFOCUS](https://numfocus.org/)) who will work across three subprojects: stdlib, fpm, and LFortran.
  Congratulations and welcome to students
  [Aman Godara](https://github.com/aman-godara),
  [Rohit Goswami](https://github.com/haozeke),
  [Jakub Jelínek](https://github.com/kubajj),
  [Chetan Karwa](https://github.com/chetankarwa),
  [Thirumalai Shaktivel](https://gitlab.com/Thirumalai-Shaktivel), and
  [Gagandeep Singh](https://github.com/czgdp1807).
  Read the full post [here](https://fortran-lang.org/newsletter/2021/05/18/Welcome-GSoC-students/).
  
As usual, subscribe to the [mailing list](https://groups.io/g/fortran-lang) and/or
join the [Discourse](https://fortran-lang.discourse.group) to stay tuned with the future meetings.

## Contributors

We thank everybody who contributed to fortran-lang in the past month by
commenting in any of these repositories:

* [fortran-lang/stdlib](https://github.com/fortran-lang/stdlib)
* [fortran-lang/stdlib-cmake-example](https://github.com/fortran-lang/stdlib-cmake-example)
* [fortran-lang/fpm](https://github.com/fortran-lang/fpm)
* [fortran-lang/fpm-registry](https://github.com/fortran-lang/fpm-registry)
* [fortran-lang/setup-fpm](https://github.com/fortran-lang/setup-fpm)
* [fortran-lang/fpm-haskell](https://github.com/fortran-lang/fpm-haskell)
* [fortran-lang/fortran-lang.org](https://github.com/fortran-lang/fortran-lang.org)
* [fortran-lang/benchmarks](https://github.com/fortran-lang/benchmarks)
* [fortran-lang/fortran-forum-article-template](https://github.com/fortran-lang/fortran-forum-article-template)
* [j3-fortran/fortran\_proposals](https://github.com/j3-fortran/fortran_proposals)

<div id="gh-contributors" data-startdate="May 01 2021" data-enddate="May 31 2021" height="500px"></div>
