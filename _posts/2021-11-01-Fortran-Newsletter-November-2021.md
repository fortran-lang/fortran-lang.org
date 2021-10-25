---
layout: post
title: "Fortran newsletter: November 2021"
category: newsletter
author: Sebastian Ehlert
---

Welcome to the October 2021 edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>


## fortran-lang.org

This month we've had several updates to the website:

* [#341](https://github.com/fortran-lang/fortran-lang.org/pull/341):
  Add Cantera to package index
* [#329](https://github.com/fortran-lang/fortran-lang.org/pull/329):
  Quantum Information book, WSL GUI, and typos
* [#340](https://github.com/fortran-lang/fortran-lang.org/pull/340):
  Minor fixes in Best Practices

### Work in progress

* [#201](https://github.com/fortran-lang/fortran-lang.org/pull/201) (WIP):
  Internationalization for fortran-lang


[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues)
if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the
[contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/master/CONTRIBUTING.md)
for how to get started.


## Fortran Standard Library

Here's what's new in stdlib:

* [0.1.0](https://github.com/fortran-lang/stdlib/releases/tag/v0.1.0):
  Initial version of the Fortran standard library
* [#543](https://github.com/fortran-lang/stdlib/pull/543):
  Fix string concat

### Work in progress

* [#554](https://github.com/fortran-lang/stdlib/pull/554) (WIP):
  Hash functions
* [#552](https://github.com/fortran-lang/stdlib/pull/552) (WIP):
  Fix bug in stringlist
* [#536](https://github.com/fortran-lang/stdlib/pull/536) (WIP):
  Fix conversion warnings
* [#520](https://github.com/fortran-lang/stdlib/pull/520) (WIP):
  [stdlib\_io] add `disp` (display your data).
* [#517](https://github.com/fortran-lang/stdlib/pull/517) (WIP):
  adding SPEC\_TEMPLATE.md
* [#514](https://github.com/fortran-lang/stdlib/pull/514) (WIP):
  pop, drop & get with basic range feature for stringlist
* [#500](https://github.com/fortran-lang/stdlib/pull/500) (WIP):
  Selection algorithms
* [#499](https://github.com/fortran-lang/stdlib/pull/499) (WIP):
  [stdlib\_linalg] matrix property checks
* [#498](https://github.com/fortran-lang/stdlib/pull/498) (WIP):
  [stdlib\_math] add `arg/argd/argpi`
* [#494](https://github.com/fortran-lang/stdlib/pull/494) (WIP):
  Add testing module to allow better structuring of test suites
* [#491](https://github.com/fortran-lang/stdlib/pull/491) (WIP):
  Stdlib linked list
* [#488](https://github.com/fortran-lang/stdlib/pull/488) (WIP):
  [stdlib\_math] add `is_close` routines.
* [#473](https://github.com/fortran-lang/stdlib/pull/473) (WIP):
  Error stop improvements
* [#363](https://github.com/fortran-lang/stdlib/pull/363) (WIP):
  Sorting string's characters according to their ASCII values
* [#353](https://github.com/fortran-lang/stdlib/pull/353) (WIP):
  Initial checkin for a module for tolerant comparison of reals
* [#286](https://github.com/fortran-lang/stdlib/pull/286) (WIP):
  Probability Distribution and Statistical Functions -- Beta Distribution Module
* [#278](https://github.com/fortran-lang/stdlib/pull/278) (WIP):
  Probability Distribution and Statistical Functions -- Gamma Distribution Module
* [#276](https://github.com/fortran-lang/stdlib/pull/276) (WIP):
  Probability Distribution and Statistical Functions -- Exponential Distribution Module
* [#273](https://github.com/fortran-lang/stdlib/pull/273) (WIP):
  Probability Distribution and Statistical Functions -- Normal Distribution Module 
* [#189](https://github.com/fortran-lang/stdlib/pull/189) (WIP):
  Initial implementation of COO / CSR sparse format


Please help improve stdlib by testing and [reviewing pull requests](https://github.com/fortran-lang/stdlib/issues?q=is%3Apr+is%3Aopen+label%3A%22reviewers+needed%22)!

The candidate for file system operations to be included in stdlib is being developed by
[@MarDiehl](https://github.com/MarDiehl) and [@arjenmarkus](https://github.com/arjenmarkus)
in [this repository](https://github.com/MarDiehl/stdlib_os).
Please try it out and let us know how it works, if there are any issues, or if the API can be improved.

## Fortran Package Manager

Here's what's new in fpm:

* [#597](https://github.com/fortran-lang/fpm/pull/597):
  Add LFortran optimization flag to release profile
* [#595](https://github.com/fortran-lang/fpm/pull/595):
  List names without suffix (mainly for Windows)
* [#590](https://github.com/fortran-lang/fpm/pull/590):
  Change link command on Windows with `ifort` or `ifx`
* [#575](https://github.com/fortran-lang/fpm/pull/575):
  Enable multiple build output directories
* [#587](https://github.com/fortran-lang/fpm/pull/587):
  Bootstrapping instructions version update

### Work in progress

* [#569](https://github.com/fortran-lang/fpm/pull/569) (WIP):
  Add workflow for continuous delivery
* [#539](https://github.com/fortran-lang/fpm/pull/539) (WIP):
  Add parent packages into dependency tree
* [#498](https://github.com/fortran-lang/fpm/pull/498) (WIP):
  Compiler flags profiles


`fpm` is still in early development and we need as much help as we can get.
Here's how you can help today:

* Use it and let us know what you think! Read the [fpm packaging guide](https://github.com/fortran-lang/fpm/blob/main/PACKAGING.md)
to learn how to build your package with fpm, and the [manifest reference](https://github.com/fortran-lang/fpm/blob/main/manifest-reference.md)
to learn what are all the things that you can specify in the fpm.toml file.

* Browse existing *fpm* packages on the [fortran-lang website](https://fortran-lang.org/packages/fpm)
* Browse the [open issues](https://github.com/fortran-lang/fpm/issues) and see if you can help implement any fixes or features.
* Adapt your Fortran package for fpm and submit it to the [Registry](https://github.com/fortran-lang/fpm-registry).
* Improve the documentation.

The short-term goal of fpm is to make development and installation of Fortran packages with dependencies easier.
Its long term goal is to build a rich and decentralized ecosystem of Fortran packages and create a healthy
environment in which new open source Fortran projects are created and published with ease.


## Compilers

### Flang

Recent development updates:


Call notes are recorded and available upon request [here](https://docs.google.com/document/d/10T-S2J3GrahpG4Ooif93NSTz2zBW0MQc_RlwHi0-afY). Please contact Alexis Perry-Holby at aperry@lanl.gov for document access.

### LFortran


We are looking for new contributors. Please do not hesitate to contact us if
you are interested. We will help you get up to speed.


## Events

* We had our 17th Fortran Monthly call on September 19.
  You can watch the recording below:
  <iframe width="560" height="315" src="https://www.youtube.com/embed/4gOoEzMEh6g" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

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
* [fortran-lang/fftpack](https://github.com/fortran-lang/fftpack)
* [j3-fortran/fortran\_proposals](https://github.com/j3-fortran/fortran_proposals)

<div id="gh-contributors" data-startdate="October 01 2021" data-enddate="October 31 2021" height="500px"></div>
