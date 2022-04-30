---
layout: post
title: "Fortran newsletter: May 2022"
category: newsletter
author: Milan Curcic
---

Welcome to the May edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

## fortran-lang.org

Here's what's new in the fortran-lang.org repo:

* [#359](https://github.com/fortran-lang/fortran-lang.org/pull/359):
  Fix time calculation in the PRs script
* [#387](https://github.com/fortran-lang/fortran-lang.org/pull/387):
  Newsletter for April 2022
* [#389](https://github.com/fortran-lang/fortran-lang.org/pull/389):
  Add librsb to package index
* [#390](https://github.com/fortran-lang/fortran-lang.org/pull/390):
  Add Elk to package index
* [#391](https://github.com/fortran-lang/fortran-lang.org/pull/391):
  Add pencil-code to package index
* [#392](https://github.com/fortran-lang/fortran-lang.org/pull/392):
  Add PROPACK to package index
* [#398](https://github.com/fortran-lang/fortran-lang.org/pull/398):
  Add feed link to HTML head element
* [#369](https://github.com/fortran-lang/fortran-lang.org/pull/369):
  Resolves Issue #217
* [#400](https://github.com/fortran-lang/fortran-lang.org/pull/400):
  fix dependency of include files under `learn/building_programs` mini-book

Work in progress:

* [#397](https://github.com/fortran-lang/fortran-lang.org/pull/397) (WIP):
  Add NUFFT to package index
* [#396](https://github.com/fortran-lang/fortran-lang.org/pull/396) (WIP):
  Add OpenFFT to package index
* [#395](https://github.com/fortran-lang/fortran-lang.org/pull/395) (WIP):
  Add 2DECOMP&FFT to package index
* [#394](https://github.com/fortran-lang/fortran-lang.org/pull/394) (WIP):
  Add SLICOT to package index
* [#393](https://github.com/fortran-lang/fortran-lang.org/pull/393) (WIP):
  Add FATODE to package index
* [#347](https://github.com/fortran-lang/fortran-lang.org/pull/347) (WIP):
  Fortran Intrinsics

[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues)
if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the
[contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/CONTRIBUTING.md)
for how to get started.

## Fortran Standard Library

Here's what's new in stdlib:

* [#646](https://github.com/fortran-lang/stdlib/pull/646):
  Export symbols on Windows and set PIC flag for Unix
* [#651](https://github.com/fortran-lang/stdlib/pull/651):
  Bugfix release version 0.2.1

Work in progress:

* [#656](https://github.com/fortran-lang/stdlib/pull/656) (WIP):
  Add hint for building error with make
* [#655](https://github.com/fortran-lang/stdlib/pull/655) (WIP):
  fixed 32-bit integer overflow in stdlib_io_npy
* [#652](https://github.com/fortran-lang/stdlib/pull/652) (WIP):
  Feature: loadtxt skiprows and max_rows
* [#625](https://github.com/fortran-lang/stdlib/pull/625) (WIP):
  Gamma special function 
* [#611](https://github.com/fortran-lang/stdlib/pull/611) (WIP):
  Hash maps
* [#604](https://github.com/fortran-lang/stdlib/pull/604) (WIP):
  Add get_argument, get_variable and set_variable
* [#580](https://github.com/fortran-lang/stdlib/pull/580) (WIP):
  Add terminal and color escape sequences
* [#552](https://github.com/fortran-lang/stdlib/pull/552) (WIP):
  fixed bug in stringlist
* [#536](https://github.com/fortran-lang/stdlib/pull/536) (WIP):
  Fix conversion warnings
* [#520](https://github.com/fortran-lang/stdlib/pull/520) (WIP):
  [stdlib_io] add `disp`(display variable values formatted).
* [#517](https://github.com/fortran-lang/stdlib/pull/517) (WIP):
  adding SPEC_TEMPLATE.md #504
* [#514](https://github.com/fortran-lang/stdlib/pull/514) (WIP):
  pop, drop & get with basic range feature for stringlist
* [#491](https://github.com/fortran-lang/stdlib/pull/491) (WIP):
  Stdlib linked list
* [#473](https://github.com/fortran-lang/stdlib/pull/473) (WIP):
  Error stop improvements
* [#363](https://github.com/fortran-lang/stdlib/pull/363) (WIP):
  Sorting string's characters according to their ASCII values
* [#286](https://github.com/fortran-lang/stdlib/pull/286) (WIP):
  Probability Distribution and Statistical Functions -- Beta Distribution Module
* [#278](https://github.com/fortran-lang/stdlib/pull/278) (WIP):
  Probability Distribution and Statistical Functions -- Gamma Distribution Module
* [#189](https://github.com/fortran-lang/stdlib/pull/189) (WIP):
  Initial implementation of COO / CSR sparse format

Please help improve stdlib by testing and [reviewing pull requests](https://github.com/fortran-lang/stdlib/issues?q=is%3Apr+is%3Aopen+label%3A%22reviewers+needed%22)!

The candidate for file system operations to be included in stdlib is being developed by
[@MarDiehl](https://github.com/MarDiehl) and [@arjenmarkus](https://github.com/arjenmarkus)
in [this repository](https://github.com/MarDiehl/stdlib_os).
Please try it out and let us know how it works, if there are any issues, or if the API can be improved.

## Fortran Package Manager

Here's what's new in fpm:

* [#688](https://github.com/fortran-lang/fpm/pull/688):
  Small fix for fpm_model
* [#665](https://github.com/fortran-lang/fpm/pull/665):
  add clean command

Work in progress:

* [#693](https://github.com/fortran-lang/fpm/pull/693) (WIP):
  Fix show-model option
* [#692](https://github.com/fortran-lang/fpm/pull/692) (WIP):
  Fix for non-portable GFortran `-J` flag in install script
* [#686](https://github.com/fortran-lang/fpm/pull/686) (WIP):
  fix: remove extra space from help-test cmd
* [#685](https://github.com/fortran-lang/fpm/pull/685) (WIP):
  fix: function for getting executable path
* [#676](https://github.com/fortran-lang/fpm/pull/676) (WIP):
  Tree shaking for modules
* [#671](https://github.com/fortran-lang/fpm/pull/671) (WIP):
  Add `library-dir` to support `-Lpath`
* [#653](https://github.com/fortran-lang/fpm/pull/653) (WIP):
  Enable profiles in toml
* [#608](https://github.com/fortran-lang/fpm/pull/608) (WIP):
  --env switch lets you specify the prefix of the compiler-related environment variables
* [#539](https://github.com/fortran-lang/fpm/pull/539) (WIP):
  Add parent packages into dependency tree
* [#498](https://github.com/fortran-lang/fpm/pull/498) (WIP):
  Compiler flags profiles

`fpm` is still in early development and we need as much help as we can get.
Here's how you can help today:

* Use it and let us know what you think! Read the [fpm packaging guide](https://fpm.fortran-lang.org/en/tutorial)
to learn how to build your package with fpm, and the [manifest reference](https://fpm.fortran-lang.org/en/spec/manifest.html)
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

TODO @alexisperry

### LFortran

TODO @certik

We are looking for new contributors. Please do not hesitate to contact us if you are interested. We will help you get up to speed.

## Events

* We had our 25th Fortran Monthly call on April 22.
  You can watch the recording below:
  <iframe width="560" height="315" src="https://www.youtube.com/embed/8-_ll4f0gN8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

Join and follow the [Fortran Discourse](https://fortran-lang.discourse.group)
to stay tuned with the future meetings.

## Contributors

We thank everybody who contributed to fortran-lang in the past month by
commenting in any of these repositories:

* [fortran-lang/stdlib](https://github.com/fortran-lang/stdlib)
* [fortran-lang/stdlib-cmake-example](https://github.com/fortran-lang/stdlib-cmake-example)
* [fortran-lang/fpm](https://github.com/fortran-lang/fpm)
* [fortran-lang/fpm-registry](https://github.com/fortran-lang/fpm-registry)
* [fortran-lang/fpm-docs](https://github.com/fortran-lang/fpm-docs)
* [fortran-lang/setup-fpm](https://github.com/fortran-lang/setup-fpm)
* [fortran-lang/fpm-haskell](https://github.com/fortran-lang/fpm-haskell)
* [fortran-lang/fortran-lang.org](https://github.com/fortran-lang/fortran-lang.org)
* [fortran-lang/benchmarks](https://github.com/fortran-lang/benchmarks)
* [fortran-lang/fortran-forum-article-template](https://github.com/fortran-lang/fortran-forum-article-template)
* [fortran-lang/fftpack](https://github.com/fortran-lang/fftpack)
* [fortran-lang/minpack](https://github.com/fortran-lang/minpack)
* [fortran-lang/test-drive](https://github.com/fortran-lang/test-drive)
* [j3-fortran/fortran\_proposals](https://github.com/j3-fortran/fortran_proposals)

<div id="gh-contributors" data-startdate="April 01 2022" data-enddate="April 30 2022" height="500px"></div>
