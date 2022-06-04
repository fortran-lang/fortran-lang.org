---
layout: post
title: "Fortran newsletter: June 2022"
category: newsletter
author: Milan Curcic
---

Welcome to the June edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

## fortran-lang.org

Here's what's new in the fortran-lang.org repo:

* [#401](https://github.com/fortran-lang/fortran-lang.org/pull/401):
  Newsletter May 2022
* [#403](https://github.com/fortran-lang/fortran-lang.org/pull/403):
  Add SeisSol to package index

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

* [#656](https://github.com/fortran-lang/stdlib/pull/656):
  Add hint for building error with make
* [#655](https://github.com/fortran-lang/stdlib/pull/655):
  fixed 32-bit integer overflow in stdlib_io_npy
* [#657](https://github.com/fortran-lang/stdlib/pull/657):
  Remove support for manual make builds

Work in progress:

* [#660](https://github.com/fortran-lang/stdlib/pull/660) (WIP):
  Fix erroneous gaussian quadrature points in gauss_legendre
* [#659](https://github.com/fortran-lang/stdlib/pull/659) (WIP):
  Readme update
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

* [#692](https://github.com/fortran-lang/fpm/pull/692):
  Fix for non-portable GFortran `-J` flag in install script
* [#693](https://github.com/fortran-lang/fpm/pull/693):
  Fix show-model option

Work in progress:

* [#701](https://github.com/fortran-lang/fpm/pull/701) (WIP):
  Some cleanups and minor fixes
* [#686](https://github.com/fortran-lang/fpm/pull/686) (WIP):
  fix: remove extra space from help-test cmd
* [#685](https://github.com/fortran-lang/fpm/pull/685) (WIP):
  fix: function for getting executable path
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

TODO @AlexisPerry

### LFortran

TODO @certik

We are looking for new contributors. Please do not hesitate to contact us if you are interested. We will help you get up to speed.

## Events

* The Fortran-lang Google Summer of Code 2022 program began on May 23.
  We welcome five contributors: Arteev Raina, Ashirwad Mishra,
  Henil Shalin Panchal, Mohd Ubaid Shaikh, and Oshanath Rajawasam.
  They will be working on exciting projects from fpm and the Fortran
  website to improving the LFortran compiler.
  Read more about their projects [here](https://summerofcode.withgoogle.com/programs/2022/organizations/fortran-lang).
* We had our 26th Fortran Monthly call on May 16.
  Watch the recording below:
  <iframe width="560" height="315" src="https://www.youtube.com/embed/apuldZF_r2I" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

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
* [fortran-lang/vscode-fortran-support](https://github.com/fortran-lang/vscode-fortran-support)
* [j3-fortran/fortran\_proposals](https://github.com/j3-fortran/fortran_proposals)

<div id="gh-contributors" data-startdate="May 01 2022" data-enddate="May 31 2022" height="500px"></div>
