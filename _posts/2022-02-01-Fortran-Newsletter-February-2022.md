---
layout: post
title: "Fortran newsletter: February 2022"
category: newsletter
authors: Jérémie Vandenplas, Alexis Perry-Holby
---

Welcome to the February 2022 edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

## fortran-lang.org

Here's what's new and ongoing in the fortran-lang.org repo:

* [#369](https://github.com/fortran-lang/fortran-lang.org/pull/369):
  Resolves Issue #217
* [#359](https://github.com/fortran-lang/fortran-lang.org/pull/359):
  Fix time calculation in the PRs script

### Work in progress

* [#347](https://github.com/fortran-lang/fortran-lang.org/pull/347) (WIP):
  Fortran Intrinsics

[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues)
if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the
[contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/CONTRIBUTING.md)
for how to get started.

## Fortran Standard Library

Here's what's new in stdlib:

* [#276](https://github.com/fortran-lang/stdlib/pull/276):
  Probability Distribution and Statistical Functions -- Exponential Distribution Module
* [#605](https://github.com/fortran-lang/stdlib/pull/605):
  [stdlib_math] Add function `diff`
* [#613](https://github.com/fortran-lang/stdlib/pull/613):
  Ignore hash testing binaries and logs
* [#617](https://github.com/fortran-lang/stdlib/pull/617):
  Made format constant public
* [#622](https://github.com/fortran-lang/stdlib/pull/622):
  Fix Gauss quadrature

### Work in progress

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

* [#630](https://github.com/fortran-lang/fpm/pull/630):
  allow backfilling of current directory in fpm-new subcommand
* [#646](https://github.com/fortran-lang/fpm/pull/646):
  Respect user provided main-files
* [#645](https://github.com/fortran-lang/fpm/pull/645):
  Update module output directory command for flang-new/f18

### Work in progress

* [#654](https://github.com/fortran-lang/fpm/pull/654) (WIP):
  Ignore hidden source files
* [#653](https://github.com/fortran-lang/fpm/pull/653) (WIP):
  Enable profiles in toml
* [#652](https://github.com/fortran-lang/fpm/pull/652) (WIP):
  Get user name and email using got config if available else use defaults
* [#648](https://github.com/fortran-lang/fpm/pull/648) (WIP):
  Add note about relocation of manifest reference
* [#622](https://github.com/fortran-lang/fpm/pull/622) (WIP):
  Cleanup the backend output
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

Recent development updates:

* FIR
    * Keep runtime function name in comment
    * Add a conversion for !fir.coordinate_of
    * Add a new memory allocation rewrite pass.
    * Correct and reenable test that was removed by MLIR.
    * Fix overallocation by fir-to-llvm-ir pass
* OpenMP
    * Add some semantic checks for threadprivate and declare target directives
* Simplify RaggedArrayHeader and make it plain C struct
* Fix folding of ac-implied-do indices in structure constructors
* Avoid code duplication in mixed expressions
* Add test with shape for allocmem and freemem
* Make the frontend driver error out when requesting multiple actions
* Add semantics tests for co_reduce, co_min, and co_max
* Use GNUInstallDirs to support custom installation dirs.
* Enable support for conversion of recursive record types
* Separate temporary and user-specified object files
* update to reflect MLIR LLVM::GEPOp changes
* Do not lose call in shape inquiry on function reference
* Fix the documentation on how to build flang
* Add tests for converting arrays and refs to arrays
* Make the "flang" wrapper script check the Bash version
* Fix handling of space between # and name in preprocessor stringification
* RESHAPE(PAD=) can be arbitrary array rank
* Any type can appear in a structure constructor for an unlimited polymorphic allocatable component
* Implement semantics for DEC STRUCTURE/RECORD
* Extension: initialization of LOGICAL with INTEGER & vice versa
* Allow initialization in blank COMMON
* Support extension intrinsic function variations on ABS
* Allow pointers to non-sequence types in sequence types
* "CFI" types for Fortran REAL and COMPLEX kinds 2, 3, 10, 16
* Legacy extension: non-character formats
* Signal runtime error on WRITE after ENDFILE
* Don't blank-fill remaining lines in internal output
* Accept ENTRY names in generic interfaces
* Support substring references in NAMELIST input

Call notes are recorded and available upon request [here](https://docs.google.com/document/d/10T-S2J3GrahpG4Ooif93NSTz2zBW0MQc_RlwHi0-afY). Please contact Alexis Perry-Holby at aperry@lanl.gov for document access.

### LFortran

TO DO


We are looking for new contributors. Please do not hesitate to contact us if
you are interested. We will help you get up to speed.

## Events

* fpm has a new documentation website hosted at
  [fpm.fortran-lang.org](https://fpm.fortran-lang.org/).
  This website will provide user-oriented tutorials and how-to guides, as well
  as developer-oriented reference documents and specifications.
  We welcome all contributions to the fpm documentation, including translations
  to other languages.
  Please visit the [fpm-docs repo](https://github.com/fortran-lang/fpm-docs) to
  get started.
* We had our 22st Fortran Monthly call on January 17.
  You can watch the recording below:
  <iframe width="560" height="315" src="https://www.youtube.com/embed/93AObg7HsqM" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

As usual, subscribe to the [mailing list](https://groups.io/g/fortran-lang) and/or
join the [Discourse](https://fortran-lang.discourse.group) to stay tuned with the future meetings.

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
* [fortran-lang/test-drive](https://github.com/fortran-lang/test-drive)
* [j3-fortran/fortran\_proposals](https://github.com/j3-fortran/fortran_proposals)

<div id="gh-contributors" data-startdate="January 01 2022" data-enddate="February 01 2022" height="500px"></div>
