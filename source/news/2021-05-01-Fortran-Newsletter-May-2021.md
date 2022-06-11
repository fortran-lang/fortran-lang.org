---
layout: post
title: "Fortran newsletter: May 2021"
category: newsletter
date: 2021-05-01
author: Sebastian Ehlert, Alexis Perry-Holby, Milan Curcic, Ondřej Čertík, Laurence Kedward
---

Welcome to the May 2021 edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

# fortran-lang.org

This month we've had several updates to the website:

* [#244](https://github.com/fortran-lang/fortran-lang.org/pull/244):
  Add a first year announcement
* [#236](https://github.com/fortran-lang/fortran-lang.org/pull/236):
  Add dl\_poly\_4 to package index
* [#220](https://github.com/fortran-lang/fortran-lang.org/pull/220):
  Include learn resources to online courses

Ongoing work:

* [#201](https://github.com/fortran-lang/fortran-lang.org/pull/201) (WIP):
  Internationalization for fortran-lang
* [#246](https://github.com/fortran-lang/fortran-lang.org/pull/246) (WIP):
  Transferring fortran90.org “Fortran Best Practise” into a mini-book

[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues) if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the [contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/CONTRIBUTING.md) for how to get started.

## Fortran Standard Library

Here's what's new in `stdlib`:

* [#391](https://github.com/fortran-lang/stdlib/pull/391):
  Add issue templates
* [#388](https://github.com/fortran-lang/stdlib/pull/388):
  Changed filenames for bitset tests
* [#384](https://github.com/fortran-lang/stdlib/pull/384):
  Implement starts\_with and ends\_with functions
* [#367](https://github.com/fortran-lang/stdlib/pull/367):
  Add Intel compiler workflow for OSX
* [#360](https://github.com/fortran-lang/stdlib/pull/360):
  Summarize build toolchain workflow and implied rules
* [#343](https://github.com/fortran-lang/stdlib/pull/343):
  Implement strip and chomp as supplement to trim
* [#336](https://github.com/fortran-lang/stdlib/pull/336):
  Add functions to convert integer/logical values to character values

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
* [#349](https://github.com/fortran-lang/stdlib/pull/349) (WIP):
  Simplify test makefile
* [#353](https://github.com/fortran-lang/stdlib/pull/353) (WIP):
  Initial checkin for a module for tolerant comparison of reals
* [#355](https://github.com/fortran-lang/stdlib/pull/355) (WIP):
  Implement clip function
* [#359](https://github.com/fortran-lang/stdlib/pull/359) (WIP):
  Add general contributing guidelines to stdlib
* [#363](https://github.com/fortran-lang/stdlib/pull/363) (WIP):
  Add sort to stdlib\_string\_type module
* [#372](https://github.com/fortran-lang/stdlib/pull/372) (WIP):
  Correct implementation of to\_title
* [#386](https://github.com/fortran-lang/stdlib/pull/386) (WIP):
  Start the addition of the module stdlib\_sorting


Please help improve stdlib by testing and [reviewing pull requests](https://github.com/fortran-lang/stdlib/issues?q=is%3Apr+is%3Aopen+label%3A%22reviewers+needed%22)!

The candidate for file system operations to be included in stdlib is being developed by
[@MarDiehl](https://github.com/MarDiehl) and [@arjenmarkus](https://github.com/arjenmarkus)
in [this repository](https://github.com/MarDiehl/stdlib_os).
Please try it out and let us know how it works, if there are any issues, or if the API can be improved.

## Fortran Package Manager

Here's what's new in `fpm`:

* [#420](https://github.com/fortran-lang/fpm/pull/420):
  Phase out Haskell fpm
* [#468](https://github.com/fortran-lang/fpm/pull/468):
  Identify OpenBSD
* [#465](https://github.com/fortran-lang/fpm/pull/465):
  Fix typo in README
* [#442](https://github.com/fortran-lang/fpm/pull/442):
  Use lib instead of ar on Windows
* [#440](https://github.com/fortran-lang/fpm/pull/440):
  Minor edits to README
* [#438](https://github.com/fortran-lang/fpm/pull/438):
  Add external-modules key to build table for non-fpm modules
* [#437](https://github.com/fortran-lang/fpm/pull/437):
  Remove coarray single from default Intel flags
* [#433](https://github.com/fortran-lang/fpm/pull/433):
  Fix to allow compiling C with Intel CC
* [#431](https://github.com/fortran-lang/fpm/pull/431):
  Use different compiler flags on differnt platforms for Intel
* [#429](https://github.com/fortran-lang/fpm/pull/429):
  Use wget if curl is missing in install.sh

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

* Use it and let us know what you think! Read the [fpm packaging guide](https://github.com/fortran-lang/fpm/blob/HEAD/PACKAGING.md) to learn how to build your package with fpm, and the [manifest reference](https://github.com/fortran-lang/fpm/blob/HEAD/manifest-reference.md) to learn what are all the things that you can specify in the fpm.toml file.
* Browse existing *fpm* packages on the [fortran-lang website](https://fortran-lang.org/packages/fpm)
* Browse the [open issues](https://github.com/fortran-lang/fpm/issues) and see if you can help implement any fixes or features.
* Adapt your Fortran package for fpm and submit it to the [Registry](https://github.com/fortran-lang/fpm-registry).
* Improve the documentation.

The short-term goal of fpm is to make development and installation of Fortran packages with dependencies easier.
Its long term goal is to build a rich and decentralized ecosystem of Fortran packages and create a healthy
environment in which new open source Fortran projects are created and published with ease.

## Compilers

### Classic Flang

A total of 5 pull requests were merged in April.

* [PR#1021 Switch to new LLVM License](https://github.com/flang-compiler/flang/pull/1021)
* [PR#1025 runtime: register atfork handler to re-initialize internal flangrti locks at fork](https://github.com/flang-compiler/flang/pull/1025)
* [PR#1026 Test case update for #895](https://github.com/flang-compiler/flang/pull/1026)
* [PR#1030 Update README.md](https://github.com/flang-compiler/flang/pull/1030)
* [PR#1034 Github Action use the prebuilt clang to build flang](https://github.com/flang-compiler/flang/pull/1034)


### LLVM Flang

Recent development updates:

* OpenMP
    * [OPENMP5.1]Initial support for novariants clause.
    * [OPENMP5.1]Initial support for nocontext clause.
    * Add functionality to check "close nesting" of regions, which can be used for Semantic checks
    * [OpenMP5.1] Initial support for masked directive and filter clause
    * Modify semantic check for nesting of `ordered` regions to include `close` nesting check.
    * Remove `OmpEndLoopDirective` handles from code.
    * Add General Semantic Checks for Allocate Directive
* New Driver
    * Add options for -Werror
    * Modify the existing test cases that use -Mstandard in f18, to use -pedantic and %flang_fc1 to share with the new driver
    * Add support for `-cpp/-nocpp`
    * Fix `-fdebug-dump-provenance`
    * Add debug options not requiring semantic checks
    * Remove `%flang-new` from the LIT configuration
    * Update the regression tests to use the new driver when enabled
    * Add support for `-fget-definition`
* Move .f77 to the list of fixed-form file extensions
* Runtime
    * Implement reductions
    * Implement numeric intrinsic functions
    * TRANSFER() intrinsic function
    * RANDOM_NUMBER, RANDOM_SEED, RANDOM_INIT
    * Implement IPARITY, PARITY, and FINDLOC reductions
* Fix unit test failure on POWER
* Improve constant folding for type parameter inquiries
* Check for conflicting BIND(C) names
* Enforce a limit on recursive PDT instantiations
* Accept & fold IEEE_SELECTED_REAL_KIND
* Define missing & needed IEEE_ARITHMETIC symbols
* Handle instantiation of procedure pointer components
* Fix checking of argument passing for parameterized derived types
* Fix spurious errors from runtime derived type table construction
* Check for attributes specific to dummy arguments
* Handle structure constructors with forward references to PDTs

Call notes will be sent to the _flang-dev_ email list and also recorded [here](https://docs.google.com/document/d/10T-S2J3GrahpG4Ooif93NSTz2zBW0MQc_RlwHi0-afY).

### LFortran

* 98 Merge Requests merged in April
* Working towards compiling the [SNAP](https://github.com/lanl/SNAP) proxy app ([#313](https://gitlab.com/lfortran/lfortran/-/issues/313)):
  * Code can be parsed to AST and transformed back to source code which compiles with other compilers and works
  * About 3rd of the files can be transformed from AST to ASR and the modules saved
* Other improvements:
  * Runtime library (more functions work)
  * Nested functions
  * Derived types

## Events

* We had our 11th Fortran Monthly call on April 22.
You can watch the recording below:

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/D107yFcuZoE" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

* Wadud Miah ([@wadudmiah](https://github.com/wadudmiah)) from the University of Southampton presented a webinar on Fortran for High Performance Computing, organized by [Excellerat](https://www.excellerat.eu/). You can find the slides and the recording [here](https://services.excellerat.eu/viewevent/39).

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

<div id="gh-contributors" data-startdate="April 01 2021" data-enddate="April 30 2021" height="500px"></div>
