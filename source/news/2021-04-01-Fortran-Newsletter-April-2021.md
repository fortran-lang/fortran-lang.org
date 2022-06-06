---
layout: post
title: "Fortran newsletter: April 2021"
category: newsletter
date: 2021-04-01
author: Sebastian Ehlert, Alexis Perry-Holby, Laurence Kedward, Milan Curcic, Ondřej Čertík
---

Welcome to the April 2021 edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

# fortran-lang.org

This month we've had several updates to the website:

* [#229](https://github.com/fortran-lang/fortran-lang.org/pull/229):
  Correct value of pi in quickstart mini-book
* [#226](https://github.com/fortran-lang/fortran-lang.org/pull/226):
  Added DelaunaySparse to package list
* [#223](https://github.com/fortran-lang/fortran-lang.org/pull/223)
  [#225](https://github.com/fortran-lang/fortran-lang.org/pull/225):
  GSoC announcement
* [#222](https://github.com/fortran-lang/fortran-lang.org/pull/222):
  Avoid unclear formulation in contributing guide 
* [#221](https://github.com/fortran-lang/fortran-lang.org/pull/221):
  Add information about free compiler versions
* [#216](https://github.com/fortran-lang/fortran-lang.org/pull/216):
  Improve tags
* [#207](https://github.com/fortran-lang/fortran-lang.org/issues/207):
  Correct subtitle of setting up your os

Ongoing work:

* [#201](https://github.com/fortran-lang/fortran-lang.org/pull/201) (WIP):
  Internationalization for fortran-lang
* [#220](https://github.com/fortran-lang/fortran-lang.org/pull/220) (WIP):
  Include learn resources to online courses

[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues) if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the [contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/CONTRIBUTING.md) for how to get started.

## Fortran Standard Library

Here's what's new in `stdlib`:

* [#320](https://github.com/fortran-lang/stdlib/pull/320):
  Implement non-fancy functional string type
* [#362](https://github.com/fortran-lang/stdlib/pull/362):
  Fix wording in style guide for dimension attribute 
* [#352](https://github.com/fortran-lang/stdlib/pull/352):
  Added TOC to README
* [#346](https://github.com/fortran-lang/stdlib/pull/346)
  [#356](https://github.com/fortran-lang/stdlib/pull/356):
  Added to\_lower, to\_upper, reverse and to\_title function to stdlib\_string\_type module

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
* [#336](https://github.com/fortran-lang/stdlib/pull/336) (WIP):
  Add functions to convert integer/logical values to character values
* [#343](https://github.com/fortran-lang/stdlib/pull/343) (WIP):
  Implement strip and chomp as supplement to trim
* [#349](https://github.com/fortran-lang/stdlib/pull/349) (WIP):
  Simplify test makefile
* [#353](https://github.com/fortran-lang/stdlib/pull/353) (WIP):
  Initial checkin for a module for tolerant comparison of reals
* [#355](https://github.com/fortran-lang/stdlib/pull/355) (WIP):
  Implement clip function
* [#359](https://github.com/fortran-lang/stdlib/pull/359) (WIP):
  Add general contributing guidelines to stdlib
* [#360](https://github.com/fortran-lang/stdlib/pull/360) (WIP):
  Summarize build toolchain workflow and implied rules
* [#363](https://github.com/fortran-lang/stdlib/pull/363) (WIP):
  Add sort to stdlib\_string\_type module
* [#367](https://github.com/fortran-lang/stdlib/pull/367) (WIP):
  Add Intel compiler workflow for OSX


Please help improve stdlib by testing and reviewing these pull requests!

The candidate for file system operations to be included in stdlib is being developed by
[@MarDiehl](https://github.com/MarDiehl) and [@arjenmarkus](https://github.com/arjenmarkus)
in [this repository](https://github.com/MarDiehl/stdlib_os).
Please try it out and let us know how it works, if there are any issues, or if the API can be improved.

## Fortran Package Manager

Here's what's new in `fpm`:

* [Alpha release version 0.2.0](https://github.com/fortran-lang/fpm/releases/tag/v0.2.0)
* [Fpm is now available on conda-forge](https://github.com/conda-forge/fpm-feedstock)
* [#352](https://github.com/fortran-lang/fpm/pull/352):
  Hacky fix for the help test
* [#357](https://github.com/fortran-lang/fpm/pull/357):
  Install script for Fortran fpm
* [#369](https://github.com/fortran-lang/fpm/pull/369):
  Separate build targets from model structure
* [#370](https://github.com/fortran-lang/fpm/pull/370):
  Update run subcommand
* [#377](https://github.com/fortran-lang/fpm/pull/377):
  Add explicit include-dir key to manifest
* [#378](https://github.com/fortran-lang/fpm/pull/378):
  Add ford-compatible documentation to fpm\_strings.f90
* [#386](https://github.com/fortran-lang/fpm/pull/386):
  Replace deprecated flags in debug\_fortran option
* [#390](https://github.com/fortran-lang/fpm/pull/390)
  [#407](https://github.com/fortran-lang/fpm/pull/407):
  Implement --flag option for Fortran fpm
* [#397](https://github.com/fortran-lang/fpm/pull/397):
  Add Conda install instructions to the README
* [#398](https://github.com/fortran-lang/fpm/pull/398):
  Minor fix: for setting executable link libraries
* [#402](https://github.com/fortran-lang/fpm/pull/402):
  Add fpm description and reorganize the README intro
* [#404](https://github.com/fortran-lang/fpm/pull/404):
  Correct join for null input
* [#409](https://github.com/fortran-lang/fpm/pull/409):
  Give Programs Access to Code in Subdirectories
* [#414](https://github.com/fortran-lang/fpm/pull/414):
  Add few important links to README
* [#412](https://github.com/fortran-lang/fpm/pull/412):
  Duplicate module definitions
* [#413](https://github.com/fortran-lang/fpm/pull/413):
  Add: omp\_lib to intrinsic modules list
* [#419](https://github.com/fortran-lang/fpm/pull/419):
  Split workflow for Haskell and Fortran fpm

Work in progress:

* [First beta release](https://github.com/fortran-lang/fpm/milestone/1) (WIP):
  First feature-complete release of the Fortran implementation.
* [#364](https://github.com/fortran-lang/fpm/pull/364) (WIP):
  Plugin alpha version
* [#420](https://github.com/fortran-lang/fpm/pull/420) (WIP):
  Phase out Haskell fpm
* [fpm-haskell](https://github.com/fortran-lang/fpm-haskell) (WIP):
  Separate repository for the Haskell fpm version

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

A total of 10 pull requests were merged in February.

* [PR#996 tests: one test case supporting PR #966](https://github.com/flang-compiler/flang/pull/996)
* [PR#968 Fix a clash between CONTIGUOUS and SAVE attribute in flang (issue #673)](https://github.com/flang-compiler/flang/pull/968)
* [PR#955 Do not issue an error when character kind 2 is used](https://github.com/flang-compiler/flang/pull/955)
* [PR#975 Add the option to build release_11x branch of llvm with Github Actions](https://github.com/flang-compiler/flang/pull/975)
* [PR#974 Fix hash collision handling routine that didn't work due to a fatal mistake (issue #960).](https://github.com/flang-compiler/flang/pull/974)
* [PR#1000 Add ccache support to GitHub Actions](https://github.com/flang-compiler/flang/pull/1000)
* [PR#952 Array debugging support with upgraded DISubrange](https://github.com/flang-compiler/flang/pull/952)
* [PR#1002 Fix for regression introduced by PR #922 (issue #995)](https://github.com/flang-compiler/flang/pull/1002)
* [PR#985 add asprintf](https://github.com/flang-compiler/flang/pull/985)
* [PR#966 Fixes to address cp2k compilation and runtime issues](https://github.com/flang-compiler/flang/pull/966)

A total of 8 pull requests were merged in March.

* [PR#963 Fix errors on array initialisation with an implied do loop](https://github.com/flang-compiler/flang/pull/963)
* [PR#1007 fix for issue #1006: stop passing unused uninitialized value](https://github.com/flang-compiler/flang/pull/1007)
* [PR#1004 Nested implied do loop fix for real numbers](https://github.com/flang-compiler/flang/pull/1004)
* [PR#710 Test case for capturing procedure pointers to OpenMP parallel regions](https://github.com/flang-compiler/flang/pull/710)
* [PR#561 flang2: corrected fix for #424](https://github.com/flang-compiler/flang/pull/561)
* [PR#778 Fixing NCAR test problems with error tolerance lower than E-12.](https://github.com/flang-compiler/flang/pull/778)
* [PR#1010 LLVM 12 upgrade](https://github.com/flang-compiler/flang/pull/1010)
* [PR#1012 Remove release_90 from Github Actions](https://github.com/flang-compiler/flang/pull/1012)

### LLVM Flang

Recent development updates:

* New Driver: 
    * Add options for -fdefault\* and -flarge-sizes
    * Refine tests for module search directories
    * Add -fdebug-dump-parsing-log
    * Add -fdebug-module-writer option
    * Add debug dump, measure-parse-tree and pre-fir-tree options
    * Add -Xflang and make -test-io a frontend-only flang
    * Add -J and -module-dir to f18 driver
    * Fix -fdefault\* family bug
* FIR (Fortran IR - a dialect of MLIR): 
    * Add diagnostic tests for FIR ops verifier
    * Add FIR Types parser diagnostic tests
    * Upstream the pre-FIR tree changes (The PFT has been updated to support Fortran 77)
    * Update flang test tool support classes
    * Add zero_bits, array value, and other operations
    * Upstream utility function valueHasFirAttribute() to be used in subsequent merges
* OpenMP - add semantic checks for:
    * OpenMP 4.5 - 2.7.1 Do Loop restrictions for Threadprivate
    * Occurrence of multiple list items in aligned clause for simd directive
    * Flang OpenMP 4.5 - 2.15.3.6 Reduction clause
    * 2.15.4.2 - Copyprivate clause
    * 2.15.3.4 - Firstprivate clause
    * 2.15.3.5 - Lastprivate clause
* Update character tests to use gtest
* Adaptations to MLIR API changes
* Fix call to CHECK() on overriding an erroneous type-bound procedure
* Handle type-bound procedures with alternate returns
* Runtime: implement INDEX intrinsic function
* Fix compilation on MinGW-w64
* Extension: forward refs to dummy args under IMPLICIT NONE
* Detect circularly defined interfaces of procedures
* Implement the related character intrinsic functions SCAN and VERIFY

Call notes will be sent to the _flang-dev_ email list and also recorded [here](https://docs.google.com/document/d/10T-S2J3GrahpG4Ooif93NSTz2zBW0MQc_RlwHi0-afY).

### LFortran

* LFortran is participating in GSoC under the NumFOCUS and Fortran-lang umbrella, if you are interested, please apply: [https://gitlab.com/lfortran/lfortran/-/wikis/GSoC-2021-Ideas](https://gitlab.com/lfortran/lfortran/-/wikis/GSoC-2021-Ideas)
* 7 people contributed code in the last month:
    [Gagandeep Singh](https://github.com/czgdp1807),
    [Dominic Poerio](https://dompoer.io/),
    [Himanshu Pandey](https://github.com/hp77-creator),
    [Thirumalai Shaktivel](https://github.com/Thirumalai-Shaktivel),
    [Scot Halverson](https://github.com/scothalverson),
    [Rohit Goswami](https://rgoswami.me/),
    [Ondřej Čertík](https://ondrejcertik.com/).
* 114 Merge Requests were [merged](https://gitlab.com/lfortran/lfortran/-/merge_requests?scope=all&utf8=%E2%9C%93&state=merged) in the past month, highlights
    * macOS support (both Intel and ARM), compilation and development of
      LFortran itself (stacktraces work also)
    * Initial implentation of: modules (modfiles, dependencies, ...),
      interfaces, integer/real kinds, public/private attribute, derived types,
      strings, variable initializations, pointers, modules
    * Many other fixes

LFortran is still in pre-alpha stage, but we are making rapid progress towards
getting it to compile more Fortran features. We are looking for contributors,
if you are interested, please get in touch and we will help you get started.
We can be contacted at Zulip Chat, mailinglist or GitLab issues (see
https://lfortran.org for links to all three).

## Events

* We had our 10th Fortran Monthly call on March 24.
You can watch the recording below:

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/cxDF2Sa3nvU" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

* This year Fortran-lang is a mentor organization for [Google Summer of Code](https://summerofcode.withgoogle.com/organizations/6633903353233408/).
If you're a student, or know students who are [eligible to participate](https://developers.google.com/open-source/gsoc/faq#what_are_the_eligibility_requirements_for_participation), and you'd like to help build the Fortran ecosystem please reach out and let us know.
The student application window opened on March 29 and will close on April 13 at 14:00 Eastern Time.

* The 223rd meeting of the US Fortran Standards Committee concluded on March 2.
  Main topics of dicussion were the planned changes for the Fortran 202X revision of the Standard.
  Here's the [list of all submitted papers](https://j3-fortran.org/doc/meeting/223),
  and the [summary](https://github.com/j3-fortran/fortran_proposals/issues/199)
  of the papers discussed and voting results.
  The committee also welcomed a new member, Milan Curcic ([@milancurcic](https://github.com/milancurcic)),
  who is the voting alternate to Gary Klimowicz ([@gklimowicz](https://github.com/gklimowicz)).

  If you have ideas for new improvements to the language, please propose them
  [here](https://github.com/j3-fortran/fortran_proposals).

* Registration is open for the upcoming free webinar on
  [Fortran for High Performance Computing](https://register.gotowebinar.com/register/7343048137688004108).
  The webinar is organized by [Excellerat](https://www.excellerat.eu/)
  and will be presented by Wadud Miah ([@wadudmiah](https://github.com/wadudmiah)) from the University of Southampton.

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

<div id="gh-contributors" data-startdate="March 01 2021" data-enddate="March 31 2021" height="500px"></div>
