---
layout: post
title: "Fortran newsletter: April 2022"
category: newsletter
date: 2022-04-07
author: Milan Curcic, Alexis Perry-Holby
---

Welcome to the April edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

# fortran-lang.org

Here's what's new in the fortran-lang.org repo:

* [#379](https://github.com/fortran-lang/fortran-lang.org/pull/379):
  Newsletter for March 2022
* [#383](https://github.com/fortran-lang/fortran-lang.org/pull/383):
  Updated VS Code extensions
* [#381](https://github.com/fortran-lang/fortran-lang.org/pull/381):
  add string array to learn
* [#384](https://github.com/fortran-lang/fortran-lang.org/pull/384):
  Resolves typos #377

### Work in progress

* [#369](https://github.com/fortran-lang/fortran-lang.org/pull/369) (WIP):
  Resolves Issue #217
* [#347](https://github.com/fortran-lang/fortran-lang.org/pull/347) (WIP):
  Fortran Intrinsics

[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues)
if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the
[contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/CONTRIBUTING.md)
for how to get started.

## Fortran Standard Library

Here's what's new in stdlib:

* [#632](https://github.com/fortran-lang/stdlib/pull/632):
  doc: fix some typos
* [#629](https://github.com/fortran-lang/stdlib/pull/629):
  option to disable testing by setting BUILD_TESTING to OFF
* [#631](https://github.com/fortran-lang/stdlib/pull/631):
  Preparation for 0.2.0 release
* [#637](https://github.com/fortran-lang/stdlib/pull/637):
  Only set Fortran arguments for Fortran compiler
* [#642](https://github.com/fortran-lang/stdlib/pull/642):
  Fix linking issue with shared libraries

### Work in progress

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

* [#675](https://github.com/fortran-lang/fpm/pull/675):
  Fix for backtrace error when file not found in: src/fpm_source_parsing.f90
* [#677](https://github.com/fortran-lang/fpm/pull/677):
  Fix issue with backend pretty output
* [#684](https://github.com/fortran-lang/fpm/pull/684):
  fix: remove remove unnecessary space in fpm new cmd
* [#8](https://github.com/fortran-lang/setup-fpm/pull/8) (`setup-fpm`):
  Update to v4 in usage example in README

### Work in progress

* [#685](https://github.com/fortran-lang/fpm/pull/685) (WIP):
  fix: function for getting executable path
* [#676](https://github.com/fortran-lang/fpm/pull/676) (WIP):
  Tree shaking for modules
* [#671](https://github.com/fortran-lang/fpm/pull/671) (WIP):
  Add `library-dir` to support `-Lpath`
* [#665](https://github.com/fortran-lang/fpm/pull/665) (WIP):
  add clean command
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

Recent development updates:

* FIR
    * Lower IO open and close statements
    * Lower basic IO file statements
    * Lower inquire statement
    * Handle module in lowering pass
    * Lower more cases of assignments on allocatable variables
    * Add lowering for host association
    * Lower allocate and deallocate statements
    * Lower sum intrinsic
    * Lower computed and assigned goto
    * Lower associate construct
    * Update ArrayValueCopy to support array_amend and array_access
    * Lower more array character cases
    * Lower basic derived types
    * Lower where statement
    * Lower general forall statement
    * Lower pointer component in derived type
    * Lower of elemental calls in array expression
    * Add tests for allocatable global
    * Add support for linkonce_odr in FIR
    * Lower elemental calls
    * Lower ALL intrinsic
    * Lower common block
    * Lower format statement
    * Write a pass to annotate constant operands on FIR ops
    * Lower ANY intrinsic
    * Add support for lowering the dot_product intrinsic
    * Add support for lowering the dim intrinsic
    * Add support for lowering of the ibits intrinsic
    * Lower more pointer assignments/disassociation cases
    * Lower entry statement
    * Lower alternate return
    * Lower allocated intrinsic
    * Add lowering for the following character related intrinsics: len, len_trim, lge, lgt, lle and llt
    * Adds lowering for min/max intrinsics: max, maxloc, maxval, minloc, minval
    * Lower random_[init|number|seed] intrinsics
    * Lower date_and_time and cpu_time intrinsics
    * Lower system_clock intrinsic
    * Add support for lowering of the ibset intrinsic
    * Lower transfer instrinsic
    * Lower adjustl and adjustr intrinsics
    * Lower count intrinsic
    * Add lowering for the set_exponent intrinsic
* Driver
    * Add support for -debug-dump-pft
    * Add support for -S and implement -c/-emit-obj
    * Add support for -mllvm
* OpenMP
    * [mlir]Generating enums in accordance with the guidelines
    * Added basic connect to lower OpenMP constructs
    * Support for dump OpenMP/OpenACC declarative directives PFT in module
    * Add OpenMP and OpenACC flags to bbc
* Allow data transfer stmt control list errors to be caught
* Extension: don't require commas between most edit descriptors in formats
* Fix result type of "procedure(abs) :: f"
* Catch READ/WRITE on direct-access file without REC=
* Honor RECL= in list-directed/namelist output
* Accommodate module subprograms defined in the same module
* Extend ProvenanceRange::Suffix() to handle crash case
* Remove bogus messages for actual/dummy procedure argument compatibility
* Support PDT type descriptors in codegen
* Handle optional TARGET associate in ASSOCIATED runtime
* Generate PDT runtime type info in the type definition scope
* Accommodate arrays with a zero-extent dimension in location folding
* Avoid crash case in provenance mapping
* Make per-argument intrinsic error messages more localized
* Use faster path for default formatted character input
* Runtime validation of SPREAD(DIM=dim) argument
* Make uninitialized allocatable components explicitly NULL() in structure constructors
* Fix module file missing USE for shadowed derived type
* Add nonfatal message classes
* Distinguish usage and portability warning messages
* Use unix logical representation for fir.logical
* Fix extent computation in finalization
* Fix processing ModuleLikeUnit evaluationList
* Do not return true for pointer sub-object in IsPointerObject
* Fix DYLIB builds
* Improve runtime crash messages
* Add runtime support for GET_COMMAND
* IEEE_ARITHMETIC must imply USE IEEE_EXCEPTIONS
* LBOUND() edge case: empty dimension
* Hanlde COMPLEX 2/3/10 in runtime TypeCode(cat, kind)
* fulfill -Msave/-fno-automatic in main programs too
* Relax fir.rebox verifier with characters

Call notes are recorded and available upon request [here](https://docs.google.com/document/d/10T-S2J3GrahpG4Ooif93NSTz2zBW0MQc_RlwHi0-afY). Please contact Alexis Perry-Holby at aperry@lanl.gov for document access.

### LFortran

We are looking for new contributors. Please do not hesitate to contact us if you are interested. We will help you get up to speed.

## Events

* The "State of Fortran" paper by Kedward et al. has been accepted for publication in the IEEE journal Computing in Science and Engineering (CiSE).
  You can read the pre-print on [arXiv](https://arxiv.org/abs/2203.15110), or find the paper in Early Access on the [CiSE website](https://ieeexplore.ieee.org/document/9736688).
* The contributor application window for this year's [Google Summer of Code](https://summerofcode.withgoogle.com) is approaching fast.
  It opens **April 4** and closes **April 19**.
  See the [Fortran-lang GSoC 2022 page](https://summerofcode.withgoogle.com/programs/2022/organizations/fortran-lang) for information about the projects and how to apply.
  To learn more about GSoC and what has changed since last year, please see the [GSoC 2022 announcement](https://opensource.googleblog.com/2021/11/expanding-google-summer-of-code-in-2022.html).
  If you'd like to participate, please let us know and we'll help you get started.
* We had our 24th Fortran Monthly call on March 15.
  You can watch the recording below:
  <iframe width="560" height="315" src="https://www.youtube.com/embed/hekwzPzIfu8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

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
* [fortran-lang/minpack](https://github.com/fortran-lang/minpack)
* [fortran-lang/test-drive](https://github.com/fortran-lang/test-drive)
* [j3-fortran/fortran\_proposals](https://github.com/j3-fortran/fortran_proposals)

<div id="gh-contributors" data-startdate="March 01 2022" data-enddate="March 31 2022" height="500px"></div>
