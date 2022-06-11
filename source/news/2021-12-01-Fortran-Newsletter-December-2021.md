---
layout: post
title: "Fortran newsletter: December 2021"
category: newsletter
date: 2021-12-01
author: Milan Curcic, Sebastian Ehlert, Alexis Perry-Holby
---

Welcome to the December 2021 edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

# fortran-lang.org

Here's what's new and ongoing in the fortran-lang.org repo:

* [#348](https://github.com/fortran-lang/fortran-lang.org/pull/348):
  Fix typo in author field

### Work in progress

* [#347](https://github.com/fortran-lang/fortran-lang.org/pull/347) (WIP):
  Fortran Intrinsics
* [#201](https://github.com/fortran-lang/fortran-lang.org/pull/201) (WIP):
  Internationalization for fortran-lang

[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues)
if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the
[contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/CONTRIBUTING.md)
for how to get started.

## Fortran Standard Library

Here's what's new in stdlib:

* [#558](https://github.com/fortran-lang/stdlib/pull/558):
  Pin specific fpm version
* [#556](https://github.com/fortran-lang/stdlib/pull/556):
  fix some FORD links
* [#494](https://github.com/fortran-lang/stdlib/pull/494):
  Add testing module to allow better structuring of test suites
* [#562](https://github.com/fortran-lang/stdlib/pull/562):
  Minor update `pure/elemental` in `string_type` module
* [#565](https://github.com/fortran-lang/stdlib/pull/565):
  Make support for quadruple precision optional
* [#566](https://github.com/fortran-lang/stdlib/pull/566):
  Create a call for reviewers pull request template
* [#578](https://github.com/fortran-lang/stdlib/pull/578):
  Update error in case fypp preprocessor is not found
* [#579](https://github.com/fortran-lang/stdlib/pull/579):
  Add module for handling version information of stdlib

### Work in progress

* [#581](https://github.com/fortran-lang/stdlib/pull/581) (WIP):
  Add routines for saving/loading arrays in npy format
* [#580](https://github.com/fortran-lang/stdlib/pull/580) (WIP):
  Add terminal and color escape sequences
* [#573](https://github.com/fortran-lang/stdlib/pull/573) (WIP):
  Revised Hash functions incorporating changes in the main Stdlib repository.
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
* [#500](https://github.com/fortran-lang/stdlib/pull/500) (WIP):
  Selection algorithms
* [#499](https://github.com/fortran-lang/stdlib/pull/499) (WIP):
  [stdlib_linalg] matrix property checks
* [#498](https://github.com/fortran-lang/stdlib/pull/498) (WIP):
  [stdlib_math] add `arg/argd/argpi`
* [#491](https://github.com/fortran-lang/stdlib/pull/491) (WIP):
  Stdlib linked list
* [#488](https://github.com/fortran-lang/stdlib/pull/488) (WIP):
  [stdlib_math] add `is_close` routines.
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

* [v0.5.0](https://github.com/fortran-lang/fpm/releases/tag/v0.5.0):
  Alpha release update
* [#598](https://github.com/fortran-lang/fpm/pull/598):
  Update README.md compiler, archiver, & link flags
* [#569](https://github.com/fortran-lang/fpm/pull/569):
  Add workflow for continuous delivery
* [#602](https://github.com/fortran-lang/fpm/pull/602):
  fix(fpm_compiler): intel windows release flag was incorrect
* [#607](https://github.com/fortran-lang/fpm/pull/607):
  Repair --list option and correct obsolete descriptions of the --list option
* [#612](https://github.com/fortran-lang/fpm/pull/612):
  Fix modules listing (for install)
* [#613](https://github.com/fortran-lang/fpm/pull/613):
  Add: critical section to mkdir in backend
* [#616](https://github.com/fortran-lang/fpm/pull/616):
  Add: workflow to make installer on push and release
* [#614](https://github.com/fortran-lang/fpm/pull/614):
  Bump version to 0.5.0
* [setup-fpm#7](https://github.com/fortran-lang/setup-fpm/pull/7):
  Fix Latest Option

### Work in progress

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

* OpenMP
    * Use the ultimate symbol in a call to the IsPointer function
    * Add parsing/sema/serialization for 'bind' clause.
* FIR
    * Add base of the FIR to LLVM IR pass
    * Add various FIR to LLVM IR conversion patterns:
        * fir.unreachable
        * fir.insert_on_range
        * fir.zero_bits
        * fir.select and fir.select_rank
        * fir.extract_value and fir.insert_value
        * types - fir.box, fir.logical, fir.char, fir.ptr
        * fir.box_rank, fir.box_addr, fir.box_dims, fir.box_elesize
        * fir.convert
        * fir.call
        * fir.store and fir.load
    * Add substr information to fircg.ext_embox and fircg.ext_rebox operations
    * Use notifyMatchFailure in fir.zero_bits conversion
    * Restrict array type on fir.insert_on_range
    * Add test for FIR types conversion
    * Use contralized values for indexing box
    * Add complex operations conversion from FIR LLVM IR
    * Add TargetRewrite pass and TargetRewrite: Rewrite COMPLEX values
* Runtime
    * Read environment variables directly
* flang-omp-report
    * Removed unnecessary comments in flang-omp-report plugin tests
    * Remove the loop workarounds for nowait clause
    * Add flang-omp-report summarising script
* Checks for pointers to intrinsic functions
* Fold SPREAD
* Improve error message for misuse of NULL(mold) as data statement constant
* Fix crash on "call system_clock(count_max=j)"
* Fix combined folding of FINDLOC/MAXLOC/MINLOC
* Implement GET_ENVIRONMENT_VARIABLE(VALUE)
* Remove builder that takes SSA value instead of Attribute on ExtractValueOp, InsetValueOp, and InsertOnRangeOp
* Remove getModel<Fortran::ISO::CFI_dim_t> in DescriptorModel.h
* Set the addendum when establishing pointer section in descriptor
* Fix error in characteristics check at procedure pointer assignment
* Initial parsing/sema for 'align' clause
* Don't reference non-invariant symbols in shape expressions
* Make subscript list argument a nullable pointer
* Distinguish error/warning cases for bad jumps into constructs
* Fix folding of EPSILON()

Call notes are recorded and available upon request [here](https://docs.google.com/document/d/10T-S2J3GrahpG4Ooif93NSTz2zBW0MQc_RlwHi0-afY). Please contact Alexis Perry-Holby at aperry@lanl.gov for document access.

### LFortran

* 32 Merge Requests merged in November 2021
* Support for same name interface and subroutine/function
* Compile-time evaluation for bit intrinsics
* Implement the `repeat` and `shape` intrinsics
* Variadic support for `min` and `max` intrinsics
* Implement the scalar `random_number` function
* Fixes and improved error message for `read` and `write` statements
* Support the `final`, `intrinsic`, and `private` attributes
* Implement the `ieee_arithmetic` intrinsic module
* Support for the `abstract` class
* Support for `assignment(=)` on `use` statement

We are looking for new contributors. Please do not hesitate to contact us if
you are interested. We will help you get up to speed.


## Events

* Sebastian Ehlert presented the Fortran Package Manager at the PackagingCon 2021 conference.
  You can watch the recording below:
  <iframe width="560" height="315" src="https://www.youtube.com/embed/YG8zEM1lAVM" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
* Recordings of all FortranCon 2021 presentations are now available to view in
  the [FortranCon YouTube Channel](https://www.youtube.com/playlist?list=PLeKbr7eYHjt5UaV9zQtY24oEbne9_uFni).
  Enjoy!
* We had our 20th Fortran Monthly call on November 15.
  You can watch the recording below:
  <iframe width="560" height="315" src="https://www.youtube.com/embed/S2kKwmg8OjE" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

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

<div id="gh-contributors" data-startdate="November 01 2021" data-enddate="November 30 2021" height="500px"></div>
