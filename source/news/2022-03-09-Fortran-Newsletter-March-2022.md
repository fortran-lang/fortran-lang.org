---
layout: post
title: "Fortran newsletter: March 2022"
category: newsletter
date: 2022-03-09
author: Milan Curcic, Alexis Perry-Holby, Ondřej Čertík
---

Welcome to the March edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

# fortran-lang.org

Here's what's new in the fortran-lang.org repo:

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

* [#624](https://github.com/fortran-lang/stdlib/pull/624):
  [stdlib_math] Minor update to `stdlib_math` module and document

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

* [#652](https://github.com/fortran-lang/fpm/pull/652):
  get user name and email using git config if available else use defaults
* [#654](https://github.com/fortran-lang/fpm/pull/654):
  Ignore hidden source files
* [#622](https://github.com/fortran-lang/fpm/pull/622):
  Cleanup the backend output
* [#648](https://github.com/fortran-lang/fpm/pull/648):
  Add note about relocation of manifest reference
- [fpm-docs#42](https://github.com/fortran-lang/fpm-docs/issues/42):
  Dutch translation
- [fpm-docs#48](https://github.com/fortran-lang/fpm-docs/pull/48):
  Chinese translation improvements
- [fpm-docs#52](https://github.com/fortran-lang/fpm-docs/pull/52):
  Update plugin tutorial

### Work in progress

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
- [fpm-docs#51](https://github.com/fortran-lang/fpm-docs/pull/51) (WIP):
  Add page about fpm logo

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
    * Initial lowering for empty program
    * Upstream partial lowering of COMMAND_ARGUMENT_COUNT intrinsic
    * Add lowering placeholders
    * Add lowering for basic empty SUBROUTINE
    * Upstream partial lowering of EXIT intrinsic
    * Lower basic STOP statement
    * Lower PAUSE statement
    * Add lowering for integer constant
    * Lower integer constant code for STOP stmt
    * Add fir.array_access op
    * Add fir.array_amend operation definition
    * Handle logical constant value for quiet in STOP stmt
    * Upstream partial lowering of GET_COMMAND_ARGUMENT intrinsic
    * Basic local variable lowering
    * Add lowering for ASCII character constant
    * Handle character constant for error code in STOP stmt
    * Upstream partial lowering of GET_ENVIRONMENT_VARIABLE intrinsic
    * Add missing CFI case for REAL and COMPLEX
    * Add support for lowering the goto statement
    * Add type conversion for !fir.box<none>
    * Add FIRInlinerInterface
    * Lower simple RETURN statement
    * Upstream fix to allocmem codegen to deal with missing dimensions for sequence of character types
    * Lower basic function with scalar integer/logical return value
    * Enable scalar real type in lowering
    * Enable complex type in function lowering
    * Handle lowering of ranked array
    * Lower simple scalar assignment
    * Lower scalar negation
    * Lower basic binary operation for scalars
    * Initial patch to lower a Fortran intrinsic
    * Lower real constant
    * Lower complex constant
    * Lower function and subroutine calls
    * Handle allocatable dummy arguments
    * Lower allocatable assignment for scalar
    * Simple array assignment lowering
    * Lower simple character return
    * Lower Fortran intrinsic to a runtime call/llvm intrinsic
    * Lower integer comparison operation
    * Lower real comparison operations
    * Lower logical comparison and logical operations
    * Lower power operations
    * Add complex operations lowering tests
    * Lower basic IO statement
    * Handle dynamic array lowering
* Driver
    * Add support for `-emit-mlir`
    * Add support for `-emit-llvm`
    * Make `flang-new` always generate run-time type info
    * Add support for `--target`/`--triple`
* OpenMP
    * Added OpenMP 5.0 specification based semantic checks for atomic update construct
    * The device expression must evaluate to a non-negative integer value
    * Remove clauses from OpenMP Dialect that are handled by the flang frontend instead:
        * private, firstprivate, lastprivate, shared, default, copyin, copyprivate
* Runtime
    * Implement a runtime routine to report fatal errors with source position
    * Rename the runtime routine that reports a fatal user error
    * runtime perf: larger I/O buffer growth increments
    * Add runtime interface for GET_COMMAND
    * Upstream runtime changes for inquiry intrinsics
* Improve error message (initialized variable in pure subprogram)
* Accept BOZ literals for some actual arguments
* Accept sparse argument keyword names for MAX/MIN
* Accept INDEX(..., BACK=array)
* Fix OPEN/WRITE(SIGN='SUPPRESS')
* Handle FLUSH(unknown unit)
* Allow explicit '+' in NAMELIST input subscripts
* Extension: skip over NAMELIST groups
* Add array operations documentation
* Fix crash from USE-associated defined I/O subprograms
* Allow INQUIRE() on a child unit in user-defined I/O procedure
* Don't drop format string for external child I/O
* Support DECIMAL='COMMA' mode in namelist I/O
* Update tco tool pipline and add translation to LLVM IR
* Add MemoryAllocation pass to the pipeline
* Add ieee_is_normal/ieee_is_negative to ieee_arithmetic module.
* Add a custom target for the "flang" wrapper script.
* split character procedure arguments in target-rewrite pass
* Expand the semantics test for co_sum
* Correct interpretation of RECL=
* Distinguish intrinsic from non-intrinsic modules
* Make NEWUNIT= use a range suitable for INTEGER(KIND=1) and recycle unit numbers
* Modify right modes for READ/WRITE vs OPEN
* Add a semantics test for co_broadcast
* catch implicit interface incompatibility with global scope symbol
* Add an assert to guard against nullptr dereferencing
* Fix FlangOptimizerTests link on Solaris
* Handle "type(foo) function f" when foo is defined in f
* Refine pointer/target test for ASSOCIATED intrinsic
* Allow mixed association of procedure pointers and targets
* Fix edge case in USE-associated generics
* Fail at link time if derived type descriptors were not generated
* Allow for deferred-length character in EstablishDescriptor
* Allow DATA initialization of derived types w/ allocatable components
* Accept NULL(mold=x) as constant component value in constant structure constructor
* Ensure a characterized ENTRY in a PURE subprogram is also marked PURE
* Accept structure constructor value for polymorphic component
* Remove deprecated parser/printer/verifier utilities
* Accept pointer assignment w/ remapping to function result
* Allow extension cases of EQUIVALENCE with optional warnings
* Handle CALL C_F_POINTER(without SHAPE=)
* Make source location more accurate for actual arguments
* Add Win32 to the list of supported triples
* Allow tabs as white space in formats
* Do not print format tabs
* Catch I/O of bad derived type at compile time
* Allow more concurrently open NEWUNIT= values, with recycling

Call notes are recorded and available upon request [here](https://docs.google.com/document/d/10T-S2J3GrahpG4Ooif93NSTz2zBW0MQc_RlwHi0-afY). Please contact Alexis Perry-Holby at aperry@lanl.gov for document access.

### LFortran

* LFortran is participating in GSoC, please see [GSoC 2022 Student Instructions for LFortran](https://gitlab.com/lfortran/lfortran/-/wikis/GSoC%202022%20Student%20Instructions) for instructions how to apply
* 19 Merge Requests merged
* New ASR optimizations
* Addes support for fma (fused-multiply add) in LLVM
* Semantic improves to compile more of stdlib

We are looking for new contributors. Please do not hesitate to contact us if you are interested. We will help you get up to speed.

## Events

* Fortran-lang has been selected as a mentoring organization for Google Summer of Code 2022!
  Thanks to everybody who helped prepare the application.
  [GSoC](https://summerofcode.withgoogle.com/) is Google's global, online program that allows newcomers to open-source to work on a project and get paid for it. 
  See the [Fortran-lang GSoC 2022 page](https://summerofcode.withgoogle.com/programs/2022/organizations/fortran-lang) for information about the projects and how to apply.
  Applications for contributors open **April 4** and close **April 19**.
  To learn more about GSoC and what has changed since last year, please see the [GSoC 2022 announcement](https://opensource.googleblog.com/2021/11/expanding-google-summer-of-code-in-2022.html).
  If you'd like to participate as a contributor or a mentor, please let us know in this thread and we'll help you get started.
* Fortran-lang community now maintains a modernized fork of the classic library
  [minpack](https://github.com/fortran-lang/minpack). Give it a try!
* We had our 23rd Fortran Monthly call on February 15.
  You can watch the recording below:
  <iframe width="560" height="315" src="https://www.youtube.com/embed/oi0F-4QSdIY" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
* US Fortran Standards Committee meeting #226 started on February 28.
  See the draft agenda [here](https://j3-fortran.org/doc/year/22/agenda226.txt)
  and the submitted papers [here](https://j3-fortran.org/doc/meeting/226).

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

<div id="gh-contributors" data-startdate="February 01 2022" data-enddate="February 28 2022" height="500px"></div>
