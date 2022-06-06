---
layout: post
title: "Fortran newsletter: January 2022"
category: newsletter
date: 2022-01-01
author: Milan Curcic, Sebastian Ehlert, Jérémie Vandenplas, Alexis Perry-Holby, Ondřej Čertík
---

Happy New Year and welcome to the January 2022 edition of the monthly Fortran 
newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

# fortran-lang.org

Here's what's new and ongoing in the fortran-lang.org repo:

* [#349](https://github.com/fortran-lang/fortran-lang.org/pull/349):
  Newsletter draft for December 2021
* [#350](https://github.com/fortran-lang/fortran-lang.org/pull/350):
  Updated CaNS item so that it shows the version
* [#353](https://github.com/fortran-lang/fortran-lang.org/pull/353):
  Add MCST LCC C, C++ and Fortran compiler
* [#351](https://github.com/fortran-lang/fortran-lang.org/pull/351):
  Use HEAD to reference default branch
* [#355](https://github.com/fortran-lang/fortran-lang.org/pull/355):
  2021 review article draft
* [#356](https://github.com/fortran-lang/fortran-lang.org/pull/356) (WIP):
  Adding Fortran Error Handler to packages index

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

* [#500](https://github.com/fortran-lang/stdlib/pull/500):
  Selection algorithms
* [#586](https://github.com/fortran-lang/stdlib/pull/586):
  Update of stdlib_stats.md
* [#581](https://github.com/fortran-lang/stdlib/pull/581):
  Add routines for saving/loading arrays in npy format
* [#590](https://github.com/fortran-lang/stdlib/pull/590):
  Update changelog
* [#588](https://github.com/fortran-lang/stdlib/pull/588):
  Error on no tests in CTest
* [#585](https://github.com/fortran-lang/stdlib/pull/585):
  stdlib_selection: correction of typos and addition of some checks
* [#591](https://github.com/fortran-lang/stdlib/pull/591):
  Fix compilation errors with makefiles due to command-line variable assignments
* [#273](https://github.com/fortran-lang/stdlib/pull/273):
  Probability Distribution and Statistical Functions -- Normal Distribution Module 
* [#584](https://github.com/fortran-lang/stdlib/pull/584):
  Replace the call to sort by select in stdlib_stats_median
* [#593](https://github.com/fortran-lang/stdlib/pull/593):
  Probability Distribution and Statistical Functions -- Uniform Distribution Module
* [#594](https://github.com/fortran-lang/stdlib/pull/594):
  Minor update to makefile installation instructions
* [#596](https://github.com/fortran-lang/stdlib/pull/596):
  Rename references to default branch
* [#600](https://github.com/fortran-lang/stdlib/pull/600):
  Fix iomsg allocation in save_npy
* [#488](https://github.com/fortran-lang/stdlib/pull/488):
  [stdlib_math] add `is_close` routines.
* [#597](https://github.com/fortran-lang/stdlib/pull/597):
  Add getline to read whole line from formatted unit
* [#498](https://github.com/fortran-lang/stdlib/pull/498):
  [stdlib_math] add `arg/argd/argpi`
* [#603](https://github.com/fortran-lang/stdlib/pull/603):
  Implement trueloc/falseloc
* [#573](https://github.com/fortran-lang/stdlib/pull/573):
  Revised Hash functions incorporating changes in the main Stdlib repository.
* [#609](https://github.com/fortran-lang/stdlib/pull/609):
  Consistent spec titles
* [#610](https://github.com/fortran-lang/stdlib/pull/610):
  Fixed tables in stdlib_hash_procedures.md
* [#499](https://github.com/fortran-lang/stdlib/pull/499):
  [stdlib_linalg] matrix property checks
* [#613](https://github.com/fortran-lang/stdlib/pull/613):
  Ignore hash testing binaries and logs

### Work in progress

* [#611](https://github.com/fortran-lang/stdlib/pull/611) (WIP):
  Hash maps
* [#605](https://github.com/fortran-lang/stdlib/pull/605) (WIP):
  [stdlib_math] Add function `diff`
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
* [#276](https://github.com/fortran-lang/stdlib/pull/276) (WIP):
  Probability Distribution and Statistical Functions -- Exponential Distribution Module
* [#189](https://github.com/fortran-lang/stdlib/pull/189) (WIP):
  Initial implementation of COO / CSR sparse format

Please help improve stdlib by testing and [reviewing pull requests](https://github.com/fortran-lang/stdlib/issues?q=is%3Apr+is%3Aopen+label%3A%22reviewers+needed%22)!

The candidate for file system operations to be included in stdlib is being developed by
[@MarDiehl](https://github.com/MarDiehl) and [@arjenmarkus](https://github.com/arjenmarkus)
in [this repository](https://github.com/MarDiehl/stdlib_os).
Please try it out and let us know how it works, if there are any issues, or if the API can be improved.

## Fortran Package Manager

Here's what's new in fpm:

* [#634](https://github.com/fortran-lang/fpm/pull/634):
  Better extraction of the Fortran compiler from the MPI wrapper

### Work in progress

* [#642](https://github.com/fortran-lang/fpm/pull/642) (WIP):
  Replace polymorphic assignment with move_alloc
* [#630](https://github.com/fortran-lang/fpm/pull/630) (WIP):
  allow backfilling of current directory in fpm-new subcommand
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
    * Add fir.box_isarray, fir.box_isptr and fir.box_isalloc conversion.
    * Add !fir.vector type conversion
    * Add type conversion for `fir.boxchar`
    * Add !fir.alloca conversion
    * Add placeholder conversion pattern for disptach operations
    * Add fir.select_case conversion
    * Add !fir.field type conversion
    * Transform `IsPresentOpConversion` and `AbsentOpConversion`
    * Add type conversion for FIR heap type
    * Add type conversion for FIR integer kind
    * Add !fir.len type conversion
    * Transform `fir.emboxchar` to a sequence of LLVM MLIR
    * Add fir.select_type conversion placeholder
    * Remove extra return in SelectTypeOpConversion
    * Transform `fir.unboxchar` to a sequence of LLVM MLIR
    * Add fir.global_len conversion placeholder
    * Add the FIR LLVMPointer Type
    * Add fir.cmpc conversion
    * Add fir.gentypedesc conversion
    * Transform `fir.boxchar_len` to a sequence of LLVM MLIR
    * Add missing `HasParent` in `fir_DTEntryOp`
    * Add fir.string_lit conversion
    * Add conversion patterns for slice, shape, shapeshift and shift ops
    * Add fir.box_tdesc conversion
    * !fir.tdesc type conversion
    * Add fir.constc conversion
    * Add fir.embox conversion
    * Notify conversion failure for Proc ops, types
    * Add tests for mlir::ComplexType conversion
    * Add `fir.end` conversion placeholder
    * Transform `fir.field_index` to a sequence of LLVM MLIR
    * Add a factory class for creating Complex Ops
    * Add fir.no_reassoc conversion
    * Set !fir.len_param_index conversion to unimplemented
    * Add base for runtime builder unittests
    * Add fir transformational intrinsic builder
    * Add assignment runtime API builder
    * Add data flow optimization pass
    * Add array value copy pass
    * Add fir reduction builder
    * Add fir numeric intrinsic runtime call builder
    * Add fircg.ext_embox conversion
    * Add fir derived type runtime builder
    * Add fir character builder
    * Add fircg.ext_array_coor conversion
    * Upstream conversion of the XRebox Op
    * Convert fir.allocmem and fir.freemem operations to calls to malloc and free, respectively
* Runtime
    * Fix vector cshift runtime with non zero lower bounds
    * Respect NO_STOP_MESSAGE=1 in runtime
    * Runtime performance improvements to real formatted input
    * Allow write after non advancing read in IO runtime
    * Fix reversed comparison in RESHAPE() runtime
    * Define & implement a lowering support API IsContiguous() in runtime
    * Don't close stderr in runtime (fixes STOP output)
    * Return arrays in Transfer runtime with SIZE argument
    * Fix INQUIRE(FILE=,NAME=)
    * Add ragged array runtime functions
* Allow exterior branch to outermost WHERE construct statement
* Fix ORDER= argument to RESHAPE
* Fix rounding edge case in F output editing
* Handle ENTRY names in IsPureProcedure() predicate
* Allow implicit procedure pointers to associate with explicit procedures
* Fix a bug in INQUIRE(IOLENGTH=) output
* Remove default argument from function template specialization
* Check ArrayRef base for contiguity in IsSimplyContiguousHelper
* Deal with negative character lengths in semantics
* Fix INQUIRE(PAD=) and (POSITION=) for predefined units
* Add a semantics test for co_sum
* Fix off-by-one results from folding MAXEXPONENT and MINEXPONENT
* Skip `Fortran STOP:` before message when NO_STOP_MESSAGE is set
* Fix printing of constc and parsing of  #fir.real
* Predefine unit 0 connected to stderr
* Add -fno-automatic, refine IsSaved()
* Correct the argument keyword for AIMAG(Z=...)
* Inclusive language: remove instances of master
* Return true in IsSymplyContiguous for allocatables
* Fix usage & catch errors for MAX/MIN with keyword= arguments
* Re-fold bounds expressions in DATA implied DO loops
* Correct INQUIRE(POSITION= & PAD=)
* Rearrange prototype & code placement of IsCoarray()    
* Replace notifyMatchFailure with TODO hard failures
* TargetRewrite: Rewrite fir.address_of(func)
* Fix folding of EXPONENT() intrinsic function
* OPEN(RECL=) handling for sequential formatted I/O
* Avoid potential deadlock in CloseAll()

Call notes are recorded and available upon request [here](https://docs.google.com/document/d/10T-S2J3GrahpG4Ooif93NSTz2zBW0MQc_RlwHi0-afY). Please contact Alexis Perry-Holby at aperry@lanl.gov for document access.

### LFortran

* Beginning of refactoring of ASR (Abstract Semantic Representation) into a standalone library
* New intrinsics: mvbits, bge, bgt, ble, blt, ibits
* generic procedure resolution fixes
* FreeBSD fixes
* Implement `.xor.` (LFortran as well as GFortran extension)
* Fixes to large integers (BigInt) handling
* Added support for `private`, `final` attributes in derived types/classes

The following people contributed code in December 2021: Ondřej Čertík, Gagandeep Singh, Harris Snyder

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
* We had our 21st Fortran Monthly call on December 14.
  You can watch the recording below:
  <iframe width="560" height="315" src="https://www.youtube.com/embed/NSvL2yrefH8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
* We also wrote a review of the Fortran-lang projects in 2021. Read it
  [here]({{ site.baseurl }}/newsletter/2021/12/29/Fortran-lang-2021-in-review/).

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

<div id="gh-contributors" data-startdate="December 01 2021" data-enddate="January 01 2022" height="500px"></div>
