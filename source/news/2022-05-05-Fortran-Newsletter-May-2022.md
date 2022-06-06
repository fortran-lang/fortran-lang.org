---
layout: post
title: "Fortran newsletter: May 2022"
category: newsletter
date: 2022-05-05
author: Milan Curcic, Alexis Perry-Holby, Giannis Nikiteas, Gagandeep Singh
---

Welcome to the May edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

# fortran-lang.org

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

Recent development updates:

* FIR
    * Lower various intrinsics:
        * character related intrinsics, array related intrinsics, index intrinsics, present, exit, btest, ceiling, nearest, scale, matmul, trim, transpose, command, environment, repeat, aint, anint, cmplx, conjg, dble, dprod, sign, spacing, rrspacing, merge intrinsics, lbound, ubound, ior, exp, log, log10, sqrt, atan, sinh, cosh, sin, cos, mvbits, achar
    * Add IO lowering test
    * Add more lowering tests for dummy arguments
    * Add equivalence lowering tests
    * Add array constructor lowering tests
    * Lower more array expressions
    * Lower statement function
    * Lower length on character storage
    * Lower select case statement
    * Add OpenMP Conversion patterns
    * Lower procedure designator
    * Lower boxed procedure
    * Flush and master constructs
    * Add lowering C interoperability test
    * Add misc lowering tests
    * Handle zero extent case in LBOUND
    * Lower some coarray statements to their runtime function
    * Options to lower math intrinsics to relaxed, precise variants
    * Lower optionals in GET_COMMAND_ARGUMENT and  GET_ENVIRONMENT_VARIABLE
    * Added lowering support for atomic read and write constructs
    * fix LBOUND lowering with KIND and no DIM arguments
    * Keep fully qualified !fir.heap type for fir.freemem op
    * Update the conversion code for fir.coordinate_of
    * Set lower bounds of array section fir.embox to one
    * Fix fir.embox codegen with constant interior shape
    * Do not fold fir.box_addr when it has a slice
* Driver
    * Make --version and -version consistent with clang
    * Add support for -mmlir
    * Make the plugin API independent of the driver internals
    * Add support for generating executables
* OpenMP
    * Lowering critical construct
    * Added assembly format for omp.wsloop and remove parseClauses
    * Added lowering support for sections construct
    * Added ReductionClauseInterface
    * Added parallel sections translation
    * Revert "[Flang][OpenMP] Add semantic check for OpenMP Private, Firstprivate and Lastprivate clauses."
    * Added allocate clause translation for OpenMP block constructs
    * Support export/import OpenMP Threadprivate Flag
    * Add implementation of privatisation
    * Add checks and tests for hint clause and fix empty hint
* OpenACC
    * Lower enter data directive
    * Lower exit data directive
    * Lower init/shutdown directive
    * Lower update directive
    * Lower data directive
    * Lower wait directive
* Runtime
    * Error recovery improvement in runtime (IOMSG=)
    * Initial UTF-8 support in runtime I/O
    * Ensure PointerDeallocate actually deallocate pointers
    * Add runtime API to catch unit number out of range
    * Prefer process time over thread time in CPU_TIME
    * Raise FP exceptions from runtime conversion to binary
    * Preserve effect of positioning in record in non-advancing output
    * Don't skip input spaces when they are significant
    * Fix ENDFILE for formatted stream output
    * Don't emit empty lines for bad writes
    * Ignore leading spaces even in BZ mode
    * Fix edge-case FP input bugs
    * Enforce some limits on kP scale factors
    * Signal record read overrun when PAD='NO'
    * Fix KIND=16 real/complex component I/O
    * Fix total MAXLOC/MINLOC for non-integer data
* Handle allocatable components when creating array temps
* [Parser] Add a node for individual sections in sections construct
* Add explanatory messages to grammar for language extensions
* Convert RUNTIME_CHECK to better error for user errors in transformational.cpp
* Accept legacy aliases for intrinsic function names
* Expose error recovery cases in external I/O
* Fix crash: ENTRY with generic interface of the same name
* Fold DBLE
* Single construct translation from PFT to FIR
* UBOUND() edge case: empty dimension
* Make not yet implemented messages more consistent
* Fix LBOUND rewrite on descriptor components
* Ensure descriptor lower bounds are LBOUND compliant
* Fix cycle-catcher in procedure characterization
* Fix bogus error from assignment to CLASS(*)
* Mark C_ASSOCIATED specific procedures as PURE
* Catch bad OPEN(STATUS=) cases
* Fold NEAREST() and its relatives
* Prevent undefined behavior in character MAXLOC folding
* Fix invalid overflow check
* Skip D when including D debug line
* Allow user to recover from bad edit descriptor with INTEGER
* Fold instantiated PDT character component length when needed
* Add one semantic check for allocatable/pointer argument association
* [cmake] Make CMake copy "omp_lib.h" into the build directory
* Handle dynamically optional argument in EXIT
* Fix semantic analysis for "forall" targeted by "label"
* Emit a portability warning for padding in COMMON
* Expand the num_images test coverage
* Fold IBITS() intrinsic function
* Error handling for out-of-range CASE values
* Respect left tab limit with Tn editing after ADVANCE='NO'
* Allow IMPLICIT NONE(EXTERNAL) with GenericDetails
* Do not ICE on out-of-range data statement designator
* Fix ICE for sqrt(0.0) evaluation
* Fix float-number representation bug
* Fix intrinsic interface for DIMAG/DCONJG
* Improve appearance of message attachments
* Fix combining cases of USE association & generic interfaces
* Defer all function result type processing
* Always encode multi-byte output in UTF-8
* Fix shape analysis of RESHAPE result
* Use full result range for clock_gettime implementation of SYSTEM_CLOCK
* Correct interaction between generics and intrinsics
* Make F0.1 output editing of zero edge case consistent
* Inner INTRINSIC must not shadow host generic
* Local generics must not shadow host-associated generics
* Fix TYPE/CLASS IS (T(...)) in SELECT TYPE
* Allow modification of construct entities
* Defer NAMELIST group item name resolution
* Accept TYPE(intrinsic type) in declarations only for non-extension type
* Finer control over error recovery with GetExpr()
* Handle parameter-dependent types in PDT initializers
* Upgrade short actual character arguments to errors
* Allow POINTER attribute statement on procedure interfaces
* Accept KIND type parameter inquiries on RE, IM, etc.
* Add & use a better visit()
* Fix regression with recent work on intrinsic/generic interactions
* Do not pass derived type by descriptor when not needed
* Fix LBOUND() folding for constant arrays
* Set LBOUND() folding for (x) expression as ones
* Semantics limits on kP scale factors
* Do not ICE on recursive function definition in function result
* Fold transformational bessels when host runtime has bessels
* Do not create arith.extui with same from/to type
* Disambiguate F(X)=Y case where F is a function returning a pointer
* Avoid global name conflict when BIND(C,NAME=) is used
* Accept "INFINITY" as real input
* Add semantic checks for intrinsic function REDUCE()
* Fix crash from PDT component init in module file

Call notes are recorded and publicly available [here](https://docs.google.com/document/d/1Z2U5UAtJ-Dag5wlMaLaW1KRmNgENNAYynJqLW2j2AZQ/edit).

### LFortran

**Compiling `stdlib` with `lfortran`**

- [Fixes for intrinsics while compiling `stdlib` using `lfortran`](https://gitlab.com/lfortran/lfortran/-/merge_requests/1718)
- [Draft: Stdlib sprint](https://gitlab.com/lfortran/lfortran/-/merge_requests/1719)
- [Draft: Sprint Compiling stdlib with LFortran](https://gitlab.com/lfortran/lfortran/-/merge_requests/1689)

**Addition of ASR Optimization Passes**

- [Implementing dead code elemination optimization](https://gitlab.com/lfortran/lfortran/-/merge_requests/1688)
- [Supporting duplication of Function/Subroutine calls](https://gitlab.com/lfortran/lfortran/-/merge_requests/1686)
- [Implementing loop unrolling optimization for fixed sized loops](https://gitlab.com/lfortran/lfortran/-/merge_requests/1681)

**`libasr`**

- [AST->ASR: Remove current_body](https://gitlab.com/lfortran/lfortran/-/merge_requests/1720)
- [Added support keyword argument in class procedures](https://gitlab.com/lfortran/lfortran/-/merge_requests/1717)
- [Implementing ArrayBound node to replace `lbound`, `ubound` as function calls](https://gitlab.com/lfortran/lfortran/-/merge_requests/1715)
- [Implementing `Block` and `BlockCall`](https://gitlab.com/lfortran/lfortran/-/merge_requests/1714)
- [Made scope private in SymbolTable struct and added interface methods to modify scope](https://gitlab.com/lfortran/lfortran/-/merge_requests/1711)
- [Removing dead code which treats size intrinsic as function](https://gitlab.com/lfortran/lfortran/-/merge_requests/1710)
- [Implementing `ArraySize` node to replace function call to `size`](https://gitlab.com/lfortran/lfortran/-/merge_requests/1708)
- [Fixing adjustl](https://gitlab.com/lfortran/lfortran/-/merge_requests/1707)
- [Implementing AssociateBlock](https://gitlab.com/lfortran/lfortran/-/merge_requests/1706)
- [Move ASR.asdl into the src/libasr directory](https://gitlab.com/lfortran/lfortran/-/merge_requests/1703)
- [Fixing handling of return type](https://gitlab.com/lfortran/lfortran/-/merge_requests/1699)
- [Fill function in FunctionCall of len expr of Character type after completing symbol table](https://gitlab.com/lfortran/lfortran/-/merge_requests/1698)
- [Fixing inline function calls pass to skip intrinsics](https://gitlab.com/lfortran/lfortran/-/merge_requests/1694)
- [Draft: ASR: Add all intrinsic operations into ASR itself](https://gitlab.com/lfortran/lfortran/-/merge_requests/1700)


**WASM**

- [Wasm backend](https://gitlab.com/lfortran/lfortran/-/merge_requests/1713)
- [draft: Wasm Intial Base](https://gitlab.com/lfortran/lfortran/-/merge_requests/1704)
- [draft: Compiling LFortran to WASM](https://gitlab.com/lfortran/lfortran/-/merge_requests/1705)


**Miscellaneous**

- [bind(c): Fix call_fortran_i64](https://gitlab.com/lfortran/lfortran/-/merge_requests/1723)
- [bind(c): Add tests for i64, f32, f64](https://gitlab.com/lfortran/lfortran/-/merge_requests/1722)
- [Fix a bug in a test](https://gitlab.com/lfortran/lfortran/-/merge_requests/1721)
- [Add a test for calling Fortran from C](https://gitlab.com/lfortran/lfortran/-/merge_requests/1716)
- [Update ASR from LPython](https://gitlab.com/lfortran/lfortran/-/merge_requests/1712)
- [CI: add git safe.directory](https://gitlab.com/lfortran/lfortran/-/merge_requests/1702)
- [Update ASR from LPython](https://gitlab.com/lfortran/lfortran/-/merge_requests/1701)
- [Update ASR from LPython](https://gitlab.com/lfortran/lfortran/-/merge_requests/1696)
- [Adding and Improving tests](https://gitlab.com/lfortran/lfortran/-/merge_requests/1695)
- [Remove --target install and CMAKE_INSTALL_PREFIX from build1.sh](https://gitlab.com/lfortran/lfortran/-/merge_requests/1709)

**Contributors**

- [Gagandeep Singh](https://gitlab.com/czgdp18071)
- [Ondřej Čertík](https://gitlab.com/certik)
- [Ubaid Shaikh](https://gitlab.com/shaikhubaid769)

We are looking for new contributors. Please do not hesitate to contact us if you are interested. We will help you get up to speed.

## Events

* We had our 25th Fortran Monthly call on April 22.
  You can watch the recording below:
  <iframe width="560" height="315" src="https://www.youtube.com/embed/8-_ll4f0gN8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
* Visual Studio Code's popular [Modern Fortran](https://marketplace.visualstudio.com/items?itemName=fortran-lang.linter-gfortran) extension joined the [fortran-lang GitHub](https://github.com/fortran-lang/vscode-fortran-support) organization.

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
