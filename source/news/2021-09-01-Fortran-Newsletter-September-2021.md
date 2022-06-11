---
layout: post
title: "Fortran newsletter: September 2021"
category: newsletter
date: 2021-09-01
author: Milan Curcic, Alexis Perry-Holby, Sebastian Ehlert, Ondřej Čertík
---

Welcome to the September 2021 edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

# fortran-lang.org

This month we've had several updates to the website:

* [#303](https://github.com/fortran-lang/fortran-lang.org/pull/303):
  Add NJOY to package index

### Work in progress

* [#302](https://github.com/fortran-lang/fortran-lang.org/pull/302) (WIP):
  Update Silverfrost compiler description.
* [#300](https://github.com/fortran-lang/fortran-lang.org/pull/300) (WIP):
  Add QCxMS to package index
* [#246](https://github.com/fortran-lang/fortran-lang.org/pull/246) (WIP):
  Transferring fortran90.org "Fortran Best Practices" into a mini-book
* [#201](https://github.com/fortran-lang/fortran-lang.org/pull/201) (WIP):
  Internationalization for fortran-lang

[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues) if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the [contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/CONTRIBUTING.md) for how to get started.

## Fortran Standard Library

Here's what's new in stdlib:

* [#467](https://github.com/fortran-lang/stdlib/pull/467):
  Implemented move_alloc for string_type
* [#470](https://github.com/fortran-lang/stdlib/pull/470):
  Revival string list 
* [#481](https://github.com/fortran-lang/stdlib/pull/481):
  [stdlib_linalg] Update eye function.
* [#493](https://github.com/fortran-lang/stdlib/pull/493):
  Update copyright and remove old artifact
* [#444](https://github.com/fortran-lang/stdlib/pull/444):
  Add format_string routine to format other types to strings
* [#483](https://github.com/fortran-lang/stdlib/pull/483):
  Remove GCC Fortran MinGW 8.4.0 from known to work list

### Work in progress

* [#501](https://github.com/fortran-lang/stdlib/pull/501) (WIP):
  Minor updates to README.md
* [#500](https://github.com/fortran-lang/stdlib/pull/500) (WIP):
  Selection algorithms
* [#499](https://github.com/fortran-lang/stdlib/pull/499) (WIP):
  [stdlib_linalg] matrix property checks
* [#498](https://github.com/fortran-lang/stdlib/pull/498) (WIP):
  [stdlib_math] add `arg/argd/argpi`
* [#494](https://github.com/fortran-lang/stdlib/pull/494) (WIP):
  Add testing module to allow better structuring of test suites
* [#491](https://github.com/fortran-lang/stdlib/pull/491) (WIP):
  Stdlib linked list
* [#488](https://github.com/fortran-lang/stdlib/pull/488) (WIP):
  [stdlib_math] add `is_close` routines.
* [#478](https://github.com/fortran-lang/stdlib/pull/478) (WIP):
  [stdlib_linalg] Add zeros, ones function.
* [#475](https://github.com/fortran-lang/stdlib/pull/475) (WIP):
  Generating sorting subroutines specific to `character` type with fypp
* [#473](https://github.com/fortran-lang/stdlib/pull/473) (WIP):
  Error stop improvements
* [#455](https://github.com/fortran-lang/stdlib/pull/455) (WIP):
  stdlib_hash: waterhash algorithm
* [#452](https://github.com/fortran-lang/stdlib/pull/452) (WIP):
  Implementation of a map data type
* [#445](https://github.com/fortran-lang/stdlib/pull/445) (WIP):
  [stdlib_io] `disp`(display your data)
* [#437](https://github.com/fortran-lang/stdlib/pull/437) (WIP):
  [FPM] add fpm support
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
* [#272](https://github.com/fortran-lang/stdlib/pull/272) (WIP):
  Probability Distribution and Statistical Functions -- Uniform Distribution Module
* [#189](https://github.com/fortran-lang/stdlib/pull/189) (WIP):
  Initial implementation of COO / CSR sparse format


Please help improve stdlib by testing and [reviewing pull requests](https://github.com/fortran-lang/stdlib/issues?q=is%3Apr+is%3Aopen+label%3A%22reviewers+needed%22)!

The candidate for file system operations to be included in stdlib is being developed by
[@MarDiehl](https://github.com/MarDiehl) and [@arjenmarkus](https://github.com/arjenmarkus)
in [this repository](https://github.com/MarDiehl/stdlib_os).
Please try it out and let us know how it works, if there are any issues, or if the API can be improved.

## Fortran Package Manager

Here's what's new in fpm:

* [Version 0.4.0](https://github.com/fortran-lang/fpm/releases/tag/v0.4.0):
  Alpha release update
* [#546](https://github.com/fortran-lang/fpm/pull/546):
  Update version for release 0.4.0
* [#548](https://github.com/fortran-lang/fpm/pull/548):
  Fix build on MacOS/ARM64
* [#527](https://github.com/fortran-lang/fpm/pull/527):
  Add objects for handling compiler and archiver
* [#536](https://github.com/fortran-lang/fpm/pull/536):
  Always call `git init` in fpm new when backfilling
* [#533](https://github.com/fortran-lang/fpm/pull/533):
  Allow extra section in package manifest
* [#528](https://github.com/fortran-lang/fpm/pull/528):
  Generate `build/.gitignore`

### Work in progress

* [#539](https://github.com/fortran-lang/fpm/pull/539) (WIP):
  Add parent packages into dependency tree
* [#521](https://github.com/fortran-lang/fpm/pull/521) (WIP):
  Expand tabs in source parsing
* [#506](https://github.com/fortran-lang/fpm/pull/506) (WIP):
  Initial implementation of implicit_none
* [#498](https://github.com/fortran-lang/fpm/pull/498) (WIP):
  Compiler flags profiles

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

## FFTPACK

Zuo Zhihua ([@zoziha](https://github.com/zoziha)) and Ondřej Čertík ([@certik](https://github.com/certik)) started maintaining the public domain project FFTPACK under the Fortran-lang namespace.
The project is readily available for usage in with fpm.

Here is what is new in FFTPACK:

* [#10](https://github.com/fortran-lang/fftpack/pull/10):
  Add `(i)qct/dcosqi/dcosqf/dcosqb` interfaces for quarter wave data.
* [#7](https://github.com/fortran-lang/fftpack/pull/7):
  Add `dzffti/dzfftf/dzfftb` interfaces
* [#4](https://github.com/fortran-lang/fftpack/pull/4):
  Improve fft interface for `complex` sequences: `(i)fft/zffti/zfftf/zfftb`
* [#6](https://github.com/fortran-lang/fftpack/pull/6):
  Add  `(i)rfft/dffti/dfftf/dfftb` interface and ready to move to `fortran-lang`
* [#5](https://github.com/fortran-lang/fftpack/pull/5):
  Add `fftshift/ifftshift`
* [#3](https://github.com/fortran-lang/fftpack/pull/3):
  Add CI: fpm.yml

### Work in progress

* [#11](https://github.com/fortran-lang/fftpack/pull/11) (WIP):
  Add `(i)dct/dcosti/dcost` interfaces.

Feedback and ideas for this project are welcome.

## Compilers

### Flang

Recent development updates:

* New Driver and CMake integration:
    * The new driver has replaced the old, hence f18 has been deleted. 
    * flang-new (aka. the new driver) now drives the flang bash script before an external compiler is called.
    * Code-generation work is ongoing.
    * Work is now proceeding to enable CMake to recognize the compiler and set the appropriate options for build configurations.
* FIR (Fortran IR - a dialect of MLIR):
    * Fortran 95 lowering and runtime support is nearing completion, particularly of intrinsics
    * Code upstreaming will begin again in earnest once F95 is deemed complete
* OpenMP
    * Nesting of region semantic checks
    * enter_data MLIR to LLVM IR lowering
    * Semantic checks for allocate directive
    * Lowering for various modifiers for the schedule clause
    * Pretty printer and parser for omp.target operation
    * Semantic checks for linear, nested barrier, allocate directive
    * Progress with firstprivate, critical, collapse, ordered, reduction
* Lift -Werror checks into local functions
* Document the flang wrapper script
* Fix the extent calculation when upper bounds are less than lower bounds
* Fix list-directed plural repeated null values at end of record
* Fix build failure on MacOS involving std::clock_t
* Fix error messages on Windows.
* Disable Plugins in out-of-tree builds
* Correct off-by-one error in SET_EXPONENT

Call notes are recorded and available upon request [here](https://docs.google.com/document/d/10T-S2J3GrahpG4Ooif93NSTz2zBW0MQc_RlwHi0-afY). Please contact Alexis Perry-Holby at aperry@lanl.gov for document access.

### LFortran

* 168
  [merged](https://gitlab.com/lfortran/lfortran/-/merge_requests?scope=all&state=merged)
  Merge Requests in August 2021
* The following people have contributed code to LFortran in August:
   * Ondřej Čertík
   * Thirumalai Shaktivel
   * Rohit Goswami
   * Gagandeep Singh
   * Andrew Best
   * Carlos Une
* Sebastian Ehlert got LFortran working with fpm
* Many people have reported bugs (thank you all!)
* Our 3 Google Summer of Code (GSoC) students have successfully finished their
  projects. Here are their final reports:
   * Gagandeep Singh: [https://czgdp1807.github.io/2021/08/16/z_final_report.html](https://czgdp1807.github.io/2021/08/16/z_final_report.html)
   * Thirumalai Shaktivel: [https://gist.github.com/Thirumalai-Shaktivel/c2a1aaa5239e792e499eaa8942987519](https://gist.github.com/Thirumalai-Shaktivel/c2a1aaa5239e792e499eaa8942987519)
   * Rohit Goswami: [https://rgoswami.me/posts/gsoc21-fin-reprot/](https://rgoswami.me/posts/gsoc21-fin-reprot/)

#### LFortran 0.12.0 was released on August 15

Changes since the last release.

* Fixed all issues in the parser that were reported (AST)
  * multiple loop single end do
  * arithmetic if
* Comments and empty lines are now represented in AST and formatted correctly (AST)
* The formatter (`lfortran fmt`) now uses the minimal amount of parentheses in expressions
* Initial fixed-form parser (AST)
* Initial class support (ASR, LLVM)
* Allocate / deallocate, allocatable arrays (ASR, LLVM)
* Associate block (ASR, LLVM)
* Runtime library refactoring (ASR, LLVM)
  * Split into builtin, pure and impure
  * `iso_fortran_env`, `iso_c_binding` intrinsic modules added
* Compile time evaluation (ASR, LLVM)

Commits (`git shortlog -ns v0.11.0..v0.12.0`):
```
   369  Ondřej Čertík
    85  Thirumalai Shaktivel
    79  Gagandeep Singh
    75  Rohit Goswami
    20  Andrew Best
     4  Dominic Poerio
```


#### Updates in master since the last release:

* LFortran can now compile binaries on Windows
* C interoperation works on all platforms (including Windows and MSVC)
* Runtime library improvements
  * Complex intrinsics fixed on all platforms
  * All trigonometric functions now use the Fortran `impure` interface in the runtime library
  * More intrinsics implemented
* Initial implementation of classes and methods
* LFortran now works with `fpm` and compiles the hello world project and a few other example projects
* Parser improvements: team and sync statements
* Improved handling of character types as function arguments and return values


We are looking for new contributors, so if you are interested, please [get in
touch with us](https://lfortran.org/)! We would be happy to do a video call
with you to get you started.


## Events

* We had our 15th Fortran Monthly call on August 17.
  You can watch the recording below:
  <iframe width="560" height="315" src="https://www.youtube.com/embed/496oZFYcA00" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

* FortranCon 2021 will be held virtually from September 23-24, 2021.
  Registration is free of charge and is due by September 15.
  The second call for abstracts is due September 1.
  For more information, visit the [FortranCon website](https://tcevents.chem.uzh.ch/event/14/).

* Our Google Summer of Code program for 2021 is coming to a close.
  Read about our students and their progress so far on Discourse: <https://fortran-lang.discourse.group/c/gsoc-2021/11>
  
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

<div id="gh-contributors" data-startdate="August 01 2021" data-enddate="August 31 2021" height="500px"></div>
