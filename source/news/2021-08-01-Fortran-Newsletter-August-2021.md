---
layout: post
title: "Fortran newsletter: August 2021"
category: newsletter
date: 2021-08-01
author: Milan Curcic, Ondřej Čertík, Laurence Kedward
---

Welcome to the August 2021 edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

# fortran-lang.org

This month we've had several updates to the website:

* [#281](https://github.com/fortran-lang/fortran-lang.org/pull/281):
  July newsletter
* [#274](https://github.com/fortran-lang/fortran-lang.org/pull/274):
  Add `convert_FORTRAN_case` formatter to package index
* [#277](https://github.com/fortran-lang/fortran-lang.org/pull/277):
  Add projects for Fortran-lua interfacing to package index
* [#284](https://github.com/fortran-lang/fortran-lang.org/pull/284):
  PRs script updates
* [#286](https://github.com/fortran-lang/fortran-lang.org/pull/286):
  Installation process for GFortran on OpenBSD
* [#288](https://github.com/fortran-lang/fortran-lang.org/pull/288):
  Add Flatiron institute multipole libraries to the package index
* [#289](https://github.com/fortran-lang/fortran-lang.org/pull/289):
  Small fix in packages index
* [#291](https://github.com/fortran-lang/fortran-lang.org/pull/291):
  Bump addressable from 2.7.0 to 2.8.0
* [#293](https://github.com/fortran-lang/fortran-lang.org/pull/293):
  add Apogee and Edinburgh compilers
* [#290](https://github.com/fortran-lang/fortran-lang.org/pull/290):
  Add arrayfire-fortran to package index
* [#294](https://github.com/fortran-lang/fortran-lang.org/pull/294):
  compilers: use more objective tone
* [#296](https://github.com/fortran-lang/fortran-lang.org/pull/296):
  my software with at least 5 stars
* [#297](https://github.com/fortran-lang/fortran-lang.org/pull/297):
  Fix insecure workflow.

Ongoing work:

* [#246](https://github.com/fortran-lang/fortran-lang.org/pull/246) (WIP):
  Transferring fortran90.org "Fortran Best Practices" into a mini-book
* [#201](https://github.com/fortran-lang/fortran-lang.org/pull/201) (WIP):
  Draft: Internationalization for fortran-lang

[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues) if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the [contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/CONTRIBUTING.md) for how to get started.

## Fortran Standard Library

Here's what's new in stdlib:

* [#436](https://github.com/fortran-lang/stdlib/pull/436):
  implemented low-level `replace_all` function
* [#454](https://github.com/fortran-lang/stdlib/pull/454):
  added `stdlib_math` to specs/index.md
* [#453](https://github.com/fortran-lang/stdlib/pull/453):
  implemented count function
* [#441](https://github.com/fortran-lang/stdlib/pull/441):
  implemented pad function
* [#456](https://github.com/fortran-lang/stdlib/pull/456):
  slice function's documentation made user friendly
* [#459](https://github.com/fortran-lang/stdlib/pull/459):
  Fix CMake variable usage
* [#420](https://github.com/fortran-lang/stdlib/pull/420):
  First implementation of real-valued linspace.
* [#468](https://github.com/fortran-lang/stdlib/pull/468):
  Update CI
* [#469](https://github.com/fortran-lang/stdlib/pull/469):
  CMake: corrections and updates
* [#426](https://github.com/fortran-lang/stdlib/pull/426):
  Addition of a subroutine to compute the median of array elements
* [#474](https://github.com/fortran-lang/stdlib/pull/474):
  Bug fix: Allocatable argument 'x' is not allocated #472

Work in progress:

* [#481](https://github.com/fortran-lang/stdlib/pull/481) (WIP):
  [`stdlib_linalg`] Update eye function.
* [#480](https://github.com/fortran-lang/stdlib/pull/480) (WIP):
  [`stdlib_math`] Add seq function.
* [#478](https://github.com/fortran-lang/stdlib/pull/478) (WIP):
  [`stdlib_linalg`] Add zeros, ones, ex function.
* [#477](https://github.com/fortran-lang/stdlib/pull/477) (WIP):
  [`stdlib_linalg`] Add empty function.
* [#475](https://github.com/fortran-lang/stdlib/pull/475) (WIP):
  Generating sorting subroutines specific to character type with fypp
* [#473](https://github.com/fortran-lang/stdlib/pull/473) (WIP):
  Error stop improvements
* [#470](https://github.com/fortran-lang/stdlib/pull/470) (WIP):
  Revival string list 
* [#467](https://github.com/fortran-lang/stdlib/pull/467) (WIP):
  implemented `move_alloc` for `string_type`
* [#455](https://github.com/fortran-lang/stdlib/pull/455) (WIP):
  `stdlib_hash`: waterhash algorithm
* [#452](https://github.com/fortran-lang/stdlib/pull/452) (WIP):
  Implementation of a map data type
* [#445](https://github.com/fortran-lang/stdlib/pull/445) (WIP):
  [feature] `disp`(display your data) & `format_string`(format other type to string, see #444)
* [#444](https://github.com/fortran-lang/stdlib/pull/444) (WIP):
  Add `format_string` routine to format other types to strings
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
* [#157](https://github.com/fortran-lang/stdlib/pull/157) (WIP):
  Update CMAKE files

Please help improve stdlib by testing and [reviewing pull requests](https://github.com/fortran-lang/stdlib/issues?q=is%3Apr+is%3Aopen+label%3A%22reviewers+needed%22)!

The candidate for file system operations to be included in stdlib is being developed by
[@MarDiehl](https://github.com/MarDiehl) and [@arjenmarkus](https://github.com/arjenmarkus)
in [this repository](https://github.com/MarDiehl/stdlib_os).
Please try it out and let us know how it works, if there are any issues, or if the API can be improved.

## Fortran Package Manager

Here's what's new in fpm:

* [#507](https://github.com/fortran-lang/fpm/pull/507):
  optimize file listing
* [#511](https://github.com/fortran-lang/fpm/pull/511):
  check name used for package, executable, test, or example
* [#516](https://github.com/fortran-lang/fpm/pull/516):
  initialize allocatable strings before using in a comparison
* [#517](https://github.com/fortran-lang/fpm/pull/517):
  Fix run
* [#522](https://github.com/fortran-lang/fpm/pull/522):
  remove warnings and fix truncated help text
* [#523](https://github.com/fortran-lang/fpm/pull/523):
  Fix compilation error in ifort

Work in progress:

* [#525](https://github.com/fortran-lang/fpm/pull/525) (WIP):
  proposal to close #525 by generating build/.gitignore
* [#527](https://github.com/fortran-lang/fpm/pull/527) (WIP):
  Add objects for handling compiler and archiver
* [#521](https://github.com/fortran-lang/fpm/pull/521) (WIP):
  expand tabs
* [#506](https://github.com/fortran-lang/fpm/pull/506) (WIP):
  Draft: initial implementation of `implicit_none`
* [#498](https://github.com/fortran-lang/fpm/pull/498) (WIP):
  Draft - Compiler flags profiles

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

### LFortran

Updates for July 2021:

* 90 [merged](https://gitlab.com/lfortran/lfortran/-/merge_requests?scope=all&state=merged) MRs, this month we have crossed 1000 total merged MRs, 12 total contributors
* Parser: we asked the community to test it, several people have reported about
  15 bugs, we have fixed all of them (AST)
* Initial fixed form parser (AST)
* Classes and class procedures (ASR, LLVM)
* Many common array usage now works, including allocatable (ASR, LLVM)
* Associate construct (ASR, LLVM)
* Compile time evaluation of constant expressions (ASR)
* 7 people contributed code:
    * Ondřej Čertík
    * Thirumalai Shaktivel
    * Gagandeep Singh
    * Rohit Goswami
    * Dominic Poerio
    * Andrew Best
    * Sebastian Ehlert

We are looking for new contributors, so if you are interested, please [get in
touch with us](https://lfortran.org/)!


## Events

* We had our 14th Fortran Monthly call on July 20.
You can watch the recording below:

<iframe width="560" height="315" src="https://www.youtube.com/embed/9goA20JAHls" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

* FortranCon 2021 will be held virtually from September 23-24, 2021.
Registration is free of charge and is due by September 15.
The first call for abstracts is due August 1, and the second is due September 1.
For more information, visit the [FortranCon website](https://tcevents.chem.uzh.ch/event/14/).

* Work is well under way started for our Google Summer of Code program. Read about our students and their progress so far on Discourse: <https://fortran-lang.discourse.group/c/gsoc-2021/11>
  
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

<div id="gh-contributors" data-startdate="June 01 2021" data-enddate="June 30 2021" height="500px"></div>
