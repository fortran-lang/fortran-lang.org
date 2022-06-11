---
layout: post
title: "Fortran newsletter: July 2021"
category: newsletter
date: 2021-07-01
author: Laurence Kedward, Sebastian Ehlert, Ondřej Čertík, Zachary Moon, Milan Curcic
---

Welcome to the July 2021 edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

# fortran-lang.org

This month we've had several updates to the website:

* [#276](https://github.com/fortran-lang/fortran-lang.org/pull/276):
  Add LATTE tight-binding molecular dynamics code to package index                                                               
* [#275](https://github.com/fortran-lang/fortran-lang.org/pull/275):
  Add crest program to package index
* [#255](https://github.com/fortran-lang/fortran-lang.org/pull/255):
  Quickstart edits
* [#273](https://github.com/fortran-lang/fortran-lang.org/pull/273):
  Add the SNaC package to package index
* [#272](https://github.com/fortran-lang/fortran-lang.org/pull/272):
  Add QUICK to package index

Ongoing work:

* [#277](https://github.com/fortran-lang/fortran-lang.org/pull/277) (WIP):
  Add projects for Fortran-lua interfacing to package index                                                                
* [#274](https://github.com/fortran-lang/fortran-lang.org/pull/274) (WIP):
  Add convert_FORTRAN_case formatter to package index
* [#246](https://github.com/fortran-lang/fortran-lang.org/pull/246) (WIP):
  Transferring fortran90.org "Fortran Best Practices" into a mini-book                                                     
* [#201](https://github.com/fortran-lang/fortran-lang.org/pull/201) (WIP):
  Draft: Internationalization for fortran-lang    

[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues) if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the [contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/CONTRIBUTING.md) for how to get started.

## Fortran Standard Library

Here's what's new in `stdlib`:

* [#313](https://github.com/fortran-lang/stdlib/pull/313):
  Legendre polynomials and gaussian quadrature
* [#432](https://github.com/fortran-lang/stdlib/pull/432):
  Outer product
* [#439](https://github.com/fortran-lang/stdlib/pull/439):
  Reduce time spent on sorting tests
* [#440](https://github.com/fortran-lang/stdlib/pull/440):
  Make maximum rank an option
* [#433](https://github.com/fortran-lang/stdlib/pull/433):
  Implemented low level find function for string matching                                                                                  
* [#414](https://github.com/fortran-lang/stdlib/pull/414):
  Implemented intelligent slice functionality
* [#428](https://github.com/fortran-lang/stdlib/pull/428):
  Fix issue with stdlib_sorting
* [#419](https://github.com/fortran-lang/stdlib/pull/419):
  Allow modification of install directory for module files
* [#430](https://github.com/fortran-lang/stdlib/pull/430):
  Remove support for GCC 7 and 8
* [#424](https://github.com/fortran-lang/stdlib/pull/424):
  Add separate logical kind parameters

Work in progress:

* [#445](https://github.com/fortran-lang/stdlib/pull/445) (WIP):
  Add `disp` function to display your data
* [#444](https://github.com/fortran-lang/stdlib/pull/444) (WIP):
  Add `format_string` to format other type to string
* [#441](https://github.com/fortran-lang/stdlib/pull/441) (WIP):
  Implement pad function
* [#437](https://github.com/fortran-lang/stdlib/pull/437) (WIP):
[FPM] add fpm support
* [#436](https://github.com/fortran-lang/stdlib/pull/436) (WIP):
  Implement low-level replace_all function
* [#426](https://github.com/fortran-lang/stdlib/pull/426) (WIP):
  Addition of a subroutine to compute the median of array elements
* [#420](https://github.com/fortran-lang/stdlib/pull/420) (WIP):
  First implementation of real-valued linspace.
* [#363](https://github.com/fortran-lang/stdlib/pull/363) (WIP):
  Sorting string's characters according to their ASCII values
* [#353](https://github.com/fortran-lang/stdlib/pull/353) (WIP):
  Initial checkin for a module for tolerant comparison of reals
* [#333](https://github.com/fortran-lang/stdlib/pull/333) (WIP):
  Provide abstract base class for a string object
* [#311](https://github.com/fortran-lang/stdlib/pull/311) (WIP):
  String list new
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

__Alpha release update:__ Last month saw the release of __v0.3.0__ for *fpm* which includes a number of [new features and bug fixes](https://github.com/fortran-lang/fpm/releases/tag/v0.3.0).

Here is what is new in *fpm*:

* [#504](https://github.com/fortran-lang/fpm/pull/504):
  install.sh, README.md: Update version number, single source file extension
* [#501](https://github.com/fortran-lang/fpm/pull/501):
  Bump version for new release
* [#491](https://github.com/fortran-lang/fpm/pull/491):
  Catch execute_command_line errors and print useful messages
* [#500](https://github.com/fortran-lang/fpm/pull/500):
  Allow reading version number from file
* [#497](https://github.com/fortran-lang/fpm/pull/497):
  correct for equal sign in flag options to fix #495
* [#449](https://github.com/fortran-lang/fpm/pull/449):
  Response files with ar on Windows
* [#490](https://github.com/fortran-lang/fpm/pull/490):
  Minor fix to module parsing
* [#489](https://github.com/fortran-lang/fpm/pull/489):
  Redirect output when searching for archiver
* [#484](https://github.com/fortran-lang/fpm/pull/484):
  Add support for invoking simple plugins
* [#483](https://github.com/fortran-lang/fpm/pull/483):
  Allow fpm to change the working directory

Work in progress:

* [First beta release](https://github.com/fortran-lang/fpm/milestone/1) (WIP):
  First feature-complete release of the Fortran implementation.
* [#505](https://github.com/fortran-lang/fpm/pull/505) (WIP):
  quiet mode for #502
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

* 49 Merge Requests [merged](https://gitlab.com/lfortran/lfortran/-/merge_requests?scope=all&state=merged)
* Highlights
  * Improvements to array support in the LLVM backend and at the ASR level:
    array sections, allocatable arrays, and other improvements
  * Many parser fixes (`lfortran fmt` works on more projects): block data,
    common block, equivalence, custom operator declaration, flush, critical and
    event statements
  * More runtime functions: minval, maxval, real, sum, abs
  * Optional human readable mod files


## Events

* We had our 13th Fortran Monthly call on June 15.
You can watch the recording below:

<iframe width="560" height="315" src="https://www.youtube.com/embed/YRVLAlQpE5g" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

* Joint J3/WG5 (Fortran Standards Committees) meeting was held virtually from June 21-30 (Mondays and Wednesdays only). You can find all the papers that were discussed [here](https://j3-fortran.org/doc/meeting/224). Highlights from the meeting:
  - Conditional expressions syntax for Fortran 202X ([paper](https://j3-fortran.org/doc/year/21/21-157r2.txt)).
  - Protected components specifications and syntax for Fortran 202X ([paper](https://j3-fortran.org/doc/year/21/21-168.txt)).
  - The generics feature planned for Fortran 202Y was discussed at depth ([paper](https://j3-fortran.org/doc/year/21/21-144r4.txt)).
  - Jeff Hammond (NVidia Corporation) is the new J3 member as a voting alternate to Bryce Adelstein-Lelbach.
  - Target year for Fortran 202X is 2023, subject to change.

* FortranCon 2021 will be held virtually from September 23-24, 2021. For more information, visit the [FortranCon website](https://tcevents.chem.uzh.ch/event/14/).

* Work has started for our Google Summer of Code program. You read about our students and their progress so far on Discourse: <https://fortran-lang.discourse.group/c/gsoc-2021/11>
  
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
