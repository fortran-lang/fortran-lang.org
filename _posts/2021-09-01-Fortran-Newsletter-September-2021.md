---
layout: post
title: "Fortran newsletter: September 2021"
category: newsletter
authors: Milan Curcic, Alexis Perry-Holby
---

Welcome to the September 2021 edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

## fortran-lang.org

This month we've had several updates to the website:

TODO @milancurcic

[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues) if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the [contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/master/CONTRIBUTING.md) for how to get started.

## Fortran Standard Library

Here's what's new in stdlib:

TODO @milancurcic

Please help improve stdlib by testing and [reviewing pull requests](https://github.com/fortran-lang/stdlib/issues?q=is%3Apr+is%3Aopen+label%3A%22reviewers+needed%22)!

The candidate for file system operations to be included in stdlib is being developed by
[@MarDiehl](https://github.com/MarDiehl) and [@arjenmarkus](https://github.com/arjenmarkus)
in [this repository](https://github.com/MarDiehl/stdlib_os).
Please try it out and let us know how it works, if there are any issues, or if the API can be improved.

## Fortran Package Manager

Here's what's new in fpm:

TODO @milancurcic

`fpm` is still in early development and we need as much help as we can get.
Here's how you can help today:

* Use it and let us know what you think! Read the [fpm packaging guide](https://github.com/fortran-lang/fpm/blob/master/PACKAGING.md) to learn how to build your package with fpm, and the [manifest reference](https://github.com/fortran-lang/fpm/blob/master/manifest-reference.md) to learn what are all the things that you can specify in the fpm.toml file.
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

Call notes are recorded [here](https://docs.google.com/document/d/10T-S2J3GrahpG4Ooif93NSTz2zBW0MQc_RlwHi0-afY).

### LFortran

TODO @certik

We are looking for new contributors, so if you are interested, please [get in
touch with us](https://lfortran.org/)!


## Events

* We had our 15th Fortran Monthly call on August 17.
  You can watch the recording below:
  <iframe width="560" height="315" src="https://www.youtube.com/embed/9goA20JAHls" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

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
