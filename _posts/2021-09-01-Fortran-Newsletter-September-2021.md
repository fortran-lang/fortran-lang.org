---
layout: post
title: "Fortran newsletter: September 2021"
category: newsletter
author: Milan Curcic
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

TODO @AlexisPerry

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
