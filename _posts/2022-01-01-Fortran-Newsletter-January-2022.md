---
layout: post
title: "Fortran newsletter: January 2022"
category: newsletter
author: Milan Curcic, Sebastian Ehlert, Jérémie Vandenplas
---

Happy New Year and welcome to the January 2022 edition of the monthly Fortran 
newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

## fortran-lang.org

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
  just allow . on new subcommand instead of changing canonical path pro…
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

TODO @AlexisPerry

### LFortran

TODO @certik

We are looking for new contributors. Please do not hesitate to contact us if
you are interested. We will help you get up to speed.

## Events

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
* [j3-fortran/fortran\_proposals](https://github.com/j3-fortran/fortran_proposals)

<div id="gh-contributors" data-startdate="December 01 2021" data-enddate="January 01 2022" height="500px"></div>
