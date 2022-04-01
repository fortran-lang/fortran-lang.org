---
layout: post
title: "Fortran newsletter: April 2022"
category: newsletter
author: Milan Curcic
---

Welcome to the April edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

## fortran-lang.org

Here's what's new in the fortran-lang.org repo:

* [#379](https://github.com/fortran-lang/fortran-lang.org/pull/379):
  Newsletter for March 2022
* [#383](https://github.com/fortran-lang/fortran-lang.org/pull/383):
  Updated VS Code extensions
* [#381](https://github.com/fortran-lang/fortran-lang.org/pull/381):
  add string array to learn
* [#384](https://github.com/fortran-lang/fortran-lang.org/pull/384):
  Resolves typos #377

### Work in progress

* [#369](https://github.com/fortran-lang/fortran-lang.org/pull/369) (WIP):
  Resolves Issue #217
* [#347](https://github.com/fortran-lang/fortran-lang.org/pull/347) (WIP):
  Fortran Intrinsics

[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues)
if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the
[contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/CONTRIBUTING.md)
for how to get started.

## Fortran Standard Library

Here's what's new in stdlib:

* [#632](https://github.com/fortran-lang/stdlib/pull/632):
  doc: fix some typos
* [#629](https://github.com/fortran-lang/stdlib/pull/629):
  option to disable testing by setting BUILD_TESTING to OFF
* [#631](https://github.com/fortran-lang/stdlib/pull/631):
  Preparation for 0.2.0 release
* [#637](https://github.com/fortran-lang/stdlib/pull/637):
  Only set Fortran arguments for Fortran compiler
* [#642](https://github.com/fortran-lang/stdlib/pull/642):
  Fix linking issue with shared libraries

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

* [#675](https://github.com/fortran-lang/fpm/pull/675):
  Fix for backtrace error when file not found in: src/fpm_source_parsing.f90
* [#677](https://github.com/fortran-lang/fpm/pull/677):
  Fix issue with backend pretty output
* [#684](https://github.com/fortran-lang/fpm/pull/684):
  fix: remove remove unnecessary space in fpm new cmd
* [#8](https://github.com/fortran-lang/setup-fpm/pull/8) (`setup-fpm`):
  Update to v4 in usage example in README

### Work in progress

* [#685](https://github.com/fortran-lang/fpm/pull/685) (WIP):
  fix: function for getting executable path
* [#676](https://github.com/fortran-lang/fpm/pull/676) (WIP):
  Tree shaking for modules
* [#671](https://github.com/fortran-lang/fpm/pull/671) (WIP):
  Add `library-dir` to support `-Lpath`
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

### LFortran

We are looking for new contributors. Please do not hesitate to contact us if you are interested. We will help you get up to speed.

## Events

* The "State of Fortran" paper by Kedward et al. has been accepted for publication in the IEEE journal Computing in Science and Engineering (CiSE).
  You can read the pre-print on [arXiv](https://arxiv.org/abs/2203.15110), or find the paper in Early Access on the [CiSE website](https://ieeexplore.ieee.org/document/9736688).
* The contributor application window for this year's [Google Summer of Code](https://summerofcode.withgoogle.com) is approaching fast.
  It opens **April 4** and closes **April 19**.
  See the [Fortran-lang GSoC 2022 page](https://summerofcode.withgoogle.com/programs/2022/organizations/fortran-lang) for information about the projects and how to apply.
  To learn more about GSoC and what has changed since last year, please see the [GSoC 2022 announcement](https://opensource.googleblog.com/2021/11/expanding-google-summer-of-code-in-2022.html).
  If you'd like to participate, please let us know and we'll help you get started.
* We had our 24th Fortran Monthly call on March 15.
  You can watch the recording below:
  <iframe width="560" height="315" src="https://www.youtube.com/embed/hekwzPzIfu8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

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

<div id="gh-contributors" data-startdate="March 01 2022" data-enddate="March 31 2022" height="500px"></div>
