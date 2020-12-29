---
layout: post
title: "Fortran newsletter: January 2021"
category: newsletter
author: Milan Curcic, Jérémie Vandenplas, Laurence Kedward, Gary Klimowicz, Ondřej Čertík
---

Welcome to the January 2021 edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

## fortran-lang.org

This month we've had a few updates to the website:

* [#171](https://github.com/fortran-lang/fortran-lang.org/pull/171):
Add Sebastian to the list of authors in the building-programs mini-book
* [#173](https://github.com/fortran-lang/fortran-lang.org/pull/173):
Add Octopus to the package index
* [#178](https://github.com/fortran-lang/fortran-lang.org/pull/178):
Fix build preview
* [#179](https://github.com/fortran-lang/fortran-lang.org/pull/179):
Fix word spelling error in quickstart page
* [#180](https://github.com/fortran-lang/fortran-lang.org/pull/180):
Add TOML-Fortran to the package index
* [#182](https://github.com/fortran-lang/fortran-lang.org/pull/182):
Update compilers page following Intel oneAPI release

Ongoing work:

* [#160](https://github.com/fortran-lang/fortran-lang.org/pull/160) (WIP):
In-depth introduction for Fortran with Make.
* [#186](https://github.com/fortran-lang/fortran-lang.org/pull/186) (WIP):
Add missing packages to the list of popular Fortran projects
* [#187](https://github.com/fortran-lang/fortran-lang.org/pull/187) (WIP):
Correct Compiler page and tutorial regarding Intel oneAPI and PGI to NVIDIA
* [#188](https://github.com/fortran-lang/fortran-lang.org/pull/188) (WIP):
Use setup-ruby 2.7 to fix CI build


[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues) if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the [contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/master/CONTRIBUTING.md) for how to get started.

## Fortran Standard Library

Here's what's new in `stdlib`:

* [#256](https://github.com/fortran-lang/stdlib/pull/256): Add the method `log_debug` to `stdlib_logger`
* [#257](https://github.com/fortran-lang/stdlib/pull/257): Improve CMake check for F18 error stop
* [#260](https://github.com/fortran-lang/stdlib/pull/260): Add Intel oneAPI Fortran compiler to CI
* [#261](https://github.com/fortran-lang/stdlib/pull/261): Add a level option to ignore logging messages
* [#263](https://github.com/fortran-lang/stdlib/pull/263), 
  [#267](https://github.com/fortran-lang/stdlib/pull/267): Minor fixes to CI
* [#270](https://github.com/fortran-lang/stdlib/pull/270): Add GFortran 10 to CI
* [#275](https://github.com/fortran-lang/stdlib/pull/275): Add MSYS2 systems to Windows CI
* [#282](https://github.com/fortran-lang/stdlib/pull/282): Add a note about memory issues when compiling stdlib with the support of arrays to up 15 ranks
* [#283](https://github.com/fortran-lang/stdlib/pull/283): Improve the compilation load by splitting submodules


Work in progress:

* [#269](https://github.com/fortran-lang/stdlib/pull/269) (WIP): Implementation of a module for handling lists of strings
* [#271](https://github.com/fortran-lang/stdlib/pull/271) (WIP),
 [#272](https://github.com/fortran-lang/stdlib/pull/272) (WIP),
 [#273](https://github.com/fortran-lang/stdlib/pull/273) (WIP),
 [#276](https://github.com/fortran-lang/stdlib/pull/276) (WIP),
 [#278](https://github.com/fortran-lang/stdlib/pull/278) (WIP): Implementation of the `stdlib_stats_distribution` modules. It provides probability distribution and statistical functions.
* [#284](https://github.com/fortran-lang/stdlib/pull/284) (WIP): Required changes to be able to use `stdlib` as a subproject in CMake
* [#189](https://github.com/fortran-lang/stdlib/pull/189) (WIP): Initial implementation of sparse matrices.

Don't hesitate to test and review these pull requests!

Otherwise, ongoing discussions continue about usability of `stdlib`
([#7](https://github.com/fortran-lang/stdlib/issues/7),
[#215](https://github.com/fortran-lang/stdlib/issues/215),
[#279](https://github.com/fortran-lang/stdlib/issues/279),
[#280](https://github.com/fortran-lang/stdlib/issues/280),
[#285](https://github.com/fortran-lang/stdlib/issues/285)),
and new implementations for `stdlib`
([#135](https://github.com/fortran-lang/stdlib/issues/135),
[#212](https://github.com/fortran-lang/stdlib/issues/212),
[#234](https://github.com/fortran-lang/stdlib/issues/234),
[#241](https://github.com/fortran-lang/stdlib/issues/241),
[#258](https://github.com/fortran-lang/stdlib/issues/258),
[#259](https://github.com/fortran-lang/stdlib/issues/259),
[#262](https://github.com/fortran-lang/stdlib/issues/262),
[#268](https://github.com/fortran-lang/stdlib/issues/268),
[#277](https://github.com/fortran-lang/stdlib/issues/277)).


The candidate for file system operations to be included in stdlib is being developed by
[@MarDiehl](https://github.com/MarDiehl) and [@arjenmarkus](https://github.com/arjenmarkus)
in [this repository](https://github.com/MarDiehl/stdlib_os).
Please try it out and let us know how it works, if there are any issues, or if the API can be improved.

## Fortran Package Manager

Here's what's new in `fpm`:


TO DO

Work in progress:

TO DO

`fpm` is still in early development and we need as much help as we can get.
Here's how you can help today:

* Use it and let us know what you think! Read the [fpm packaging guide](https://github.com/fortran-lang/fpm/blob/master/PACKAGING.md) to learn how to build your package with fpm, and the [manifest reference](https://github.com/fortran-lang/fpm/blob/master/manifest-reference.md) to learn what are all the things that you can specify in the fpm.toml file.
* Browse the [open issues](https://github.com/fortran-lang/fpm/issues) and see if you can help implement any fixes or features.
* Adapt your Fortran package for fpm and submit it to the [Registry](https://github.com/fortran-lang/fpm-registry).
* Improve the documentation.

The short-term goal of fpm is to make development and installation of Fortran packages with dependencies easier.
Its long term goal is to build a rich and decentralized ecosystem of Fortran packages and create a healthy
environment in which new open source Fortran projects are created and published with ease.

## Compilers

### Classic Flang

TO DO

The Classic Flang biweekly call has been set up to discuss issues and plans
for the next pull requests to be validated and merged. Our next calls are Wednesday, December 16 and 30, 8:00 AM Pacific time. The notes from previous calls, upcoming agenda and a link to join the call can be found
[here](https://docs.google.com/document/d/1-OuiKx4d7O6eLEJDBDKSRnSiUO2rgRR-c2Ga4AkrzOI).

### LLVM Flang

TO DO


## Events

* We had our 7th Fortran Monthly call on December 15.
You can watch the recording below:


<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/S_xQCSRlefE" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

As usual, subscribe to the [mailing list](https://groups.io/g/fortran-lang) and/or
join the [Discourse](https://fortran-lang.discourse.group) to stay tuned with the future meetings.

## Contributors

We thank everybody who contributed to fortran-lang in the past month by
commenting in any of these repositories:

* [fortran-lang/stdlib](https://github.com/fortran-lang/stdlib)
* [fortran-lang/fpm](https://github.com/fortran-lang/fpm)
* [fortran-lang/fpm-registry](https://github.com/fortran-lang/fpm-registry)
* [fortran-lang/fortran-lang.org](https://github.com/fortran-lang/fortran-lang.org)
* [fortran-lang/benchmarks](https://github.com/fortran-lang/benchmarks)
* [j3-fortran/fortran_proposals](https://github.com/j3-fortran/fortran_proposals)

<div id="gh-contributors" data-startdate="December 01 2020" data-enddate="December 31 2020" height="500px"></div>
