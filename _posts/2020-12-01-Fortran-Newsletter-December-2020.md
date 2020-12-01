---
layout: post
title: "Fortran newsletter: December 2020"
category: newsletter
author: Milan Curcic, Jérémie Vandenplas, Laurence Kedward
---

Welcome to the December 2020 edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

## fortran-lang.org

This month we've had a few updates to the website:

* [#156](https://github.com/fortran-lang/fortran-lang.org/pull/156):
Updates to the mini-book on building Fortran programs, including the addition of
short guides on meson and CMake.
You can read the mini-book [here](https://fortran-lang.org/learn/building_programs).
* [#169](https://github.com/fortran-lang/fortran-lang.org/pull/169):
Add PSBLAS to the package index.

Ongoing work:

* [#160](https://github.com/fortran-lang/fortran-lang.org/pull/160) (WIP):
In-depth introduction for Fortran with Make.

[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues) if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the [contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/master/CONTRIBUTING.md) for how to get started.

## Fortran Standard Library

Here's what's new in stdlib:

* [#239](https://github.com/fortran-lang/stdlib/pull/239): Implementation of bitsets in `stdlib_bitsets`.
* [#243](https://github.com/fortran-lang/stdlib/pull/243),
  [#245](https://github.com/fortran-lang/stdlib/pull/245),
  [#252](https://github.com/fortran-lang/stdlib/pull/253),
  [#255](https://github.com/fortran-lang/stdlib/pull/255): Various improvements to `stdlib_logger`.
* [#245](https://github.com/fortran-lang/stdlib/pull/245),
  [#250](https://github.com/fortran-lang/stdlib/pull/250): Minor fixes to the CI.

Work in progress:

* (WIP) [#240](https://github.com/fortran-lang/stdlib/pull/240): Implementation of the `stdlib_stats_distribution` module. It provides probability distribution and statistical functions.
* (WIP) [#189](https://github.com/fortran-lang/stdlib/pull/189): Initial implementation of sparse matrices.

Don't hesitate to test and review these pull requests!

Otherwise, ongoing discussions continue:
* [#220](https://github.com/fortran-lang/stdlib/issues/220): API for file system operations: directory manipulation
* [#241](https://github.com/fortran-lang/stdlib/issues/241): Include a `split` function (202X feature)
* [#254](https://github.com/fortran-lang/stdlib/issues/254): Proposition to add a logger for debug phases and levels among the different logs.

The candidate for file system operations to be included in stdlib is being developed by
[@MarDiehl](https://github.com/MarDiehl) and [@arjenmarkus](https://github.com/arjenmarkus)
in [this repository](https://github.com/MarDiehl/stdlib_os).
Please try it out and let us know how it works, if there are any issues, or if the API can be improved.

## Fortran Package Manager

Here's what's new in fpm:

* [#259](https://github.com/fortran-lang/fpm/pull/259): Update the instructions for building from source in README.md.
* [#246](https://github.com/fortran-lang/fpm/pull/246): Automated binary releases in CI.
* [#233](https://github.com/fortran-lang/fpm/pull/233): Allow linking with external libraries.
* [#224](https://github.com/fortran-lang/fpm/pull/224): Add a reference document for the package manifest (fpm.toml).
* [#221](https://github.com/fortran-lang/fpm/pull/221),
  [#239](https://github.com/fortran-lang/fpm/pull/239): Runner options for test and app executables.
* [#220](https://github.com/fortran-lang/fpm/pull/220): Implement compiler and flags settings in Haskell fpm.
* [#209](https://github.com/fortran-lang/fpm/pull/209):
  [#237](https://github.com/fortran-lang/fpm/pull/237): Developer API docs.
* [#216](https://github.com/fortran-lang/fpm/pull/216),
  [#225](https://github.com/fortran-lang/fpm/pull/225),
  [#226](https://github.com/fortran-lang/fpm/pull/226),
  [#229](https://github.com/fortran-lang/fpm/pull/229),
  [#236](https://github.com/fortran-lang/fpm/pull/236),
  [#240](https://github.com/fortran-lang/fpm/pull/240),
  [#247](https://github.com/fortran-lang/fpm/pull/240): Other fixes and improvements.

Work in progress:

* [First beta release](https://github.com/fortran-lang/fpm/milestone/1) (WIP): First feature-complete release of the Fortran implementation.
* (WIP) [#230](https://github.com/fortran-lang/fpm/pull/230),
        [#261](https://github.com/fortran-lang/fpm/pull/261): Specification of the fpm CLI.
* (WIP) [#232](https://github.com/fortran-lang/fpm/pull/232): Allowing the `extra` section in fpm.toml.
* (WIP) [#248](https://github.com/fortran-lang/fpm/pull/248): Refactor backend for incremental rebuilds.
* (WIP) [#251](https://github.com/fortran-lang/fpm/pull/251): Dependency management.
* (WIP) [#255](https://github.com/fortran-lang/fpm/pull/255): Setting the compiler and specifying test or app target.
* (WIP) [#257](https://github.com/fortran-lang/fpm/pull/257): Implement `fpm install`.
* (WIP) [#260](https://github.com/fortran-lang/fpm/pull/260): Fix CI to test release build.

fpm is still in early development and we need as much help as we can get.
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

TODO @gklimowicz

### LLVM Flang

TODO @gklimowicz

### LFortran

What's new in LFortran:

TODO @certik

You can follow LFortran on Twitter for latest updates: [@lfortranorg](https://twitter.com/lfortranorg).

## Events

* Brian Friesen (Lawrence Berkeley National Laboratory) was selected to be the new Chair of PL22.3 (J3, US Standards Committee).
Brian will serve in his first term until November 2023. Congratulations, Brian!
* We had our 6th Fortran Monthly call on November 17.
You can watch the recording below:

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/HI-Yhn7Q8Ko" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

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

<div id="gh-contributors" data-startdate="November 01 2020" data-enddate="November 31 2020" height="500px"></div>
