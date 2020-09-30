---
layout: post
title: "Fortran newsletter: October 2020"
category: newsletter
author: Milan Curcic
---

Welcome to the October 2020 edition of the monthly Fortran newsletter.
The newsletter comes out on the first calendar day of every month
and details Fortran news from the previous month.

<ul id="page-nav"></ul>

## fortran-lang.org

This month we've had only one minor change to the website:

* [#136](https://github.com/fortran-lang/fortran-lang.org/pull/136):
Small fix in the opening sentence on the compilers page
  
Ongoing work:

* [#117](https://github.com/fortran-lang/fortran-lang.org/issues/117): Adding a
  Benchmarks section, a new dedicated repository was created at
  https://github.com/fortran-lang/benchmarks and many details have been
  discussed in [issues](https://github.com/fortran-lang/benchmarks/issues) there.

[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues) if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the [contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/master/CONTRIBUTING.md) for how to get started.

## Fortran Standard Library

This month we've had an improvement to the `stdlib_ascii` module,
as well as addition of logging facilities.

* [#239](https://github.com/fortran-lang/stdlib/pull/239) (WIP): Implementation of the `stdlib_bitsets` module. It provides a bitset data type.
* [#238](https://github.com/fortran-lang/stdlib/pull/238): Improvements to the `stdlib_stats` module by adding explicit conversions.
* [#235](https://github.com/fortran-lang/stdlib/pull/235) (WIP): Improvements to the `stdlib_ascii` module
* [#228](https://github.com/fortran-lang/stdlib/pull/228): Implementation of the `stdlib_logger` module.
It provides a global logger instance for easy use in user applications, as well as a `logger_type` derived type
if multiple concurrent loggers are needed.
See the [logger specification](https://stdlib.fortran-lang.org/page/specs/stdlib_linalg.html)
to learn more.

Otherwise, ongoing discussions continue:

* [#225](https://github.com/fortran-lang/stdlib/issues/225): Name convention for derived types in stdlib
* [#224](https://github.com/fortran-lang/stdlib/issues/224): Handling and propagating errors inside stdlib
* [#221](https://github.com/fortran-lang/stdlib/issues/221): API for a bitset data type
* [#201](https://github.com/fortran-lang/stdlib/issues/201): API for file system operations

The candidate for file system operations to be included in stdlib is being developed by
[@MarDiehl](https://github.com/MarDiehl) and [@arjenmarkus](https://github.com/arjenmarkus)
in [this repository](https://github.com/MarDiehl/stdlib_os).
Please try it out and let us know how it works, if there are any issues, or if the API can be improved.

## Fortran Package Manager

This month has seen over a dozen additions and improvements to the Fortran implementation of fpm:

* [#185](https://github.com/fortran-lang/fpm/issues/185): Update CI workflow
* [#180](https://github.com/fortran-lang/fpm/issues/180): Recursive source discovery
* [#178](https://github.com/fortran-lang/fpm/issues/178): Add more example packages
* [#177](https://github.com/fortran-lang/fpm/issues/177): Allow selective testing of single suites and tests
* [#175](https://github.com/fortran-lang/fpm/issues/175): Updated formatting of Markdown documents
* [#174](https://github.com/fortran-lang/fpm/issues/174): Cache Haskell Stack build in CI
* [#171](https://github.com/fortran-lang/fpm/issues/171): Increase test coverage of fpm manifest
* [#170](https://github.com/fortran-lang/fpm/issues/170): Source parsing tests
* [#163](https://github.com/fortran-lang/fpm/issues/163): Use different strategy to fetch git dependencies
* [#162](https://github.com/fortran-lang/fpm/issues/162): Updated OS type identification
* [#160](https://github.com/fortran-lang/fpm/issues/160): Add contributing guidelines
(you can read them [here](https://github.com/fortran-lang/fpm/CONTRIBUTING.md))
* [#157](https://github.com/fortran-lang/fpm/issues/157): Implement reading of fpm.toml
* [#155](https://github.com/fortran-lang/fpm/issues/155): Internal dependencies and build backend


Work in progress:

* [#190](https://github.com/fortran-lang/fpm/issues/190) (WIP): Auto discovery of executables
* [#189](https://github.com/fortran-lang/fpm/issues/189) (WIP): Implement `fpm new`
* [#186](https://github.com/fortran-lang/fpm/issues/186) (WIP): Implement version string validation and comparison
* [#182](https://github.com/fortran-lang/fpm/issues/182) (WIP): CLI interface implementation

fpm is still in early development and we need as much help as we can get.
Here's how you can help today:

* Try to use it. Does it work? No? Let us know! Read the [fpm packaging guide](https://github.com/fortran-lang/fpm/blob/master/PACKAGING.md) to learn how to build your package with fpm.
* Browse the [open issues](https://github.com/fortran-lang/fpm/issues) and see if you can help implement any fixes or features.
* Adapt your Fortran package for fpm and submit it to the [Registry](https://github.com/fortran-lang/fpm-registry).
* Improve the documentation.

The short term goal of fpm is to make development and installation of Fortran packages with dependencies easier.
Its long term goal is to build a rich and decentralized ecosystem of Fortran packages and create a healthy
environment in which new open source Fortran projects are created and published with ease.

##  Fortran benchmarks

We created the [benchmarks repository](https://github.com/fortran-lang/benchmarks) with the goal to design and implement a comprehensive set of benchmarks.
The benchmarks will aim to compare the performance of various Fortran compilers, as well as the performance of canonical algorithms implemented in Fortran and different languages.
If you'd like to contribute in any way, be it the design, implementation, or testing of benchmarks, please join the ongoing discussion [here](https://github.com/fortran-lang/benchmarks/issues).

## Compilers

### GFortran

TODO

### Classic Flang

TODO @gklimowicz

### LLVM Flang

TODO @gklimowicz

### LFortran

TODO @certik

You can follow LFortran on Twitter for latest updates: [@lfortranorg](https://twitter.com/lfortranorg).

## Events

* We had our fourth Fortran Monthly call on September 25.
You can watch the recording below:

TODO update URL when uploaded
<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/fiAyhHkAKFc" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

As usual, subscribe to the [mailing list](https://groups.io/g/fortran-lang) and/or
join the [Discourse](https://fortran-lang.discourse.group) to stay tuned with the future meetings.

## Contributors

We thank everybody who contributed to fortran-lang in the past month by
commenting in any of these repositories:

* [fortran-lang/stdlib](https://github.com/fortran-lang/stdlib),
* [fortran-lang/fpm](https://github.com/fortran-lang/fpm),
* [fortran-lang/fpm-registry](https://github.com/fortran-lang/fpm-registry),
* [fortran-lang/fortran-lang.org](https://github.com/fortran-lang/fortran-lang.org),
* [fortran-lang/benchmarks](https://github.com/fortran-lang/benchmarks),
* [j3-fortran/fortran_proposals](https://github.com/j3-fortran/fortran_proposals):

<div id="gh-contributors" data-startdate="September 01 2020" data-enddate="September 30 2020" height="500px"></div>
