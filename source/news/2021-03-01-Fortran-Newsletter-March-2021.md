---
layout: post
title: "Fortran newsletter: March 2021"
category: newsletter
date: 2021-03-01
author: Milan Curcic, Laurence Kedward, Ondřej Čertík
---

Welcome to the March 2021 edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

# fortran-lang.org

This month we've had several updates to the website:

* [#191](https://github.com/fortran-lang/fortran-lang.org/pull/191):
  Fix author/maintainer output in fpm registry
* [#193](https://github.com/fortran-lang/fortran-lang.org/pull/193):
  Rename all instances of fortran-lang.github.io to fortran-lang.org
* [#196](https://github.com/fortran-lang/fortran-lang.org/pull/196):
  Update package index
* [#199](https://github.com/fortran-lang/fortran-lang.org/pull/199):
  Fix broken link for LLVM flang
* [#205](https://github.com/fortran-lang/fortran-lang.org/pull/205):
  Add more electronic structure and atomistic simulation packages
* [#206](https://github.com/fortran-lang/fortran-lang.org/pull/206):
  Add books to learning section
* [#208](https://github.com/fortran-lang/fortran-lang.org/pull/208):
  Fix package information

Ongoing work:

* [#201](https://github.com/fortran-lang/fortran-lang.org/pull/201) (WIP):
  Internationalization for fortran-lang
* [#207](https://github.com/fortran-lang/fortran-lang.org/issues/207) (WIP):
  Correct subtitle of setting up your os

[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues) if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the [contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/CONTRIBUTING.md) for how to get started.

## Fortran Standard Library

Here's what's new in `stdlib`:

* [#271](https://github.com/fortran-lang/stdlib/pull/271):
  Probability Distribution and Statistical Functions--PRNG Module
* [#304](https://github.com/fortran-lang/stdlib/pull/304):
  Add supported compilers MinGW 8, 9, 10
* [#310](https://github.com/fortran-lang/stdlib/pull/310):
  Extend `stdlib_ascii` module for handling character variables
* [#324](https://github.com/fortran-lang/stdlib/pull/324):
  Install setuptools for MinGW builds

Work in progress:

* [#189](https://github.com/fortran-lang/stdlib/pull/189) (WIP):
  Initial implementation of sparse matrices.
* [#272](https://github.com/fortran-lang/stdlib/pull/272) (WIP),
  [#273](https://github.com/fortran-lang/stdlib/pull/273) (WIP),
  [#276](https://github.com/fortran-lang/stdlib/pull/276) (WIP),
  [#278](https://github.com/fortran-lang/stdlib/pull/278) (WIP):
  Implementation of the `stdlib_stats_distribution` modules.
  It provides probability distribution and statistical functions.
* [#311](https://github.com/fortran-lang/stdlib/pull/311) (WIP):
  Implementation of a module for handling lists of strings
* [#320](https://github.com/fortran-lang/stdlib/pull/320) (WIP):
  Implement non-fancy functional string type
* [#313](https://github.com/fortran-lang/stdlib/pull/313) (WIP):
  Legendre polynomials and Gaussian quadrature

Please help improve stdlib by testing and reviewing these pull requests!

The candidate for file system operations to be included in stdlib is being developed by
[@MarDiehl](https://github.com/MarDiehl) and [@arjenmarkus](https://github.com/arjenmarkus)
in [this repository](https://github.com/MarDiehl/stdlib_os).
Please try it out and let us know how it works, if there are any issues, or if the API can be improved.

## Fortran Package Manager

Here's what's new in `fpm`:

* [#316](https://github.com/fortran-lang/fpm/pull/316):
  Update subcommand "new" to reflect the addition of support for the example/ directory
* [#345](https://github.com/fortran-lang/fpm/pull/345):
  Fpm backend with dynamic openmp scheduling
* [#346](https://github.com/fortran-lang/fpm/pull/346):
  Include root dir in path to default example setup
* [#349](https://github.com/fortran-lang/fpm/pull/349):
  Suggest to move the fpm version in the boostrapping process
* [#372](https://github.com/fortran-lang/fpm/pull/372):
  Unify release mode calling convention

Work in progress:

* [First beta release](https://github.com/fortran-lang/fpm/milestone/1) (WIP):
  First feature-complete release of the Fortran implementation.
* [#230](https://github.com/fortran-lang/fpm/pull/230),
  [#261](https://github.com/fortran-lang/fpm/pull/261) (WIP):
  Document the specification of the fpm CLI.
* [#352](https://github.com/fortran-lang/fpm/pull/352) (WIP):
  Hacky fix for the help test
* [#357](https://github.com/fortran-lang/fpm/pull/357) (WIP):
  Install script for Fortran fpm
* [#364](https://github.com/fortran-lang/fpm/pull/364) (WIP):
  Plugin alpha version
* [#369](https://github.com/fortran-lang/fpm/pull/369) (WIP):
  Separate build targets from model structure
* [#370](https://github.com/fortran-lang/fpm/pull/370) (WIP):
  Update run subcommand

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

* The LFortran team is excited to announce that LFortran is now a [NumFOCUS sponsored project](https://numfocus.org/project/lfortran).
Please consider donating to LFortran to accelerate its development.
* 4 people contributed code in the last month:
    [Gagandeep Singh](https://github.com/czgdp1807),
    [Dominic Poerio](https://dompoer.io/),
    [Rohit Goswami](https://rgoswami.me/),
    [Ondřej Čertík](https://ondrejcertik.com/).
* Recent Merge Requests highlights:
    * Complex type support (thanks to [Gagandeep Singh](https://github.com/czgdp1807)):
        [!654](https://gitlab.com/lfortran/lfortran/-/merge_requests/654),
        [!657](https://gitlab.com/lfortran/lfortran/-/merge_requests/657),
        [!658](https://gitlab.com/lfortran/lfortran/-/merge_requests/658),
        [!660](https://gitlab.com/lfortran/lfortran/-/merge_requests/660),
        [!663](https://gitlab.com/lfortran/lfortran/-/merge_requests/663),
        [!664](https://gitlab.com/lfortran/lfortran/-/merge_requests/664).
        [!672](https://gitlab.com/lfortran/lfortran/-/merge_requests/672).
    * Multiline REPL (thanks to [Dominic Poerio](https://dompoer.io/)):
        [!655](https://gitlab.com/lfortran/lfortran/-/merge_requests/655),
        [!662](https://gitlab.com/lfortran/lfortran/-/merge_requests/662),
        [!670](https://gitlab.com/lfortran/lfortran/-/merge_requests/670),
        [!674](https://gitlab.com/lfortran/lfortran/-/merge_requests/674).
    * Initial support for runtime math functions:
        [!667](https://gitlab.com/lfortran/lfortran/-/merge_requests/667),
        [!673](https://gitlab.com/lfortran/lfortran/-/merge_requests/673),
    * [!648](https://gitlab.com/lfortran/lfortran/-/merge_requests/648): Implement --show-stacktrace
    * [!666](https://gitlab.com/lfortran/lfortran/-/merge_requests/666): Refactor
      ImplicitCast nodes handling
    * [!665](https://gitlab.com/lfortran/lfortran/-/merge_requests/665): Fixed
      floating point printing



## Events

* We had our 9th Fortran Monthly call on February 25.
You can watch the recording below:

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/-Hkm7_iO1wk" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

* This year Fortran-lang applied as a mentor organization for [Google Summer of Code](https://summerofcode.withgoogle.com/).
Accepted mentor organizations will be announced on March 9.
If you're a student, or know students who are [eligible to participate](https://developers.google.com/open-source/gsoc/faq#what_are_the_eligibility_requirements_for_participation), and you'd like to help build the Fortran ecosystem please reach out and let us know.

* The 223rd meeting of the US Fortran Standards Committee is held virtually from
  February 22 to March 2 (Monday and Tuesday only).
  Main topics of dicussion are the planned changes for the Fortran 202X revision
  of the Standard:

    * [List](https://j3-fortran.org/doc/meeting/223) of all submitted papers
    * [Summary](https://github.com/j3-fortran/fortran_proposals/issues/199) of which papers were discussed each day and voting results

  If you have ideas for new improvements to the language, please propose them
  [here](https://github.com/j3-fortran/fortran_proposals).

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
* [fortran-lang/fortran-lang.org](https://github.com/fortran-lang/fortran-lang.org)
* [fortran-lang/benchmarks](https://github.com/fortran-lang/benchmarks)
* [j3-fortran/fortran\_proposals](https://github.com/j3-fortran/fortran_proposals)

<div id="gh-contributors" data-startdate="February 01 2021" data-enddate="March 31 2021" height="500px"></div>
