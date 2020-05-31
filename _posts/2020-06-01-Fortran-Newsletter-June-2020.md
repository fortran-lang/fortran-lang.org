---
layout: post
title: "Fortran newsletter: June 2020"
category: newsletter
author: Jeremie Vandenplas, Brad Richardson, and Milan Curcic
---

Welcome to the June 2020 edition of the monthly Fortran newsletter.
The newsletter comes out on the first calendar day of every month
and details Fortran news from the previous month.

* [fortran-lang.org](#fortran-lang.org)
* [Fortran Discourse](#fortran-discourse)
* [Standard Library](#fortran-standard-library)
* [Package Manager](#package-manager)
* [Events](#events)

## fortran-lang.org

The Fortran website has been up since mid-April, and we've already got great
feedback from the community.
In the past month we've updated the [Compilers](/compilers) page which is now
comprehensive and includes all major open source and commercial compilers.
The [Learn](/learn) page has also seen significant updates—it's been
reorganized for easier navigation and currently features a quickstart tutorial,
Fortran books, and other online resources.

If you haven't yet, please explore the website and [let us know](https://github.com/fortran-lang/fortran-lang.org/issues)
if you have any suggestions for improvement.
Specifically, we'll be focusing on the [Learn](/learn) page and its mini-books
in the coming months.
Please help us make them better!

Here are some specific items that we worked on:

* [#90](https://github.com/fortran-lang/fortran-lang.org/pull/90)
WIP: Mini-book on building programs
* [#83](https://github.com/fortran-lang/fortran-lang.org/pull/83)
Improving the structure and navigation of the [Learn](/learn) pages
* [#46](https://github.com/fortran-lang/fortran-lang.org/pull/46)
Build website previews from pull requests

## Fortran Discourse

On May 4 we launched the [Fortran Discourse](https://fortran-lang.discourse.group), an online discussion board
for anything and everything Fortran related.
You can use it discuss the Fortran language, ask for help, announce events and/or personal projects, or just lurk
around. 
There are already quite a few interesting discussions going on.
Join us!

## Fortran Standard Library

Here's what's new in the Fortran Standard Library:

* [#191](https://github.com/fortran-lang/stdlib/pull/191)
WIP: Function for computing Pearson correlations among elements of
an array in the `stdlib_experimental_stats` module
* [#189](https://github.com/fortran-lang/stdlib/pull/189)
WIP: Procedures for sparse matrices operations. Ongoing discussion on the API can be found
[here](https://github.com/fortran-lang/stdlib/wiki/Stdlib-Sparse-matrix-API).
* [#183](https://github.com/fortran-lang/stdlib/pull/183)
Automatic API-doc generation and deployment of this [stdlib website](https://stdlib.fortran-lang.org)
* [#170](https://github.com/fortran-lang/stdlib/pull/170)
Addition of the new functions `diag`, `eye`, and `trace` functions to make working with
matrices easier.
Read the full specifications [here](https://stdlib.fortran-lang.org/page/specs/stdlib_experimental_linalg.html).

## Package Manager

In this past month support for dependencies between packages has been added
to the [Fortran Package Manager (fpm)](https://github.com/fortran-lang/fpm).
You can specify either a path to another folder on your machine with an fpm package,
or a git repository (and optionally a specific branch, tag or commit) that
contains the package. fpm will then take care of fetching the dependency for you
(if necessary) and any packages it depends on, and compiling and linking it into
your project. Check out an example [hello world package](https://gitlab.com/everythingfunctional/hello_fpm)
that uses this functionality.

fpm is still in very early development, and we need as much help as we can get.
Here's how you can help today:

* Try to use it. Does it work? No? Let us know! Read the [fpm packaging guide](https://github.com/fortran-lang/fpm/blob/master/PACKAGING.md) to learn how to build your package with fpm.
* Browse the [open issues](https://github.com/fortran-lang/fpm/issues) and see if you can help implement any fixes or features.
* Adapt your Fortran package for fpm.
* Improve the documentation.

The short term goal of fpm is to make development and installation of Fortran packages with dependencies easier.
Its long term goal is to build a rich and decentralized ecosystem of Fortran packages and create a healthy
environment in which new open source Fortran projects are created and published with ease.

Specific items that are new this month:

* [#82](https://github.com/fortran-lang/fpm/pull/82)
You can now add remote git repositories as Fortran dependencies to your project.
* [#73](https://github.com/fortran-lang/fpm/pull/73)
Improved output messages for the user

## Events

* We hosted the very first Fortran Monthly call on May 14.
The turnout was astonishing--over 23 people joined.
You can read the notes from the call [here](https://fortran-lang.discourse.group/t/fortran-monthly-call-may-2020).
We'll have another one this month.
Subscribe to the [mailing list](https://groups.io/g/fortran-lang) and/or
join the [Discourse](https://fortran-lang.discourse.group) to stay tuned.
* [FortranCon 2020](https://tcevents.chem.uzh.ch/event/12) will take place on July 2-4 in Zurich, Switzerland.
Virtual participation is enabled for both attendees and speakers.
Registration is free and due by June 1, 2020.
There are quite a few submissions from the fortran-lang community:
A talk on [stdlib](https://github.com/fortran-lang/talks/tree/master/FortranCon2020-stdlib) by Jeremie Vandenplas,
one about the [Fortran Package Manager (fpm)](https://github.com/fortran-lang/talks/tree/master/FortranCon2020-fpm) by Brad Richardson,
a talk on [LFortran compiler](https://gitlab.com/lfortran/talks/fortrancon-2020-talk) by Ondřej Čertík,
as well as one about [building the Fortran community](https://github.com/fortran-lang/talks/tree/master/FortranCon2020-community)
by Milan Curcic.
* J3/WG5 joint meeting, originally slated for October 12-16 in Las Vegas, Nevada, has been [cancelled](https://mailman.j3-fortran.org/pipermail/j3/2020-May/012034.html).
However, the work on proposals for the Fortran Standard doesn't stop.
You can submit a proposal for the Standards committee [here](https://github.com/j3-fortran/fortran_proposals).
For reference, you can read the [notes from the February meeting](/newsletter/2020/02/28/J3-february-meeting).
