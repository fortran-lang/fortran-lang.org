---
layout: post
title: "Fortran newsletter: June 2020"
category: newsletter
author: Jeremie Vandenplas, Brad Richardson, and Milan Curcic
---

```fortran
print *, 'Hello, World!'
```

Welcome to this monthly Fortran newsletter.
It will come out on the first calendar day of every month,
detailing Fortran news from the previous month.

* [This website](#this-website)
* [Standard Library](#standard-library)
* [Package Manager](#package-manager)
* [Events](#events)

## This website

If you came to this newsletter from elsewhere, welcome to the new Fortran website.
We built this site mid-April and hope for it to be _the_ home of Fortran on the internet,
which traditionally there hasn't been any to date.
Look around and [let us know](https://github.com/fortran-lang/fortran-lang.github.io/issues)
if you have any suggestions for improvement.
Specifically, [Learn](/learn) and [Packages](/packages) are the pages that
we'll be focusing on in the coming months.
Please help us make them better!

* [#90](https://github.com/fortran-lang/fortran-lang.org/pull/90)
WIP: Mini-book on building programs

## Fortran Standard Library

Here's what's new in Fortran Standard Library:

* [#191](https://github.com/fortran-lang/stdlib/pull/191)
WIP: Function for computing Pearson correlations among elements of
an array in the `stdlib_experimental_stats` module.

* [#189](https://github.com/fortran-lang/stdlib/pull/189)
WIP: Procedures for sparse matrices operations. Ongoing discussion on the API can be found
[here](https://github.com/fortran-lang/stdlib/wiki/Stdlib-Sparse-matrix-API)

* [#183](https://github.com/fortran-lang/stdlib/pull/183)
Automatic API-doc generation and deployment of this [stdlib website](https://stdlib.fortran-lang.org)

* [#170](https://github.com/fortran-lang/stdlib/pull/170)
Addition of the new functions `diag`, `eye`, and `trace` functions to make working with
matrices easier.
Read the full specifications [here](https://stdlib.fortran-lang.org/page/specs/stdlib_experimental_linalg.html)

## Package Manager

In this past month support for dependencies between packages has been added. You
can specify either a path to another folder on your machine with an fpm package,
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

## Events

* [FortranCon 2020](https://tcevents.chem.uzh.ch/event/12) will take place on July 2-4 in Zurich, Switzerland.
Virtual participation is enabled for both attendees and speakers.
Registration is free and due by June 1, 2020.
* J3/WG5 joint meeting, originally slated for October 12-16 in Las Vegas, Nevada, has been [cancelled](https://mailman.j3-fortran.org/pipermail/j3/2020-May/012034.html).
However, the work on proposals for the Fortran Standard doesn't stop.
You can submit a proposal for the Standards committee [here](https://github.com/j3-fortran/fortran_proposals).
For reference, you can read the [notes from the February meeting](/newsletter/2020/02/28/J3-february-meeting).
