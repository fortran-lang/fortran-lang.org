---
layout: post
title: "Fortran newsletter: September 2020"
category: newsletter
author: Milan Curcic, Ondřej Čertík, and Gary Klimowicz
---

Welcome to the September 2020 edition of the monthly Fortran newsletter.
The newsletter comes out on the first calendar day of every month
and details Fortran news from the previous month.

<ul id="page-nav"></ul>

## fortran-lang.org

We continued the work on the Fortran-lang website, specifically:

* [#133](https://github.com/fortran-lang/fortran-lang.org/pull/133):
Listing fpm packages on the Packages page of the website
  
Ongoing work:

* [#117](https://github.com/fortran-lang/fortran-lang.org/issues/117): Adding a
  Benchmarks section, a new dedicated repository was created at
  https://github.com/fortran-lang/benchmarks and many details have been
  discussed in [issues](https://github.com/fortran-lang/benchmarks/issues) there

[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues) if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the [contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/master/CONTRIBUTING.md) for how to get started.

## Fortran Standard Library

There hasn't been new stdlib development in August, however ongoing work and discussions continue:

* [#227](https://github.com/fortran-lang/stdlib/issues/227): API proposal for logging facilities in stdlib
* [#225](https://github.com/fortran-lang/stdlib/issues/225): Name convention for derived types in stdlib
* [#224](https://github.com/fortran-lang/stdlib/issues/224): Handling and propagating errors inside stdlib
* [#221](https://github.com/fortran-lang/stdlib/issues/221): API for a bitset data type
* [#201](https://github.com/fortran-lang/stdlib/issues/201): API for file system operations

The candidate for file system operations to be included in stdlib is being developed by
[@MarDiehl](https://github.com/MarDiehl) and [@arjenmarkus](https://github.com/arjenmarkus)
in [this repository](https://github.com/MarDiehl/stdlib_os).
Please try it out and let us know how it works, if there are any issues, or if the API can be improved.

## Fortran Package Manager

Ongoing work in fpm:

* [#146](https://github.com/fortran-lang/fpm/issues/146) (WIP): 
Implementing internal dependencies and build backend in the Fortran fpm

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

### Classic Flang

TODO @gklimowicz

### LLVM Flang

TODO @gklimowicz

### LFortran

TODO @certik

## Events

* We had our fourth Fortran Monthly call on August 20.
You can watch the recording below:

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

<div id="gh-contributors" data-startdate="August 01 2020" data-enddate="August 31 2020" height="500px"></div>
