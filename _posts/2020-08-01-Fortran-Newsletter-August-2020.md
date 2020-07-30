---
layout: post
title: "Fortran newsletter: August 2020"
category: newsletter
author: Ondřej Čertík, Milan Curcic, Laurence Kedward, and Jérémie Vandenplas
---

Welcome to the August 2020 edition of the monthly Fortran newsletter.
The newsletter comes out on the first calendar day of every month
and details Fortran news from the previous month.

<ul id="page-nav"></ul>

## fortran-lang.org

Work has continued on the Fortran-lang website, including:

* [#116](https://github.com/fortran-lang/fortran-lang.org/pull/116): updates to the Quickstart tutorial on loop control and syntax

* [#120](https://github.com/fortran-lang/fortran-lang.org/pull/120): updated the
  [Book section](https://fortran-lang.org/learn/) with a comprehensive list of
  books about Fortran

* [#121](https://github.com/fortran-lang/fortran-lang.org/pull/121), [#122](https://github.com/fortran-lang/fortran-lang.org/pull/122), [#127](https://github.com/fortran-lang/fortran-lang.org/pull/127), [#128](https://github.com/fortran-lang/fortran-lang.org/pull/128): additional packages added to the Fortran-lang.org [packages](https://fortran-lang.org/packages) page
  
Ongoing work:

* [#117](https://github.com/fortran-lang/fortran-lang.org/issues/117): Adding a
  Benchmarks section, a new dedicated repository was created at
  https://github.com/fortran-lang/benchmarks and many details have been
  discussed in [issues](https://github.com/fortran-lang/benchmarks/issues) there



[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues) if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the [contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/master/CONTRIBUTING.md) for how to get started.


## Fortran Standard Library

What's new in the Fortran Standard Library:
[#223](https://github.com/fortran-lang/stdlib/pull/223): the structure of the Fortran Standard Library has been modified for clarity and ease of use. With these changes, both experimental and stable procedures will reside together in the same modules. The status of the procedures (experimental vs stable) are documented in the code, in the specs, and in the [website](https://stdlib.fortran-lang.org/)



## Fortran Package Manager

What's new in fpm:


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

* [FortranCon 2020](https://tcevents.chem.uzh.ch/event/12) was held July 2 - 4.
with many interesting talks.
See the talk schedule
[here](https://tcevents.chem.uzh.ch/event/12/timetable/#20200702.detailed).
FIXME: is recordings available?

* We had our second Fortran Monthly call on July 16.
You can read watch the recording below:

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/_ojLReFvjbs" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

As usual, subscribe to the [mailing list](https://groups.io/g/fortran-lang) and/or
join the [Discourse](https://fortran-lang.discourse.group) to stay tuned with the future meetings.


## Contributors

We thank everybody who contributed to fortran-lang in the past month by
commenting in any of the four repositories:

* [fortran-lang/stdlib](https://github.com/fortran-lang/stdlib),
* [fortran-lang/fpm](https://github.com/fortran-lang/fpm),
* [fortran-lang/fortran-lang.org](https://github.com/fortran-lang/fortran-lang.org),
* [j3-fortran/fortran_proposals](https://github.com/j3-fortran/fortran_proposals):



<div id="gh-contributors" data-startdate="July 01 2020" data-enddate="July 30 2020" height="500px"></div>
