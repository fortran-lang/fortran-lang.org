---
layout: book
title: Fortran Intrinsics
permalink: /learn/intrinsics
---
This is a collection of extended descriptions of the Fortran intrinsics
based on the reference document
"[Current F2018 Working Document as of April 2018](http://isotc.iso.org/livelink/livelink?func=ll&objId=19442438&objAction=Open)".
Vendor-specific extensions are not included.

### &#9755;[Array Operations]({{site.baseurl}}/learn/intrinsics/ARRAY_index) 
#### Properties and attributes of arrays
### &#9755;[Mathematics]({{site.baseurl}}/learn/intrinsics/MATH_index) 
#### General mathematical functions
### &#9755;[Type and Kind]({{site.baseurl}}/learn/intrinsics/TYPE_index) 
### &#9755;[Numeric]({{site.baseurl}}/learn/intrinsics/NUMERIC_index) 
#### Manipulation and properties of numeric values
### &#9755;[Transformational]({{site.baseurl}}/learn/intrinsics/TRANSFORM_index) 
#### Matrix multiplication, Dot product, array shifts,
### &#9755;[General Intrinsics]({{site.baseurl}}/learn/intrinsics/GENERAL_index) 
#### General and miscellaneous intrinsics
### &#9755;[Character]({{site.baseurl}}/learn/intrinsics/CHARACTER_index) 
#### basic procedures specifically for manipulating _character_ variables
### &#9755;[System Environment]({{site.baseurl}}/learn/intrinsics/SYSTEM_index) 
#### accessing external system information
### &#9755;[C Interface]({{site.baseurl}}/learn/intrinsics/C_index) 
#### procedures useful for binding to C interfaces
### &#9755;[Bit-level]({{site.baseurl}}/learn/intrinsics/BIT_index) 
#### bit-level manipulation and inquiry of values0
### &#9755;[Parallel Programming]({{site.baseurl}}/learn/intrinsics/PARALLEL_index) 
#### Parallel programming using co-arrays and co-indexing
### &#9755;[Numeric Model]({{site.baseurl}}/learn/intrinsics/MODEL_index) 
#### numeric compiler-specific numeric model information
### &#9755;[Compiler Information]({{site.baseurl}}/learn/intrinsics/COMPILER_index) 
#### information about compiler version and compiler options used for building

## Overview
The standard documents and most vendor-supplied descriptions of
the intrinsics are often very brief and concise to the point of the
functionality of the intrinsics being obscure, particularly to someone
unfamiliar with  the procedure.

By describing the procedures here 
   * in greater detail 
   * with a working example
   * providing links to additional resources
     (including additional documents at fortran-lang.org and related
     discussions in Fortran Discourse)

these documents strive to be a valuable asset for Fortran programmers.

This is a community-driven resource and everyone is encouraged to contribute 
to the documents. For contribution guidelines see
[MINIBOOKS](https://github.com/fortran-lang/fortran-lang.org/blob/master/MINIBOOKS.md)
and the following Copyright guidelines.

## See Also
   - The [Fortran stdlib](https://stdlib.fortran-lang.org/) project
   - [fpm(1)](https://fortran-lang.org/packages/fpm) packages, many of which are general-purpose libraries/modules

## Experimental

   - [fman(1)](http://www.urbanjost.altervista.org/SUPPLEMENTS/fman.f90) A self-contained Fortran program that
     lets you view the non-graphical plain ASCII portions of the
     documentation from a terminal interface. Compile the program and
     enter "./fman --help" for directions. 

   - [man pages](http://www.urbanjost.altervista.org/SUPPLEMENTS/fortran.tgz) A gzipped tar(1) file containing
     early versions of man-pages derived from the markdown documents.

     Typical installation on a Linux platform, but it varies
```bash
     # as the administrator
     cd /usr/share
     tar xvfz /tmp/fortran.tgz
     cd man
     mand -c
```
     then anyone on that plaform can enter commands like (see man(1))
```bash
     # as a user, placing the files in ~/man:
     cd 
     tar xvfz /tmp/fortran.tgz
     cd man
     mand -c
     export MANPATH="$MANPATH:$HOME/man"
     export MANWIDTH=80
```
```bash
     man sinh.3fortran     # specifically show Fortran sinh(3) documentation
     man -k . -s 3fortran  # list all fortran pages
```

     Still debating whether having to keep the document limited to ANSI
     characters is worth-while so these formats can be generated, and
     still having issues converting the markdown to the proper formats.

## Text Content Copyrights

Each document file should indicate any specific license under which the
document falls.

Many of the documents presented here are modified versions of man-pages from the
[Fortran Wiki](https://fortranwiki.org)
and as such are available under the terms of the GNU
Free Documentation License [__GFDL__](GNU_Free_Documentation_License.md)
with no invariant sections, front-cover texts, or back-cover texts.

If you contribute to this site by modifying the files marked as GFDL,
you thereby agree to license the contributed material to the public
under the GFDL (version 1.2 or any later version published by the Free
Software Foundation, with no invariant sections, front-cover texts,
or back-cover texts).

If you contribute new material you thereby agree to release it under
the MIT license.

###### Written in [Markdown](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet) [kramdown](https://kramdown.gettalong.org/syntax.html)
