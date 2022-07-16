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
### &#9755;[General State]({{site.baseurl}}/learn/intrinsics/STATE_index) 
#### General and miscellaneous intrinsics on state of variables and I/O
### &#9755;[Character]({{site.baseurl}}/learn/intrinsics/CHARACTER_index) 
#### basic procedures specifically for manipulating _character_ variables
### &#9755;[System Environment]({{site.baseurl}}/learn/intrinsics/SYSTEM_index) 
#### accessing external system information such as environmental variables, command line arguments, date and timing data ...
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

   - [review by procedure ](http://www.urbanjost.altervista.org/SUPPLEMENTS/slidy_byprocedure.html)

   - [review by header ](http://www.urbanjost.altervista.org/SUPPLEMENTS/slidy_byheader.html)

   - [fman(1)](http://www.urbanjost.altervista.org/SUPPLEMENTS/fman.f90) A self-contained Fortran program that
     lets you view the non-graphical plain ASCII portions of the
     documentation from a terminal interface. Compile the program and
     enter "./fman --help" for directions. 

   - [man pages](http://www.urbanjost.altervista.org/SUPPLEMENTS/fortran.tgz) A gzipped tar(1) file containing
     early versions of man-pages derived from the markdown documents.

     Typical installation on a Linux platform as an administrator ( but it varies) :
```bash
     # as the administrator
     cd /usr/share
     tar xvfz /tmp/fortran.tgz
     cd man
     mandb -c
```
     then anyone on that plaform can enter commands like 
```bash
     man sinh.3fortran     # specifically show Fortran sinh(3) documentation
     man -k . -s 3fortran  # list all fortran pages
     man -s 3fortran --regex '.*' |col -b # show all Fortran intrinsics
```
    See man(1) (ie. enter "man man") for more information.
    
    If you can only install the pages on your own ID, try

```bash
     # as a user, placing the files in ~/man:
     cd 
     tar xvfz /tmp/fortran.tgz
     cd man
     mandb -c
     export MANPATH="$MANPATH:$HOME/man"
     export MANWIDTH=80
```

     Still debating whether having to keep the document limited to ANSI
     characters is worth-while so these formats can be generated, and
     still having issues converting the markdown to the proper formats.

## Text Content Copyrights

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
the MIT license, and should indicate this by placing MIT on the
specially-formatted last line. For example, change
```text
###### fortran-lang intrinsic descriptions
```
to
```text
###### fortran-lang intrinsic descriptions (Iicense: MIT) @urbanjost
```

###### Written in [Markdown](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet) [kramdown](https://kramdown.gettalong.org/syntax.html)



````{toctree}
:hidden:
ABS <ABS>
ICHAR <ICHAR>
ACHAR <ACHAR>
IEOR <IEOR>
ACOSH <ACOSH>
IMAGE_INDEX <IMAGE_INDEX>
ACOS <ACOS>
ADJUSTL <ADJUSTL>
INDEX <INDEX>
ADJUSTR <ADJUSTR>
INT <INT>
AIMAG <AIMAG>
IOR <IOR>
AINT <AINT>
IPARITY <IPARITY>
ALL <ALL>
IS_CONTIGUOUS <IS_CONTIGUOUS>
ALLOCATED <ALLOCATED>
ISHFTC <ISHFTC>
ANINT <ANINT>
ISHFT <ISHFT>
ANY <ANY>
IS_IOSTAT_END <IS_IOSTAT_END>
ARRAY_index <ARRAY_index>
IS_IOSTAT_EOR <IS_IOSTAT_EOR>
ASINH <ASINH>
KIND <KIND>
ASIN <ASIN>
LBOUND <LBOUND>
ASSOCIATED <ASSOCIATED>
LEADZ <LEADZ>
ATAN2 <ATAN2>
LEN <LEN>
ATANH <ATANH>
LEN_TRIM <LEN_TRIM>
ATAN <ATAN>
LGE <LGE>
ATOMIC_ADD <ATOMIC_ADD>
LGT <LGT>
ATOMIC_AND <ATOMIC_AND>
LLE <LLE>
ATOMIC_CAS <ATOMIC_CAS>
LLT <LLT>
ATOMIC_DEFINE <ATOMIC_DEFINE>
LOG10 <LOG10>
ATOMIC_FETCH_ADD <ATOMIC_FETCH_ADD>
LOG_GAMMA <LOG_GAMMA>
ATOMIC_FETCH_AND <ATOMIC_FETCH_AND>
LOGICAL <LOGICAL>
ATOMIC_FETCH_OR <ATOMIC_FETCH_OR>
LOG <LOG>
ATOMIC_FETCH_XOR <ATOMIC_FETCH_XOR>
MASKL <MASKL>
ATOMIC_OR <ATOMIC_OR>
MASKR <MASKR>
ATOMIC_REF <ATOMIC_REF>
MATH_index <MATH_index>
ATOMIC_XOR <ATOMIC_XOR>
MATMUL <MATMUL>
BESSEL_J0 <BESSEL_J0>
MAXEXPONENT <MAXEXPONENT>
BESSEL_J1 <BESSEL_J1>
MAXLOC <MAXLOC>
BESSEL_JN <BESSEL_JN>
MAX <MAX>
BESSEL_Y0 <BESSEL_Y0>
MAXVAL <MAXVAL>
BESSEL_Y1 <BESSEL_Y1>
MERGE_BITS <MERGE_BITS>
BESSEL_YN <BESSEL_YN>
MERGE <MERGE>
BGE <BGE>
MINEXPONENT <MINEXPONENT>
BGT <BGT>
MINLOC <MINLOC>
BIT_index <BIT_index>
MIN <MIN>
BIT_SIZE <BIT_SIZE>
MINVAL <MINVAL>
BLE <BLE>
MODEL_index <MODEL_index>
BLT <BLT>
MOD <MOD>
BTEST <BTEST>
MODULO <MODULO>
C_ASSOCIATED <C_ASSOCIATED>
MOVE_ALLOC <MOVE_ALLOC>
CEILING <CEILING>
MVBITS <MVBITS>
C_F_POINTER <C_F_POINTER>
NEAREST <NEAREST>
C_F_PROCPOINTER <C_F_PROCPOINTER>
NEW_LINE <NEW_LINE>
C_FUNLOC <C_FUNLOC>
NINT <NINT>
CHARACTER_index <CHARACTER_index>
NORM2 <NORM2>
CHAR <CHAR>
NOT <NOT>
C_index <C_index>
NULL <NULL>
C_LOC <C_LOC>
NUMERIC_index <NUMERIC_index>
CMPLX <CMPLX>
NUM_IMAGES <NUM_IMAGES>
CO_BROADCAST <CO_BROADCAST>
PACK <PACK>
CO_LBOUND <CO_LBOUND>
PARALLEL_index <PARALLEL_index>
CO_MAX <CO_MAX>
PARITY <PARITY>
CO_MIN <CO_MIN>
POPCNT <POPCNT>
COMMAND_ARGUMENT_COUNT <COMMAND_ARGUMENT_COUNT>
POPPAR <POPPAR>
COMPILER_index <COMPILER_index>
PRECISION <PRECISION>
COMPILER_OPTIONS <COMPILER_OPTIONS>
PRESENT <PRESENT>
COMPILER_VERSION <COMPILER_VERSION>
PRODUCT <PRODUCT>
CONJG <CONJG>
RADIX <RADIX>
CO_REDUCE <CO_REDUCE>
RANDOM_NUMBER <RANDOM_NUMBER>
COSH <COSH>
RANDOM_SEED <RANDOM_SEED>
COS <COS>
RANGE <RANGE>
CO_SUM <CO_SUM>
RANK <RANK>
CO_UBOUND <CO_UBOUND>
REAL <REAL>
COUNT <COUNT>
REPEAT <REPEAT>
CPU_TIME <CPU_TIME>
RESHAPE <RESHAPE>
CSHIFT <CSHIFT>
RRSPACING <RRSPACING>
C_SIZEOF <C_SIZEOF>
SAME_TYPE_AS <SAME_TYPE_AS>
DATE_AND_TIME <DATE_AND_TIME>
SCALE <SCALE>
DBLE <DBLE>
SCAN <SCAN>
DIGITS <DIGITS>
SELECTED_CHAR_KIND <SELECTED_CHAR_KIND>
DIM <DIM>
SELECTED_INT_KIND <SELECTED_INT_KIND>
DOT_PRODUCT <DOT_PRODUCT>
SELECTED_REAL_KIND <SELECTED_REAL_KIND>
DPROD <DPROD>
SET_EXPONENT <SET_EXPONENT>
DSHIFTL <DSHIFTL>
SHAPE <SHAPE>
DSHIFTR <DSHIFTR>
SHIFTA <SHIFTA>
EOSHIFT <EOSHIFT>
SHIFTL <SHIFTL>
EPSILON <EPSILON>
SHIFTR <SHIFTR>
ERFC <ERFC>
SIGN <SIGN>
ERFC_SCALED <ERFC_SCALED>
SINH <SINH>
ERF <ERF>
SIN <SIN>
EVENT_QUERY <EVENT_QUERY>
SIZE <SIZE>
EXECUTE_COMMAND_LINE <EXECUTE_COMMAND_LINE>
SPACING <SPACING>
EXP <EXP>
SPREAD <SPREAD>
EXPONENT <EXPONENT>
SQRT <SQRT>
EXTENDS_TYPE_OF <EXTENDS_TYPE_OF>
STATE_index <STATE_index>
FINDLOC <FINDLOC>
STORAGE_SIZE <STORAGE_SIZE>
FLOOR <FLOOR>
SUM <SUM>
FRACTION <FRACTION>
SYSTEM_CLOCK <SYSTEM_CLOCK>
GAMMA <GAMMA>
SYSTEM_index <SYSTEM_index>
GET_COMMAND_ARGUMENT <GET_COMMAND_ARGUMENT>
TANH <TANH>
GET_COMMAND <GET_COMMAND>
TAN <TAN>
GET_ENVIRONMENT_VARIABLE <GET_ENVIRONMENT_VARIABLE>
THIS_IMAGE <THIS_IMAGE>
GNU_Free_Documentation_License <GNU_Free_Documentation_License>
TINY <TINY>
HUGE <HUGE>
TRAILZ <TRAILZ>
HYPOT <HYPOT>
TRANSFER <TRANSFER>
IACHAR <IACHAR>
TRANSFORM_index <TRANSFORM_index>
IALL <IALL>
TRANSPOSE <TRANSPOSE>
IAND <IAND>
TRIM <TRIM>
IANY <IANY>
TYPE_index <TYPE_index>
IBCLR <IBCLR>
UBOUND <UBOUND>
IBITS <IBITS>
UNPACK <UNPACK>
IBSET <IBSET>
VERIFY <VERIFY>
````