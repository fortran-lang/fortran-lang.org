---
layout: book
title: Parallel Programming
permalink: /learn/intrinsics/PARALLEL_index
---
### These routines support parallel programming using co_arrays and co_indexed arrays.

|----------------------------||||-----------------------------------------------------------------------------||---------------------------------------------------------------|
| category                   |||| page                                                                        || description                                                   |
|----------------------------||||-----------------------------------------------------------------------------||---------------------------------------------------------------|
| *COLLECTIVE*               |||| [__co\_broadcast__]({{site.baseurl}}/learn/intrinsics/CO_BROADCAST)         || &#9679; Copy a value to all images the current set of images  |
| *COLLECTIVE*               |||| [__co\_lbound__]({{site.baseurl}}/learn/intrinsics/CO_LBOUND)               || &#9679; Lower codimension bounds of an array                  |
| *COLLECTIVE*               |||| [__co\_max__]({{site.baseurl}}/learn/intrinsics/CO_MAX)                     || &#9679; Maximal value on the current set of images            |
| *COLLECTIVE*               |||| [__co\_min__]({{site.baseurl}}/learn/intrinsics/CO_MIN)                     || &#9679; Minimal value on the current set of images            |
| *COLLECTIVE*               |||| [__co\_reduce__]({{site.baseurl}}/learn/intrinsics/CO_REDUCE)               || &#9679; Reduction of values on the current set of images      |
| *COLLECTIVE*               |||| [__co\_sum__]({{site.baseurl}}/learn/intrinsics/CO_SUM)                     || &#9679; Sum of values on the current set of images            |
| *COLLECTIVE*               |||| [__co\_ubound__]({{site.baseurl}}/learn/intrinsics/CO_UBOUND)               || &#9679; Upper codimension bounds of an array                  |
| *COLLECTIVE*               |||| [__event\_query__]({{site.baseurl}}/learn/intrinsics/EVENT_QUERY)           || &#9679; Query whether a coarray event has occurred            |
| *COLLECTIVE*               |||| [__image\_index__]({{site.baseurl}}/learn/intrinsics/IMAGE_INDEX)           || &#9679; Cosubscript to image index conversion                 |
| *COLLECTIVE*               |||| [__num\_images__]({{site.baseurl}}/learn/intrinsics/NUM_IMAGES)             || &#9679; Number of images                                      |
| *COLLECTIVE*               |||| [__this\_image__]({{site.baseurl}}/learn/intrinsics/THIS_IMAGE)             || &#9679; Cosubscript index of this image                       |
|----------------------------||||-----------------------------------------------------------------------------||---------------------------------------------------------------|
| *ATOMIC:BIT\_MANIPULATION* |||| [__atomic\_and__]({{site.baseurl}}/learn/intrinsics/ATOMIC_AND)             || &#9679; Atomic bitwise AND operation                          |
| *ATOMIC:BIT\_MANIPULATION* |||| [__atomic\_fetch\_and__]({{site.baseurl}}/learn/intrinsics/ATOMIC_FETCH_AND)|| &#9679; Atomic bitwise AND operation with prior fetch         |
| *ATOMIC:BIT\_MANIPULATION* |||| [__atomic\_fetch\_or__]({{site.baseurl}}/learn/intrinsics/ATOMIC_FETCH_OR)  || &#9679; Atomic bitwise OR operation with prior fetch          |
| *ATOMIC:BIT\_MANIPULATION* |||| [__atomic\_fetch\_xor__]({{site.baseurl}}/learn/intrinsics/ATOMIC_FETCH_XOR)|| &#9679; Atomic bitwise XOR operation with prior fetch         |
| *ATOMIC:BIT\_MANIPULATION* |||| [__atomic\_or__]({{site.baseurl}}/learn/intrinsics/ATOMIC_OR)               || &#9679; Atomic bitwise OR operation                           |
| *ATOMIC:BIT\_MANIPULATION* |||| [__atomic\_xor__]({{site.baseurl}}/learn/intrinsics/ATOMIC_XOR)             || &#9679; Atomic bitwise OR operation                           |
|----------------------------||||-----------------------------------------------------------------------------||---------------------------------------------------------------|
| *ATOMIC*                   |||| [__atomic\_add__]({{site.baseurl}}/learn/intrinsics/ATOMIC_ADD)             || &#9679; Atomic ADD operation                                  |
| *ATOMIC*                   |||| [__atomic\_cas__]({{site.baseurl}}/learn/intrinsics/ATOMIC_CAS)             || &#9679; Atomic compare and swap                               |
| *ATOMIC*                   |||| [__atomic\_define__]({{site.baseurl}}/learn/intrinsics/ATOMIC_DEFINE)       || &#9679; Setting a variable atomically                         |
| *ATOMIC*                   |||| [__atomic\_fetch\_add__]({{site.baseurl}}/learn/intrinsics/ATOMIC_FETCH_ADD)|| &#9679; Atomic ADD operation with prior fetch                 |
| *ATOMIC*                   |||| [__atomic\_ref__]({{site.baseurl}}/learn/intrinsics/ATOMIC_REF)             || &#9679; Obtaining the value of a variable atomically          |
|----------------------------||||-----------------------------------------------------------------------------||---------------------------------------------------------------|

###### fortran-lang intrinsic descriptions
