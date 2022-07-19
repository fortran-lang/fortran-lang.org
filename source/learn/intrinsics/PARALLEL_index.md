---
layout: book
title: Parallel Programming
permalink: PARALLEL_index
---
# PARALLEL_index
### These routines support parallel programming using co_arrays and co_indexed arrays.

<table>
  <thead>
    <tr>
      <th>category</th>
      <th>&nbsp;</th>
      <th>&nbsp;</th>
      <th>&nbsp;</th>
      <th>page</th>
      <th>&nbsp;</th>
      <th>description</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td><em>COLLECTIVE</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="CO_BROADCAST"><strong>co_broadcast</strong></a></td>
      <td>&nbsp;</td>
      <td>● Copy a value to all images the current set of images</td>
    </tr>
    <tr>
      <td><em>COLLECTIVE</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="CO_LBOUND"><strong>co_lbound</strong></a></td>
      <td>&nbsp;</td>
      <td>● Lower codimension bounds of an array</td>
    </tr>
    <tr>
      <td><em>COLLECTIVE</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="CO_MAX"><strong>co_max</strong></a></td>
      <td>&nbsp;</td>
      <td>● Maximal value on the current set of images</td>
    </tr>
    <tr>
      <td><em>COLLECTIVE</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="CO_MIN"><strong>co_min</strong></a></td>
      <td>&nbsp;</td>
      <td>● Minimal value on the current set of images</td>
    </tr>
    <tr>
      <td><em>COLLECTIVE</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="CO_REDUCE"><strong>co_reduce</strong></a></td>
      <td>&nbsp;</td>
      <td>● Reduction of values on the current set of images</td>
    </tr>
    <tr>
      <td><em>COLLECTIVE</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="CO_SUM"><strong>co_sum</strong></a></td>
      <td>&nbsp;</td>
      <td>● Sum of values on the current set of images</td>
    </tr>
    <tr>
      <td><em>COLLECTIVE</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="CO_UBOUND"><strong>co_ubound</strong></a></td>
      <td>&nbsp;</td>
      <td>● Upper codimension bounds of an array</td>
    </tr>
    <tr>
      <td><em>COLLECTIVE</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="EVENT_QUERY"><strong>event_query</strong></a></td>
      <td>&nbsp;</td>
      <td>● Query whether a coarray event has occurred</td>
    </tr>
    <tr>
      <td><em>COLLECTIVE</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="IMAGE_INDEX"><strong>image_index</strong></a></td>
      <td>&nbsp;</td>
      <td>● Cosubscript to image index conversion</td>
    </tr>
    <tr>
      <td><em>COLLECTIVE</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="NUM_IMAGES"><strong>num_images</strong></a></td>
      <td>&nbsp;</td>
      <td>● Number of images</td>
    </tr>
    <tr>
      <td><em>COLLECTIVE</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="THIS_IMAGE"><strong>this_image</strong></a></td>
      <td>&nbsp;</td>
      <td>● Cosubscript index of this image</td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <td><em>ATOMIC:BIT_MANIPULATION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="ATOMIC_AND"><strong>atomic_and</strong></a></td>
      <td>&nbsp;</td>
      <td>● Atomic bitwise AND operation</td>
    </tr>
    <tr>
      <td><em>ATOMIC:BIT_MANIPULATION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="ATOMIC_FETCH_AND"><strong>atomic_fetch_and</strong></a></td>
      <td>&nbsp;</td>
      <td>● Atomic bitwise AND operation with prior fetch</td>
    </tr>
    <tr>
      <td><em>ATOMIC:BIT_MANIPULATION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="ATOMIC_FETCH_OR"><strong>atomic_fetch_or</strong></a></td>
      <td>&nbsp;</td>
      <td>● Atomic bitwise OR operation with prior fetch</td>
    </tr>
    <tr>
      <td><em>ATOMIC:BIT_MANIPULATION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="ATOMIC_FETCH_XOR"><strong>atomic_fetch_xor</strong></a></td>
      <td>&nbsp;</td>
      <td>● Atomic bitwise XOR operation with prior fetch</td>
    </tr>
    <tr>
      <td><em>ATOMIC:BIT_MANIPULATION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="ATOMIC_OR"><strong>atomic_or</strong></a></td>
      <td>&nbsp;</td>
      <td>● Atomic bitwise OR operation</td>
    </tr>
    <tr>
      <td><em>ATOMIC:BIT_MANIPULATION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="ATOMIC_XOR"><strong>atomic_xor</strong></a></td>
      <td>&nbsp;</td>
      <td>● Atomic bitwise OR operation</td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <td><em>ATOMIC</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="ATOMIC_ADD"><strong>atomic_add</strong></a></td>
      <td>&nbsp;</td>
      <td>● Atomic ADD operation</td>
    </tr>
    <tr>
      <td><em>ATOMIC</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="ATOMIC_CAS"><strong>atomic_cas</strong></a></td>
      <td>&nbsp;</td>
      <td>● Atomic compare and swap</td>
    </tr>
    <tr>
      <td><em>ATOMIC</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="ATOMIC_DEFINE"><strong>atomic_define</strong></a></td>
      <td>&nbsp;</td>
      <td>● Setting a variable atomically</td>
    </tr>
    <tr>
      <td><em>ATOMIC</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="ATOMIC_FETCH_ADD"><strong>atomic_fetch_add</strong></a></td>
      <td>&nbsp;</td>
      <td>● Atomic ADD operation with prior fetch</td>
    </tr>
    <tr>
      <td><em>ATOMIC</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="ATOMIC_REF"><strong>atomic_ref</strong></a></td>
      <td>&nbsp;</td>
      <td>● Obtaining the value of a variable atomically</td>
    </tr>
  </tbody>
</table>
###### fortran-lang intrinsic descriptions
