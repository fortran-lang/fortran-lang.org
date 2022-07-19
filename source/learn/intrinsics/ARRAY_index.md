---
layout: book
title: Array Operations
permalink: ARRAY_index
---
# ARRAY_index
### Properties and attributes of arrays

<table>
  <thead>
    <tr>
      <th>category</th>
      <th>&nbsp;</th>
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
      <td><em>ARRAY:CONSTRUCTION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="MERGE"><strong>merge</strong></a></td>
      <td>&nbsp;</td>
      <td>● Merge variables</td>
    </tr>
    <tr>
      <td><em>ARRAY:CONSTRUCTION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="PACK"><strong>pack</strong></a></td>
      <td>&nbsp;</td>
      <td>● Pack an array into an array of rank one</td>
    </tr>
    <tr>
      <td><em>ARRAY:CONSTRUCTION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="SPREAD"><strong>spread</strong></a></td>
      <td>&nbsp;</td>
      <td>● Add a dimension to an array</td>
    </tr>
    <tr>
      <td><em>ARRAY:CONSTRUCTION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="UNPACK"><strong>unpack</strong></a></td>
      <td>&nbsp;</td>
      <td>● Store the elements of a vector in an array of higher rank</td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <td><em>ARRAY:INQUIRY</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="ALLOCATED"><strong>allocated</strong></a></td>
      <td>&nbsp;</td>
      <td>● Status of an allocatable entity</td>
    </tr>
    <tr>
      <td><em>ARRAY:INQUIRY</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="IS_CONTIGUOUS"><strong>is_contiguous</strong></a></td>
      <td>&nbsp;</td>
      <td>● Test if object is contiguous</td>
    </tr>
    <tr>
      <td><em>ARRAY:INQUIRY</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="LBOUND"><strong>lbound</strong></a></td>
      <td>&nbsp;</td>
      <td>● Lower dimension bounds of an array</td>
    </tr>
    <tr>
      <td><em>ARRAY:INQUIRY</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="RANK"><strong>rank</strong></a></td>
      <td>&nbsp;</td>
      <td>● Rank of a data object</td>
    </tr>
    <tr>
      <td><em>ARRAY:INQUIRY</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="SHAPE"><strong>shape</strong></a></td>
      <td>&nbsp;</td>
      <td>● Determine the shape of an array</td>
    </tr>
    <tr>
      <td><em>ARRAY:INQUIRY</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="SIZE"><strong>size</strong></a></td>
      <td>&nbsp;</td>
      <td>● Determine the size of an array</td>
    </tr>
    <tr>
      <td><em>ARRAY:INQUIRY</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="UBOUND"><strong>ubound</strong></a></td>
      <td>&nbsp;</td>
      <td>● Upper dimension bounds of an array</td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <td><em>ARRAY:LOCATION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="MAXLOC"><strong>maxloc</strong></a></td>
      <td>&nbsp;</td>
      <td>● Location of the maximum value within an array</td>
    </tr>
    <tr>
      <td><em>ARRAY:LOCATION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="MINLOC"><strong>minloc</strong></a></td>
      <td>&nbsp;</td>
      <td>● Location of the minimum value within an array</td>
    </tr>
    <tr>
      <td><em>ARRAY:LOCATION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="FINDLOC"><strong>findloc</strong></a></td>
      <td>&nbsp;</td>
      <td>● Locate first element of ARRAY identified by MASK along dimension DIM having a value</td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <td><em>ARRAY:MANIPULATION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="TRANSPOSE"><strong>transpose</strong></a></td>
      <td>&nbsp;</td>
      <td>● Transpose an array of rank two</td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <td><em>ARRAY:REDUCTION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="ALL"><strong>all</strong></a></td>
      <td>&nbsp;</td>
      <td>● Determines if all values in the logical array are true.</td>
    </tr>
    <tr>
      <td><em>ARRAY:REDUCTION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="ANY"><strong>any</strong></a></td>
      <td>&nbsp;</td>
      <td>● Determines if any values in the logical array are true.</td>
    </tr>
    <tr>
      <td><em>ARRAY:REDUCTION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="COUNT"><strong>count</strong></a></td>
      <td>&nbsp;</td>
      <td>● Count function</td>
    </tr>
    <tr>
      <td><em>ARRAY:REDUCTION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="MAXVAL"><strong>maxval</strong></a></td>
      <td>&nbsp;</td>
      <td>● Determines the maximum value in an array or row</td>
    </tr>
    <tr>
      <td><em>ARRAY:REDUCTION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="MINVAL"><strong>minval</strong></a></td>
      <td>&nbsp;</td>
      <td>● Minimum value of an array</td>
    </tr>
    <tr>
      <td><em>ARRAY:REDUCTION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="PRODUCT"><strong>product</strong></a></td>
      <td>&nbsp;</td>
      <td>● Product of array elements</td>
    </tr>
    <tr>
      <td><em>ARRAY:REDUCTION</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="SUM"><strong>sum</strong></a></td>
      <td>&nbsp;</td>
      <td>● Sum the elements of an array</td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <td><em>ARRAY:RESHAPE</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="RESHAPE"><strong>reshape</strong></a></td>
      <td>&nbsp;</td>
      <td>● Function to reshape an array</td>
    </tr>
  </tbody>
</table>

    - Explicit-shape arrays
    - Assumed-shape arrays
    - Deferred-shape arrays
    - Implied-shape arrays 
    - Assumed-size arrays
    - Assumed-rank objects 

###### fortran-lang intrinsic descriptions
