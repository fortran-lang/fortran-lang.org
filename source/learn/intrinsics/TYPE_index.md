---
layout: book
title: Type and Kind
permalink: TYPE_index
---
# TYPE_index
### Types and kinds

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
      <td><em>TYPE:NUMERIC</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="AIMAG"><strong>aimag</strong></a></td>
      <td>&nbsp;</td>
      <td>● Imaginary part of complex number</td>
    </tr>
    <tr>
      <td><em>TYPE:NUMERIC</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="CMPLX"><strong>cmplx</strong></a></td>
      <td>&nbsp;</td>
      <td>● Complex conversion function</td>
    </tr>
    <tr>
      <td><em>TYPE:NUMERIC</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="INT"><strong>int</strong></a></td>
      <td>&nbsp;</td>
      <td>● Convert to integer type</td>
    </tr>
    <tr>
      <td><em>TYPE:NUMERIC</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="NINT"><strong>nint</strong></a></td>
      <td>&nbsp;</td>
      <td>● Nearest whole number</td>
    </tr>
    <tr>
      <td><em>TYPE:NUMERIC</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="REAL"><strong>real</strong></a></td>
      <td>&nbsp;</td>
      <td>● Convert to real type</td>
    </tr>
    <tr>
      <td><em>TYPE:NUMERIC</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="DBLE"><strong>dble</strong></a></td>
      <td>&nbsp;</td>
      <td>● Double conversion function</td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <td><em>TYPE:MOLD</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="TRANSFER"><strong>transfer</strong></a></td>
      <td>&nbsp;</td>
      <td>● Transfer bit patterns</td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <td><em>TYPE:LOGICAL</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="LOGICAL"><strong>logical</strong></a></td>
      <td>&nbsp;</td>
      <td>● Converts one kind of <em>logical</em> variable to another</td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <td><em>KIND:INQUIRY</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="KIND"><strong>kind</strong></a></td>
      <td>&nbsp;</td>
      <td>● Kind of an entity</td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <td><em>KIND</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="SELECTED_CHAR_KIND"><strong>selected_char_kind</strong></a></td>
      <td>&nbsp;</td>
      <td>● Choose character kind such as “Unicode”</td>
    </tr>
    <tr>
      <td><em>KIND</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="SELECTED_INT_KIND"><strong>selected_int_kind</strong></a></td>
      <td>&nbsp;</td>
      <td>● Choose integer kind</td>
    </tr>
    <tr>
      <td><em>KIND</em></td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td>&nbsp;</td>
      <td><a href="SELECTED_REAL_KIND"><strong>selected_real_kind</strong></a></td>
      <td>&nbsp;</td>
      <td>● Choose real kind</td>
    </tr>
  </tbody>
</table>

These intrinsics allow for explicitly casting one type of variable to
another or can be used to conditionally execute code blocks based on
variable types when working with polymorphic variables.

#### Fortran Data Types

Fortran provides five basic intrinsic data types:

  * Integer type
    : The integer types can hold only whole number values.
  * Real type
    : Stores floating point numbers, such as 2.0, 3.1415, -100.876, etc.
  * Complex type
    : A complex number has two parts,
      the real part and the imaginary part. Two consecutive floating
      point storage units store the two parts.
  * Logical type
    : There are only two logical values: .true. and .false.
  * Character type
    : The character type stores strings. The length of the string
    can be specified by the __len__ specifier. If no length is specified, it is 1.

These "types" can be of many "kinds". Often different numeric kinds
take up different storage sizes and therefore can represent
different ranges; but a different kind can have other meanings.
A _character_ variable might represent ASCII characters or UTF-8 or
Unicode characters, for example.

You can derive your own data types from these fundamental types as well.

#### Implicit Typing

Fortran allows a feature called implicit typing, i.e., you do not have
to declare some variables before use. By default if a variable is not declared,
then the first letter of its name will determine its type:

1. Variable names starting with __i-n__ (the first two letters of
   "integer") specify _integer_ variables.

2. All other variable names default to _real_.


However, in most circles it is considered good programming practice to declare all the
variables. For that to be enforced, you start your variable declaration section with 
a statement that turns off implicit typing:
the statement
```fortran
implicit none
```
For more information refer to the __implicit__ statement.

###### fortran-lang intrinsic descriptions
