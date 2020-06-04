---
layout: book
title: Derived types
permalink: /learn/quickstart/derived_types
---

As discussed in sectionn [Variables]({{site.baseurl}}/learn/quickstart/variables) there are five built-in data types in Fortran. _Derived types_ is a special form of data type that can encapsulate other built-in types as well as other _derived types_. It could be considered as the equivalent of _struct_ in C/C++.

## Declaring derived types

Example of a basic derived type is:

```fortran
type :: my_type
    integer :: i
    real    :: x
end type
```

The syntax for creating a variable of type _my_type_ is:

```fortran
type(my_type) :: foo
```

The members of _foo_ can be accessed as follows:
```fortran
foo%i
```
Note that the use of `%` is the equivalent of `.` as used in many other languages like C/C++, python etc... 

## Full syntax

```fortran
type [,attribute-list] :: name [(parameterized-decleration-list)]
    [parameterized-definition-statements]
    [private statement or sequence statement]
    [member-variables-decleration]
    contains
        [procedure decleration]
end type
```
`attribute-list` may refer to the following:

- _access-type_ that is either `public` or `private`
- `bind(c)` interoperability with C programming language 
- `extends(`_parent_`)` where _parent_ is the name of a previously declared derived type, from which, the current derived type will inherit all its members and functionality.
- `abstract` an object orianted feature that is covered in the advanced programming tutorial.

`parameterized-decleration-list`: is an optional feautre, but if a derived type is parameterized then the parameters must be listed inside, in the[parameterized-definition-statements] place and must be either `len` or `kind` parameters or both. 

Example of a derived type with `parameterized-decleration-list` and with `attribute`: `public`:
 ```fortran
module mymod
implicit none
private

type, public :: matrix(rows, cols, k)
  integer, len :: rows, cols
  integer, kind :: k = kind(0.0)
  real(kind = k), dimension(rows, cols) :: values
end type matrix
end module

program test
use mymod
implicit none
type(matrix(rows=5, cols=5)) :: m
end program
 ```
{% include note.html content="In this example the parameter **k** has already been assigned a default value of `kind(0.0)`, that is of floating point single precision, for that reason it can be ommited, as it is the case here in the declaration inside the main program."}

{% include important.html content="By default __derived types__ and their members are public. Hhowever, in this example the attribute __private__ is used at the beginning of the module, therefore, everything within the modure will be by default __private__ unless, explicitly, declared as __public__. If the type `matrix` was not given the attribute __public__ in the above example, then the compiler would throw an error inside __program test__."}