---
layout: book
title: Derived types
permalink: /learn/quickstart/derived_types
---

As discussed previously in [Variables]({{site.baseurl}}/learn/quickstart/variables) there are five built-in data types in Fortran. _Derived types_ is a special form of data type that can encapsulate other built-in types as well as other _derived types_. It could be considered as the equivalent of _struct_ in the C/C++ programming languages.

## Declaring derived types

Example of a basic derived type is:

```fortran
type :: my_type
    integer :: i
    real    :: x
end type
```

The syntax for creating a variable of type _my_type_:

```fortran
type(my_type) :: foo
```

The members of _foo_ can be accessed as follows:
```fortran
foo%i
```
{% include note.html content="The use of `%` is the equivalent of `.` as used in many other languages like C/C++ and python." %}

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

`parameterized-decleration-list`: is an optional feautre. If used, then the parameters must be listed in place of [parameterized-definition-statements] and must be either `len` or `kind` parameters or both. 

Example of a derived type with `parameterized-decleration-list` and with the `attribute: public`:
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
{% include note.html content="In this example the parameter **k** has already been assigned a default value of `kind(0.0)`, that is of floating point single precision, for that reason it can be ommited, as it is the case here in the declaration inside the main program." %}

{% include important.html content="By default derived types and their members are public. However, in this example the attribute `private` is used at the beginning of the module, therefore, everything within the module will be by default `private` unless, explicitly, declared as `public`. If the type **matrix** was not given the attribute `public` in the above example, then the compiler would throw an error inside **program test**." %}

Example with the `attribute: extends`: 
```fortran
module mymod
implicit none
private
public t_date, t_address, t_person, t_employ

type :: t_date
    integer                         :: year, month, day
end type

type :: t_address
    character(len=:), allocatable   :: city, road_name
    integer                         :: house_number
end type

type, extends(t_address) :: t_person
    character(len=:), allocatable   :: first_name, last_name, e_mail  
end type

type, extends(t_person)  :: t_employ
    type(t_date)                    :: hired_date
    character(len=:), allocatable   :: position
    real                            :: monthly_salary
end type
end module

program test
use mymod
implicit none
type(t_employ) :: employ

!example initialization
employ%hired_date%year  = 2020 ! t_employ has access to type(t_date) members not because of extends but because a type(t_date) was declared within employ
employ%hired_date%month = 1
employ%hired_date%day   = 20
employ%first_name       = 'Johny' !t_employ has acces to t_person, and inherits its members due to extends 
employ%last_name        = 'Doe'
employ%city             = 'London' ! t_employ has access to t_address, because it inherits from t_person, that in turn inherits from t_address
employ%road_name        = 'BigBen'
employ%house_number     = 1
employ%position         = 'Intern'
employ%monthly_salary   = 0.0
end program
```