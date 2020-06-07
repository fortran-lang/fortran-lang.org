---
layout: book
title: Derived types
permalink: /learn/quickstart/derived_types
---

As discussed previously in [Variables]({{site.baseurl}}/learn/quickstart/variables) there are five built-in data types in Fortran. _Derived types_ is a special form of a data type that can encapsulate other built-in types as well as other derived types. It could be considered as the equivalent of _struct_ in the C and C++ programming languages.

## A quick take on derived types

Here's an example of a basic derived type:

```fortran
type :: t_pair
    integer :: i
    real    :: x
end type
```

The syntax to create a variable of type _t_pair_ and access its members is:
```fortran
! declare
type(t_pair) :: pair
! initialize
pair%i   = 1
pair%x   = 0.5
```

{% include note.html content="The percentage symbol `%` is used to access the members of a derived type." %}

In the above snippet, we declared an instance of a derived type and initialized its members explicitly.
You can also initialize derived type members by invoking the derived type constructor:


Example using the derived type constructor:
```fortran
pair = t_pair(1, 0.5)       ! initialize with positional arguments
pair = t_pair(i=1, x=0.5)   ! initialize with keyword arguments
pair = t_pair(x=0.5, i=1)   ! keyword arguments can go in any order
```

Example with default initialization:
```fortran
type :: t_pair
    integer :: i = 1
    real    :: x = 0.5
end type

type(t_pair) :: pair
pair = t_pair()         ! pair%i is 1, pair%x is 0.5
pair = t_pair(i=2)      ! pair%i is 2, pair%x is 0.5
pair = t_pair(x=2.7)    ! pair%i is 1, pair%x is 2.7
```

## Derived types in detail

The full syntax of a derived type with all optional properties is presented below:

```fortran
type [,attribute-list] :: name [(parameterized-declaration-list)]
    [parameterized-definition-statements]
    [private statement or sequence statement]
    [member-variables]
    contains
        [type-bound-procedures]
end type
```

## Options to declare a derived type

`attribute-list` may refer to the following:

- _access-type_ that is either `public` or `private`
- `bind(c)` offers interoperability with C programming language 
- `extends(`_parent_`)` where _parent_ is the name of a previously declared derived type, from which, the current derived type will inherit all its members and functionality.
- `abstract` an object oriented feature that is covered in the advanced programming tutorial.

{% include note.html content="If the `attribute: bind(c)` or the `statement: sequence` is used then a derived type cannot have the `attribute: extends` and visa-versa." %}

The `sequence` attribute may be used only to declare that the following  members should be accessed in the same order as they are defined within the derived type. 

Example with `sequence`:
```fortran
type :: t_pair
sequence
integer :: i
real    :: x
end type
! init
type(t_pair) :: pair
pair = t_pair(1, 0.5)
```
{% include note.html content="The use of statement `sequence` presupposes that the data types defined below are neither of `allocatable` nor of `pointer` type. Furthermore, it does not imply that these data types  will be stored in memory in any particular form, there is no relation to `contiguous` attribute." %}

The _access-type_ attributes `public` and `private` if used, declare that all [member-variables] declared below will be automatically assigned the attribute accordingly. 

The attribute `bind(c)` is used to achieve compatibility between Fortran's derived type and C's struct.

Example with `bind(c)`:
```fortran
module f_to_c
use iso_c_bindings, only: c_int
implicit none
type, bind(c) :: f_type
    integer(c_int) :: i
end type
end module f_to_c
```
matches the following C struct:
```c
struct{
    int i
}c_struct;
```
{% include note.html content="A fortran derived type with the attribute `bind(c)` cannot have the `sequence` and `extends` attributes. Furthermore it cannot contain any Fortran `pointer` or `allocatable` types." %}

`parameterized-declaration-list`: is an optional feature. If used, then the parameters must be listed in place of [parameterized-definition-statements] and must be either `len` or `kind` parameters or both. 

Example of a derived type with `parameterized-declaration-list` and with the `attribute: public`:
 ```fortran
module m_matrix
implicit none
private

type, public :: t_matrix(rows, cols, k)
  integer, len :: rows, cols
  integer, kind :: k = kind(0.0)
  real(kind = k), dimension(rows, cols) :: values
end type 
end module m_matrix

program test_matrix
use m_matrix
implicit none
type(t_matrix(rows=5, cols=5)) :: m
end program test_matrix
 ```
{% include note.html content="In this example the parameter **k** has already been assigned a default value of `kind(0.0)`, that is of floating point single precision. Therefore, it can be omitted, as it is the case here in the declaration inside the main program." %}

{% include important.html content="By default derived types and their members are public. However, in this example the attribute `private` is used at the beginning of the module, therefore, everything within the module will be by default `private` unless, explicitly, declared as `public`. If the type **matrix** was not given the attribute `public` in the above example, then the compiler would throw an error inside **program test**." %}

The attribute `extends` was added in F2003 standard and introduces an important feature of the object oriented paradigm (OOP), namely the inheritance. It allows code reusability by letting children-derived-types like this: `type, extends(parent) :: child` to inherit all the members and functionality from a parent-derived-type: `type :: parent`.  

Example with the attribute `extends`: 
```fortran
module m_employee
implicit none
private
public t_date, t_address, t_person, t_employee ! note another way of using the public attribute by gathering all public data types in one place

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

type, extends(t_person)  :: t_employee
    type(t_date)                    :: hired_date
    character(len=:), allocatable   :: position
    real                            :: monthly_salary
end type
end module m_employee

program test_employee
use m_employee
implicit none
type(t_employee) :: employee

! initialization
employee%hired_date%year  = 2020 ! t_employee has access to type(t_date) members not because of extends but because a type(t_date) was declared within t_employee
employee%hired_date%month = 1
employee%hired_date%day   = 20
employee%first_name       = 'John' !t_employee has access to t_person, and inherits its members due to extends 
employee%last_name        = 'Doe'
employee%city             = 'London' ! t_employee has access to t_address, because it inherits from t_person, that in return inherits from t_address
employee%road_name        = 'BigBen'
employee%house_number     = 1
employee%position         = 'Intern'
employee%monthly_salary   = 0.0
end program test_employee
``` 

## Options to declare members of a derived type

`[member-variables]` refers to the declaration of all the member data types. These data types can be of any built-in data type, and/or of other derived types, as already show-cased in the above examples. However, member-variables can have their own extensive syntax, in form of:
`type [,member-attributes] :: name[attr-dependent-spec][init]`

`type`: any built-in type or other derived type

`member-attributes` (optional):

- `public` or `private` access attributes
- `protected` access attribute
- `allocatable` with or without `dimension` to specify a dynamic array
- `pointer`, `codimension`, `contiguous`, `volatile`, `asynchronous`

Examples for common cases:

```fortran
type :: t_example
    !1st case: simple built-in type with access attribute and [init]
    integer, private :: i = 0 ! private hides it from use outside of the t_example's scope. The default initialization [=0] is the [init] part. 

    !2nd case: protected
    integer, protected :: i ! In contrary to private, protected allows access to i assigned value outside of t_example but is not definable, i.e. a value may be assigned to i only within t_example.

    !3rd case: dynamic 1d_array
    real, allocatable, dimension(:) :: x
    ! the same as
    real, allocatable :: x(:) ! parenthesis implies dimension(:) and is one of the possible [attr-dependent-spec]. 
end type
```

{% include note.html content="The following attributes: `pointer`, `codimension`, `contiguous`, `volatile`, `asynchronous` are advanced features that will not be addressed in the *Quickstart* tutorial. However, they are presented here, in order for the readers to know that these features do exist and be able to recognize them. These features will be covered in detail in the upcoming *Advanced programing* mini-book." %}

## Type-bound procedures

A derived type can contain functions or subroutines that are **bound** to it. We'll refer to them as _type-bound procedures_. Type-bound procedures follow the `contains` statement that, in turn, follows all member variable declarations.

{% include note.html content="It is impossible to describe type-bound procedures in full without delving into OOP features of modern Fortran. For now we'll focus on a simple example to show their basic use." %}

Example of a derived type with basic bound-procedure:

```fortran
module m_shapes
implicit none
private
public t_square

type :: t_square
    real :: side
    contains
        procedure :: area !procedure declaration
end type

contains
    ! procedure definition
    real function area(self) result(res)
        class(t_square), intent(in) :: self 
        res = self%side**2
    end function
end module m_shapes

program main
use m_shapes
implicit none
! variables declaration
type(t_square) :: sq
real :: x, side

! variables initialization
side    = 0.5
sq%side = side

x       = sq%area() ! self does not appear here, it has been passed implicitly
! do stuff with x...
end program main
```
What is new:

 - **self** is an arbitrary name that we chose to represent the instance of the derived type t_square inside the type-bound function. This allows us to access its members and to automatically pass it as an argument when we invoke a type-bound procedure.
 - We now use `class(t_square)` instead of `type(t_square)` in the interface of the `area` function. This allows us to invoke the `area` function with any derived type that extends `t_square`. The keyword `class` introduces the OOP feature, polymorphism.

In the above example, the type-bound procedure **area** is defined as a function and can be invoked only in an expression, for example `x = sq%area()` or `print *, sq%area()`. If you define it instead as a subroutine, you can invoke it from its own `call` statement:

 ```fortran
 ! change within module
 contains
    subroutine area(self, x)
        class(t_square), intent(in)     :: self
        real,            intent(out) :: x
        x = self%side**2
    end subroutine

! change within main program
call sq%area(x)
! do stuff with x...
 ```
In contrast to the example with the type-bound function, we now have two arguments: 

* `class(t_square), intent(in) :: self`, which is the instance of the derived type itself
* `real, intent(out) :: x`, which is used to store the calculated area and return to the caller. 
