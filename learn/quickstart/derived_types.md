---
layout: book
title: Derived types
permalink: /learn/quickstart/derived_types
---

As discussed previously in [Variables]({{site.baseurl}}/learn/quickstart/variables) there are five built-in data types in Fortran. _Derived types_ is a special form of a data type that can encapsulate other built-in types as well as other derived types. It could be considered as the equivalent of _struct_ in the C and C++ programming languages.

## A quick take on derived types

Here's an example of a basic derived type:

```fortran
type :: my_type
    integer :: i
    real    :: x
end type
```

The syntax to create a variable of type _my_type_ and access its members is:
```fortran
! declare
type(my_type) :: foo
! initialize
foo%i   = 1
foo%x   = 0.5
```

{% include note.html content="The percentage symbol `%` is used to access the members of a derived type." %}

In the above snippet, we declared an instance of a derived type and initialized its members explicitly.
You can also initialize derived type members by invoking the derived type constructor:


Example using the assignment operator (=):
```fortran
foo = my_type(1, 0.5)
! or using F2003 standard and on
foo = my_type(i=1, x=0.5)
```

Example with default initialization:
```fortran
type :: my_type
    integer :: i = 1
    real    :: x = 0.5
end type
! then it is possible to use as
type(my_type) :: foo
foo = my_type(i=2) ! foo%i gets a new value, but foo%x retains the default one.
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
type :: foo
sequence
integer :: var1
real    :: var2
end type
! init
type(foo) :: bar
bar = foo(1, 0.5)
```
{% include note.html content="The use of statement `sequence` presupposes that the data types defined below are neither of `allocatable` nor of `pointer` type. Furthermore, it does not imply that these data types  will be stored in memory in any particular form, there is no relation to `contiguous` attribute." %}

The _access-type_ attributes `public` and `private` if used, declare that all [member-variables] declared below will be automatically assigned the attribute accordingly. 

The attribute `bind(c)` is used to achieve compatibility between Fortran's derived type and C's struct.

Example with `bind(c)`:
```fortran
module mymod
use iso_c_bindings, only: c_int
implicit none
type, bind(c) :: mytype
    integer(c_int) :: i
end type
end module mymod
```
matches the following C struct:
```c
struct{
    int i
}mytype;
```
{% include note.html content="A fortran derived type with the attribute `bind(c)` cannot have the `sequence` and `extends` attributes. Furthermore it cannot contain any Fortran `pointer` or `allocatable` types." %}

`parameterized-declaration-list`: is an optional feature. If used, then the parameters must be listed in place of [parameterized-definition-statements] and must be either `len` or `kind` parameters or both. 

Example of a derived type with `parameterized-declaration-list` and with the `attribute: public`:
 ```fortran
module mymod
implicit none
private

type, public :: matrix(rows, cols, k)
  integer, len :: rows, cols
  integer, kind :: k = kind(0.0)
  real(kind = k), dimension(rows, cols) :: values
end type matrix
end module mymod

program test
use mymod
implicit none
type(matrix(rows=5, cols=5)) :: m
end program
 ```
{% include note.html content="In this example the parameter **k** has already been assigned a default value of `kind(0.0)`, that is of floating point single precision. Therefore, it can be omitted, as it is the case here in the declaration inside the main program." %}

{% include important.html content="By default derived types and their members are public. However, in this example the attribute `private` is used at the beginning of the module, therefore, everything within the module will be by default `private` unless, explicitly, declared as `public`. If the type **matrix** was not given the attribute `public` in the above example, then the compiler would throw an error inside **program test**." %}

The attribute `extends` was added in F2003 standard and introduces an important feature of the object oriented paradigm (OOP), namely the inheritance. It allows code reusability by letting children-derived-types like this: `type, extends(parent) :: child` to inherit all the members and functionality from a parent-derived-type: `type :: parent`.  

Example with the attribute `extends`: 
```fortran
module mymod
implicit none
private
public t_date, t_address, t_person, t_employ ! note another way of using the public attribute by gathering all public data types in one place

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
end module mymod

program test
use mymod
implicit none
type(t_employ) :: employ

! initialization
employ%hired_date%year  = 2020 ! t_employ has access to type(t_date) members not because of extends but because a type(t_date) was declared within t_employ
employ%hired_date%month = 1
employ%hired_date%day   = 20
employ%first_name       = 'Johny' !t_employ has access to t_person, and inherits its members due to extends 
employ%last_name        = 'Doe'
employ%city             = 'London' ! t_employ has access to t_address, because it inherits from t_person, that in return inherits from t_address
employ%road_name        = 'BigBen'
employ%house_number     = 1
employ%position         = 'Intern'
employ%monthly_salary   = 0.0
end program test
``` 

## Options to declare members of a derived type

`[member-variables]` refers to the declaration of all the member data types. These data types can be of any built-in data type, and/or of other derived types, as already show-cased in the above examples. However, member-variables can have their own extensive syntax, in form of:
`type [,member-attributes] :: name[attr-dependent-spec][init]`

`type`: any built-in type or other derived type

`member-attributes` (optional):

- `pointer` to specify a pointer
- `allocatable` with or without `dimension` to specify a dynamic array
- `public` or `private` access attributes
- `protected` access attribute
- `codimension` to specify a coarray 
- `contiguous`

{% include note.html content="`pointer` and `allocatable` cannot co-exist." %}

{% include note.html content="`contiguous` requires an array with the `pointer` attribute." %}

Examples for common cases:

```fortran
type :: t_example
    !1st case: simple built-in type with access attribute and [init]
    integer, private :: i = 0 ! private hides it from use outside of the t_example's scope. The default initialization [=0] is the [init] part. 

    !2nd case: dynamic 1d_array
    real, allocatable, dimension(:) :: x
    ! the same as
    real, allocatable :: x(:) ! parenthesis implies dimension(:) and is one of the possible [attr-dependent-spec].

    !3rd case: protected
    integer, protected :: i ! In contrary to private, protected allows access to i assigned value outside of t_example but is not definable, i.e. a value may be assigned to i only within t_example.

    !4th case: pointer, with [init]
    real, pointer :: x(:) => null() ! the [init] part is the [=>null()], pointers are discussed in the Advanced programming mini-book. 

    !5th case: coarray
    real, allocatable, codimension[:] :: z(:) ! a 1d_dynamic array shared in all threads. Coarrays will be discussed in the Advanced programming mini-book.
    !or 
    real, allocatable :: z(:)[:] ! here the [:] is the [attr-dependent-spec] and implies the codimension[:].

    !6th case: contiguous
    real, contiguous, pointer :: x(:)
end type
```

{% include note.html content="In the above example the cases 4, 5 and 6 make use of Fortran `pointer` and `coarray` features that have not been addressed in this quickstart tutorial. However, they are presented here, in order for the readers to know that these features do exist and be able to recognize them. These features will be covered in detail in the upcoming `Advanced programing` mini-book." %}

## Type-bound procedures

A derived type is possible to contain procedures either `functions` or `subroutines` that are **bound** to this derived type. Type procedures must follow the `contains` statement that, in return, must be used within the derived type and after all [member-variables] have been declared. 

{% include note.html content="It is impossible to describe type-bound procedures in their full syntax without delving into OOP features of modern Fortran. For that reason only a simple example is provided in this final part, to demonstrate a very basic use." %}

Example of a derived type with basic bound-procedure:

```fortran
module mymod
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
        res = self%side * self%side
    end function
end module

program main
use mymod
implicit none
! variables declaration
type(t_square) :: sq
real :: x, side

! variables initialization
side    = 0.5
sq%side = side

x       = sq%area() ! self does not appear here, it has been passed implicitly
! do stuff with x...
end program
```
What is new:

 - **self** is a random name that was chosen to represent the derived type t_square that is passed as an argument to the bound-function in order to have access to its data-members. By passing it like that it is ensured that later during its use the t_square will be passed automatically and not by the client.
 - in order to have the above functionality the new keyword `class` replaced the `type` one. With `class` the OOP feature *polymorphism* is introduced. 

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
* `real, intent(out) :: x` which is used to store the calculated area into and return to the caller. 
