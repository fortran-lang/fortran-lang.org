---
layout: book
title: Arrays and strings
permalink: /learn/quickstart/arrays_strings
---



More often than not, we need to store and operate on long lists of numbers as opposed to just the single scalar variables
that we have been using so far; in computer programming such lists are called  _arrays_.

Arrays are _multidimensional_ variables that contain more than one value
where each value is accessed using one or more indices.

{% include important.html content="Arrays in Fortran are __one-based__ by default; this means
that the first element along any dimension is at index 1." %}


## Array declaration

We can declare arrays of any type. There are two common notations for declaring array variables:
using the `dimension` attribute or by appending the array dimensions in parentheses to the variable name.

__Example:__ static array declaration
```fortran
program arrays
  implicit none

  ! 1D integer array
  integer, dimension(10) :: array1

  ! An equivalent array declaration
  integer :: array2(10)

  ! 2D real array
  real, dimension(10,10) :: array3

  ! Custom lower and upper index bounds
  real :: array4(0:9)
  real :: array5(-5:5)

end program arrays
```

## Array slicing

A powerful feature of the Fortran language is its built-in support for array operations;
we can perform operations on all or part of an array using array _slicing_ notation:

__Example:__ array slicing
```fortran
program array_slice
  implicit none

  integer :: i
  integer :: array1(10)            ! 1D integer array of 10 elements
  integer :: array2(10,10)         ! 2D integer array of 100 elements

  array1 = [1,2,3,4,5,6,7,8,9,10]  ! Array constructor
  array1 = [(i,i=1,10)]            ! Implied do loop constructor
  array1(:) = 0                    ! Set all elements to zero
  array1(1:5) = 1                  ! Set first five elements to one
  array1(6:) = 1                   ! Set all elements after five to one

  print *, array1(1:10:2)          ! Print out elements at odd indices
  print *, array2(:,1)             ! Print out the first column in a 2D array
  print *, array1(10:1:-1)         ! Print an array in reverse

end program array_slice
```

{% include note.html content="Fortran arrays are stored in __column-major__ order; the first
index varies fastest." %}

## Allocatable (dynamic) arrays

So far we have specified the size of our array in our program code---this
type of array is known as a _static_ array since its size is fixed when
we compile our program.

Quite often, we do not know how big our array needs to be until we run
our program.
For example, if we are reading data from a file of unknown size.

For this problem, we need `allocatable` arrays, these are _allocated_
while the program is runnning once we know how big the array needs to be.

__Example:__ allocatable arrays
```fortran
program allocatable
  implicit none

  integer, allocatable :: array1(:)
  integer, allocatable :: array2(:,:)

  allocate(array1(10))
  allocate(array2(10,10))

  ! ...

  deallocate(array1)
  deallocate(array2)

end program allocatable
```

{% include note.html content="Allocatable local arrays are deallocated automatically
when they go out of scope." %}


## Character strings


__Example:__ static character string
```fortran
program string
  implicit none

  character(len=4) :: first_name
  character(len=5) :: last_name
  character(10) :: full_name

  first_name = 'John'
  last_name = 'Smith'

  ! String concatenation
  full_name = first_name//' '//last_name

  print *, full_name 

end program string
```

__Example:__ allocatable character string
```fortran
program allocatable_string
  implicit none

  character(:), allocatable :: first_name
  character(:), allocatable :: last_name

  ! Explicit allocation statement
  allocate(character(4) :: first_name)
  first_name = 'John'

  ! Allocation on assignment
  last_name = 'Smith'

  print *, first_name//' '//last_name

end program allocatable_string
```

