---
layout: book
title: Arrays and strings
permalink: /learn/quickstart/arrays_strings
---



More often than not, we need to store and operate on long lists of numbers as opposed to just the single scalar variables
that we have been using so far; in computer programming such lists are called  _arrays_.

Arrays are _multidimensional_ variables that contain more than one value
where each value is accessed using one or more indices.

>Arrays in Fortran are _one-based_ by default; this means
>that the first element along any dimension is at index 1.


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
  real, dimension(10, 10) :: array3

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
  integer :: array1(10)  ! 1D integer array of 10 elements
  integer :: array2(10, 10)  ! 2D integer array of 100 elements

  array1 = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]  ! Array constructor
  array1 = [(i, i = 1, 10)]  ! Implied do loop constructor
  array1(:) = 0  ! Set all elements to zero
  array1(1:5) = 1  ! Set first five elements to one
  array1(6:) = 1  ! Set all elements after five to one

  print *, array1(1:10:2)  ! Print out elements at odd indices
  print *, array2(:,1)  ! Print out the first column in a 2D array
  print *, array1(10:1:-1)  ! Print an array in reverse

end program array_slice
```

>Fortran arrays are stored in _column-major_ order; the first
index varies fastest.

## Allocatable (dynamic) arrays

So far we have specified the size of our array in our program code---this
type of array is known as a _static_ array since its size is fixed when
we compile our program.

Quite often, we do not know how big our array needs to be until we run our program, for example, if we are reading data from a file of unknown size.

For this problem, we need `allocatable` arrays.
These are _allocated_ while the program is running once we know how big the array needs to be.

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

Allocatable local arrays are deallocated automatically
when they go out of scope.


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


## Array of strings

An array of strings can be expressed in Fortran as an array of `character` variables.
All elements in a `character` array have equal length.
However, strings of varying lengths can be provided as input to the array constructor, as shown in the example below.
They will be truncated or right-padded with spaces if they are longer or shorter, respectively, than the declared length of the `character` array.
Finally, we use the intrinsic function `trim` to remove any excess spaces when printing the values to the standard output.


__Example:__ string array
```fortran
program string_array
  implicit none
  character(len=10), dimension(2) :: keys, vals

  keys = [character(len=10) :: "user", "dbname"]
  vals = [character(len=10) :: "ben", "motivation"]

  call show(keys, vals)

  contains

  subroutine show(akeys, avals)
    character(len=*), intent(in) :: akeys(:), avals(:)
    integer                      :: i

    do i = 1, size(akeys)
      print *, trim(akeys(i)), ": ", trim(avals(i))
    end do

  end subroutine show

end program string_array
```
