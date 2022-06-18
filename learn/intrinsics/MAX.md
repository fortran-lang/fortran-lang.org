---
layout: book
title: max
permalink: /learn/intrinsics/MAX
---
## __Name__

__max__(3) - \[NUMERIC\] Maximum value of an argument list

## __Syntax__
```fortran
result = max(a1, a2, a3, ...)
```
## __Description__

Returns the argument with the largest (most positive) value.

## __Arguments__

  - __a1__
    : The type shall be _integer_ or _real_.

  - __a2,a3,...__
    : An expression of the same type and kind as __a1__.

## __Returns__

The return value corresponds to the maximum value among the arguments,
and has the same type and kind as the first argument.

The function is both elemental and allows for an arbitrary number of
arguments. This means if some elements are scalar and some are arrays
that all the arrays must be of the same size, and the returned value
will be an array that is the result as if multiple calls were made with
all scalar values with a single element of each array used in each call.
If called with all arrays the returned array is the same as if multiple
calls were made with __max(arr1(1),arr2(1), ...)__ to
__max(arr1(N),arr2(N))__.

## __Examples__

Sample program

```fortran
program demo_max
implicit none
real :: arr1(4)= [10.0,11.0,30.0,-100.0]
real :: arr2(5)= [20.0,21.0,32.0,-200.0,2200.0]

  !! this is simple enough because it is not being called elementally
  !! because all arguments are scalar
  !!

  write(*,*)'scalars:',max(10.0,11.0,30.0,-100.0)

  !!
  !! this is all max(3) could do before it became an elemental
  !! function and is the most intuitive
  !! except that it can take an arbitrary number of options,
  !! which is not common in Fortran without
  !! declaring a lot of optional parameters.
  !!
  !! That is it unless you want to use the elemental features of max(3)!

  !! Error: Intrinsic    max    at (1) must have at least two arguments
  !!write(*,*)max(arr1)
  !! This does not work because it is like trying to return
  !! [(max(arr1(i)),i=1,size(arr1))]
  !! so it is trying to take the max of a single value.
  !! To find the largest element of an array
  !! call maxloc(3) or maxval(3).

  !! Error: Different shape for arguments 'a1' and 'a2' for intrinsic
  !! 'max' at (1) on dimension 1 (4 and 5)
  !!write(*,*)max(arr1,arr2)
  !! but this will return an array of
  !! [(max(arr1(N),arr2(N),N=1,size(arr1))]

  write(*,*)max(arr1,arr2(1:4))

  !! so this works only if all the arrays are the same size and
  !! you want an array of the largest Nth elements
  !! from the input arrays.
  !! maybe you wanted to do maxval([arr1,arr2]) or
  !! equivalently max(maxval(arr1),maxval(arr2))
  !! to find the single largest element in both arrays?

  !! compares all scalars to each member of array and
  !! returns array of size arr2

  write(*,*)'scalars and array:',max(10.0,11.0,30.0,-100.0,arr2)

  !! Error: Different shape for arguments 'a5' and 'a6'
  !! for intrinsic 'max' at (1) on dimension 1 (5 and 4)
  !! write(*,*)'scalars and array:',max(10.0,11.0,30.0,-100.0,arr2,arr1)
  !! as the same reason above when arrays are used
  !! (without scalar values) all the arrays must be the same size

  write(*,*)'scalars and array:',&
  & max(40.0,11.0,30.0,-100.0,arr2(:4),arr1)
end program demo_max
```
Results:

```text
    scalars:   30.000000
      20.0000000  21.000000  32.000000 -100.00000
    scalars and array: 30.000000 30.000000 32.000000 30.000000 2200.0000
    scalars and array: 40.000000 40.000000 40.000000 40.000000
```

## __Standard__

FORTRAN 77 and later

## __See Also__

[__maxloc__(3)](MAXLOC),
[__maxval__(3)](MAXVAL),
[__min__(3)](MIN)

###### fortran-lang intrinsic descriptions
