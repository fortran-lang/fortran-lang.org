---
layout: book
title: is_contiguous
permalink: /learn/intrinsics/IS_CONTIGUOUS
---
## __Name__

__is\_contiguous__(3) - \[ARRAY INQUIRY\] test if object is contiguous
(GFDL)

## __Syntax__

result = __is\_contiguous__(A)

## __Description__

True if and only if an object is contiguous.

An object is contiguous if it is

   - __(1)__
     an object with the CONTIGUOUS attribute,

   - __(2)__
     a nonpointer whole array that is not assumed-shape,

   - ____(3)
     an assumed-shape array that is argument associated with an array
     that is contiguous,

   - __(4)__
     an array allocated by an ALLOCATE statement,

   - __(5)__
     a pointer associated with a contiguous target, or

   - __(6)__
     a nonzero-sized array section provided that

       - __(a)__
         its base object is contiguous,

       - __(b)__
         it does not have a vector subscript,

       - __(c)__
         the elements of the section, in array element order, are a
         subset of the base object elements that are consecutive in
         array element order,

       - __(d)__
         if the array is of type character and a substring-range
         appears, the substring-range specifies all of the characters
         of the parent-string,

       - __(e)__
         only its final part-ref has nonzero rank, and

       - __(f)__
         it is not the real or imaginary part of an array of type
         complex.

An object is not contiguous if it is an array subobject, and

   - the object has two or more elements,

   - the elements of the object in array element order are not
     consecutive in the elements of the base object,

   - the object is not of type character with length zero, and

   - the object is not of a derived type that has no ultimate
     components other than zero-sized arrays and

   - characters with length zero.

It is processor-dependent whether any other object is contiguous.

## __Arguments__

  - __a__
    : may be of any type. It shall be an array. If it is a pointer it
    shall be associated.

## __Returns__

  - __Result__
    : of type Default logical scalar. The result has the value true if __a__
    is contiguous, and false otherwise.

## __Examples__

Sample program:

```fortran
program demo_is_contiguous
implicit none
intrinsic is_contiguous
real, DIMENSION (1000, 1000), TARGET :: A
real, DIMENSION (:, :), POINTER       :: IN, OUT
   IN => A              ! Associate IN with target A
   OUT => A(1:1000:2,:) ! Associate OUT with subset of target A
   !
   write(*,*)'IN is ',IS_CONTIGUOUS(IN)
   write(*,*)'OUT is ',IS_CONTIGUOUS(OUT)
   !
end program demo_is_contiguous
```

## __Standard__

Fortran 2008 and later
