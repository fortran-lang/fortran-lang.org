---
layout: book
title: all
permalink: /learn/intrinsics/ALL
---
# ALL
## __Name__

__all__(3) - \[ARRAY REDUCTION\] determines if all the values are true


## __Syntax__
```fortran
result = all(mask, dim)
```
## __Description__

Logical conjunction of elements of __mask__ along dimension __dim__.

"__all(mask, dim)__" determines if all the values are true in __mask__
in the array along dimension __dim__.

## __Arguments__

  - __mask__
    : shall be a logical array. That is, the type of the argument shall be
    _logical_ and it shall not be scalar.

  - __dim__
    : (optional) __dim__ shall be a scalar integer with a value that lies
    between one and the rank of __mask__. The corresponding actual argument
    shall not be an optional dummy argument.

## __Returns__

"__all(mask)__" returns a scalar value of type _logical_ where the kind
type parameter is the same as the kind type parameter of __mask__. If
__dim__ is present, then __all(mask, dim)__ returns an array with the rank
of __mask__ minus 1. The shape is determined from the shape of __mask__
where the __dim__ dimension is elided.

 1.  __all(mask)__ is true if all elements of __mask__ are true. It also is
     true if __mask__ has zero size; otherwise, it is false.

 2.  If the rank of __mask__ is one, then __all(mask, dim)__ is equivalent
     to __all(mask)__. If the rank is greater than one, then __all(mask,
     dim)__ is determined by applying __all()__ to the array sections.

 3.  Result Characteristics. The result is of type _logical_ with the
     same kind type parameter as __mask__. It is scalar if __dim__
     is absent or __n = 1__; otherwise, the result has rank __n - 1__
     and shape __\[d1 , d2 , . . . , dDIM-1 , dDIM+1 , . . . , dn \]__
     where __\[d1 , d2 , . . . , dn \]__ is the shape of __mask__.

 4.  Result Value.

     Case (i):
               : The result of __all(mask)__ has the value true if all
               elements of __mask__ are true or if __mask__ has
               size zero, and the result has value false if any element
               of __mask__ is false.

     Case (ii): 
               : If __mask__ has rank one, __all(mask,dim)__ is equal to
               __all(mask)__. Otherwise, the value of element __(s1 , s2 ,
               . . . , sdim-1 , sdim+1 , . . . , sn )__ of all __(mask,
               dim)__ is equal to __all(mask (s1 , s2 , . . . , sdim-1 ,
               :, sdim+1 , . . . , sn ))__.

## __Examples__

Sample program:

```fortran
program demo_all
implicit none
logical l
   l = all([.true., .true., .true.])
   print *, l
   call section

contains

subroutine section
integer a(2,3), b(2,3)
  a = 1
  b = 1
  b(2,2) = 2
  print *, all(a .eq. b, 1)
  print *, all(a .eq. b, 2)
end subroutine section
end program demo_all
```
Results:
```text
    T
    T F T
    T F
```
Case (i):

```text
     The value of all([.TRUE., .FALSE., .TRUE.]) is false.
```

Case (ii):

```text
                          1|3|5
   If B is the array      -+-+-
                          2|4|6
  
                          0|3|5
   and C is the array     -+-+-
                          7|4|8

   then all(B /= C, DIM = 1) is

      [true, false, false]
```

and __all(B /= C, DIM = 2)__ is

```
        [false, false].
```

## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
