---
layout: book
title: all
permalink: /learn/intrinsics/ALL
---
-------------------------------------------------------------------------------
## __Name__

__all__(3) - \[ARRAY REDUCTION\] determines if all the values are true
(GFDL)

## __Syntax__

result = __all__(MASK \[, DIM\])

## __Description__

Logical conjunction of elements of __MASK__ along dimension __DIM__.

"__all(MASK \[, DIM\])__" determines if all the values are true in __MASK__
in the array along dimension __DIM__.

## __Arguments__

  - __MASK__
    shall be a logical array. That is, the type of the argument shall be
    _logical_ and it shall not be scalar.

  - __DIM__
    (optional) __DIM__ shall be a scalar integer with a value that lies
    between one and the rank of __MASK__. The corresponding actual argument
    shall not be an optional dummy argument.

## __Returns__

"__all(MASK)__" returns a scalar value of type _logical_ where the kind
type parameter is the same as the kind type parameter of __MASK__. If __DIM__ is
present, then __all(MASK, DIM)__ returns an array with the rank of __MASK__
minus 1. The shape is determined from the shape of __MASK__ where the __DIM__
dimension is elided.

> 1.  __all__(MASK) is true if all elements of __MASK__ are true. It also is
>     true if MASK has zero size; otherwise, it is false.
>
> 2.  If the rank of MASK is one, then __ALL(MASK, DIM)__ is equivalent
>     to __all(MASK)__. If the rank is greater than one, then
>     __all(MASK, DIM)__ is determined by applying __all()__ to the array
>     sections.
>
> 3.  Result Characteristics. The result is of type logical with the
>     same kind type parameter as __MASK__. It is scalar if __DIM__ is absent or
>     __n = 1__; otherwise, the result has rank __n - 1__ and shape __\[d1 , d2 ,
>     . . . , dDIM-1 , dDIM+1 , . . . , dn \]__ where __\[d1 , d2 , . . . ,
>     dn \]__ is the shape of __MASK__.
>
> 4.  Result Value.
>
> <!-- end list -->
>
> ```
>        Case (i):   The result of __all(MASK)__ has the value true if all
>                    elements of __MASK__ are true or if __MASK__ has
>                    size zero, and the result has value false if any element
>                    of __MASK__ is false.
>
>        Case (ii):  If __MASK__ has rank one, __all(MASK,DIM)__ is equal to
>                    __all(MASK)__. Otherwise, the value of element
>                    __(s1 , s2 , . . . , sDIM-1 , sDIM+1 , . . . , sn )__ of all
>                    __(MASK, DIM)__ is equal to __all(MASK (s1 , s2 , . . . ,
>                    sDIM-1 , :, sDIM+1 , . . . , sn ))__.
> ```

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

Case (i):

```
     The value of __all([.TRUE., .FALSE., .TRUE.])__ is false.
```

Case (ii):

```
                          1|3|5
   If B is the array      -+-+-
                          2|4|6
  
                          0|3|5
   and C is the array     -+-+-
                          7|4|8

   then __all(B /= C, DIM = 1)__ is

      [true, false, false]
```

and __all(B /= C, DIM = 2)__ is

```
        [false, false].
```

## __Standard__

Fortran 95 and later
