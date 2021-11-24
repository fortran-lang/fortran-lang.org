---
layout: book
title: all
permalink: /learn/intrinsics/ALL
---
#### NAME

__all__(3f) - \[ARRAY REDUCTION\] determines if all the values are true
(GFDL)

#### SYNTAX

result = __ALL__(MASK \[, DIM\])

#### DESCRIPTION

Logical conjunction of elements of MASK along dimension DIM.

"__ALL__(MASK \[, DIM\])" determines if all the values are true in MASK
in the array along dimension DIM.

#### ARGUMENTS

  - __MASK__
    shall be a logical array. That is, the type of the argument shall be
    LOGICAL and it shall not be scalar.

  - __DIM__
    (optional) DIM shall be a scalar integer with a value that lies
    between one and the rank of MASK. The corresponding actual argument
    shall not be an optional dummy argument.

#### RETURN VALUE

"__ALL__(MASK)" returns a scalar value of type LOGICAL where the kind
type parameter is the same as the kind type parameter of MASK. If DIM is
present, then __ALL__(MASK, DIM) returns an array with the rank of MASK
minus 1. The shape is determined from the shape of MASK where the DIM
dimension is elided.

> 1.  __ALL__(MASK) is true if all elements of MASK are true. It also is
>     true if MASK has zero size; otherwise, it is false.
>
> 2.  If the rank of MASK is one, then __ALL__(MASK, DIM) is equivalent
>     to __ALL__(MASK). If the rank is greater than one, then
>     __ALL__(MASK, DIM) is determined by applying ALL to the array
>     sections.
>
> 3.  Result Characteristics. The result is of type logical with the
>     same kind type parameter as MASK. It is scalar if DIM is absent or
>     n = 1; otherwise, the result has rank n - 1 and shape \[d1 , d2 ,
>     . . . , dDIM-1 , dDIM+1 , . . . , dn \] where \[d1 , d2 , . . . ,
>     dn \] is the shape of MASK.
>
> 4.  Result Value.
>
> <!-- end list -->
>
> ```
>        Case (i):   The result of ALL (MASK) has the value true if all
>                    elements of MASK are true or if MASK has
>                    size zero, and the result has value false if any element
>                    of MASK is false.
>
>        Case (ii):  If MASK has rank one, ALL(MASK,DIM) is equal to
>                    ALL(MASK). Otherwise, the value of element
>                    (s1 , s2 , . . . , sDIM-1 , sDIM+1 , . . . , sn ) of ALL
>                    (MASK, DIM) is equal to ALL (MASK (s1 , s2 , . . . ,
>                    sDIM-1 , :, sDIM+1 , . . . , sn )).
> ```

#### EXAMPLE

Sample program:

```
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
     The value of ALL ([.TRUE., .FALSE., .TRUE.]) is false.
```

Case (ii):

```
     >                        1|3|5
     > If B is the array      -+-+-
     >                        2|4|6
     >
     >                        0|3|5
     > and C is the array     -+-+-
     >                        7|4|8

     then ALL (B /= C, DIM = 1) is

        [true, false, false]
```

> and ALL (B /= C, DIM = 2) is
>
> ```
>         [false, false].
> ```

#### STANDARD

Fortran 95 and later

#### CLASS

Transformational function.
