---
layout: book
title: findloc
permalink: /learn/intrinsics/FINDLOC
---
## __Name__

__findloc__(3) - \[\] Location of first element of ARRAY identified by MASK along dimension DIM having a value
(GFDL)

## __Synopsis__

FINDLOC (*ARRAY*, *VALUE*, DIM \[, *MASK*, *KIND*, *BACK*\]) or FINDLOC
(*ARRAY*, *VALUE* \[, *MASK*, *KIND*, *BACK*\])

## __Description__

Location of the first element of *ARRAY* identified by *MASK* along
dimension DIM having a value equal to *VALUE*.

> If both *ARRAY* and *VALUE* are of type logical, the comparison is
> performed with the .EQV. operator; otherwise, the comparison is
> performed with the == operator. If the value of the comparison is
> true, that element of *ARRAY* matches *VALUE*.
>
> If only one element matches *VALUE*, that element's subscripts are
> returned. Otherwise, if more than one element matches *VALUE* and
> *BACK* is absent or present with the value false, the element whose
> subscripts are returned is the first such element, taken in array
> element order. If *BACK* is present with the value true, the element
> whose subscripts are returned is the last such element, taken in array
> element order.

## __Options__

  - __*ARRAY__*
    shall be an array of intrinsic type.

  - __*VALUE__*
    shall be scalar and in type conformance with *ARRAY*, as specified
    in Table 7.3 for relational intrinsic operations 7.1.5.5.2).

  - __DIM__
    shall be an integer scalar with a value in the range 1 DIM n, where
    n is the rank of *ARRAY*. The corresponding actual argument shall
    not be an optional dummy argument.

  - __*MASK__*
    (optional) shall be of type logical and shall be conformable with
    *ARRAY*.

  - __*KIND__*
    (optional) shall be a scalar integer initialization expression.

  - __*BACK__*
    (optional) shall be a logical scalar.

## __Returns__

Result Characteristics. Integer. If *KIND* is present, the kind type
parameter is that specified by the value of *KIND*; otherwise the kind
type parameter is that of default integer type. If DIM does not appear,
the result is an array of rank one and of size equal to the rank of
*ARRAY*; otherwise, the result is of rank n - 1 and shape

```
   [d1 , d2 , . . . , dDIM-1 , dDIM+1 , . . . , dn ]
```

where

```
   [d1 , d2 , . . . , dn ]
```

is the shape of *ARRAY*.

## __Returns__

  - __Case (i):__
    The result of FINDLOC (*ARRAY*, *VALUE*) is a rank-one array whose
    element values are the values of the subscripts of an element of
    *ARRAY* whose value matches *VALUE*. If there is such a value, the
    ith subscript returned lies in the range 1 to ei , where ei is the
    extent of the ith dimension of *ARRAY*. If no elements match *VALUE*
    or *ARRAY* has size zero, all elements of the result are zero.

  - __Case (ii):__
    The result of FINDLOC (*ARRAY*, *VALUE*, *MASK* = *MASK*) is a
    rank-one array whose element values are the values of the subscripts
    of an element of *ARRAY*, corresponding to a true element of *MASK*,
    whose value matches *VALUE*. If there is such a value, the ith
    subscript returned lies in the range 1 to ei , where ei is the
    extent of the ith dimension of *ARRAY*. If no elements match
    *VALUE*, *ARRAY* has size zero, or every element of *MASK* has the
    value false, all elements of the result are zero.

  - __Case (iii):__
    If *ARRAY* has rank one, the result of

<!-- end list -->

```
                 findloc (array, value, dim=dim [, mask = mask])
```

> is a scalar whose value is equal to that of the first element of
>
> ```
>                  findloc (array, value [, mask = mask])
> ```
>
> Otherwise, the value of element
>
> ```
>                  (s1 , s2 , . . . , sDIM-1 , sDIM+1 , . . . , sn )
> ```
>
> of the result is equal to
>
> ````
>                  findloc (array (s1, s2, ```, sdim-1, :, sdim+1, ..., sn ), &
>                  value, dim=1 [, mask = mask (s1, s2, ```, sdim-1, :,
>                                  sdim+1 ,``` , sn )]).
> ````

## __Examples__

  - __Case (i):__
    The value of

<!-- end list -->

```
                  findloc ([2, 6, 4, 6,], value = 6)
```

> is \[2\], and the value of
>
> ```
>                   findloc ([2, 6, 4, 6], value = 6, back = .true.)
> ```
>
> is \[4\].

  - __Case (ii):__
    If A has the value

<!-- end list -->

```
                 > 0 -5  7 7
                 > 3  4 -1 2
                 > 1  5  6 7
```

> and M has the value
>
> ```
>                  > T T F T
>                  > T T F T
>                  > T T F T
>
>                  FINDLOC (A, 7, MASK = M)
> ```
>
> has the value \[1, 4\] and
>
> ```
>                  FINDLOC (A, 7, MASK = M, BACK = .TRUE.)
> ```
>
> has the value \[3, 4\]. This is independent of the declared lower
> bounds for A.

  - __Case (iii):__
    The value of

<!-- end list -->

```
                 FINDLOC ([2, 6, 4], VALUE = 6, DIM = 1)
```

> is 2. If B has the value
>
> ```
>                   > 1 2 -9
>                   > 2 2  6
> ```
>
> > FINDLOC (B, *VALUE* = 2, DIM = 1)
>
> has the value \[2, 1, 0\] and
>
> ```
>                  FINDLOC (B, VALUE = 2, DIM = 2)
> ```
>
> has the value \[2, 1\]. This is independent of the declared lower
> bounds for B.
