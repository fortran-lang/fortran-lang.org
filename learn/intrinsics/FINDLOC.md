---
layout: book
title: findloc
permalink: /learn/intrinsics/FINDLOC
---
## __Name__

__findloc__(3) - \[ARRAY:LOCATION\] Location of first element of ARRAY identified by MASK along dimension DIM having a value
(GFDL)

## __Syntax__
```fortran
findloc (array, value, dim, mask, kind, back) 

or 

findloc(array, value, mask, kind, back)
```
## __Description__

Location of the first element of __array__ identified by __mask__ along
dimension __dim__ having a value equal to __value__.

If both __array__ and __value__ are of type logical, the comparison is
performed with the __.eqv.__ operator; otherwise, the comparison is
performed with the == operator. If the value of the comparison is
true, that element of __array__ matches __value__.

If only one element matches __value__, that element's subscripts are
returned. Otherwise, if more than one element matches __value__ and
__back__ is absent or present with the value false, the element whose
subscripts are returned is the first such element, taken in array
element order. If __back__ is present with the value true, the element
whose subscripts are returned is the last such element, taken in array
element order.

## __Options__

  - __array__
    : shall be an array of intrinsic type.

  - __value__
    : shall be scalar and in type conformance with __array__, as specified
    in Table 7.3 for relational intrinsic operations 7.1.5.5.2).

  - __dim__
    : shall be an integer scalar with a value in the range 1 __DIM__ n, where
    n is the rank of __array__. The corresponding actual argument shall
    not be an optional dummy argument.

  - __mask__
    : (optional) shall be of type logical and shall be conformable with
    __array__.

  - __kind__
    : (optional) shall be a scalar integer initialization expression.

  - __back__
    : (optional) shall be a logical scalar.

## __Returns__

Result Characteristics. Integer. If __kind__ is present, the kind type
parameter is that specified by the value of __kind__; otherwise the kind
type parameter is that of default integer type. If __dim__ does not appear,
the result is an array of rank one and of size equal to the rank of
__array__; otherwise, the result is of rank n - 1 and shape

```
   [d1 , d2 , . . . , dDIM-1 , dDIM+1 , . . . , dn ]
```

where

```
   [d1 , d2 , . . . , dn ]
```

is the shape of __array__.

## __Returns__

  - __Case (i):__
    The result of __findloc (array, value)__ is a rank-one array whose
    element values are the values of the subscripts of an element of
    __array__ whose value matches __value__. If there is such a value, the
    ith subscript returned lies in the range 1 to ei , where ei is the
    extent of the ith dimension of __array__. If no elements match __value__
    or __array__ has size zero, all elements of the result are zero.

  - __Case (ii):__
    the result of __findloc (array, value, mask = mask)__ is a
    rank-one array whose element values are the values of the subscripts
    of an element of __array__, corresponding to a true element of __mask__,
    whose value matches __value__. If there is such a value, the ith
    subscript returned lies in the range 1 to ei , where ei is the
    extent of the ith dimension of __array__. If no elements match
    __value__, __array__ has size zero, or every element of __mask__ has the
    value false, all elements of the result are zero.

  - __Case (iii):__
    If __array__ has rank one, the result of

```
      findloc (array, value, dim=dim [, mask = mask])
```

is a scalar whose value is equal to that of the first element of

```
      findloc (array, value [, mask = mask])
```

Otherwise, the value of element

```
      (s1 , s2 , . . . , sDIM-1 , sDIM+1 , . . . , sn )
```

of the result is equal to

```
      findloc (array (s1, s2, ..., sdim-1, :, sdim+1, ..., sn ), &
      value, dim=1 [, mask = mask (s1, s2, ..., sdim-1, :,
                      sdim+1 , ... , sn )]).
```
## __Examples__

  - __Case (i):__
    The value of

```
        findloc ([2, 6, 4, 6,], value = 6)
```

is \[2\], and the value of

```
        findloc ([2, 6, 4, 6], value = 6, back = .true.)
```

is \[4\].

  - __Case (ii):__
    If __a__ has the value

```text
      0 -5  7 7
      3  4 -1 2
      1  5  6 7
```

and __m__ has the value

```text
       T T F T
       T T F T
       T T F T

      findloc (a, 7, mask = m)
```

has the value \[1, 4\] and

```
      findloc (a, 7, mask = m, back = .true.)
```

has the value \[3, 4\]. This is independent of the declared lower
bounds for A.

  - __Case (iii):__
    The value of

```
      findloc ([2, 6, 4], value = 6, dim = 1)
```

is 2. If B has the value

```
       1 2 -9
       2 2  6
```

> findloc (b, __value__ = 2, dim = 1)

has the value \[2, 1, 0\] and

```
      findloc (b, value = 2, dim = 2)
```

has the value \[2, 1\]. This is independent of the declared lower
bounds for B.

###### fortran-lang intrinsic descriptions
