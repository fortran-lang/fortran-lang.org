---
layout: book
title: parity
permalink: /learn/intrinsics/PARITY
---
# PARITY
## __Name__

__parity__(3) - \[TRANSFORMATIONAL\] Reduction with exclusive __OR__()


## __Syntax__
```fortran
result = parity(mask, dim)

    function parity(mask, dim)
    type(logical(kind=LKIND))                    :: dim
    type(logical(kind=LKIND)),intent(in)         :: mask(..)
    type(integer(kind=KIND)),intent(in),optional :: dim
```
where KIND and LKIND are any supported kind for the type.
```
## __Description__

Calculates the parity (i.e. the reduction using .xor.) of __mask__ along
dimension __dim__.

## __Arguments__

  - __mask__
    : Shall be an array of type _logical_.

  - __dim__
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from __1 to n__, where __n__ equals the rank of __mask__.

## __Returns__

The result is of the same type as __mask__.

If __dim__ is absent, a scalar with the parity of all elements in __mask__ is
returned: __.true.__ if an odd number of elements are __.true.__ and __.false.__
otherwise.

When __dim__ is specified the returned shape is similar to that of __mask__
with dimension __dim__ dropped.

## __Examples__

Sample program:

```fortran
program demo_parity
implicit none
logical :: x(2) = [ .true., .false. ]
   print *, parity(x) 
end program demo_parity
```
  Results:
```text
    T
```
## __Standard__

Fortran 2008 and later

###### fortran-lang intrinsic descriptions
