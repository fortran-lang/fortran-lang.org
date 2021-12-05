---
layout: book
title: associated
permalink: /learn/intrinsics/ASSOCIATED
---
## __Name__

__associated__(3) - \[\] Status of a pointer or pointer/target pair
(GFDL)

## __Syntax__

result = __associated(pointer \[, target\])__

## __Description__

__associated(pointer \[, target\])__ determines the status of the
pointer __POINTER__ or if __POINTER__ is associated with the target __TARGET__.

## __Arguments__

  - __POINTER__
    : __POINTER__ shall have the __POINTER__ attribute and it can be of any type.

  - __TARGET__
    : (Optional) __TARGET__ shall be a pointer or a target. It must have the
    same type, kind type parameter, and array rank as __POINTER__.

The association status of neither __POINTER__ nor __TARGET__ shall be undefined.

## __Returns__

__associated(pointer)__ returns a scalar value of type _logical_.
There are several cases:

1.  When the optional __TARGET__ is not present then __associated(pointer)__
    is true if __POINTER__ is associated with a target; otherwise, it
    returns false.

2.  If __TARGET__ is present and a scalar target, the result is true if
    __TARGET__ is not a zero-sized storage sequence and the target
    associated with __POINTER__ occupies the same storage units. If __POINTER__
    is disassociated, the result is false.

3.  If __TARGET__ is present and an array target, the result is true if
    __TARGET__ and __POINTER__ have the same shape, are not zero-sized arrays,
    are arrays whose elements are not zero-sized storage sequences, and
    __TARGET__ and __POINTER__ occupy the same storage units in array element
    order.

    As in case 2, the result is false, if __POINTER__ is disassociated.

4.  If __TARGET__ is present and an scalar pointer, the result is true if
    __TARGET__ is associated with __POINTER__, the target associated with __TARGET__
    are not zero-sized storage sequences and occupy the same storage
    units.

    The result is false, if either __TARGET__ or __POINTER__ is disassociated.

5.  If __TARGET__ is present and an array pointer, the result is true if
    target associated with __POINTER__ and the target associated with __TARGET__
    have the same shape, are not zero-sized arrays, are arrays whose
    elements are not zero-sized storage sequences, and __TARGET__ and
    __POINTER__ occupy the same storage units in array element order. The
    result is false, if either __TARGET__ or __POINTER__ is disassociated.

## __Examples__

Sample program:

```fortran
program demo_associated
implicit none
real, target  :: tgt(2) = [1., 2.]
real, pointer :: ptr(:)
   ptr => tgt
   if (associated(ptr)     .eqv. .false.) &
   & stop 'POINTER NOT ASSOCIATED'
   if (associated(ptr,tgt) .eqv. .false.) &
   & stop 'POINTER NOT ASSOCIATED TO TARGET'
end program demo_associated
```

## __Standard__

Fortran 95 and later

## __See Also__

[__null__(3)](NULL)
