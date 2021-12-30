---
layout: book
title: associated
permalink: /learn/intrinsics/ASSOCIATED
---
## __Name__

__associated__(3) - \[STATE\] Status of a pointer or pointer/target pair


## __Syntax__
```fortran
result = associated(pointer, target)
```
## __Description__

__associated(pointer \[, target\])__ determines the status of the
pointer __pointer__ or if __pointer__ is associated with the target __target__.

## __Arguments__

  - __pointer__
    : __pointer__ shall have the _pointer_ attribute and it can be of any type.

  - __target__
    : (Optional) __target__ shall be a pointer or a target. It must have the
    same type, kind type parameter, and array rank as __pointer__.

The association status of neither __pointer__ nor __target__ shall be undefined.

## __Returns__

__associated(pointer)__ returns a scalar value of type _logical_.
There are several cases:

1.  When the optional __target__ is not present then __associated(pointer)__
    is true if __pointer__ is associated with a target; otherwise, it
    returns false.

2.  If __target__ is present and a scalar target, the result is true if
    __target__ is not a zero-sized storage sequence and the target
    associated with __pointer__ occupies the same storage units. If __pointer__
    is disassociated, the result is false.

3.  If __target__ is present and an array target, the result is true if
    __target__ and __pointer__ have the same shape, are not zero-sized arrays,
    are arrays whose elements are not zero-sized storage sequences, and
    __target__ and __pointer__ occupy the same storage units in array element
    order.

    As in case 2, the result is false, if __pointer__ is disassociated.

4.  If __target__ is present and an scalar pointer, the result is true if
    __target__ is associated with __pointer__, the target associated with __target__
    are not zero-sized storage sequences and occupy the same storage
    units.

    The result is __.false.__, if either __target__ or __pointer__ is disassociated.

5.  If __target__ is present and an array pointer, the result is true if
    target associated with __pointer__ and the target associated with __target__
    have the same shape, are not zero-sized arrays, are arrays whose
    elements are not zero-sized storage sequences, and __target__ and
    __pointer__ occupy the same storage units in array element order. The
    result is false, if either __target__ or __pointer__ is disassociated.

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

###### fortran-lang intrinsic descriptions
