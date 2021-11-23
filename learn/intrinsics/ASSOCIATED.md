---
layout: book
title: associated
permalink: /learn/intrinsics/ASSOCIATED
---
### NAME

**associated**(3f) - \[\] Status of a pointer or pointer/target pair
(GFDL)

### SYNTAX

result = **associated**(pointer \[, target\])

### DESCRIPTION

**associated**(pointer \[, target\]) determines the status of the
pointer POINTER or if POINTER is associated with the target TARGET.

### ARGUMENTS

  - **POINTER**
    POINTER shall have the POINTER attribute and it can be of any type.

  - **TARGET**
    (Optional) TARGET shall be a pointer or a target. It must have the
    same type, kind type parameter, and array rank as POINTER.

The association status of neither POINTER nor TARGET shall be undefined.

### RETURN VALUE

**associated**(pointer) returns a scalar value of type **logical**(4).
There are several cases:

1.  When the optional TARGET is not present then **associated**(pointer)
    is true if POINTER is associated with a target; otherwise, it
    returns false.

2.  If TARGET is present and a scalar target, the result is true if
    TARGET is not a zero-sized storage sequence and the target
    associated with POINTER occupies the same storage units. If POINTER
    is disassociated, the result is false.

3.  If TARGET is present and an array target, the result is true if
    TARGET and POINTER have the same shape, are not zero-sized arrays,
    are arrays whose elements are not zero-sized storage sequences, and
    TARGET and POINTER occupy the same storage units in array element
    order.

    As in case 2, the result is false, if POINTER is disassociated.

4.  If TARGET is present and an scalar pointer, the result is true if
    TARGET is associated with POINTER, the target associated with TARGET
    are not zero-sized storage sequences and occupy the same storage
    units.

    The result is false, if either TARGET or POINTER is disassociated.

5.  If TARGET is present and an array pointer, the result is true if
    target associated with POINTER and the target associated with TARGET
    have the same shape, are not zero-sized arrays, are arrays whose
    elements are not zero-sized storage sequences, and TARGET and
    POINTER occupy the same storage units in array element order. The
    result is false, if either TARGET or POINTER is disassociated.

### EXAMPLE

Sample program:

```
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

### STANDARD

Fortran 95 and later

### CLASS

Inquiry function

### SEE ALSO

**null**(3)
