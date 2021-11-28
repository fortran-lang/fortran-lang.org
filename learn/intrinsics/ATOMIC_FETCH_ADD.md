---
layout: book
title: atomic_fetch_add
permalink: /learn/intrinsics/ATOMIC_FETCH_ADD
---
-------------------------------------------------------------------------------
## __Name__

__atomic\_fetch\_add__(3) - \[ATOMIC\] Atomic ADD operation with prior fetch
(GFDL)

## __Syntax__

call __atomic\_fetch\_add__(atom, value, old \[, stat\])

## __Description__

__atomic\_fetch\_add__(atom, value, old) atomically stores the value of
ATOM in OLD and adds the value of VAR to the variable ATOM. When STAT is
present and the invocation was successful, it is assigned the value 0.
If it is present and the invocation has failed, it is assigned a
positive value; in particular, for a coindexed ATOM, if the remote image
has stopped, it is assigned the value of iso\_fortran\_env's
stat\_stopped\_image and if the remote image has failed, the value
stat\_failed\_image.

## __Arguments__

  - __ATOM__
    Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind. atomic\_logical\_kind kind.

  - __VALUE__
    Scalar of the same type as ATOM. If the kind is different, the value
    is converted to the kind of ATOM.

  - __OLD__
    Scalar of the same type and kind as ATOM.

  - __STAT__
    (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_fetch_add
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*], old
   call atomic_add(atom[1], this_image(), old)
end program demo_atomic_fetch_add
```

## __Standard__

TS 18508 or later

## __See Also__

__atomic\_define__(3), __atomic\_add__(3), __iso\_fortran\_env__(3),
__atomic\_fetch\_and__(3), __atomic\_fetch\_or__(3),
__atomic\_fetch\_xor__(3)
