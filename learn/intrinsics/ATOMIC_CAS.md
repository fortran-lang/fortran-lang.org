---
layout: book
title: atomic_cas
permalink: /learn/intrinsics/ATOMIC_CAS
---
## __Name__

__atomic\_cas__(3) - \[ATOMIC\] Atomic compare and swap
(GFDL)

## __Syntax__

call atomic\_cas (atom, old, compare, new \[, stat\])

## __Description__

atomic\_cas compares the variable ATOM with the value of COMPARE; if the
value is the same, ATOM is set to the value of NEW. Additionally, OLD is
set to the value of ATOM that was used for the comparison. When STAT is
present and the invocation was successful, it is assigned the value 0.
If it is present and the invocation has failed, it is assigned a
positive value; in particular, for a coindexed ATOM, if the remote image
has stopped, it is assigned the value of iso\_fortran\_env's
stat\_stopped\_image and if the remote image has failed, the value
stat\_failed\_image.

## __Arguments__

  - __ATOM__
    Scalar coarray or coindexed variable of either integer type with
    atomic\_int\_kind kind or logical type with atomic\_logical\_kind
    kind.

  - __OLD__
    Scalar of the same type and kind as ATOM.

  - __COMPARE__
    Scalar variable of the same type and kind as ATOM.

  - __NEW__
    Scalar variable of the same type as ATOM. If kind is different, the
    value is converted to the kind of ATOM.

  - __STAT__
    (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_cas
use iso_fortran_env
implicit none
logical(atomic_logical_kind) :: atom[*], prev
   call atomic_cas(atom[1], prev, .false., .true.)
end program demo_atomic_cas
```

## __Standard__

TS 18508 or later

## __See Also__

__atomic\_define__(3), __atomic\_ref__(3), __iso\_fortran\_env__(3)
