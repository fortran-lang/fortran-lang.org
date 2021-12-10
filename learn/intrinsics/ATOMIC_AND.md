---
layout: book
title: atomic_and
permalink: /learn/intrinsics/ATOMIC_AND
---
## __Name__

__atomic\_and__(3) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise AND operation
(GFDL)

## __Syntax__
```fortran
call atomic_and(atom, value, stat)
```
## __Description__

__atomic\_and(atom, value)__ atomically defines __atom__ with the bitwise
__and__ between the values of __atom__ and __value__. When __stat__ is present and the
invocation was successful, it is assigned the value 0. If it is present
and the invocation has failed, it is assigned a positive value; in
particular, for a coindexed __atom__, if the remote image has stopped, it is
assigned the value of iso\_fortran\_env's stat\_stopped\_image and if
the remote image has failed, the value stat\_failed\_image.

## __Arguments__

  - __atom__
    : Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind.

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_and
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*]
   call atomic_and(atom[1], int(b'10100011101'))
end program demo_atomic_and
```
## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_fetch\_and__(3)](ATOMIC_FETCH_AND),
[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_ref__(3)](ATOMIC_REF),
[__atomic\_cas__(3)](ATOMIC_CAS),
__iso\_fortran\_env__(3),
[__atomic\_add__(3)](ATOMIC_ADD),
[__atomic\_or__(3)](ATOMIC_OR),
[__atomic\_xor__(3)](ATOMIC_XOR)
