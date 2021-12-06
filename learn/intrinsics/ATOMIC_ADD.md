---
layout: book
title: atomic_add
permalink: /learn/intrinsics/ATOMIC_ADD
---
## __Name__

__atomic\_add__(3) - \[ATOMIC\] Atomic ADD operation (GFDL)

## __Syntax__

call atomic\_add (atom, value \[, stat\])

## __Description__

__atomic\_ad_(atom, value)__ atomically adds the value of VAR to the
variable __atom__. When __stat__ is present and the invocation was successful,
it is assigned the value 0. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
ATOM, if the remote image has stopped, it is assigned the value of
iso\_fortran\_env's stat\_stopped\_image and if the remote image has
failed, the value stat\_failed\_image.

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
program demo_atomic_add
use iso_fortran_env
implicit none
integer(atomic_int_kind) :: atom[*]
   call atomic_add (atom[1], this_image())
end program demo_atomic_add
```

## __Standard__

TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_fetch\_add__(3)](ATOMIC_FETCH),
[__atomic\_and__(3)](ATOMIC_AND),
[__atomic\_or__(3)](ATOMIC_OR),
[__atomic\_xor__(3)](ATOMIC_XOR)
__iso\_fortran\_env__(3),
