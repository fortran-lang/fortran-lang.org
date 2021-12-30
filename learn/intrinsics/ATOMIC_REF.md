---
layout: book
title: atomic_ref
permalink: /learn/intrinsics/ATOMIC_REF
---
## __Name__

__atomic\_ref__(3) - \[ATOMIC\] Obtaining the value of a variable atomically


## __Syntax__
```fortran
call atomic_ref(value, atom, stat)
```
## __Description__

__atomic\_ref(value, atom)__ atomically assigns the value of the
variable __atom__ to __value__. When __stat__ is present and the invocation was
successful, it is assigned the value __0__. If it is present and the
invocation has failed, it is assigned a positive value; in particular,
for a coindexed __atom__, if the remote image has stopped, it is assigned
the value of iso\_fortran\_env's __stat\_stopped\_image__ and if the remote
image has failed, the value __stat\_failed\_image__.

## __Arguments__

  - __value__
    : Scalar of the same type as __atom__. If the kind is different, the value
    is converted to the kind of __atom__.

  - __atom__
    : Scalar coarray or coindexed variable of either integer type with
    atomic\_int\_kind kind or logical type with atomic\_logical\_kind
    kind.

  - __stat__
    : (optional) Scalar default-kind integer variable.

## __Examples__

Sample program:

```fortran
program demo_atomic_ref
use iso_fortran_env
implicit none
logical(atomic_logical_kind) :: atom[*]
logical :: val
   call atomic_ref( val, atom[1] )
   ! ```
   call atomic_ref( val, atom[1] )
   if (val) then
      print *, "Obtained"
   endif
end program demo_atomic_ref
```

## __Standard__

Fortran 2008 and later; with STAT, TS 18508 or later

## __See Also__

[__atomic\_define__(3)](ATOMIC_DEFINE),
[__atomic\_cas__(3)](ATOMIC_CAS),
[__iso\_fortran\_env__(3)](),

[__atomic\_fetch\_add__(3)](ATOMIC_ADD),
[__atomic\_fetch\_and__(3)](ATOMIC_AND),

[__atomic\_fetch\_or__(3)](ATOMIC_OR),
[__atomic\_fetch\_xor__(3)](ATOMIC_XOR)

###### fortran-lang intrinsic descriptions
