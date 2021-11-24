---
layout: book
title: atomic_cas
permalink: /learn/intrinsics/ATOMIC_CAS
---
### NAME

__atomic\_cas__(3f) - \[ATOMIC\] Atomic compare and swap
(GFDL)

### SYNTAX

call atomic\_cas (atom, old, compare, new \[, stat\])

### DESCRIPTION

atomic\_cas compares the variable ATOM with the value of COMPARE; if the
value is the same, ATOM is set to the value of NEW. Additionally, OLD is
set to the value of ATOM that was used for the comparison. When STAT is
present and the invocation was successful, it is assigned the value 0.
If it is present and the invocation has failed, it is assigned a
positive value; in particular, for a coindexed ATOM, if the remote image
has stopped, it is assigned the value of iso\_fortran\_env's
stat\_stopped\_image and if the remote image has failed, the value
stat\_failed\_image.

### ARGUMENTS

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

### EXAMPLE

Sample program:

```
   program demo_atomic_cas
   use iso_fortran_env
   implicit none
   logical(atomic_logical_kind) :: atom[*], prev
      call atomic_cas(atom[1], prev, .false., .true.)
   end program demo_atomic_cas
```

### STANDARD

TS 18508 or later

### CLASS

Atomic subroutine

### SEE ALSO

__atomic\_define__(3), __atomic\_ref__(3), __iso\_fortran\_env__(3)
