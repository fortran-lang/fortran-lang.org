---
layout: book
title: atomic_define
permalink: /learn/intrinsics/ATOMIC_DEFINE
---
### NAME

__atomic\_define__(3f) - \[ATOMIC\] Setting a variable atomically
(GFDL)

### SYNTAX

call atomic\_define (atom, value \[, stat\])

### DESCRIPTION

__atomic\_define__(atom, value) defines the variable ATOM with the value
VALUE atomically. When STAT is present and the invocation was
successful, it is assigned the value 0. If it is present and the
invocation has failed, it is assigned a positive value; in particular,
for a coindexed ATOM, if the remote image has stopped, it is assigned
the value of iso\_fortran\_env's stat\_stopped\_image and if the remote
image has failed, the value stat\_failed\_image.

### ARGUMENTS

  - __ATOM__
    Scalar coarray or coindexed variable of either integer type with
    atomic\_int\_kind kind or logical type with atomic\_logical\_kind
    kind.

  - __VALUE__
    Scalar of the same type as ATOM. If the kind is different, the value
    is converted to the kind of ATOM.

  - __STAT__
    (optional) Scalar default-kind integer variable.

### EXAMPLE

Sample program:

```
   program demo_atomic_define
   use iso_fortran_env
   implicit none
   integer(atomic_int_kind) :: atom[*]
      call atomic_define(atom[1], this_image())
   end program demo_atomic_define
```

### STANDARD

Fortran 2008 and later; with STAT, TS 18508 or later

### CLASS

Atomic subroutine

### SEE ALSO

__atomic\_ref__(3), __atomic\_cas__(3), __iso\_fortran\_env__(3),
__atomic\_add__(3), __atomic\_and__(3), __atomic\_or__(3),
__atomic\_xor__(3)
