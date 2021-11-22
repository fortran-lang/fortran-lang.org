---
layout: book
title: atomic_or
permalink: /learn/intrinsics/f_atomic_or
---
### NAME

**atomic\_or**(3f) - \[ATOMIC:BIT MANIPULATION\]
Atomic bitwise OR operation

### SYNTAX

call **atomic\_or**(atom, value \[, stat\])

### DESCRIPTION

**atomic\_or**(atom, value) atomically defines ATOM with the bitwise OR
between the values of ATOM and VALUE. When STAT is present and the
invocation was successful, it is assigned the value 0. If it is present
and the invocation has failed, it is assigned a positive value; in
particular, for a coindexed ATOM, if the remote image has stopped, it is
assigned the value of iso\_fortran\_env's stat\_stopped\_image and if
the remote image has failed, the value stat\_failed\_image.

### ARGUMENTS

  - **ATOM**
    Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind.

  - **VALUE**
    Scalar of the same type as ATOM. If the kind is different, the value
    is converted to the kind of ATOM.

  - **STAT**
    (optional) Scalar default-kind integer variable.

### EXAMPLE

Sample program:

```
   program demo_atomic_or
   use iso_fortran_env
   implicit none
   integer(atomic_int_kind) :: atom[*]
      call atomic_or(atom[1], int(b'10100011101'))
   end program demo_atomic_or
```

### STANDARD

TS 18508 or later

### CLASS

Atomic subroutine

### SEE ALSO

**atomic\_define**(3), **atomic\_fetch\_or**(3),
**iso\_fortran\_env**(3), **atomic\_add**(3), **atomic\_or**(3),
**atomic\_xor**(3)
