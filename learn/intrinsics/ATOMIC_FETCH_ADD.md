---
layout: book
title: atomic_fetch_add
permalink: /learn/intrinsics/ATOMIC_FETCH_ADD
---
### NAME

**atomic\_fetch\_add**(3f) - \[ATOMIC\] Atomic ADD operation with prior fetch
(GFDL)

### SYNTAX

call **atomic\_fetch\_add**(atom, value, old \[, stat\])

### DESCRIPTION

**atomic\_fetch\_add**(atom, value, old) atomically stores the value of
ATOM in OLD and adds the value of VAR to the variable ATOM. When STAT is
present and the invocation was successful, it is assigned the value 0.
If it is present and the invocation has failed, it is assigned a
positive value; in particular, for a coindexed ATOM, if the remote image
has stopped, it is assigned the value of iso\_fortran\_env's
stat\_stopped\_image and if the remote image has failed, the value
stat\_failed\_image.

### ARGUMENTS

  - **ATOM**
    Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind. atomic\_logical\_kind kind.

  - **VALUE**
    Scalar of the same type as ATOM. If the kind is different, the value
    is converted to the kind of ATOM.

  - **OLD**
    Scalar of the same type and kind as ATOM.

  - **STAT**
    (optional) Scalar default-kind integer variable.

### EXAMPLE

Sample program:

```
   program demo_atomic_fetch_add
   use iso_fortran_env
   implicit none
   integer(atomic_int_kind) :: atom[*], old
      call atomic_add(atom[1], this_image(), old)
   end program demo_atomic_fetch_add
```

### STANDARD

TS 18508 or later

### CLASS

Atomic subroutine

### SEE ALSO

**atomic\_define**(3), **atomic\_add**(3), **iso\_fortran\_env**(3),
**atomic\_fetch\_and**(3), **atomic\_fetch\_or**(3),
**atomic\_fetch\_xor**(3)
