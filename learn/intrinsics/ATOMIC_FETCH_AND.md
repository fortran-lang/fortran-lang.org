---
layout: book
title: atomic_fetch_and
permalink: /learn/intrinsics/ATOMIC_FETCH_AND
---
### NAME

**atomic\_fetch\_and**(3f) - \[ATOMIC:BIT MANIPULATION\] Atomic bitwise AND operation with prior fetch
(GFDL)

### SYNTAX

call **atomic\_fetch\_and**(atom, value, old \[, stat\])

### DESCRIPTION

**atomic\_fetch\_and**(atom, value, old) atomically stores the value of
ATOM in OLD and defines ATOM with the bitwise AND between the values of
ATOM and VALUE. When STAT is present and the invocation was successful,
it is assigned the value 0. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
ATOM, if the remote image has stopped, it is assigned the value of
iso\_fortran\_env's stat\_stopped\_image and if the remote image has
failed, the value stat\_failed\_image.

### ARGUMENTS

  - **ATOM**
    Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind.

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
   program demo_atomic_fetch_and
   use iso_fortran_env
   implicit none
   integer(atomic_int_kind) :: atom[*], old
      call atomic_fetch_and (atom[1], int(b'10100011101'), old)
   end program demo_atomic_fetch_and
```

### STANDARD

TS 18508 or later

### CLASS

Atomic subroutine

### SEE ALSO

**atomic\_define**(3), **atomic\_and**(3), **iso\_fortran\_env**(3),
**atomic\_fetch\_add**(3), **atomic\_fetch\_or**(3),
**atomic\_fetch\_xor**(3)
