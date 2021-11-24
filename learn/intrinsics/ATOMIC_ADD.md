---
layout: book
title: atomic_add
permalink: /learn/intrinsics/ATOMIC_ADD
---
#### NAME

__atomic\_add__(3f) - \[ATOMIC\] Atomic ADD operation (GFDL)

#### SYNTAX

call atomic\_add (atom, value \[, stat\])

#### DESCRIPTION

__atomic\_add__(atom, value) atomically adds the value of VAR to the
variable ATOM. When STAT is present and the invocation was successful,
it is assigned the value 0. If it is present and the invocation has
failed, it is assigned a positive value; in particular, for a coindexed
ATOM, if the remote image has stopped, it is assigned the value of
iso\_fortran\_env's stat\_stopped\_image and if the remote image has
failed, the value stat\_failed\_image.

#### ARGUMENTS

  - __ATOM__
    Scalar coarray or coindexed variable of integer type with
    atomic\_int\_kind kind.

  - __VALUE__
    Scalar of the same type as ATOM. If the kind is different, the value
    is converted to the kind of ATOM.

  - __STAT__
    (optional) Scalar default-kind integer variable.

#### EXAMPLE

Sample program:

```
   program demo_atomic_add
   use iso_fortran_env
   implicit none
   integer(atomic_int_kind) :: atom[*]
      call atomic_add (atom[1], this_image())
   end program demo_atomic_add
```

#### STANDARD

TS 18508 or later

#### CLASS

Atomic subroutine

#### SEE ALSO

__atomic\_define__(3), __atomic\_fetch\_add__(3),
__iso\_fortran\_env__(3), __atomic\_and__(3), __atomic\_or__(3),
__atomic\_xor__(3)
