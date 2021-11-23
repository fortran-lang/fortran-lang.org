---
layout: book
title: atomic_ref
permalink: /learn/intrinsics/ATOMIC_REF
---
### NAME

**atomic\_ref**(3f) - \[ATOMIC\] Obtaining the value of a variable atomically
(GFDL)

### SYNTAX

call **atomic\_ref**(value, atom \[, stat\])

### DESCRIPTION

**atomic\_ref**(VALUE, ATOM ) atomically assigns the value of the
variable ATOM to VALUE. When STAT is present and the invocation was
successful, it is assigned the value 0. If it is present and the
invocation has failed, it is assigned a positive value; in particular,
for a coindexed ATOM, if the remote image has stopped, it is assigned
the value of iso\_fortran\_env's STAT\_STOPPED\_IMAGE and if the remote
image has failed, the value STAT\_FAILED\_IMAGE.

### ARGUMENTS

  - **VALUE**
    Scalar of the same type as ATOM. If the kind is different, the value
    is converted to the kind of ATOM.

  - **ATOM**
    Scalar coarray or coindexed variable of either integer type with
    atomic\_int\_kind kind or logical type with atomic\_logical\_kind
    kind.

  - **STAT**
    (optional) Scalar default-kind integer variable.

### EXAMPLE

Sample program:

````
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
````

### STANDARD

Fortran 2008 and later; with STAT, TS 18508 or later

### CLASS

Atomic subroutine

### SEE ALSO

**atomic\_define**(3), **atomic\_cas**(3), **iso\_fortran\_env**(3),
**atomic\_fetch\_add**(3), **atomic\_fetch\_and**(3),
**atomic\_fetch\_or**(3), **atomic\_fetch\_xor**(3)
