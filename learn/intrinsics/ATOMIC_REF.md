---
layout: book
title: atomic_ref
permalink: /learn/intrinsics/ATOMIC_REF
---
#### NAME

__atomic\_ref__(3f) - \[ATOMIC\] Obtaining the value of a variable atomically
(GFDL)

#### SYNTAX

call __atomic\_ref__(value, atom \[, stat\])

#### DESCRIPTION

__atomic\_ref__(VALUE, ATOM ) atomically assigns the value of the
variable ATOM to VALUE. When STAT is present and the invocation was
successful, it is assigned the value 0. If it is present and the
invocation has failed, it is assigned a positive value; in particular,
for a coindexed ATOM, if the remote image has stopped, it is assigned
the value of iso\_fortran\_env's STAT\_STOPPED\_IMAGE and if the remote
image has failed, the value STAT\_FAILED\_IMAGE.

#### ARGUMENTS

  - __VALUE__
    Scalar of the same type as ATOM. If the kind is different, the value
    is converted to the kind of ATOM.

  - __ATOM__
    Scalar coarray or coindexed variable of either integer type with
    atomic\_int\_kind kind or logical type with atomic\_logical\_kind
    kind.

  - __STAT__
    (optional) Scalar default-kind integer variable.

#### EXAMPLE

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

#### STANDARD

Fortran 2008 and later; with STAT, TS 18508 or later

#### CLASS

Atomic subroutine

#### SEE ALSO

__atomic\_define__(3), __atomic\_cas__(3), __iso\_fortran\_env__(3),
__atomic\_fetch\_add__(3), __atomic\_fetch\_and__(3),
__atomic\_fetch\_or__(3), __atomic\_fetch\_xor__(3)
