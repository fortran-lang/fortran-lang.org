---
layout: book
title: "null"
permalink: /learn/intrinsics/NULL
---
## __Name__

__null__(3) - \[TRANSFORMATIONAL\] Function that returns a disassociated pointer
(GFDL)

## __Syntax__
```fortran
ptr => null(mold)

```
## __Description__

Returns a disassociated pointer.

If __mold__ is present, a disassociated pointer of the same type is
returned, otherwise the type is determined by context.

In _Fortran 95_, __mold__ is optional. Please note that _Fortran 2003_ includes cases where it is required.

## __Arguments__

  - __mold__
    : (Optional) shall be a pointer of any association status and of any
    type.

## __Returns__

A disassociated pointer or an unallocated allocatable entity.

## __Examples__

Sample program:

```fortran
!program demo_null
module showit
implicit none
private
character(len=*),parameter :: g='(*(g0,1x))'
public gen
! a generic interface that only differs in the 
! type of the pointer the second argument is
interface gen
 module procedure s1
 module procedure s2
end interface

contains

subroutine s1 (j, pi)
 integer j
 integer, pointer :: pi
   if(associated(pi))then
      write(*,g)'Two integers in S1:,',j,'and',pi
   else
      write(*,g)'One integer in S1:,',j
   endif
end subroutine s1

subroutine s2 (k, pr)
 integer k
 real, pointer :: pr
   if(associated(pr))then
      write(*,g)'integer and real in S2:,',k,'and',pr
   else
      write(*,g)'One integer in S2:,',k
   endif
end subroutine s2

end module showit

use showit, only : gen

real,target :: x = 200.0
integer,target :: i = 100

real, pointer :: real_ptr
integer, pointer :: integer_ptr

! so how do we call S1() or S2() with a disassociated pointer?

! the answer is the null() function with a mold value

! since s1() and s2() both have a first integer
! argument the NULL() pointer must be associated
! to a real or integer type via the mold option
! so the following can distinguish whether s1(1)
! or s2() is called, even though the pointers are
! not associated or defined
 
call gen (1, null (real_ptr) )    ! invokes s2
call gen (2, null (integer_ptr) ) ! invokes s1
real_ptr => x
integer_ptr => i
call gen (3, real_ptr ) ! invokes s2
call gen (4, integer_ptr ) ! invokes s1

end
!end program demo_null
```
  Results:
```text
   One integer in S2:, 1
   One integer in S1:, 2
   integer and real in S2:, 3 and 200.000000
   Two integers in S1:, 4 and 100
```
## __Standard__

Fortran 95 and later

## __See Also__

[__associated__(3)](ASSOCIATED)

###### fortran-lang intrinsic descriptions
