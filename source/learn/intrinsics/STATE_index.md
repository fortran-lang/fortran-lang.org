# STATE_index
### General and miscellaneous intrinsics

# ASSOCIATED
## __Name__

__associated__(3) - \[STATE\] Status of a pointer or pointer/target pair


## __Syntax__
```fortran
result = associated(pointer, target)
```
## __Description__

__associated(pointer \[, target\])__ determines the status of the
pointer __pointer__ or if __pointer__ is associated with the target __target__.

## __Arguments__

  - __pointer__
    : __pointer__ shall have the _pointer_ attribute and it can be of any type.

  - __target__
    : (Optional) __target__ shall be a pointer or a target. It must have the
    same type, kind type parameter, and array rank as __pointer__.

The association status of neither __pointer__ nor __target__ shall be undefined.

## __Returns__

__associated(pointer)__ returns a scalar value of type _logical_.
There are several cases:

1.  When the optional __target__ is not present then __associated(pointer)__
    is true if __pointer__ is associated with a target; otherwise, it
    returns false.

2.  If __target__ is present and a scalar target, the result is true if
    __target__ is not a zero-sized storage sequence and the target
    associated with __pointer__ occupies the same storage units. If __pointer__
    is disassociated, the result is false.

3.  If __target__ is present and an array target, the result is true if
    __target__ and __pointer__ have the same shape, are not zero-sized arrays,
    are arrays whose elements are not zero-sized storage sequences, and
    __target__ and __pointer__ occupy the same storage units in array element
    order.

    As in case 2, the result is false, if __pointer__ is disassociated.

4.  If __target__ is present and an scalar pointer, the result is true if
    __target__ is associated with __pointer__, the target associated with __target__
    are not zero-sized storage sequences and occupy the same storage
    units.

    The result is __.false.__, if either __target__ or __pointer__ is disassociated.

5.  If __target__ is present and an array pointer, the result is true if
    target associated with __pointer__ and the target associated with __target__
    have the same shape, are not zero-sized arrays, are arrays whose
    elements are not zero-sized storage sequences, and __target__ and
    __pointer__ occupy the same storage units in array element order. The
    result is false, if either __target__ or __pointer__ is disassociated.

## __Examples__

Sample program:

```fortran
program demo_associated
implicit none
real, target  :: tgt(2) = [1., 2.]
real, pointer :: ptr(:)
   ptr => tgt
   if (associated(ptr)     .eqv. .false.) &
   & stop 'POINTER NOT ASSOCIATED'
   if (associated(ptr,tgt) .eqv. .false.) &
   & stop 'POINTER NOT ASSOCIATED TO TARGET'
end program demo_associated
```
## __Standard__

Fortran 95 and later

## __See Also__

[__null__(3)](NULL)

###### fortran-lang intrinsic descriptions
# EXTENDS_TYPE_OF
## __Name__

__extends\_type\_of__(3) - \[STATE\] determine if the dynamic type of __a__ is an extension of the dynamic type of __mold__.


## __Syntax__
```fortran
result=extends_type_of(a, mold)
```
## __Description__

__extends\_type\_of__(3) is __.true.__ if and only if the dynamic type of __a__
is an extension of the dynamic type of __mold__.

## __Options__

  - __a__
    : shall be an object of extensible type. If it is a pointer, it
    shall not have an undefined association status.

  - __mold__
    : shall be an object of extensible type. If it is a pointer, it
    shall not have an undefined association status.

## __Returns__

  - __result__
    : Default logical scalar.

  - __value__
    : If __mold__ is unlimited polymorphic and is either a disassociated
    pointer or unallocated allocatable variable, the result is
    true; otherwise if __a__ is unlimited polymorphic and is either a
    disassociated pointer or unallocated allocatable variable, the result
    is false; otherwise the result is true if and only if the dynamic
    type of __a__ is an extension type of the dynamic type of __mold__.

    The dynamic type of a disassociated pointer or unallocated
    allocatable variable is its declared type.

## __Examples__

###### fortran-lang intrinsic descriptions
# IS_IOSTAT_END
## __Name__

__is\_iostat\_end__(3) - \[STATE\] Test for end-of-file value


## __Syntax__
```fortran
function is_iostat_end(i)

    logical function   :: is_iostat_end (i) result(yesno)
    integer,intent(in) :: i
```
## __Description__

is\_iostat\_end(3) tests whether a variable (assumed returned as a status
from an I/O statement) has the "end of file" I/O status value. 

The function is equivalent to comparing the variable with the
__iostat\_end__ parameter of the intrinsic module __iso\_fortran\_env__.

## __Arguments__

  - __i__
    : An _integer_ status value to test if indicating end of file.

## __Returns__

Returns a _logical_ of the default kind, __.true.__ if __i__ has the value
which indicates an end of file condition for __iostat=__ specifiers, and is
__.false.__ otherwise.

## __Examples__

Sample program:

```fortran
program demo_iostat
implicit none
real               :: value
integer            :: ios
character(len=256) :: message
   write(*,*)'Begin entering numeric values, one per line'
   do
      read(*,*,iostat=ios,iomsg=message)value
      if(ios.eq.0)then
         write(*,*)'VALUE=',value
      elseif( is_iostat_end(ios) ) then
         stop 'end of file. Goodbye!'
      else
         write(*,*)'ERROR:',ios,trim(message)
      endif
      !
   enddo
end program demo_iostat
```

## __Standard__

Fortran 2003 and later

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# IS_IOSTAT_EOR
## __Name__

__is\_iostat\_eor__(3) - \[STATE\] Test for end-of-record value


## __Syntax__
```fortran
result = is_iostat_eor(i)
```
## __Description__

is\_iostat\_eor tests whether an variable has the value of the I/O
status "end of record". The function is equivalent to comparing the
variable with the iostat\_eor parameter of the intrinsic module
__iso\_fortran\_env__.

## __Arguments__

  - __i__
    : Shall be of the type _integer_.

## __Returns__

Returns a _logical_ of the default kind, which .true. if __i__ has the value
which indicates an end of file condition for iostat= specifiers, and is
.false. otherwise.

## __Examples__

Sample program:

```fortran
program demo_is_iostat_eor
implicit none
integer :: stat, i(50)

  open(88, file='test.dat', form='unformatted')
  read(88, iostat=stat) i

  if(is_iostat_eor(stat)) stop 'end of record'

end program demo_is_iostat_eor
```

## __Standard__

Fortran 2003 and later

###### fortran-lang intrinsic descriptions
# MOVE_ALLOC
## __Name__

__move\_alloc__(3) - \[\] Move allocation from one object to another


## __Syntax__
```fortran
call move_alloc(src, dest)
```
## __Description__

__move\_alloc(src, dest)__ moves the allocation from SRC to DEST. SRC
will become deallocated in the process.

## __Arguments__

  - __src__
    : allocatable, __intent(inout)__, may be of any type and kind.

  - __dest__
    : allocatable, __intent(out)__, shall be of the same type, kind and
    rank as SRC.

## __Examples__

Basic Sample program to allocate a bigger grid

```fortran
program demo_move_alloc
implicit none
! Example to allocate a bigger GRID
real, allocatable :: grid(:), tempgrid(:)
integer :: n, i

   ! initialize small GRID
   n = 3
   allocate (grid(1:n))
   grid = [ (real (i), i=1,n) ]

   ! initialize TEMPGRID which will be used to replace GRID
   allocate (tempgrid(1:2*n))    ! Allocate bigger grid
   tempgrid(::2)  = grid         ! Distribute values to new locations
   tempgrid(2::2) = grid + 0.5   ! initialize other values

   ! move TEMPGRID to GRID
   call MOVE_ALLOC (from=tempgrid, to=grid)

   ! TEMPGRID should no longer be allocated
   ! and GRID should be the size TEMPGRID was
   if (size (grid) /= 2*n .or. allocated (tempgrid)) then
      print *, "Failure in move_alloc!"
   endif
   print *, allocated(grid), allocated(tempgrid)
   print '(99f8.3)', grid
end program demo_move_alloc
```
  Results:
```text
    T F
      1.000   1.500   2.000   2.500   3.000   3.500
```
## __Standard__

Fortran 2003 and later

## __See Also__

[__allocated__(3)](ALLOCATED)

###### fortran-lang intrinsic descriptions
# PRESENT
## __Name__

__present__(3) - [STATE\] Determine whether an optional dummy argument
                 is specified

## __Syntax__
```fortran
result = present(a)

   function present (a)
   logical :: present
```
## __Description__

Determines whether an optional dummy argument is present.

## __Arguments__

  - __a__
    : May be of any type and may be a pointer, scalar or array value,
    or a dummy procedure. It shall be the name of an optional dummy
    argument accessible within the current subroutine or function.

## __Returns__

Returns either __.true.__ if the optional argument __a__ is present,
or __.false.__ otherwise.

## __Examples__

Sample program:

```fortran
program demo_present
implicit none
   write(*,*) func(), func(42)
contains

integer function func(x)
integer, intent(in), optional :: x
   if(present(x))then
     func=x**2
   else
     func=0
   endif
end function

end program demo_present
```
  Results:
```text
     0        1764
```

## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# SAME_TYPE_AS
## __Name__

__same\_type\_as__(3) - \[STATE\] Query dynamic types for equality


## __Syntax__
```fortran
result = same_type_as(a, b)
```
## __Description__

Query dynamic types for equality.

## __Arguments__

  - __a__
    : Shall be an object of extensible declared type or unlimited
    polymorphic.

  - __b__
    : Shall be an object of extensible declared type or unlimited
    polymorphic.

## __Returns__

The return value is a scalar of type default logical. It is true if and
only if the dynamic type of __a__ is the same as the dynamic type of __b__.

## __Standard__

Fortran 2003 and later

## __See Also__

[__extends\_type\_of__(3)](EXTENDS_TYPE_OF)

###### fortran-lang intrinsic descriptions
