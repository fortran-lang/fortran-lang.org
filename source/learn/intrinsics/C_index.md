# C_index
### procedures useful for binding to C interfaces

# C_ASSOCIATED
## __Name__

__c\_associated__(3) - \[ISO\_C\_BINDING\] Status of a C pointer


## __Syntax__
```fortran
result = c_associated(c_prt_1, c_ptr_2)
```
## __Description__

__c\_associated(c\_prt\_1\[, c\_ptr\_2\])__ determines the status of the
C pointer c\_ptr\_1 or if c\_ptr\_1 is associated with the target
c\_ptr\_2.

## __Arguments__

  - __c\_ptr\_1__
    : Scalar of the type c\_ptr or c\_funptr.

  - __c\_ptr\_2__
    : (Optional) Scalar of the same type as c\_ptr\_1.

## __Returns__

The return value is of type _logical_; it is .false. if either c\_ptr\_1
is a C NULL pointer or if c\_ptr1 and c\_ptr\_2 point to different
addresses.

## __Examples__

Sample program:

```fortran
program demo_c_associated

contains

subroutine association_test(a,b)
use iso_c_binding, only: c_associated, c_loc, c_ptr
implicit none
real, pointer :: a
type(c_ptr) :: b
   if(c_associated(b, c_loc(a))) &
      stop 'b and a do not point to same target'
end subroutine association_test

end program demo_c_associated
```

## __Standard__

Fortran 2003 and later

## __See Also__

[__c\_loc__(3)](C_LOC),
[__c\_funloc__(3)](C_FUNLOC),
__iso\_c\_binding__(3)

###### fortran-lang intrinsic descriptions
# C_F_POINTER
## __Name__

__c\_f\_pointer__(3) - \[ISO\_C\_BINDING\] Convert C into Fortran pointer


## __Syntax__
```fortran
call c_f_pointer(cptr, fptr, shape)
```
## __Description__

__c\_f\_pointer(cptr, fptr\[, shape\])__ Assign the target, the C
pointer, __cptr__ to the Fortran pointer __fptr__ and specify its shape.

## __Arguments__

  - __cptr__
    : scalar of the type c\_ptr. It is __intent(in)__.

  - __fptr__
    : pointer interoperable with __cptr__. it is __intent(out)__.

  - __shape__
    : (Optional) Rank-one array of type _integer_ with __intent(in)__ .
    It shall be present if and only if __fptr__ is an array. The size
    must be equal to the rank of __fptr__.

## __Examples__

Sample program:

```fortran
program demo_c_f_pointer
use iso_c_binding
implicit none
interface
   subroutine my_routine(p) bind(c,name='myC_func')
      import :: c_ptr
      type(c_ptr), intent(out) :: p
   end subroutine
end interface
type(c_ptr) :: cptr
real,pointer :: a(:)
   call my_routine(cptr)
   call c_f_pointer(cptr, a, [12])
end program demo_c_f_pointer
```

## __Standard__

Fortran 2003 and later

## __See Also__

[__c\_loc__(3)](C_LOC),
[__c\_f\_procpointer__(3)](C_F_PROCPOINTER),
__iso\_c\_binding__(3)

###### fortran-lang intrinsic descriptions
# C_F_PROCPOINTER
## __Name__

__c\_f\_procpointer__(3) - \[ISO\_C\_BINDING\] Convert C into Fortran procedure pointer


## __Syntax__
```fortran
call c_f_procpointer(cptr, fptr)
```
## __Description__

__c\_f\_procpointer(cptr, fptr)__ assigns the target of the C function
pointer __cptr__ to the Fortran procedure pointer __fptr__.

## __Arguments__

  - __cptr__
    : scalar of the type c\_funptr. It is __intent(in)__.

  - __fptr__
    : procedure pointer interoperable with __cptr__. It is __intent(out)__.

## __Examples__

Sample program:

```fortran
program demo_c_f_procpointer
use iso_c_binding
implicit none
abstract interface
   function func(a)
   import :: c_float
   real(c_float), intent(in) :: a
   real(c_float) :: func
   end function
end interface
interface
   function getIterFunc() bind(c,name="getIterFunc")
   import :: c_funptr
   type(c_funptr) :: getIterFunc
   end function
end interface
type(c_funptr) :: cfunptr
procedure(func), pointer :: myFunc
   cfunptr = getIterFunc()
   call c_f_procpointer(cfunptr, myFunc)
end program demo_c_f_procpointer
```

## __Standard__

Fortran 2003 and later

## __See Also__

[__c\_loc__(3)](C_LOC),
[__c\_f\_pointer__(3)](C_F_POINTER),
__iso\_c\_binding__(3)

###### fortran-lang intrinsic descriptions
# C_FUNLOC
## __Name__

__c\_funloc__(3) - \[ISO\_C\_BINDING\] Obtain the C address of a procedure


## __Syntax__
```fortran
result = c_funloc(x)
```
## __Description__

__c\_funloc(x)__ determines the C address of the argument.

## __Arguments__

  - __x__
    : Interoperable function or pointer to such function.

## __Returns__

The return value is of type c\_funptr and contains the C address of the
argument.

## __Examples__

Sample program:

```fortran
! program demo_c_funloc and module
module x
use iso_c_binding
implicit none
contains
subroutine sub(a) bind(c)
real(c_float) :: a
   a = sqrt(a)+5.0
end subroutine sub
end module x
!
program demo_c_funloc
use iso_c_binding
use x
implicit none
interface
   subroutine my_routine(p) bind(c,name='myC_func')
     import :: c_funptr
     type(c_funptr), intent(in) :: p
   end subroutine
end interface
   call my_routine(c_funloc(sub))
!
end program demo_c_funloc
```

## __Standard__

Fortran 2003 and later

## __See Also__

[__c\_associated__(3)](C_ASSOCIATED),
[__c\_loc__(3)](C_LOC),
[__c\_f\_pointer__(3)](C_F_POINTER),

[__c\_f\_procpointer__(3)](C_F_PROCPOINTER),
__iso\_c\_binding__(3)

###### fortran-lang intrinsic descriptions
# C_LOC
## __Name__

__c\_loc__(3) - \[ISO\_C\_BINDING\] Obtain the C address of an object


## __Syntax__
```fortran
result = c_loc(x)
```
## __Description__

__c\_loc(x)__ determines the C address of the argument.

## __Arguments__

  - __x__
    : Shall have either the _pointer_ or _target_ attribute. It shall not be a
    coindexed object. It shall either be a variable with interoperable
    type and kind type parameters, or be a scalar, nonpolymorphic
    variable with no length type parameters.

## __Returns__

The return value is of type c\_ptr and contains the C address of the
argument.

## __Examples__

Sample program:

```fortran
   subroutine association_test(a,b)
   use iso_c_binding, only: c_associated, c_loc, c_ptr
   implicit none
   real, pointer :: a
   type(c_ptr) :: b
     if(c_associated(b, c_loc(a))) &
        stop 'b and a do not point to same target'
   end subroutine association_test
```

## __Standard__

Fortran 2003 and later

## __See Also__

[__c\_associated__(3)](C_ASSOCIATED),
[__c\_funloc__(3)](C_FUNLOC),
[__c\_f\_pointer__(3)](C_F_POINTER),

[__c\_f\_procpointer__(3)](C_F_PROCPOINTER),
__iso\_c\_binding__(3)

###### fortran-lang intrinsic descriptions
# C_SIZEOF
## __Name__

__c\_sizeof__(3) - \[ISO\_C\_BINDING\] Size in bytes of an expression


## __Syntax__
```fortran
n = c_sizeof(x)
```
## __Description__

__c\_sizeof(x)__ calculates the number of bytes of storage the
expression __x__ occupies.

## __Arguments__

  - __x__
    : The argument shall be an interoperable data entity.

## __Returns__

The return value is of type integer and of the system-dependent kind
c\_size\_t (from the *iso\_c\_binding* module). Its value is the
number of bytes occupied by the argument. If the argument has the
_pointer_ attribute, the number of bytes of the storage area pointed to is
returned. If the argument is of a derived type with _pointer_ or
_allocatable_ components, the return value does not account for the sizes
of the data pointed to by these components.

## __Examples__

Sample program:

```fortran
program demo_c_sizeof
use iso_c_binding
implicit none
real(c_float) :: r, s(5)
   print *, (c_sizeof(s)/c_sizeof(r) == 5)
end program demo_c_sizeof
```
  Results:
```text
    T
```
The example will print .true. unless you are using a platform where
default _real_ variables are unusually padded.

## __Standard__

Fortran 2008

## __See Also__

[__storage\_size__(3)](STORAGE_SIZE)

###### fortran-lang intrinsic descriptions
