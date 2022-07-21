# ARRAY_index
### Properties and attributes of arrays

# MERGE
## __Name__

__merge__(3) - \[ARRAY CONSTRUCTION\] Merge variables

## __Syntax__
```fortran
result = merge(tsource, fsource, mask)
```
## __Description__

The elemental function __merge__(3) selects values from two arrays or
scalars according to a logical mask. The result is equal to an element
of __tsource__ where the corresponding element of __mask__ is __.true.__, or an
element of __fsource__ when it is .false. .

Multi-dimensional arrays are supported.

Note that argument expressions to __merge__(3) are not required to be
short-circuited so (as an example) if the array __x__ contains zero values
in the statement below the standard does not prevent floating point
divide by zero being generated; as __1.0/x__ may be evaluated for all values
of __x__ before the mask is used to select which value to retain:

```fortran
      y = merge( 1.0/x, 0.0, x /= 0.0 )
```

Note the compiler is also free to short-circuit or to generate an
infinity so this may work in many programming environments but is not
recommended.

For cases like this one may instead use masked assignment via the __where__
construct:

```fortran
      where(x .ne. 0.0)
         y = 1.0/x
      elsewhere
         y = 0.0
      endwhere
```

instead of the more obscure

```fortran
      merge(1.0/merge(x,1.0,x /= 0.0), 0.0, x /= 0.0)
```

## __Arguments__

  - __tsource__
    : May be of any type, including user-defined.

  - __fsource__
    : Shall be of the same type and type parameters as __tsource__.

  - __mask__
    : Shall be of type _logical_.

Note that (currently) _character_ values must be of the same length.

## __Returns__

The result is of the same type and type parameters as __tsource__. For any
element the result is __tsource__ if __mask__ is true and __fsource__ otherwise.

## __Examples__

The value of

```fortran
     merge (1.0, 0.0, k > 0)
```

is 1.0 for K=5 and 0.0 for K=__-2__.

```fortran
program demo_merge
implicit none
integer :: tvals(2,3), fvals(2,3), answer(2,3)
logical :: mask(2,3)
integer :: i
logical :: chooseleft

   tvals(1,:)=[  10, -60,  50 ]
   tvals(2,:)=[ -20,  40, -60 ]

   fvals(1,:)=[ 0, 3, 2 ]
   fvals(2,:)=[ 7, 4, 8 ]

   mask(1,:)=[ .true.,  .false., .true. ]
   mask(2,:)=[ .false., .false., .true. ]

   write(*,*)'mask of logicals'
   answer=merge( tvals, fvals, mask )
   call printme()

   write(*, *)'highest values'
   answer=merge( tvals, fvals, tvals > fvals )
   call printme()

   write(*, *)'lowest values'
   answer=merge( tvals, fvals, tvals < fvals )
   call printme()

   write(*, *)'zero out negative values'
   answer=merge( tvals, 0, tvals < 0)
   call printme()

   write(*, *)'binary choice'
   chooseleft=.false.
   write(*, '(3i4)')merge([1,2,3],[10,20,30],chooseleft)
   chooseleft=.true.
   write(*, '(3i4)')merge([1,2,3],[10,20,30],chooseleft)

contains

subroutine printme()
      write(*, '(3i4)')(answer(i, :), i=1, size(answer, dim=1))
end subroutine printme

end program demo_merge
```
Expected Results:
```
    mask of logicals
     10   3  50
      7   4 -60
    highest values
     10   3  50
      7  40   8
    lowest values
      0 -60   2
    -20   4 -60
    zero out negative values
      0 -60   0
    -20   0 -60
    binary choice
     10  20  30
      1   2   3
```

## __Standard__

Fortran 95 and later

## __See Also__

[__pack__(3)](PACK),
[__unpack__(3)](UNPACK),
[__pack__(3)](PACK),
[__spread__(3)](SPREAD),
[__unpack__(3)](UNPACK)   

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# PACK
## __Name__

__pack__(3) - \[ARRAY CONSTRUCTION\] Pack an array into an array of rank one

## __Syntax__
```fortran
result = pack(array, mask,vector)

   TYPE(kind=KIND) function pack(array,mask,vector)
   TYPE(kind=KIND),option(in) :: array(*)
   logical  :: mask(*) 
   TYPE(kind=KIND),option(in),optional :: vector(*)
```
   where TYPE(kind=KIND) may be any type, where __array__ and __vector__
   and the returned value must by of the same type. __mask__ may be a
   scalar as well an an array.

## __Description__

Stores the elements of ARRAY in an array of rank one.

The beginning of the resulting array is made up of elements whose __mask__
equals __.true.__. Afterwards, positions are filled with elements taken from
__vector__.

## __Arguments__

  - __array__
    : Shall be an array of any type.

  - __mask__
    : Shall be an array of type _logical_ and of the same size as __array__.
    Alternatively, it may be a _logical_ scalar.

  - __vector__
    : (Optional) shall be an array of the same type as __array__ and of rank
    one. If present, the number of elements in __vector__ shall be equal to
    or greater than the number of true elements in __mask__. If __mask__ is
    scalar, the number of elements in __vector__ shall be equal to or
    greater than the number of elements in __array__.

## __Returns__

The result is an array of rank one and the same type as that of __array__.
If __vector__ is present, the result size is that of __vector__, the number of
__.true.__ values in __mask__ otherwise.

## __Examples__

Sample program:

```fortran
program demo_pack
implicit none
   call test1()
   call test2()
   call test3()
contains
!
subroutine test1()
! gathering nonzero elements from an array:
integer :: m(6)

   m = [ 1, 0, 0, 0, 5, 0 ]
   write(*, fmt="(*(i0, ' '))") pack(m, m /= 0)  ! "1 5"

end subroutine test1
!
subroutine test2()
! Gathering nonzero elements from an array and appending elements
! from VECTOR till the size of the mask array (or array size if the
! mask is scalar):
integer :: m(4)

   m = [ 1, 0, 0, 2 ]
   write(*, fmt="(*(i0, ' '))") pack(m, m /= 0, [ 0, 0, 3, 4 ])

end subroutine test2
!
subroutine test3()
! select strings whose second character is "a"
character(len=10) :: m(4)

m = [ character(len=10) :: 'ape', 'bat', 'cat', 'dog']
   write(*, fmt="(*(g0, ' '))") pack(m, m(:)(2:2) == 'a' )

end subroutine test3
!
end program demo_pack
```
  Results:
```text
   1 5 
   1 2 3 4 
   bat        cat        
```

## __Standard__

Fortran 95 and later

## __See Also__

[__unpack__(3)](UNPACK),
[__merge__(3)](MERGE),
[__pack__(3)](PACK),
[__spread__(3)](SPREAD),
[__unpack__(3)](UNPACK)   

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# SPREAD
## __Name__

__spread__(3) - \[ARRAY CONSTRUCTION\] Add a dimension to an array


## __Syntax__
```fortran
result = spread(source, dim, ncopies)

  TYPE(kind=KIND) function spread(source, dim, ncopies)

   TYPE(kind=KIND)    :: source(..)
   integer,intent(in) :: dim
   integer,intent(in) :: ncopies
```
## __Description__

Replicates a __source__ array __ncopies__ times along a specified
dimension __dim__.

If SOURCE is scalar, the shape of the result is (MAX (NCOPIES, 0)).
and each element of the result has a value equal to SOURCE.

## __Arguments__

  - __source__
    : Shall be a scalar or an array of any type and a rank less than
    fifteen.

  - __dim__
    : Shall be a scalar of type _integer_ with a value in the range from
    __1__ to __n+1__, where __n__ equals the rank of __source__.

  - __ncopies__
    : Shall be a scalar of type _integer_.

## __Returns__

The result is an array of the same type as __source__ and has rank __n+1__
where __n__ equals the rank of __source__.


## __Examples__

Sample program:

```fortran
program demo_spread
implicit none
integer :: a = 1, b(2) = [ 1, 2 ]

   write(*,*) spread(a, 1, 2)            ! "1 1"
   write(*,*) spread(b, 1, 2)            ! "1 1 2 2"

end program demo_spread

program example_spread
!  Author:
!    John Burkardt, 03 July 2006
implicit none
     !
integer ( kind = 4 ) a1(4,3)
integer ( kind = 4 ) a2(3,4)
integer i
integer ( kind = 4 ) s
integer ( kind = 4 ) v(4)
     !
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) 'TEST_SPREAD'
     write ( *, '(a)' ) '  SPREAD is a FORTRAN90 function which replicates'
     write ( *, '(a)' ) '  an array by adding a dimension.'
     write ( *, '(a)' ) ' '
     !
     s = 99
     !
     write ( *, '(a,i6)' ) '  Suppose we have a scalar S = ', s
     write ( *, '(a)' ) ' '
     !
     v = spread ( s, 1, 4 )
     !
     write ( *, '(a)' ) '  V = spread ( s, 1, 4 )'
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  adds a new dimension (1) of extent 4'
     write ( *, '(a)' ) ' '
     write ( *, '(4i6)' ) v(1:4)
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  Now first reset V to (1,2,3,4)'
     v = [ 1, 2, 3, 4 ]
     !
     a1 = spread ( v, 2, 3 )
     !
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  A1 = spread ( v, 2, 3 )'
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  adds a new dimension (2) of extent 3'
     write ( *, '(a)' ) ' '
     do i = 1, 4
       write ( *, '(3i6)' ) a1(i,1:3)
     end do
     !
     a2 = spread ( v, 1, 3 )
     !
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  A2 = spread ( v, 1, 3 )'
     write ( *, '(a)' ) ' '
     write ( *, '(a)' ) '  adds a new dimension (1) of extent 3'
     write ( *, '(a)' ) ' '
     do i = 1, 3
       write ( *, '(4i6)' ) a2(i,1:4)
     end do
end program example_spread
```

## __Standard__

Fortran 95 and later

## __See Also__

[__pack__(3)](PACK),
[__unpack__(3)](UNPACK),
[__merge__(3)](MERGE),
[__pack__(3)](PACK),
[__unpack__(3)](UNPACK)   

###### fortran-lang intrinsic descriptions
# UNPACK
## __Name__

__unpack__(3) - \[ARRAY CONSTRUCTION\] Store the elements of a vector in an array of higher rank


## __Syntax__
```fortran
result = unpack(vector, mask, field)
```
## __Description__

Store the elements of __vector__ in an array of higher rank.

## __Arguments__

  - __vector__
    : Shall be an array of any type and rank one. It shall have at least
    as many elements as __mask__ has __.true.__ values.

  - __mask__
    : Shall be an array of type _logical_.

  - __field__
    : Shall be of the same type as __vector__ and have the same shape as __mask__.

## __Returns__

The resulting array corresponds to __field__ with __.true.__ elements of __mask__
replaced by values from __vector__ in array element order.

## __Examples__

Sample program:

```fortran
program demo_unpack
implicit none
integer :: vector(2)  = [1,1]
logical :: mask(4)  = [ .true., .false., .false., .true. ]
integer :: field(2,2) = 0, unity(2,2)

   ! result: unity matrix
   unity = unpack(vector, reshape(mask, [2,2]), field)
   write(*,*)unity,size(unity),shape(unity)

end program demo_unpack
```
  Results:
```text
              1           0           0           1           4
              2           2
```
## __Standard__

Fortran 95 and later

## __See Also__

[__pack__(3)](PACK),
[__merge__(3)](MERGE),
[__pack__(3)](PACK),
[__spread__(3)](SPREAD),
[__unpack__(3)](UNPACK)   

###### fortran-lang intrinsic descriptions
# ALLOCATED
## __Name__

__allocated__(3) - \[ARRAY INQUIRY\] Status of an allocatable entity


## __Syntax__
```fortran
  result = allocated(array)
```
   or
```fortran
  result = allocated(scalar)
```
## __Description__

__allocated(array)__ and __allocated(scalar)__ check the allocation
status of __array__ and __scalar__, respectively.

## __Arguments__

  - __array__
    : the argument shall be an _allocatable_ array.

  - __scalar__
    : the argument shall be an _allocatable_ scalar.

## __Returns__

The return value is a scalar _logical_ with the default logical kind type
parameter. If the argument is allocated then the result is .true.;
otherwise, it returns .false..

## __Examples__

Sample program:

```fortran
program demo_allocated
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
integer :: i = 4
real(kind=sp), allocatable :: x(:)

   ! if already allocated, deallocate
   if ( allocated(x) ) deallocate(x)

   ! only if not allocated, allocate
   if ( .not. allocated(x) ) allocate(x(i))

   write(*,*)allocated(x), size(x)
   if( allocated(x)) then
       write(*,*)'do things if allocated'
   else
       write(*,*)'do things if not allocated'
   endif
   call intentout(x)
   write(*,*)'note it is deallocated!',allocated(x)
   contains
   subroutine intentout(arr)
   ! note that if arr has intent(out) and is allocatable,
   ! arr is deallocated on entry
   real(kind=sp),intent(out),allocatable :: arr(:)
       write(*,*)'note it was allocated in calling program',allocated(arr)
   end subroutine intentout

end program demo_allocated
```
  Results:
```text
    T           4
    do things if allocated
    note it was allocated in calling program F
    note it is deallocated! F
```

## __Standard__

Fortran 95 and later. Note, the scalar= keyword and allocatable
scalar entities are available in Fortran 2003 and later.

## __See Also__

[__move\_alloc__(3)](MOVE_ALLOC)

###### fortran-lang intrinsic descriptions
# IS_CONTIGUOUS
## __Name__

__is\_contiguous__(3) - \[ARRAY INQUIRY\] test if object is contiguous


## __Syntax__
```fortran
result = is_contiguous(a)
```
## __Description__

True if and only if an object is contiguous.

An object is contiguous if it is

   - __(1)__
     an object with the CONTIGUOUS attribute,

   - __(2)__
     a nonpointer whole array that is not assumed-shape,

   - __(3)__
     an assumed-shape array that is argument associated with an array
     that is contiguous,

   - __(4)__
     an array allocated by an ALLOCATE statement,

   - __(5)__
     a pointer associated with a contiguous target, or

   - __(6)__
     a nonzero-sized array section provided that

       - __(a)__
         its base object is contiguous,

       - __(b)__
         it does not have a vector subscript,

       - __(c)__
         the elements of the section, in array element order, are a
         subset of the base object elements that are consecutive in
         array element order,

       - __(d)__
         if the array is of type character and a substring-range
         appears, the substring-range specifies all of the characters
         of the parent-string,

       - __(e)__
         only its final part-ref has nonzero rank, and

       - __(f)__
         it is not the real or imaginary part of an array of type
         complex.

An object is not contiguous if it is an array subobject, and

   - the object has two or more elements,

   - the elements of the object in array element order are not
     consecutive in the elements of the base object,

   - the object is not of type character with length zero, and

   - the object is not of a derived type that has no ultimate
     components other than zero-sized arrays and

   - characters with length zero.

It is processor-dependent whether any other object is contiguous.

## __Arguments__

  - __a__
    : may be of any type. It shall be an array. If it is a pointer it
    shall be associated.

## __Returns__

  - __Result__
    : of type Default logical scalar. The result has the value true if __a__
    is contiguous, and false otherwise.

## __Examples__

Sample program:

```fortran
program demo_is_contiguous
implicit none
intrinsic is_contiguous
real, DIMENSION (1000, 1000), TARGET :: A
real, DIMENSION (:, :), POINTER       :: IN, OUT
   IN => A              ! Associate IN with target A
   OUT => A(1:1000:2,:) ! Associate OUT with subset of target A
   !
   write(*,*)'IN is ',IS_CONTIGUOUS(IN)
   write(*,*)'OUT is ',IS_CONTIGUOUS(OUT)
   !
end program demo_is_contiguous
```
  Results:
```text
    IN is  T
    OUT is  F
```
## __Standard__

Fortran 2008 and later

###### fortran-lang intrinsic descriptions
# LBOUND
## __Name__

__lbound__(3) - \[ARRAY INQUIRY\] Lower dimension bounds of an array

## __Syntax__
```fortran
result = lbound(array, dim, kind)

   TYPE(kind=KIND) elemental function lbound(array,dim,kind)
   TYPE(kind=KIND),intent(in)  :: array 
   integer,optional,intent(in) :: dim
   integer,optional,intent(in) :: kind
```
## __Description__

Returns the lower bounds of an array, or a single lower bound along the
__dim__ dimension.

## __Arguments__

  - __array__
    : Shall be an array, of any type.

  - __dim__
    : Shall be a scalar _integer_.

  - __kind__
    : An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default integer kind. If __dim__ is absent, the
result is an array of the lower bounds of __array__. If __dim__ is present, the
result is a scalar corresponding to the lower bound of the array along
that dimension. If __array__ is an expression rather than a whole array or
array structure component, or if it has a zero extent along the relevant
dimension, the lower bound is taken to be 1.

## __Examples__

Note that in my opinion this function should not be used on assumed-size
arrays or in any function without an explicit interface. Errors can
occur if there is no interface defined.

Sample program

```fortran
! program demo_lbound
module m_bounds
implicit none
 contains
    subroutine msub(arr)
       !!integer,intent(in) :: arr(*)  ! cannot be assumed-size array
       integer,intent(in) :: arr(:)
       write(*,*)'MSUB: LOWER=',lbound(arr), &
       & 'UPPER=',ubound(arr), &
       & 'SIZE=',size(arr)
    end subroutine msub
 end module m_bounds

 use m_bounds, only : msub
 implicit none
 interface
    subroutine esub(arr)
    integer,intent(in) :: arr(:)
    end subroutine esub
 end interface
 integer :: arr(-10:10)
    write(*,*)'MAIN: LOWER=',lbound(arr), &
    & 'UPPER=',ubound(arr), &
    & 'SIZE=',size(arr)
    call csub()
    call msub(arr)
    call esub(arr)
 contains
subroutine csub
   write(*,*)'CSUB: LOWER=',lbound(arr), &
   & 'UPPER=',ubound(arr), &
   & 'SIZE=',size(arr)
end subroutine csub
end

 subroutine esub(arr)
 implicit none
 integer,intent(in) :: arr(:)
    ! WARNING: IF CALLED WITHOUT AN EXPLICIT INTERFACE
    ! THIS WILL GIVE UNDEFINED ANSWERS (like 0,0,0)
    write(*,*)'ESUB: LOWER=',lbound(arr), &
    & 'UPPER=',ubound(arr), &
    & 'SIZE=',size(arr)
 end subroutine esub

!end program demo_lbound
```
Results:

```
   MAIN: LOWER=         -10 UPPER=          10 SIZE=          21
   CSUB: LOWER=         -10 UPPER=          10 SIZE=          21
   MSUB: LOWER=           1 UPPER=          21 SIZE=          21
   ESUB: LOWER=           1 UPPER=          21 SIZE=          21
```

## __Standard__

Fortran 95 and later, with KIND argument - Fortran 2003 and later

## __See Also__

[__ubound__(3)](UBOUND),
[__co\_lbound__(3)](CO_LBOUND)

###### fortran-lang intrinsic descriptions
# RANK
## __Name__

__rank__(3) - \[ARRAY INQUIRY\] Rank of a data object


## __Syntax__
```fortran
result = rank(a)
```
## __Description__

__rank(a)__ returns the rank of a scalar or array data object.

## __Arguments__

  - __a__
    : can be of any type

## __Returns__

The return value is of type _integer_ and of the default integer kind. For
arrays, their rank is returned; for scalars zero is returned.

## __Examples__

Sample program:

```fortran
program demo_rank
implicit none
integer :: a
real, allocatable :: b(:,:)
real  :: c(10,20,30)
   print *, rank(a), rank(b), rank(c)
end program demo_rank
```
Results:
```text
   0           2           3
```
## __Standard__

TS 29113

###### fortran-lang intrinsic descriptions
# SHAPE
## __Name__

__shape__(3) - \[ARRAY INQUIRY\] Determine the shape of an array


## __Syntax__
```fortran
result = shape(source, kind)
```
## __Description__

Determines the shape of an array.

## __Arguments__

  - __source__
    : Shall be an array or scalar of any type. If __source__ is a pointer it
    must be associated and allocatable arrays must be allocated.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

An _integer_ array of rank one with as many elements as __source__ has
dimensions. The elements of the resulting array correspond to the extend
of __source__ along the respective dimensions. If __source__ is a scalar, the
result is the rank one array of size zero. If __kind__ is absent, the return
value has the default integer kind otherwise the specified kind.

## __Examples__

Sample program:

```fortran
program demo_shape
implicit none
character(len=*),parameter :: all='(*(g0,1x))'
integer, dimension(-1:1, -1:2) :: a
   print all, 'shape of array=',shape(a)
   print all, 'shape of constant=',shape(42)
   print all, 'size of shape of constant=',size(shape(42))
   print all, 'ubound of array=',ubound(a)
   print all, 'lbound of array=',lbound(a)
end program demo_shape
```
  Results:
```text
   shape of array= 3 4
   shape of constant=
   size of shape of constant= 0
   ubound of array= 1 2
   lbound of array= -1 -1
```
## __Standard__

Fortran 95 and later; with KIND argument Fortran 2003 and later

## __See Also__

[__reshape__(3)](RESHAPE),
[__size__(3)](SIZE)

###### fortran-lang intrinsic descriptions
# SIZE
## __Name__

__size__(3) - \[ARRAY INQUIRY\] Determine the size of an array


## __Syntax__
```fortran
result = size(array, dim, kind)
```
## __Description__

Determine the extent of __array__ along a specified dimension __dim__,
or the total number of elements in __array__ if __dim__ is absent.

## __Arguments__

  - __array__
    : be an array of any type. If __array__ is a pointer it must be
    associated and allocatable arrays must be allocated.

  - __dim__
    : shall be a scalar of type _integer_ and its value shall be
    in the range from 1 to n, where n equals the rank of __array__.

  - __kind__
    : An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__
is absent, the return value is of default _integer_ kind.

## __Examples__

Sample program:

```fortran
program demo_size
implicit none
integer :: i, j
integer :: arr(0:2,-5:5)=reshape([(((i-1)*11+j,i=1,3),j=1,11)],[3,11])
   write(*,*) 'SIZE of simple one-dimensional array=', &
   & size([ 11, 22, 33 ])    ! 3

   write(*,*)'body'
   write(*,*)'SHAPE(arr)       :',shape(arr)
   write(*,*)'SIZE(arr)        :',size(arr)
   write(*,*)'SIZE(arr,DIM=1)  :',size(arr,dim=1)
   write(*,*)'SIZE(arr,DIM=2)  :',size(arr,dim=2)
   write(*,*)'note lower bound is not "1"'
   write(*,*)'LBOUND(arr)      :',lbound(arr)
   write(*,*)'UBOUND(arr)      :',ubound(arr)
   write(*,*)'LBOUND(arr,DIM=1):',lbound(arr,dim=1)
   write(*,*)'UBOUND(arr,DIM=1):',ubound(arr,dim=1)
   write(*,*)'LBOUND(arr,DIM=2):',lbound(arr,dim=2)
   write(*,*)'UBOUND(arr,DIM=2):',ubound(arr,dim=2)

   call interfaced(arr,arr)
   call nointerface(arr)
contains

subroutine interfaced(arr,arr2)
integer,intent(in)  :: arr(:,:)
integer,intent(in)  :: arr2(2,*)
   !
   write(*,*)'interfaced assumed-shape arr2ay'
   !
   ! source argument of shape intrinsic at (1) must not be
   ! an assumed size array
   !!write(*,*)'SHAPE(arr2)       :',shape(arr2)
   ! The upper bound in the last dimension must appear in the reference
   ! to the assumed size array    arr2    at (1)
   !!write(*,*)'SIZE(arr2)        :',size(arr2)
   write(*,*)'SIZE(arr2,DIM=1)  :',size(arr2,dim=1)
   !    dim    argument of    size    intrinsic at (1) is not
   !a valid dimension index
   !!write(*,*)'SIZE(arr2,DIM=2)  :',size(arr2,dim=2)
   write(*,*)'note lower bound is "1"'
   write(*,*)'LBOUND(arr2)      :',lbound(arr2)
   write(*,*)'LBOUND(arr2)      :',lbound(arr2)
   ! The upper bound in the last dimension must appear in the
   ! reference to the assumed size array    arr2    at (1)
   !!write(*,*)'UBOUND(arr2)      :',ubound(arr2)
   write(*,*)'LBOUND(arr2,DIM=1):',lbound(arr2,dim=1)
   write(*,*)'UBOUND(arr2,DIM=1):',ubound(arr2,dim=1)
   write(*,*)'LBOUND(arr2,DIM=2):',lbound(arr2,dim=2)
   !    dim    argument of    ubound    intrinsic at (1) is not
   ! a valid dimension index
   !!write(*,*)'UBOUND(arr2,DIM=2):',ubound(arr2,dim=2)
   !
   write(*,*)'interfaced'
   !
   write(*,*)'SHAPE(arr)       :',shape(arr)
   write(*,*)'SIZE(arr)        :',size(arr)
   write(*,*)'SIZE(arr,DIM=1)  :',size(arr,dim=1)
   write(*,*)'SIZE(arr,DIM=2)  :',size(arr,dim=2)
   write(*,*)'note lower bound is "1"'
   write(*,*)'LBOUND(arr)      :',lbound(arr)
   write(*,*)'LBOUND(arr)      :',lbound(arr)
   write(*,*)'UBOUND(arr)      :',ubound(arr)
   write(*,*)'LBOUND(arr,DIM=1):',lbound(arr,dim=1)
   write(*,*)'UBOUND(arr,DIM=1):',ubound(arr,dim=1)
   write(*,*)'LBOUND(arr,DIM=2):',lbound(arr,dim=2)
   write(*,*)'UBOUND(arr,DIM=2):',ubound(arr,dim=2)
   !
end subroutine interfaced
!!
! NOTE: If NOINTERFACE(3) had an assumed-shape argument with :
!       for dimensions it could only be properly called with
!       an explicit interface
!!
subroutine nointerface(arr)
integer,intent(in) :: arr(3,*)
   write(*,*)'nointerface'
 ! SHAPE(3) CANNOT BE USED ON AN ASSUMED SIZE ARRAY
 !!write(*,*)'SHAPE(arr)       :',shape(arr)
 !!write(*,*)'SIZE(arr)        :',size(arr)
   write(*,*)'SIZE(arr,DIM=1)  :',size(arr,dim=1)
 ! CANNOT DETERMINE SIZE OF ASSUMED SIZE ARRAY LAST DIMENSION
 !!write(*,*)'SIZE(arr,DIM=2)  :',size(arr,dim=2)
   write(*,*)'note lower bound is "1"'
   write(*,*)'LBOUND(arr)      :',lbound(arr)
 !!write(*,*)'UBOUND(arr)      :',ubound(arr)
   write(*,*)'LBOUND(arr,DIM=1):',lbound(arr,dim=1)
   write(*,*)'UBOUND(arr,DIM=1):',ubound(arr,dim=1)
   write(*,*)'LBOUND(arr,DIM=2):',lbound(arr,dim=2)
 !!write(*,*)'UBOUND(arr,DIM=2):',ubound(arr,dim=2)
end subroutine nointerface
!!
end program demo_size
```
Results:
```text
    SIZE of simple one-dimensional array=           3
    body
    SHAPE(arr)       :           3          11
    SIZE(arr)        :          33
    SIZE(arr,DIM=1)  :           3
    SIZE(arr,DIM=2)  :          11
    note lower bound is not "1"
    LBOUND(arr)      :           0          -5
    UBOUND(arr)      :           2           5
    LBOUND(arr,DIM=1):           0
    UBOUND(arr,DIM=1):           2
    LBOUND(arr,DIM=2):          -5
    UBOUND(arr,DIM=2):           5
    interfaced assumed-shape arr2ay
    SIZE(arr2,DIM=1)  :           2
    note lower bound is "1"
    LBOUND(arr2)      :           1           1
    LBOUND(arr2)      :           1           1
    LBOUND(arr2,DIM=1):           1
    UBOUND(arr2,DIM=1):           2
    LBOUND(arr2,DIM=2):           1
    interfaced
    SHAPE(arr)       :           3          11
    SIZE(arr)        :          33
    SIZE(arr,DIM=1)  :           3
    SIZE(arr,DIM=2)  :          11
    note lower bound is "1"
    LBOUND(arr)      :           1           1
    LBOUND(arr)      :           1           1
    UBOUND(arr)      :           3          11
    LBOUND(arr,DIM=1):           1
    UBOUND(arr,DIM=1):           3
    LBOUND(arr,DIM=2):           1
    UBOUND(arr,DIM=2):          11
    nointerface
    SIZE(arr,DIM=1)  :           3
    note lower bound is "1"
    LBOUND(arr)      :           1           1
    LBOUND(arr,DIM=1):           1
    UBOUND(arr,DIM=1):           3
    LBOUND(arr,DIM=2):           1
```
## __Standard__

Fortran 95 and later, with __kind__ argument - Fortran 2003 and later

## __See Also__

[__shape__(3)](SHAPE),
[__reshape__(3)])(RESHAPE)

###### fortran-lang intrinsic descriptions
# UBOUND
## __Name__

__ubound__(3) - \[ARRAY INQUIRY\] Upper dimension bounds of an array

## __Syntax__
```fortran
result = ubound(array, dim, kind)
```
## __Description__

Returns the upper bounds of an array, or a single upper bound along the
__dim__ dimension.

## __Arguments__

  - __array__
    : Shall be an array, of any type.

  - __dim__
    : (Optional) Shall be a scalar _integer_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__
is absent, the return value is of default integer kind.

If __dim__ is absent, the result is an array of the upper bounds of
__array__.

If __dim__ is present, the result is a scalar corresponding to the upper
bound of the array along that dimension.

If __array__ is an expression rather than a whole array or array
structure component, or if it has a zero extent along the relevant
dimension, the upper bound is taken to be the number of elements along
the relevant dimension.

## __Examples__

Note this function should not be used on assumed-size arrays or in any
function without an explicit interface. Errors can occur if there is no
interface defined.

Sample program

```fortran
! program demo_ubound
module m2_bounds
implicit none

contains

subroutine msub(arr)
!!integer,intent(in) :: arr(*)  ! cannot be assumed-size array
integer,intent(in) :: arr(:)
   write(*,*)'MSUB: LOWER=',lbound(arr),'UPPER=',ubound(arr), &
   & 'SIZE=',size(arr)
end subroutine msub

end module m2_bounds

use m2_bounds, only : msub
implicit none
interface
   subroutine esub(arr)
   integer,intent(in) :: arr(:)
   end subroutine esub
end interface
integer :: arr(-10:10)
   write(*,*)'MAIN: LOWER=',lbound(arr),'UPPER=',ubound(arr), &
   & 'SIZE=',size(arr)
   call csub()
   call msub(arr)
   call esub(arr)
contains
subroutine csub
   write(*,*)'CSUB: LOWER=',lbound(arr),'UPPER=',ubound(arr), &
   & 'SIZE=',size(arr)
end subroutine csub

end

subroutine esub(arr)
implicit none
integer,intent(in) :: arr(:)
   ! WARNING: IF CALLED WITHOUT AN EXPLICIT INTERFACE
   ! THIS WILL GIVE UNDEFINED ANSWERS (like 0,0,0)
   write(*,*)'ESUB: LOWER=',lbound(arr),'UPPER=',ubound(arr), &
   & 'SIZE=',size(arr)
end subroutine esub
!end program demo_ubound
```
Results:
```text
  MAIN: LOWER=         -10 UPPER=          10 SIZE=          21
  CSUB: LOWER=         -10 UPPER=          10 SIZE=          21
  MSUB: LOWER=           1 UPPER=          21 SIZE=          21
  ESUB: LOWER=           1 UPPER=          21 SIZE=          21
```
## __Standard__

Fortran 95 and later, with KIND argument Fortran 2003
and later

## __See Also__

[__lbound__(3)](LBOUND),
[__co\_ubound__(3)](CO_UBOUND),
[__co\_lbound__(3)(CO_LBOUND)]

###### fortran-lang intrinsic descriptions
# MAXLOC
## __Name__

__maxloc__(3) - \[ARRAY:LOCATION\] Location of the maximum value within an array


## __Syntax__
```fortran
result = maxloc(array, dim, mask) result = maxloc(array, mask)
```
## __Description__

Determines the location of the element in the array with the maximum
value, or, if the __dim__ argument is supplied, determines the locations of
the maximum element along each row of the array in the __dim__ direction. If
__mask__ is present, only the elements for which __mask__ is __.true.__ are
considered. If more than one element in the array has the maximum value,
the location returned is that of the first such element in array element
order. If the array has zero size, or all of the elements of __mask__ are
.false., then the result is an array of zeroes. Similarly, if __dim__ is
supplied and all of the elements of __mask__ along a given row are zero, the
result value for that row is zero.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_, _real_, or _character_.

  - __dim__
    : (Optional) Shall be a scalar of type _integer_, with a value between
    one and the rank of __array__, inclusive. It may not be an optional
    dummy argument.

  - __mask__
    : Shall be an array of type _logical_, and conformable with __array__.

## __Returns__

If __dim__ is absent, the result is a rank-one array with a length equal to
the rank of __array__. If __dim__ is present, the result is an array with a rank
one less than the rank of __array__, and a size corresponding to the size of
__array__ with the __dim__ dimension removed. If __dim__ is present and __array__ has a
rank of one, the result is a scalar. In all cases, the result is of
default _integer_ type.

The value returned is reference to the offset from the beginning of the
array, not necessarily the subscript value if the array subscripts do
not start with one.

## __Examples__

sample program

```fortran
program demo_maxloc
implicit none
integer      :: ii
integer,save :: i(-3:3)=[(abs(abs(ii)-50),ii=-3,3)]
integer,save :: ints(3,5)= reshape([&
   1,  2,  3,  4,  5, &
   10, 20, 30, 40, 50, &
   11, 22, 33, 44, 55  &
],shape(ints),order=[2,1])

    write(*,*) maxloc(ints)
    write(*,*) maxloc(ints,dim=1)
    write(*,*) maxloc(ints,dim=2)
    ! when array bounds do not start with one remember MAXLOC(3) returns the
    ! offset relative to the lower bound-1 of the location of the maximum
    ! value, not the subscript of the maximum value. When the lower bound of
    ! the array is one, these values are the same. In other words, MAXLOC(3)
    ! returns the subscript of the value assuming the first subscript of the
    ! array is one no matter what the lower bound of the subscript actually
    ! is.
    write(*,'(g0,1x,g0)') (ii,i(ii),ii=lbound(i,dim=1),ubound(i,dim=1))
    write(*,*)maxloc(i)

end program demo_maxloc
```
Results:
```text
      3       5
      3       3       3       3       3
      5       5       5
   -3 47
   -2 48
   -1 49
   0 50
   1 49
   2 48
   3 47
```
## __Standard__

Fortran 95 and later

## __See Also__

[__max__(3)](MAX),
[__maxval__(3)](MAXVAL)

###### fortran-lang intrinsic descriptions
# MINLOC
## __Name__

__minloc__(3) - \[ARRAY:LOCATION\] Location of the minimum value within an array


## __Syntax__
```fortran
    result = minloc(array, dim, mask) 
```
   or
```fortran
    result = minloc(array, mask)
```
## __Description__

Determines the location of the element in the array with the minimum
value, or, if the __dim__ argument is supplied, determines the locations of
the minimum element along each row of the array in the __dim__ direction. If
__mask__ is present, only the elements for which __mask__ is __.true.__ are
considered. If more than one element in the array has the minimum value,
the location returned is that of the first such element in array element
order. If the array has zero size, or all of the elements of __mask__ are
.false., then the result is an array of zeroes. Similarly, if __dim__ is
supplied and all of the elements of __mask__ along a given row are zero, the
result value for that row is zero.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_, _real_, or _character_.

  - __dim__
    : (Optional) Shall be a scalar of type _integer_, with a value between
    one and the rank of __array__, inclusive. It may not be an optional
    dummy argument.

  - __mask__
    : Shall be an array of type _logical_, and conformable with __array__.

## __Returns__

If __dim__ is absent, the result is a rank-one array with a length equal to
the rank of __array__. If __dim__ is present, the result is an array with a rank
one less than the rank of __array__, and a size corresponding to the size of
__array__ with the __dim__ dimension removed. If __dim__ is present and __array__ has a
rank of one, the result is a scalar. In all cases, the result is of
default _integer_ type.

## __Examples__

sample program:

```fortran
program demo_minloc
implicit none
integer,save :: ints(3,5)= reshape([&
   4, 10,  1,  7, 13, &
   9, 15,  6, 12,  3, &
  14,  5, 11,  2,  8  &
],shape(ints),order=[2,1])
    write(*,*) minloc(ints)
    write(*,*) minloc(ints,dim=1)
    write(*,*) minloc(ints,dim=2)
    ! where in each column is the smallest number .gt. 10 ?
    write(*,*) minloc(ints,dim=2,mask=ints.gt.10)
    ! a one-dimensional array with dim=1 explicitly listed returns a scalar
    write(*,*) minloc(pack(ints,.true.),dim=1) ! scalar
end program demo_minloc
```
Results:

```text
         1       3
         1       3       1       3       2
         3       5       4
         5       4       3
         7
```
## __Standard__

Fortran 95 and later

## __See Also__

[__min__(3)](MIN),
[__minval__(3)](MINVAL)

###### fortran-lang intrinsic descriptions
# FINDLOC
## __Name__

__findloc__(3) - \[ARRAY:LOCATION\] Location of first element of ARRAY identified by MASK along dimension DIM having a value


## __Syntax__
```fortran
findloc (array, value, dim, mask, kind, back) 

or 

findloc(array, value, mask, kind, back)
```
## __Description__

Location of the first element of __array__ identified by __mask__ along
dimension __dim__ having a value equal to __value__.

If both __array__ and __value__ are of type logical, the comparison is
performed with the __.eqv.__ operator; otherwise, the comparison is
performed with the == operator. If the value of the comparison is
true, that element of __array__ matches __value__.

If only one element matches __value__, that element's subscripts are
returned. Otherwise, if more than one element matches __value__ and
__back__ is absent or present with the value false, the element whose
subscripts are returned is the first such element, taken in array
element order. If __back__ is present with the value true, the element
whose subscripts are returned is the last such element, taken in array
element order.

## __Options__

  - __array__
    : shall be an array of intrinsic type.

  - __value__
    : shall be scalar and in type conformance with __array__, as specified
    in Table 7.3 for relational intrinsic operations 7.1.5.5.2).

  - __dim__
    : shall be an integer scalar with a value in the range 1 __DIM__ n, where
    n is the rank of __array__. The corresponding actual argument shall
    not be an optional dummy argument.

  - __mask__
    : (optional) shall be of type logical and shall be conformable with
    __array__.

  - __kind__
    : (optional) shall be a scalar integer initialization expression.

  - __back__
    : (optional) shall be a logical scalar.

## __Returns__

Result Characteristics. Integer. If __kind__ is present, the kind type
parameter is that specified by the value of __kind__; otherwise the kind
type parameter is that of default integer type. If __dim__ does not appear,
the result is an array of rank one and of size equal to the rank of
__array__; otherwise, the result is of rank n - 1 and shape

```
   [d1 , d2 , . . . , dDIM-1 , dDIM+1 , . . . , dn ]
```

where

```
   [d1 , d2 , . . . , dn ]
```

is the shape of __array__.

## __Returns__

  - __Case (i):__
    The result of __findloc (array, value)__ is a rank-one array whose
    element values are the values of the subscripts of an element of
    __array__ whose value matches __value__. If there is such a value, the
    ith subscript returned lies in the range 1 to ei , where ei is the
    extent of the ith dimension of __array__. If no elements match __value__
    or __array__ has size zero, all elements of the result are zero.

  - __Case (ii):__
    the result of __findloc (array, value, mask = mask)__ is a
    rank-one array whose element values are the values of the subscripts
    of an element of __array__, corresponding to a true element of __mask__,
    whose value matches __value__. If there is such a value, the ith
    subscript returned lies in the range 1 to ei , where ei is the
    extent of the ith dimension of __array__. If no elements match
    __value__, __array__ has size zero, or every element of __mask__ has the
    value false, all elements of the result are zero.

  - __Case (iii):__
    If __array__ has rank one, the result of

```
      findloc (array, value, dim=dim [, mask = mask])
```

is a scalar whose value is equal to that of the first element of

```
      findloc (array, value [, mask = mask])
```

Otherwise, the value of element

```
      (s1 , s2 , . . . , sDIM-1 , sDIM+1 , . . . , sn )
```

of the result is equal to

```
      findloc (array (s1, s2, ..., sdim-1, :, sdim+1, ..., sn ), &
      value, dim=1 [, mask = mask (s1, s2, ..., sdim-1, :,
                      sdim+1 , ... , sn )]).
```
## __Examples__

  - __Case (i):__
    The value of

```
        findloc ([2, 6, 4, 6,], value = 6)
```

is \[2\], and the value of

```
        findloc ([2, 6, 4, 6], value = 6, back = .true.)
```

is \[4\].

  - __Case (ii):__
    If __a__ has the value

```text
      0 -5  7 7
      3  4 -1 2
      1  5  6 7
```

and __m__ has the value

```text
       T T F T
       T T F T
       T T F T

      findloc (a, 7, mask = m)
```

has the value \[1, 4\] and

```
      findloc (a, 7, mask = m, back = .true.)
```

has the value \[3, 4\]. This is independent of the declared lower
bounds for __a__ .

  - __Case (iii):__
    The value of

```
      findloc ([2, 6, 4], value = 6, dim = 1)
```

is 2. If __b__ has the value

```
       1 2 -9
       2 2  6
```

> findloc (b, __value__ = 2, dim = 1)

has the value \[2, 1, 0\] and

```
      findloc (b, value = 2, dim = 2)
```

has the value \[2, 1\]. This is independent of the declared lower
bounds for __b__.

###### fortran-lang intrinsic descriptions
# TRANSPOSE
## __Name__

__transpose__(3) - \[ARRAY MANIPULATION\] Transpose an array of rank two


## __Syntax__
```fortran
result = transpose(matrix)
```
## __Description__

Transpose an array of rank two. Element (i, j) of the result has the
value __matrix(j, i)__, for all i, j.

## __Arguments__

  - __matrix__
    : Shall be an array of any type and have a rank of two.

## __Returns__

The result has the same type as __matrix__, and has shape \[ m, n \] if
__matrix__ has shape \[ n, m \].

## __Examples__

Sample program:

```fortran
program demo_transpose
implicit none
integer,save :: xx(3,5)= reshape([&
    1,  2,  3,  4,  5,    &
   10, 20, 30, 40, 50,    &
   11, 22, 33, 44, -1055  &
 ],shape(xx),order=[2,1])

call print_matrix_int('xx array:',xx)
call print_matrix_int('xx array transposed:',transpose(xx))

contains

subroutine print_matrix_int(title,arr)
! print small 2d integer arrays in row-column format
implicit none
character(len=*),intent(in)  :: title
integer,intent(in)           :: arr(:,:)
integer                      :: i
character(len=:),allocatable :: biggest
   write(*,*)trim(title)  ! print title
   biggest='           '  ! make buffer to write integer into
   ! find how many characters to use for integers
   write(biggest,'(i0)')ceiling(log10(real(maxval(abs(arr)))))+2
   ! use this format to write a row
   biggest='(" > [",*(i'//trim(biggest)//':,","))'
   ! print one row of array at a time
   do i=1,size(arr,dim=1)
      write(*,fmt=biggest,advance='no')arr(i,:)
      write(*,'(" ]")')
   enddo
end subroutine print_matrix_int

end program demo_transpose
```

Results:

```
    xx array:
    > [     1,     2,     3,     4,     5 ]
    > [    10,    20,    30,    40,    50 ]
    > [    11,    22,    33,    44, -1055 ]
    xx array transposed:
    > [     1,    10,    11 ]
    > [     2,    20,    22 ]
    > [     3,    30,    33 ]
    > [     4,    40,    44 ]
    > [     5,    50, -1055 ]
```

## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# ALL
## __Name__

__all__(3) - \[ARRAY REDUCTION\] determines if all the values are true


## __Syntax__
```fortran
result = all(mask, dim)
```
## __Description__

Logical conjunction of elements of __mask__ along dimension __dim__.

"__all(mask, dim)__" determines if all the values are true in __mask__
in the array along dimension __dim__.

## __Arguments__

  - __mask__
    : shall be a logical array. That is, the type of the argument shall be
    _logical_ and it shall not be scalar.

  - __dim__
    : (optional) __dim__ shall be a scalar integer with a value that lies
    between one and the rank of __mask__. The corresponding actual argument
    shall not be an optional dummy argument.

## __Returns__

"__all(mask)__" returns a scalar value of type _logical_ where the kind
type parameter is the same as the kind type parameter of __mask__. If
__dim__ is present, then __all(mask, dim)__ returns an array with the rank
of __mask__ minus 1. The shape is determined from the shape of __mask__
where the __dim__ dimension is elided.

 1.  __all(mask)__ is true if all elements of __mask__ are true. It also is
     true if __mask__ has zero size; otherwise, it is false.

 2.  If the rank of __mask__ is one, then __all(mask, dim)__ is equivalent
     to __all(mask)__. If the rank is greater than one, then __all(mask,
     dim)__ is determined by applying __all()__ to the array sections.

 3.  Result Characteristics. The result is of type _logical_ with the
     same kind type parameter as __mask__. It is scalar if __dim__
     is absent or __n = 1__; otherwise, the result has rank __n - 1__
     and shape __\[d1 , d2 , . . . , dDIM-1 , dDIM+1 , . . . , dn \]__
     where __\[d1 , d2 , . . . , dn \]__ is the shape of __mask__.

 4.  Result Value.

     Case (i):
               : The result of __all(mask)__ has the value true if all
               elements of __mask__ are true or if __mask__ has
               size zero, and the result has value false if any element
               of __mask__ is false.

     Case (ii): 
               : If __mask__ has rank one, __all(mask,dim)__ is equal to
               __all(mask)__. Otherwise, the value of element __(s1 , s2 ,
               . . . , sdim-1 , sdim+1 , . . . , sn )__ of all __(mask,
               dim)__ is equal to __all(mask (s1 , s2 , . . . , sdim-1 ,
               :, sdim+1 , . . . , sn ))__.

## __Examples__

Sample program:

```fortran
program demo_all
implicit none
logical l
   l = all([.true., .true., .true.])
   print *, l
   call section

contains

subroutine section
integer a(2,3), b(2,3)
  a = 1
  b = 1
  b(2,2) = 2
  print *, all(a .eq. b, 1)
  print *, all(a .eq. b, 2)
end subroutine section
end program demo_all
```
Results:
```text
    T
    T F T
    T F
```
Case (i):

```text
     The value of all([.TRUE., .FALSE., .TRUE.]) is false.
```

Case (ii):

```text
                          1|3|5
   If B is the array      -+-+-
                          2|4|6
  
                          0|3|5
   and C is the array     -+-+-
                          7|4|8

   then all(B /= C, DIM = 1) is

      [true, false, false]
```

and __all(B /= C, DIM = 2)__ is

```
        [false, false].
```

## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# ANY
## __Name__

__any__(3) - \[ARRAY REDUCTION\] determines if any of the values in the logical array are true.


## __Syntax__
```fortran
result = any(mask, dim)
```
## __Description__

__any(mask, dim)__ determines if any of the values in the logical
array __mask__ along dimension __dim__ are __.true.__.

## __Arguments__

  - __mask__
    : the type of the argument shall be _logical_ and it shall not be
    scalar.

  - __dim__
    : (optional) dim shall be a scalar integer with a value that lies
    between one and the rank of mask.

## __Returns__

__any(mask)__ returns a scalar value of type _logical_ where the kind type
parameter is the same as the kind type parameter of __mask__. If __dim__ is
present, then __any(mask, dim)__ returns an array with the rank of __mask__
minus 1. The shape is determined from the shape of __mask__ where the __dim__
dimension is elided.

1.  __any(mask)__ is true if any element of __mask__ is true; otherwise, it
    is __.false.__. It also is false if __mask__ has zero size.

2.  If the rank of __mask__ is one, then __any(mask, dim)__ is equivalent to
    __any(mask)__. If the rank is greater than one, then __any(mask,
    dim)__ is determined by applying __any()__ to the array sections.

## __Examples__

Sample program:
```fortran
program demo_any
implicit none
logical l
   l = any([.true., .true., .true.])
   print *, l
   call section
   contains
     subroutine section
     integer a(2,3), b(2,3)
       a = 1
       b = 1
       b(2,2) = 2
       print *, any(a .eq. b, 1)
       print *, any(a .eq. b, 2)
     end subroutine section
end program demo_any
```
  Results:
```text
    T
    T T T
    T T
```
## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# COUNT
## __Name__

__count__(3) - \[ARRAY REDUCTION\] Count function


## __Syntax__
```fortran
result = count(mask, dim, kind)
```
## __Description__

Counts the number of __.true.__ elements in a logical __mask__, or, if the __dim__
argument is supplied, counts the number of elements along each row of
the array in the __dim__ direction. If the array has zero size, or all of
the elements of __mask__ are false, then the result is __0__.

## __Arguments__

  - __mask__
    : The type shall be _logical_.

  - __dim__
    : (Optional) The type shall be _integer_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default integer kind. If __dim__ is present, the
result is an array with a rank one less than the rank of __array__, and a
size corresponding to the shape of __array__ with the __dim__ dimension removed.

## __Examples__

Sample program:

```fortran
program demo_count
implicit none
integer, dimension(2,3) :: a, b
logical, dimension(2,3) :: mymask
      a = reshape( [ 1, 2, 3, 4, 5, 6 ], [ 2, 3 ])
      b = reshape( [ 0, 7, 3, 4, 5, 8 ], [ 2, 3 ])
      print '(3i3)', a(1,:)
      print '(3i3)', a(2,:)
      print *
      print '(3i3)', b(1,:)
      print '(3i3)', b(2,:)
      print *
      mymask = a.ne.b
      print '(3l3)', mymask(1,:)
      print '(3l3)', mymask(2,:)
      print *
      print '(3i3)', count(mymask)
      print *
      print '(3i3)', count(mymask, 1)
      print *
      print '(3i3)', count(mymask, 2)
end program demo_count
```
   Expected Results:
```text
  1  3  5
  2  4  6
 
  0  3  5
  7  4  8
 
  T  F  F
  T  F  T
 
  3
 
  2  0  1
 
  1  2
```
## __Standard__

Fortran 95 and later, with KIND argument - Fortran 2003
and later

###### fortran-lang intrinsic descriptions
# MAXVAL
## __Name__

__maxval__(3) - \[ARRAY REDUCTION\] determines the maximum value in an array or row


## __Syntax__
```fortran
result = maxval(array, dim, mask)
```
   or 
```fortran
result = maxval(array, mask)
```
## __Description__

Determines the maximum value of the elements in an array value, or, if
the __dim__ argument is supplied, determines the maximum value along each
row of the array in the __dim__ direction. If __mask__ is present, only the
elements for which __mask__ is __.true.__ are considered. If the array has zero
size, or all of the elements of __mask__ are .false., then the result is the
most negative number of the type and kind of __array__ if __array__ is numeric,
or a string of nulls if __array__ is of character type.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_, _real_, or _character_.

  - __dim__
    : (Optional) Shall be a scalar of type _integer_, with a value between
    one and the rank of __array__, inclusive. It may not be an optional
    dummy argument.

  - __mask__
    : (Optional) Shall be an array of type _logical_, and conformable with
    __array__.

## __Returns__

If __dim__ is absent, or if __array__ has a rank of one, the result is a scalar.
If __dim__ is present, the result is an array with a rank one less than the
rank of __array__, and a size corresponding to the size of __array__ with the
__dim__ dimension removed. In all cases, the result is of the same type and
kind as __array__.

## __Examples__

sample program:

```fortran
program demo_maxval
implicit none
integer,save :: ints(3,5)= reshape([&
   1,  2,  3,  4,  5, &
  10, 20, 30, 40, 50, &
  11, 22, 33, 44, 55  &
],shape(ints),order=[2,1])

   write(*,*) maxval(ints)
   write(*,*) maxval(ints,dim=1)
   write(*,*) maxval(ints,dim=2)
   ! find biggest number less than 30 with mask
   write(*,*) maxval(ints,mask=ints.lt.30)
end program demo_maxval
```

Results:

```
   55
   11     22     33     44     55
    5     50     55
   22
```

## __Standard__

Fortran 95 and later

## __See Also__

[__max__(3)](MAX),
[__maxloc__(3)](MAXLOC)

###### fortran-lang intrinsic descriptions
# MINVAL
## __Name__

__minval__(3) - \[ARRAY REDUCTION\] Minimum value of an array

## __Syntax__
```fortran
result = minval(array, dim, mask) result = minval(array, mask)
```
## __Description__

Determines the minimum value of the elements in an array value, or, if
the __dim__ argument is supplied, determines the minimum value along each
row of the array in the __dim__ direction. 

If __mask__ is present, only the
elements for which __mask__ is __.true.__ are considered. 

If the array has zero size, or all of the elements of __mask__ are
.false., then the result is __huge(array)__ if __array__ is numeric, or a
string of __char(len=255)__ characters if __array__ is of character type.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_, _real_, or _character_.

  - __dim__
    : (Optional) Shall be a scalar of type _integer_, with a value between
    one and the rank of ARRAY, inclusive. It may not be an optional
    dummy argument.

  - __mask__
    : Shall be an array of type _logical_, and conformable with __array__.

## __Returns__

If __dim__ is absent, or if __array__ has a rank of one, the result is a scalar.

If __dim__ is present, the result is an array with a rank one less than the
rank of __array__, and a size corresponding to the size of __array__ with the
__dim__ dimension removed. In all cases, the result is of the same type and
kind as __array__.

## __Examples__

sample program:

```fortran
program demo_minval
implicit none
integer :: i
character(len=*),parameter :: g='(3x,*(g0,1x))'

integer,save :: ints(3,5)= reshape([&
       1,  -2,   3,   4,   5,  &
      10,  20, -30,  40,  50,  &
      11,  22,  33, -44,  55  &
],shape(ints),order=[2,1])

integer,save :: box(3,5,2)

   box(:,:,1)=ints
   box(:,:,2)=-ints

   write(*,*)'Given the array'
   write(*,'(1x,*(g4.4,1x))') &
   & (ints(i,:),new_line('a'),i=1,size(ints,dim=1))

   write(*,*)'What is the smallest element in the array?'
   write(*,g) minval(ints),'at <',minloc(ints),'>'

   write(*,*)'What is the smallest element in each column?'
   write(*,g) minval(ints,dim=1)

   write(*,*)'What is the smallest element in each row?'
   write(*,g) minval(ints,dim=2)

   ! notice the shape of the output has less columns 
   ! than the input in this case
   write(*,*)'What is the smallest element in each column,'
   write(*,*)'considering only those elements that are'
   write(*,*)'greater than zero?'
   write(*,g) minval(ints, dim=1, mask = ints > 0)

   write(*,*)&
   & 'if everything is false a zero-sized array is NOT returned'
   write(*,*) minval(ints, dim=1, mask = .false.)
   write(*,*)'even for a zero-sized input'
   write(*,g) minval([integer ::], dim=1, mask = .false.)

   write(*,*)'a scalar answer for everything false is huge()'
   write(*,g) minval(ints, mask = .false.)
   write(*,g) minval([integer ::], mask = .false.)

   write(*,*)'some calls with three dimensions'
   write(*,g) minval(box, mask = .true. )
   write(*,g) minval(box, dim=1, mask = .true. )

   write(*,g) minval(box, dim=2, mask = .true. )
   write(*,g) 'shape of answer is ', &
   & shape(minval(box, dim=2, mask = .true. ))

end program demo_minval
```
Results:
```text
 Given the array
    1   -2    3    4    5    
   10   20  -30   40   50    
   11   22   33  -44   55    

 What is the smallest element in the array?
   -44 at < 3 4 >
 What is the smallest element in each column?
   1 -2 -30 -44 5
 What is the smallest element in each row?
   -2 -30 -44
 What is the smallest element in each column,
 considering only those elements that are
 greater than zero?
   1 20 3 4 5
 if everything is false a zero-sized array is NOT returned
  2147483647  2147483647  2147483647  2147483647  2147483647
 even for a zero-sized input
   2147483647
 a scalar answer for everything false is huge()
   2147483647
   2147483647
 some calls with three dimensions
   -55
   1 -2 -30 -44 5 -11 -22 -33 -40 -55
   -2 -30 -44 -5 -50 -55
   shape of answer is  3 2
```

## __Standard__

Fortran 95 and later

## __See Also__

[__min__(3)](MIN),
[__minloc__(3)](MINLOC)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# PRODUCT
## __Name__

__product__(3) - \[ARRAY REDUCTION\] Product of array elements

## __Syntax__
```fortran
  result = product(array, dim, mask)

    NUMERIC,intent(in) :: array(..)
    integer,intent(in),optional :: dim
    logical,intent(in),optional :: mask(..)
```
where __NUMERIC__ is any numeric type

## __Description__

Multiplies together all the selected elements of __array__, or along
dimension __dim__ if the corresponding element in __mask__ is __.true.__.

If __dim__ is absent, a scalar with the product of all elements in __array__ is
returned. (Note a zero-sized __array__ returns __1__).

When __dim__ is present, If the masked array has a dimension of one
(ie. is a vector) the result is a scalar.  Otherwise, an array of rank
__n-1__, where __n__ equals the rank of __array__, and a shape similar
to that of __array__ with dimension __dim__ dropped is returned.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_, _real_ or _complex_.

  - __dim__
    : shall be a scalar of type _integer_ with a value in the
    range from __1 to n__, where __n__ equals the rank of __array__.

  - __mask__
    : shall be of type _logical_ and either be a scalar or an
    array of the same shape as __array__.

## __Returns__

The result is of the same type as __array__.


## __Examples__

Sample program:

```fortran
program demo_product
implicit none
character(len=*),parameter :: all='(*(g0,1x))' ! a handy format
character(len=1),parameter :: nl=new_line('a') 

NO_DIM: block
!    If DIM is not specified, the result is the product of all the 
!    selected array elements. 
integer :: i,n, p1, p2
integer,allocatable :: array(:)
   ! all elements are selected by default 
   do n=1,10
      print all, 'factorial of ',n,' is ', product([(real(i),i=1,n)])
   enddo

   ! using a mask
   array=[10,12,13,15,20,25,30]
   p1=product(array, mask=mod(array, 2)==1) ! only odd elements
   p2=product(array, mask=mod(array, 2)/=1) ! only even elements
   print all, nl,'product of all elements',product(array) ! all elements
   print all, ' odd * even =',nl,p1,'*',p2,'=',p1*p2

   ! NOTE: If ARRAY is a zero-sized array, the result is equal to one
   print all
   print all, 'zero-sized array=>',product([integer :: ]) 
   ! NOTE: If nothing in the mask is true, this also results in a null
   !       array
   print all, 'all elements have a false mask=>',product(array,mask=.false.) 

endblock NO_DIM

WITH_DIM: block
integer :: rect(2,3)
integer :: box(2,3,4)

!  lets fill a few arrays
   rect = reshape([ &
     1, 2, 3,       &
     4, 5, 6        &
   ],shape(rect),order=[2,1])
   call print_matrix_int('rect',rect)

!  Find the product of each column in RECT.
   print all, 'product of columns=',product(rect, dim = 1)

! Find the product of each row in RECT.
   print all, 'product of rows=',product(rect, dim = 2)

! now lets try a box
   box(:,:,1)=rect
   box(:,:,2)=rect*(+10)
   box(:,:,3)=rect*(-10)
   box(:,:,4)=rect*2
   ! lets look at the values
   call print_matrix_int('box 1',box(:,:,1))
   call print_matrix_int('box 2',box(:,:,2))
   call print_matrix_int('box 3',box(:,:,3))
   call print_matrix_int('box 4',box(:,:,4))

   ! remember without dim= even a box produces a scalar
   print all, 'no dim gives a scalar',product(real(box))

   ! only one plane has negative values, so note all the "1" values
   ! for vectors with no elements
   call print_matrix_int('negative values',product(box,mask=box < 0,dim=1))

!   If DIM is specified and ARRAY has rank greater than one, the
!   result is a new array in which dimension DIM has been eliminated.
 
   ! pick a dimension to multiply though 
   call print_matrix_int('dim=1',product(box,dim=1))

   call print_matrix_int('dim=2',product(box,dim=2))

   call print_matrix_int('dim=3',product(box,dim=3))

endblock WITH_DIM
  
contains
  
subroutine print_matrix_int(title,arr)
implicit none
  
!@(#) print small 2d integer arrays in row-column format
  
character(len=*),intent(in)  :: title
integer,intent(in)           :: arr(:,:)
integer                      :: i
character(len=:),allocatable :: biggest
  
   print all
   print all, trim(title),':(',shape(arr),')'  ! print title
   biggest='           '  ! make buffer to write integer into
   ! find how many characters to use for integers
   write(biggest,'(i0)')ceiling(log10(real(maxval(abs(arr)))))+2 
   ! use this format to write a row
   biggest='(" > [",*(i'//trim(biggest)//':,","))'     
   ! print one row of array at a time
   do i=1,size(arr,dim=1)                      
      write(*,fmt=biggest,advance='no')arr(i,:)
      write(*,'(" ]")')
   enddo
  
end subroutine print_matrix_int
  
end program demo_product
```
Results:
```text
factorial of  1  is  1.000000
factorial of  2  is  2.000000
factorial of  3  is  6.000000
factorial of  4  is  24.00000
factorial of  5  is  120.0000
factorial of  6  is  720.0000
factorial of  7  is  5040.000
factorial of  8  is  40320.00
factorial of  9  is  362880.0
factorial of  10  is  3628800.

 product of all elements 351000000
 odd * even = 
 4875 * 72000 = 351000000

zero-sized array=> 1
all elements have a false mask=> 1

rect :( 2 3 )
 > [  1,  2,  3 ]
 > [  4,  5,  6 ]
product of columns= 4 10 18
product of rows= 6 120

box 1 :( 2 3 )
 > [  1,  2,  3 ]
 > [  4,  5,  6 ]

box 2 :( 2 3 )
 > [  10,  20,  30 ]
 > [  40,  50,  60 ]

box 3 :( 2 3 )
 > [ -10, -20, -30 ]
 > [ -40, -50, -60 ]

box 4 :( 2 3 )
 > [   2,   4,   6 ]
 > [   8,  10,  12 ]
no dim gives a scalar .1719927E+26

negative values :( 3 4 )
 > [     1,     1,   400,     1 ]
 > [     1,     1,  1000,     1 ]
 > [     1,     1,  1800,     1 ]

dim=1 :( 3 4 )
 > [     4,   400,   400,    16 ]
 > [    10,  1000,  1000,    40 ]
 > [    18,  1800,  1800,    72 ]

dim=2 :( 2 4 )
 > [       6,    6000,   -6000,      48 ]
 > [     120,  120000, -120000,     960 ]

dim=3 :( 2 3 )
 > [    -200,   -3200,  -16200 ]
 > [  -51200, -125000, -259200 ]
```
## __Standard__

Fortran 95 and later

## __See Also__

[__sum__(3)](SUM), note that an element by element multiplication is done
directly using the star character.

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# SUM
## __Name__

__sum__(3) - \[ARRAY REDUCTION\] sum the elements of an array


## __Syntax__
```fortran
   result = sum(array[, mask])
   result = sum(array, dim[, mask])
```
## __Description__

Adds the elements of ARRAY along dimension DIM if the corresponding
element in MASK is TRUE.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_, _real_ or _complex_.

  - __dim__
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from 1 to n, where n equals the rank of ARRAY.

  - __mask__
    : (Optional) shall be of type _logical_ and either be a scalar or an
    array of the same shape as ARRAY.

## __Returns__

The result is of the same type as ARRAY.

If __dim__(3) is absent, a scalar with the sum of all elements in ARRAY
is returned. Otherwise, an array of rank n-1, where n equals the rank of
ARRAY, and a shape similar to that of ARRAY with dimension DIM dropped
is returned.

## __Examples__

Sample program:

```fortran
program simple_sum
implicit none
integer :: x(5) = [ 1, 2, 3, 4 ,5 ]
   print *, sum(x)                        ! all elements, sum = 15
   print *, sum(x, mask=mod(x, 2)==1)     ! odd elements, sum = 9
end program simple_sum
```
Demonstrate Fortran 90 SUM function with MASK option

```fortran
program demo_sum
! John Mahaffy  2/16/96
implicit none
integer nd, ndh, nduh, j
parameter (nd=10,ndh=nd/2, nduh=nd-ndh)
real csum, cpsum, cbpsum
real, dimension(nd):: c=[(j, j=-1,nd-2)], b
data b/ndh*-1.0, nduh*2.0/
   csum= sum(c(1:nd))
   cpsum= sum (c(1:nd), mask=c.gt.0)
   cbpsum= sum(c(1:nd), mask=b.gt.0.0)
   print *, 'Sum of all elements in c = ' , csum
   print *, 'Sum of Positive elements in c = ', cpsum
   print *, 'Sum of elements in c when corresponding elements in b>0', &
   & ' =', cbpsum
end program demo_sum
```

Results:

```text
 Sum of all elements in c =    35.0000000
 Sum of Positive elements in c =    36.0000000
 Sum of elements in c when corresponding elements in b>0 =   30.0000000
```

## __Standard__

Fortran 95 and later

## __See Also__

intrinsics

###### fortran-lang intrinsic descriptions
# RESHAPE
## __Name__

__reshape__(3) - \[ARRAY RESHAPE\] Function to reshape an array


## __Syntax__
```fortran
result = reshape(source, shape, pad, order)
```
## __Description__

Reshapes array __source__ to correspond to __shape__. If necessary, the new
array may be padded with elements from __pad__ or permuted as defined by
__order__.

## __Arguments__

  - __source__
    : an array of any type.

  - __shape__
    : an array of rank one and type _integer_. Its values must be positive
    or zero.

  - __pad__
    : (Optional) an array of the same type as __source__.

  - __order__
    : (Optional) an array of type _integer_ and the same shape as __shape__. Its
    values shall be a permutation of the numbers from 1 to n, where n is
    the size of __shape__. If __order__ is absent, the natural ordering shall be
    assumed.

## __Returns__

The result is an array of shape __shape__ with the same type as __source__.

## __Examples__

Sample program:

```fortran
program demo_reshape
implicit none
integer :: i
integer, dimension(4) :: x=[(i,i=10,40,10)]
real :: xx(3,4)
real,allocatable :: v(:)
    ! x is originally a vector with four elements
    write(*,*) shape(x) ! what is the current shape of the array?
    write(*,*) shape(reshape(x, [2, 2]))    ! prints "2 2"

    ! pack any array into a vector
    xx=1.0
    v=reshape(xx,[size(xx)])
    write(*,*)shape(v),ubound(v) 
end program demo_reshape
```
  Results:
```text
              4
              2           2
             12          12
```
## __Standard__

Fortran 95 and later

## __See Also__

[__shape__(3)](SHAPE)

###### fortran-lang intrinsic descriptions

    - Explicit-shape arrays
    - Assumed-shape arrays
    - Deferred-shape arrays
    - Implied-shape arrays 
    - Assumed-size arrays
    - Assumed-rank objects 
