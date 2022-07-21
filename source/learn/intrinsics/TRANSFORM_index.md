# TRANSFORM_index
### Matrix multiplication, Dot product, array shifts,

# CSHIFT
## __Name__

__cshift__(3) - \[TRANSFORMATIONAL\] Circular shift elements of an array


## __Syntax__
```fortran
result = cshift(array, shift, dim)
```
## __Description__

__cshift(array, shift \[, dim\])__ performs a circular shift on elements
of __array__ along the dimension of __dim__. If __dim__ is omitted it is taken to be
__1__. __dim__ is a scalar of type _integer_ in the range of __1 \<= dim \<= n__,
where "n" is the rank of __array__. If the rank of __array__ is one, then all
elements of __array__ are shifted by __shift__ places. If rank is greater than
one, then all complete rank one sections of __array__ along the given
dimension are shifted. Elements shifted out one end of each rank one
section are shifted back in the other end.

## __Arguments__

  - __array__
    : Shall be an array of any type.

  - __shift__
    : The type shall be _integer_.

  - __dim__
    : The type shall be _integer_.

## __Returns__

Returns an array of same type and rank as the __array__ argument.

## __Examples__

Sample program:

```fortran
program demo_cshift
implicit none
integer, dimension(3,3) :: a
    a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])
    print '(3i3)', a(1,:)
    print '(3i3)', a(2,:)
    print '(3i3)', a(3,:)
    a = cshift(a, SHIFT=[1, 2, -1], DIM=2)
    print *
    print '(3i3)', a(1,:)
    print '(3i3)', a(2,:)
    print '(3i3)', a(3,:)
end program demo_cshift
```
  Results:
```text
     1  4  7
     2  5  8
     3  6  9
    
     4  7  1
     8  2  5
     9  3  6
```
## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# DOT_PRODUCT
## __Name__

__dot\_product__(3) - \[TRANSFORMATIONAL\] Dot product function


## __Syntax__
```fortran
result = dot_product(vector_a, vector_b)
```
## __Description__

__dot\_product(vector\_a, vector\_b)__ computes the dot product
multiplication of two vectors vector\_a and vector\_b. The two vectors
may be either numeric or logical and must be arrays of rank one and of
equal size. If the vectors are _integer_ or _real_, the result is
__sum(vector\_a\*vector\_b)__. If the vectors are _complex_, the result is
__sum(conjg(vector\_a)\*vector\_b)__. If the vectors are _logical_, the
result is __any(vector\_a .and. vector\_b)__.

## __Arguments__

  - __vector\_a__
    : The type shall be numeric or _logical_, rank 1.

  - __vector\_b__
    : The type shall be numeric if vector\_a is of numeric type or _logical_
    if vector\_a is of type _logical_. vector\_b shall be a rank-one
    array.

## __Returns__

If the arguments are numeric, the return value is a scalar of numeric
type, _integer_, _real_, or _complex_. If the arguments are _logical_, the
return value is .true. or .false..

## __Examples__

Sample program:

```fortran
program demo_dot_prod
implicit none
    integer, dimension(3) :: a, b
    a = [ 1, 2, 3 ]
    b = [ 4, 5, 6 ]
    print '(3i3)', a
    print *
    print '(3i3)', b
    print *
    print *, dot_product(a,b)
end program demo_dot_prod
```
  Results:
```text
     1  2  3
   
     4  5  6
   
             32
```
## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# EOSHIFT
## __Name__

__eoshift__(3) - \[TRANSFORMATIONAL\] End-off shift elements of an array


## __Syntax__
```fortran
result = eoshift(array, shift, boundary, dim)
```
## __Description__

__eoshift(array, shift\[, boundary, dim\])__ performs an end-off shift
on elements of __array__ along the dimension of __dim__. If __dim__ is omitted it is
taken to be __1__. __dim__ is a scalar of type _integer_ in the range of __1 \<= DIM
\<= n__ where __"n"__ is the rank of __array__. If the rank of __array__ is one, then
all elements of __array__ are shifted by __shift__ places. If rank is greater
than one, then all complete rank one sections of __array__ along the given
dimension are shifted. Elements shifted out one end of each rank one
section are dropped. If __boundary__ is present then the corresponding value
of from __boundary__ is copied back in the other end. If __boundary__ is not
present then the following are copied in depending on the type of __array__.

\*Array Type\* - \*Boundary Value\*

   - Numeric 0 of the type and kind of __array__

   - Logical .false.

   - __Character(len)__ LEN blanks

## __Arguments__

  - __array__
    : May be any type, not scalar.

  - __shift__
    : The type shall be _integer_.

  - __boundary__
    : Same type as ARRAY.

  - __dim__
    : The type shall be _integer_.

## __Returns__

Returns an array of same type and rank as the __array__ argument.

## __Examples__

Sample program:

```fortran
program demo_eoshift
implicit none
    integer, dimension(3,3) :: a
    a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])
    print '(3i3)', a(1,:)
    print '(3i3)', a(2,:)
    print '(3i3)', a(3,:)
    a = eoshift(a, SHIFT=[1, 2, 1], BOUNDARY=-5, DIM=2)
    print *
    print '(3i3)', a(1,:)
    print '(3i3)', a(2,:)
    print '(3i3)', a(3,:)
end program demo_eoshift
```
  Results:
```text
     1  4  7
     2  5  8
     3  6  9
   
     4  7 -5
     8 -5 -5
     6  9 -5
```
## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# MATMUL
## __Name__

__matmul__(3) - \[TRANSFORMATIONAL\] matrix multiplication


## __Syntax__
```fortran
result = matmul(matrix_a, matrix_b)
```
## __Description__

Performs a matrix multiplication on numeric or logical arguments.

## __Arguments__

  - __matrix\_a__
    : An array of _integer_, _real_, _complex_, or _logical_ type, with a rank of
    one or two.

  - __matrix\_b__
    : An array of _integer_, _real_, or _complex_ type if __matrix\_a__ is of a
    numeric type; otherwise, an array of _logical_ type. The rank shall be
    one or two, and the first (or only) dimension of __matrix\_b__ shall be
    equal to the last (or only) dimension of __matrix\_a__.

## __Returns__

The matrix product of __matrix\_a__ and __matrix\_b__. The type and kind of the
result follow the usual type and kind promotion rules, as for the \* or
.and. operators.

## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# PARITY
## __Name__

__parity__(3) - \[TRANSFORMATIONAL\] Reduction with exclusive __OR__()


## __Syntax__
```fortran
result = parity(mask, dim)

    function parity(mask, dim)
    type(logical(kind=LKIND))                    :: dim
    type(logical(kind=LKIND)),intent(in)         :: mask(..)
    type(integer(kind=KIND)),intent(in),optional :: dim
```
where KIND and LKIND are any supported kind for the type.
```
## __Description__

Calculates the parity (i.e. the reduction using .xor.) of __mask__ along
dimension __dim__.

## __Arguments__

  - __mask__
    : Shall be an array of type _logical_.

  - __dim__
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from __1 to n__, where __n__ equals the rank of __mask__.

## __Returns__

The result is of the same type as __mask__.

If __dim__ is absent, a scalar with the parity of all elements in __mask__ is
returned: __.true.__ if an odd number of elements are __.true.__ and __.false.__
otherwise.

When __dim__ is specified the returned shape is similar to that of __mask__
with dimension __dim__ dropped.

## __Examples__

Sample program:

```fortran
program demo_parity
implicit none
logical :: x(2) = [ .true., .false. ]
   print *, parity(x) 
end program demo_parity
```
  Results:
```text
    T
```
## __Standard__

Fortran 2008 and later

###### fortran-lang intrinsic descriptions
# NULL
## __Name__

__null__(3) - \[TRANSFORMATIONAL\] Function that returns a disassociated pointer


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
