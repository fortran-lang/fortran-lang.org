# TYPE_index
### Types and kinds

These intrinsics allow for explicitly casting one type of variable to
another or can be used to conditionally execute code blocks based on
variable types when working with polymorphic variables.

#### Fortran Data Types

Fortran provides five basic intrinsic data types:

  * Integer type
    : The integer types can hold only whole number values.
  * Real type
    : Stores floating point numbers, such as 2.0, 3.1415, -100.876, etc.
  * Complex type
    : A complex number has two parts,
      the real part and the imaginary part. Two consecutive floating
      point storage units store the two parts.
  * Logical type
    : There are only two logical values: .true. and .false.
  * Character type
    : The character type stores strings. The length of the string
    can be specified by the __len__ specifier. If no length is specified, it is 1.

These "types" can be of many "kinds". Often different numeric kinds
take up different storage sizes and therefore can represent
different ranges; but a different kind can have other meanings.
A _character_ variable might represent ASCII characters or UTF-8 or
Unicode characters, for example.

You can derive your own data types from these fundamental types as well.

#### Implicit Typing

Fortran allows a feature called implicit typing, i.e., you do not have
to declare some variables before use. By default if a variable is not declared,
then the first letter of its name will determine its type:

1. Variable names starting with __i-n__ (the first two letters of
   "integer") specify _integer_ variables.

2. All other variable names default to _real_.


However, in most circles it is considered good programming practice to declare all the
variables. For that to be enforced, you start your variable declaration section with 
a statement that turns off implicit typing:
the statement
```fortran
implicit none
```
For more information refer to the __implicit__ statement.

###### fortran-lang intrinsic descriptions

# AIMAG
## __Name__

__aimag__(3) - \[TYPE:NUMERIC\] Imaginary part of complex number


## __Syntax__
```fortran
    result = aimag(z)

     complex(kind=KIND),elemental :: aimag

     complex(kind=KIND),intent(in) :: z
```
## __Description__

__aimag(z)__ yields the imaginary part of complex argument __z__.

## __Arguments__

  - __z__
    : The type of the argument shall be _complex_.

## __Returns__

The return value is of type _real_ with the kind type parameter of the
argument.

## __Examples__

Sample program:

```fortran
program demo_aimag
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
complex(kind=real32) z4
complex(kind=real64) z8
    z4 = cmplx(1.e0, 2.e0)
    z8 = cmplx(3.e0_real64, 4.e0_real64,kind=real64)
    print *, aimag(z4), aimag(z8)
    ! an elemental function can be passed an array
    print *
    print *, [z4,z4/2.0,z4+z4,z4**3]
    print *
    print *, aimag([z4,z4/2.0,z4+z4,z4**3])
end program demo_aimag
```
Results:
```text
  2.000000       4.00000000000000

 (1.000000,2.000000) (0.5000000,1.000000) (2.000000,4.000000)
 (-11.00000,-2.000000)

       2.000000       1.000000       4.000000      -2.000000
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions
# CMPLX
## __Name__

__cmplx__(3) - \[TYPE:NUMERIC\] Complex conversion function


## __Syntax__
```fortran
result = cmplx(x, y, kind)

   complex elemental function :: cmplx
   TYPE(kind=KIND),intent(in), x
   TYPE(kind=KIND),intent(in),optional, y
   integer,intent(in),optional :: kind
```
## __Description__

To convert numeric variables to complex, use the __cmplx__(3) function.
Constants can be used to define a complex variable using the syntax

```
      z8 = (1.2345678901234567d0, 1.2345678901234567d0)
```

but this will not work for variables. You must use the __cmplx__(3) function.

__cmplx(x \[, y \[, kind\]\])__ returns a complex number where __x__ is
converted to the _real_ component. If __x__ is _complex_ then __y__ must not be
present. If __y__ is present it is converted to the imaginary component. If
__y__ is not present then the imaginary component is set to __0.0__.

## __cmplx(3) and double precision__

The Fortran 90 language defines __cmplx__(3) as always returning a result
that is type __complex(kind=KIND(0.0))__.

This means \`__cmplx(d1,d2)__', where __\`d1'__ and __\`d2'__ are
_doubleprecision_, is treated as:
fortran
```
      cmplx(sngl(d1), sngl(d2))
```

_doubleprecision complex_ numbers require specifying a precision.

It was necessary for Fortran 90 to specify this behavior for
_doubleprecision_ arguments, since that is the behavior mandated by
FORTRAN 77.

So Fortran 90 extends the __cmplx__(3) intrinsic by adding an extra
argument used to specify the desired kind of complex result.

```fortran
      integer,parameter :: dp=kind(0.0d0)
      complex(kind=dp) :: z8
      !
      ! NO: result is just the precision of default _real_ values
      !     because KIND parameter is not specified
      !
      ! note this was stored with default real precision
      z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0)
      print *, 'NO, Z8=',z8,real(z8),aimag(z8)
      z8 = cmplx(1.2345678901234567e0_dp, 1.2345678901234567e0_dp)
      ! again, note components are just _real_
      print *, 'NO, Z8=',z8,real(z8),aimag(z8)
      !
      ! YES
      !
      ! kind= makes it work
      z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0,kind=dp)
      print *, 'YES, Z8=',z8,real(z8),aimag(z8)
```

F2018 COMPONENT SYNTAX The real and imaginary parts of a complex entity
can be accessed independently with a component-like syntax in f2018:

A complex-part-designator is

``fortran
      designator % RE
      or
      designator % IM.
```

Where the designator is of complex type.

So designator%RE designates the real part of a complex value,
designator%IM designates the imaginary part of complex value. The type
of a complex-part-designator is _real_, and its kind and shape are those
of the designator.

The following are examples of complex part designators:

```fortran
       impedance%re           !-- Same value as _real_(impedance)
       fft%im                 !-- Same value as AIMAG(fft)
       x%im = 0.0             !-- Sets the imaginary part of x to zero
```

## __Arguments__

  - __x__
    The type may be _integer_, _real_, or _complex_.

  - __y__
    (Optional; only allowed if __x__ is not _complex_.). May be _integer_ or
    _real_.

  - __kind__
    (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of _complex_ type, with a kind equal to __kind__ if it is
specified. If __kind__ is not specified, the result is of the default
_complex_ kind, regardless of the kinds of __x__ and __y__.

## __Examples__

Sample program:

```fortran
program demo_aimag
implicit none
integer,parameter :: dp=kind(0.0d0)
complex          :: z4
complex(kind=dp) :: z8
   z4 = cmplx(1.23456789, 1.23456789)
   print *, 'Z4=',z4
   ! using kind=dp makes it keep DOUBLEPRECISION precision
   z8 = cmplx(1.2345678901234567d0, 1.2345678901234567d0,kind=dp)
   print *, 'Z8=',z8
   ! NOTE:
   ! The following is intuitive and works without calling cmplx(3)
   ! but does not work for variables just constants
   z8 = (1.2345678901234567d0 , 1.2345678901234567d0 )
   print *, 'Z8 defined with constants=',z8
end program demo_aimag
```

Typical Results:

```
    Z4= (1.23456788,1.23456788)
    Z8= (1.2345678901234567,1.2345678901234567)
    Z8 defined with constants= (1.2345678901234567,1.2345678901234567)
```

## __See Also__

  - [__aimag__(3)](AIMAG) - Imaginary part of complex number

  - [__cmplx__(3)](CMPLX) - Complex conversion function

  - [__conjg__(3)](CONJG) - Complex conjugate function

  - [__real__(3)](REAL) - Convert to real type

## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions
# INT
## __Name__
__int__(3) - \[TYPE:NUMERIC\] Convert to integer type by truncating towards zero

## __Syntax__
```fortran
result = int(a, kind)

 integer(kind=KIND) elemental function int(a,kind)
 TYPE(kind=KIND),intent(in),optional :: a
 integer,optional :: kind
```
## __Description__

Convert to integer type by truncating towards zero.

## __Arguments__

  - __a__
    : Shall be of type _integer_, _real_, or _complex_ or a BOZ-literal-constant.

  - __kind__
    : An _integer_ initialization expression indicating the kind
    parameter of the result.

    If not present the returned type is that of default integer type.

## __Returns__

returns an _integer_ variable or array applying the following rules:

 __Case__:

 1.  If __a__ is of type _integer_, __int__(a) = a

 2.  If __a__ is of type _real_ and __|a| \< 1, int(a)__ equals __0__. If __|a| \>=
     1__, then __int(a)__ equals the integer whose magnitude does not exceed
     __a__ and whose sign is the same as the sign of __a__.

 3.  If __a__ is of type _complex_, rule 2 is applied to the _real_ part of __a__.

 4.  If _a_ is a boz-literal constant, it is treated as an _integer_
     with the _kind_ specified.

     The interpretation of a bit sequence whose most significant bit is
     __1__ is processor dependent.

The result is undefined if it cannot be represented in the specified integer type.

## __Examples__

Sample program:

```fortran
program demo_int
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
implicit none
integer :: i = 42 
complex :: z = (-3.7, 1.0)
real :: x=-10.5, y=10.5

   print *, int(x), int(y)

   print *, int(i)

   print *, int(z), int(z,8)
   ! elemental
   print *, int([-10.9,-10.5,-10.3,10.3,10.5,10.9])
   ! note int(3) truncates towards zero

   ! CAUTION:
   ! a number bigger than a default integer can represent
   ! produces an incorrect result and is not required to
   ! be detected by the program. 
   x=real(huge(0))+1000.0
   print *, int(x),x
   ! using a larger kind
   print *, int(x,kind=int64),x

   print *, int(&
   & B"111111111111111111111111111111111111111111111111111111111111111",&
   & kind=int64)
   print *, int(O"777777777777777777777",kind=int64)
   print *, int(Z"7FFFFFFFFFFFFFFF",kind=int64)

   ! elemental
   print *
   print *,int([ &
   &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
   &  0.0,   &
   &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

end program demo_int
```
  Results:
```text
            -10   10
             42
             -3  -3
            -10  -10  -10   10   10  10
    -2147483648   2.14748467E+09
     2147484672   2.14748467E+09
     9223372036854775807
     9223372036854775807
     9223372036854775807
   
    -2          -2          -2          -2          -1
    -1           0           0           0           1
     1           2           2           2           2
```
## __Standard__

FORTRAN 77 and later

## __See Also__

[__aint__(3)](AINT),
[__anint__(3)](ANINT),
[__nint__(3)](NINT),
[__selected_int_kind__(3)](SELECTED_INT_KIND),
[__ceiling__(3)](CEILING),
[__floor__(3)](FLOOR)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# NINT
## __Name__

__nint__(3) - \[TYPE:NUMERIC\] Nearest whole number

## __Syntax__
```fortran
    elemental function nint(x [, kind=NN]) result(ANSWER)
     real(kind=??),intent(in) :: x
     integer(kind=NN) :: ANSWER
```
## __Description__

__nint(x)__ rounds its argument to the nearest whole number with its
sign preserved.

The user must ensure the value is a valid value for the range of the
__kind__ returned. If the processor cannot represent the result in the kind
specified, the result is undefined.

If __x__ is greater than zero, __nint(x)__ has the value __int(x+0.5)__.

If __x__ is less than or equal to zero, __nint(x)__ has the value
__int(a-0.5)__.

## __Arguments__

  - __x__
    : The type of the argument shall be _real_.

  - __kind__
    : (Optional) A constant _integer_ expression indicating the kind
    parameter of the result. Otherwise, the kind type parameter is that
    of default _integer_ type.

## __Returns__

  - __answer__
    : The result is the integer nearest __x__, or if there are two integers
    equally near __x__, the result is whichever such _integer_ has the greater
    magnitude.

    The result is undefined if it cannot be represented in the specified
    integer type.

## __Examples__

Sample program:

```fortran
program demo_nint
implicit none
integer,parameter :: dp=kind(0.0d0)
real              :: x4 = 1.234E0
real(kind=dp)     :: x8 = 4.721_dp

! basic use
   print *, nint(x4), nint(x8),nint(-x8)
   ! elemental
   print *,nint([ &
   &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
   &  0.0,   &
   &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

! issues
ISSUES: block
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
integer :: icheck
   ! make sure input is in range for the type returned
   write(*,*)'Range limits for typical KINDS:'
   write(*,'(1x,g0,1x,g0)')  &
   & int8,huge(0_int8),   &
   & int16,huge(0_int16), &
   & int32,huge(0_int32), &
   & int64,huge(0_int64)

   ! the standard does not require this to be an error ...
   x8=12345.67e15 ! too big of a number
   icheck=selected_int_kind(ceiling(log10(x8)))
   write(*,*)'Any KIND big enough? ICHECK=',icheck
   print *, 'These are all wrong answers for ',x8
   print *, nint(x8,kind=int8)
   print *, nint(x8,kind=int16)
   print *, nint(x8,kind=int32)
   print *, nint(x8,kind=int64)
endblock ISSUES

end program demo_nint
```
  Results:
```text
     1    5   -5
    -3   -3   -2   -2   -2
    -1   -1    0    1    1
     2    2    2    3    3
    Range limits for typical KINDS:
    1 127
    2 32767
    4 2147483647
    8 9223372036854775807
    Any KIND big enough? ICHECK=          16
    These are all wrong answers for    1.2345669499901444E+019
       0
         0
              0
    -9223372036854775808
```

## __Standard__

FORTRAN 77 and later, with KIND argument - Fortran 90 and later

## __See Also__

[__aint__(3)](AINT),
[__anint__(3)](ANINT),
[__int__(3)](INT),
[__selected_int_kind__(3)](SELECTED_INT_KIND),
[__ceiling__(3)](CEILING),
[__floor__(3)](FLOOR)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# REAL
## __Name__
__real__(3) - \[TYPE:NUMERIC\] Convert to real type


## __Syntax__
```fortran
result = real(x, kind)
```
## __Description__

__real(x, kind)__ converts its argument __x__ to a real type.

## __Arguments__

  - __x__
    : Shall be _integer_, _real_, or _complex_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

These functions return a _real_ variable or array under the following
rules:

1.  __real__(x) is converted to a default _real_ type if __x__ is an _integer_
    or _real_ variable.

2.  __real__(x) is converted to a real type with the kind type parameter
    of __x__ if __x__ is a _complex_ variable.

3.  __real(x, kind)__ is converted to a _real_ type with kind type
    parameter __kind__ if __x__ is a _complex_, _integer_, or _real_ variable.

## __Examples__

Sample program:

```fortran
program demo_real
use,intrinsic :: iso_fortran_env, only : dp=>real64
implicit none
complex              :: zr = (1.0, 2.0)
doubleprecision      :: xd=huge(3.0d0)
complex(kind=dp) :: zd=cmplx(4.0e0_dp,5.0e0_dp,kind=dp)

   print *, real(zr), aimag(zr)
   print *, dble(zd), aimag(zd)

   write(*,*)xd,real(xd,kind=kind(0.0d0)),dble(xd)
end program demo_real
```

Results:

```
 1.00000000       2.00000000
 4.0000000000000000       5.0000000000000000
 1.7976931348623157E+308  1.7976931348623157E+308  1.7976931348623157E+308
```

## __Standard__

FORTRAN 77 and later

## __See Also__

[__dble__(3)](DBLE),
[__float__(3)](FLOAT)

###### fortran-lang intrinsic descriptions
# DBLE
## __Name__

__dble__(3) - \[TYPE:NUMERIC\] Double conversion function


## __Syntax__
```fortran
result = dble(a)

    elemental function dble(a)
    type(real(kind=kind(0.0d0)))     :: dble
    type(TYPE(kind=KIND)),intent(in) :: a
```
where TYPE may be _integer_, _real_, or _complex_ and KIND any kind
supported by the TYPE.
## __Description__

__dble(a)__ Converts __a__ to double precision _real_ type.

## __Arguments__

  - __a__
    : The type shall be _integer_, _real_, or _complex_.

## __Returns__

The return value is of type _doubleprecision_. For _complex_ input,
the returned value has the magnitude and sign of the real component
of the input value.

## __Examples__

Sample program:

```fortran
program demo_dble
implicit none
real:: x = 2.18
integer :: i = 5
complex :: z = (2.3,1.14)
   print *, dble(x), dble(i), dble(z)
end program demo_dble
```
  Results:
```text
  2.1800000667572021  5.0000000000000000   2.2999999523162842     
```
## __Standard__

FORTRAN 77 and later

## __See Also__

[__float__(3)](FLOAT),
[__real__(3)](REAL)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# TRANSFER
## __Name__

__transfer__(3) - \[TYPE:MOLD\] Transfer bit patterns


## __Syntax__
```fortran
result = transfer(source, mold, size)
```
## __Description__

Interprets the bitwise representation of __source__ in memory as if it
is the representation of a variable or array of the same type and type
parameters as __mold__.

This is approximately equivalent to the C concept of \*casting\* one
type to another.

## __Arguments__

  - __source__
    : Shall be a scalar or an array of any type.

  - __mold__
    : Shall be a scalar or an array of any type.

  - __size__
    : (Optional) shall be a scalar of type _integer_.

## __Returns__

The result has the same type as __mold__, with the bit level representation
of __source__. If __size__ is present, the result is a one-dimensional array of
length __size__. If __size__ is absent but __mold__ is an array (of any size or
shape), the result is a one-dimensional array of the minimum length
needed to contain the entirety of the bitwise representation of __source__.
If __size__ is absent and __mold__ is a scalar, the result is a scalar.

If the bitwise representation of the result is longer than that of
__source__, then the leading bits of the result correspond to those of
__source__ and any trailing bits are filled arbitrarily.

When the resulting bit representation does not correspond to a valid
representation of a variable of the same type as __mold__, the results are
undefined, and subsequent operations on the result cannot be guaranteed
to produce sensible behavior. For example, it is possible to create
_logical_ variables for which __var__ and .not. var both appear to be true.

## __Examples__

Sample program:
```fortran
program demo_transfer
use,intrinsic :: iso_fortran_env, only : int32, real32
integer(kind=int32) :: i = 2143289344
real(kind=real32)   :: x
character(len=10)   :: string
character(len=1)    :: chars(10)
   x=transfer(i, 1.0)    ! prints "nan" on i686
   ! the bit patterns are the same
   write(*,'(b0,1x,g0)')x,x ! create a NaN
   write(*,'(b0,1x,g0)')i,i

   ! a string to an array of characters
   string='abcdefghij'
   chars=transfer(string,chars)
   write(*,'(*("[",a,"]":,1x))')string
   write(*,'(*("[",a,"]":,1x))')chars
end program demo_transfer
```
Results:
```text
   1111111110000000000000000000000 NaN
   1111111110000000000000000000000 2143289344
   [abcdefghij]
   [a] [b] [c] [d] [e] [f] [g] [h] [i] [j]
```
## __Comments__

_Joe Krahn_: Fortran uses __molding__ rather than __casting__.

Casting, as in C, is an in-place reinterpretation. A cast is a device
that is built around an object to change its shape.

Fortran TRANSFER reinterprets data out-of-place. It can be considered
__molding__ rather than casting. A __mold__ is a device that
confers a shape onto an object placed into it.

The advantage of molding is that data is always valid in the context
of the variable that holds it. For many cases, a decent compiler should
optimize TRANSFER into a simple assignment.

There are disadvantages of this approach. It is problematic to define a
union of data types because you must know the largest data object, which
can vary by compiler or compile options. In many cases, an EQUIVALENCE
would be far more effective, but Fortran Standards committees seem
oblivious to the benefits of EQUIVALENCEs when used sparingly.

## __Standard__

Fortran 90 and later

###### fortran-lang intrinsic descriptions
# LOGICAL
## __Name__

__logical__(3) - \[TYPE:LOGICAL\] Converts one kind of _logical_ variable to another


## __Syntax__
```fortran
result = logical(l, kind)

 logical(kind=KIND) function logical(L,KIND)
  logical(kind=INK),intent(in) :: L
  integer,intent(in),optional :: KIND
```
## __Description__

Converts one kind of _logical_ variable to another.

## __Arguments__


  - __l__
    : The type shall be _logical_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is a _logical_ value equal to __l__, with a kind
corresponding to __kind__, or of the default logical kind if __kind__ is not
given.

## __Examples__
```fortran
program demo_logical
! Access array containing the kind type parameter values supported by this
! compiler for entities of logical type
use iso_fortran_env, only : logical_kinds

   ! list kind values supported on this platform, which generally vary
   ! in storage size
   do i =1, size(logical_kinds) 
      write(*,*)logical_kinds(i)
   enddo

end program demo_logical
```
  Results:
```text
              1
              2
              4
              8
             16
```
## __Standard__

Fortran 95 and later, related ISO_FORTRAN_ENV module - fortran 2009

## __See Also__

[__int__(3)](INT),
[__real__(3)](REAL),
[__cmplx__(3)](CMPLX)

###### fortran-lang intrinsic descriptions
# KIND
## __Name__

__kind__(3) - \[KIND INQUIRY\] Kind of an entity


## __Syntax__
```fortran
k = kind(x)
```
## __Description__

__kind(x)__ returns the kind value of the entity __x__.

## __Arguments__

  - __x__
    : Shall be of type _logical_, _integer_, _real_, _complex_ or _character_.

## __Returns__

The return value is a scalar of type _integer_ and of the default integer
kind.

## __Examples__

Sample program:

```fortran
program demo_kind
implicit none
integer,parameter :: kc = kind(' ')
integer,parameter :: kl = kind(.true.)

   print *, "The default character kind is ", kc
   print *, "The default logical kind is ", kl

end program demo_kind
```
  Results:
```text
    The default character kind is            1
    The default logical kind is            4
```
## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions
# SELECTED_CHAR_KIND
## __Name__

__selected\_char\_kind__(3) - \[KIND\] Choose character kind such as "Unicode"


## __Syntax__
```fortran
result = selected_char_kind(name)
```
## __Description__

__selected\_char\_kind(name)__ returns the kind value for the character
set named NAME, if a character set with such a name is supported, or
__-1__ otherwise. Currently, supported character sets include "ASCII"
and "DEFAULT" (iwhich are equivalent), and "ISO\_10646" (Universal
Character Set, UCS-4) which is commonly known as "Unicode".

## __Arguments__

  - __name__
    : Shall be a scalar and of the default character type.

## __Examples__

Sample program:

```fortran
program demo_selected_char_kind
use iso_fortran_env
implicit none
integer, parameter :: ascii = selected_char_kind ("ascii")
integer, parameter :: ucs4  = selected_char_kind ('ISO_10646')

character(kind=ascii, len=26) :: alphabet
character(kind=ucs4,  len=30) :: hello_world

   alphabet = ascii_"abcdefghijklmnopqrstuvwxyz"
   hello_world = ucs4_'Hello World and Ni Hao -- ' &
                 // char (int (z'4F60'), ucs4)     &
                 // char (int (z'597D'), ucs4)

   write (*,*) alphabet

   open (output_unit, encoding='UTF-8')
   write (*,*) trim (hello_world)
end program demo_selected_char_kind
```
  Results:
```text
    abcdefghijklmnopqrstuvwxyz
    Hello World and Ni Hao -- 你好
```
## __Standard__

Fortran 2003 and later

###### fortran-lang intrinsic descriptions
# SELECTED_INT_KIND
## __Name__

__selected\_int\_kind__(3) - \[KIND\] Choose integer kind


## __Syntax__
```fortran
result = selected_int_kind(r)
```
## __Description__

__selected\_int\_kind(r)__ return the kind value of the smallest integer
type that can represent all values ranging from __-10\*\*r__ (exclusive)
to __10\*\*r__ (exclusive). If there is no integer kind that accommodates
this range, selected\_int\_kind returns __-1__.

## __Arguments__

  - __r__
    : Shall be a scalar and of type _integer_.

## __Examples__

Sample program:

```fortran
program demo_selected_int_kind
implicit none
integer,parameter :: k5 = selected_int_kind(5)
integer,parameter :: k15 = selected_int_kind(15)
integer(kind=k5) :: i5
integer(kind=k15) :: i15

    print *, huge(i5), huge(i15)

    ! the following inequalities are always true
    print *, huge(i5) >= 10_k5**5-1
    print *, huge(i15) >= 10_k15**15-1
end program demo_selected_int_kind
```
  Results:
```text
     2147483647  9223372036854775807
    T
    T
```
## __Standard__

Fortran 95 and later

## __See Also__

[__aint__(3)](AINT),
[__anint__(3)](ANINT),
[__int__(3)](INT),
[__nint__(3)](NINT),
[__ceiling__(3)](CEILING),
[__floor__(3)](FLOOR)

###### fortran-lang intrinsic descriptions
# SELECTED_REAL_KIND
## __Name__

__selected\_real\_kind__(3) - \[KIND\] Choose real kind


## __Syntax__
```fortran
result = selected_real_kind(p, r, radix)
```
## __Description__

__selected\_real\_kind(p, r, radix)__ return the kind value of a real
data type with decimal precision of at least __p__ digits, exponent range of
at least __r__, and with a radix of __radix__.

## __Arguments__

  - __p__
    : (Optional) shall be a scalar and of type _integer_.

  - __r__
    : (Optional) shall be a scalar and of type _integer_.

  - __radix__
    : (Optional) shall be a scalar and of type _integer_.

Before __Fortran 2008__, at least one of the arguments __r__ or __p__ shall
be present; since __Fortran 2008__, they are assumed to be zero if
absent.

## __Returns__

selected\_real\_kind returns the value of the kind type parameter of a
real data type with decimal precision of at least __p__ digits, a decimal
exponent range of at least R, and with the requested __radix__. If the __radix__
parameter is absent, real kinds with any radix can be returned. If more
than one real data type meet the criteria, the kind of the data type
with the smallest decimal precision is returned. If no real data type
matches the criteria, the result is

  - __-1__ if the processor does not support a real data type with a
    precision greater than or equal to __p__, but the __r__ and __radix__
    requirements can be fulfilled

      - __-2__ if the processor does not support a real type with an
        exponent range greater than or equal to __r__, but __p__ and __radix__ are
        fulfillable

      - __-3__ if __radix__ but not __p__ and __r__ requirements are fulfillable

      - __-4__ if __radix__ and either __p__ or __r__ requirements are fulfillable

      - __-5__ if there is no real type with the given __radix__

## __Examples__

Sample program:

```fortran
program demo_selected_real_kind
implicit none
integer,parameter :: p6 = selected_real_kind(6)
integer,parameter :: p10r100 = selected_real_kind(10,100)
integer,parameter :: r400 = selected_real_kind(r=400)
real(kind=p6) :: x
real(kind=p10r100) :: y
real(kind=r400) :: z

   print *, precision(x), range(x)
   print *, precision(y), range(y)
   print *, precision(z), range(z)
end program demo_selected_real_kind
```
  Results:
```text
              6          37
             15         307
             18        4931
```
## __Standard__

Fortran 95 and later; with RADIX - Fortran 2008 and later

## __See Also__

[__precision__(3)](PRECISION),
[__range__(3)](RANGE),
[__radix__(3)](RADIX)

###### fortran-lang intrinsic descriptions
