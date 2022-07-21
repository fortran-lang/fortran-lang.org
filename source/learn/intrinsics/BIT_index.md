# BIT_index 
### bit-level inquiry and manipulation

# BGE
## __Name__

__bge__(3) - \[BIT:COMPARE\] Bitwise greater than or equal to


## __Syntax__
```fortran
    result = bge(i, j)
```
## __Description__

Determines whether an integer is bitwise greater than or equal to
another.

## __Arguments__

  - __i__
    : Shall be of _integer_ type.

  - __j__
    : Shall be of _integer_ type, and of the same kind as __i__.

## __Returns__

The return value is of type _logical_ and of the default kind.

## __Standard__

Fortran 2008 and later

## __See Also__

[__bgt__(3)](BGT),
[__ble__(3)](BLE),
[__blt__(3)](BIT)

###### fortran-lang intrinsic descriptions
# BGT

## __Name__

__bgt__(3) - \[BIT:COMPARE\] Bitwise greater than


## __Syntax__
```fortran
    result = bgt(i, j)
```
## __Description__

Determines whether an integer is bitwise greater than another.

## __Arguments__

  - __i__
    : Shall be of _integer_ type or a BOZ literal constant.

  - __j__
    : Shall be of _integer_ type, and of the same kind as __i__; or a BOZ
    literal constant.

## __Returns__

The return value is of type _logical_ and of the default kind. The result
is true if the sequence of bits represented by _i_ is greater than the
sequence of bits represented by _j_, otherwise the result is false.

## __Standard__

Fortran 2008 and later

## __See Also__

[__bge__(3),](BGE),
[__ble__(3),](BLE),
[__blt__(3)](BLT)

###### fortran-lang intrinsic descriptions
# BLE
## __Name__

__ble__(3) - \[BIT:COMPARE\] Bitwise less than or equal to


## __Syntax__
```fortran
    result = ble(i, j)
```
## __Description__

Determines whether an integer is bitwise less than or equal to another.

## __Arguments__

  - __i__
    : Shall be of _integer_ type.

  - __j__
    : Shall be of _integer_ type, and of the same kind as __i__.

## __Returns__

The return value is of type _logical_ and of the default kind.

## __Standard__

Fortran 2008 and later

## __See Also__

[__bge__(3),](BGE),
[__bgt__(3),](BGT),
[__blt__(3)](BLT)

###### fortran-lang intrinsic descriptions
# BLT
## __Name__

__blt__(3) - \[BIT:COMPARE\] Bitwise less than


## __Syntax__
```fortran
    result = blt(i, j)
```
## __Description__

Determines whether an integer is bitwise less than another.

## __Arguments__

  - __i__
    : Shall be of _integer_ type.

  - __j__
    : Shall be of _integer_ type, and of the same kind as __i__.

## __Returns__

The return value is of type _logical_ and of the default kind.

## __Standard__

Fortran 2008 and later

## __See Also__

[__bge__(3)](BGE),
[__bgt__(3)](BGT),
[__ble__(3)](BLE)

###### fortran-lang intrinsic descriptions
# BIT_SIZE
## __Name__

__bit\_size__(3) - \[BIT:INQUIRY\] Bit size inquiry function

## __Syntax__
```fortran
    result = bit_size(i)
   
     function(kind=KIND) :: bit_size
     integer(kind=KIND),intent(in) :: ii
```
## __Description__

__bit\_size(i)__ returns the number of bits (integer precision plus sign
bit) represented by the type of the _integer_ __i__.  __i__ can be a
scalar or an array.

    

## __Arguments__

  - __i__
    : An _integer_ value of any kind to determine the size of in bits.
    Because only the type of the argument is examined, the argument need
    not be defined.

## __Returns__
    Returns the number of bits used to represent a value of the type
    of __i__.  The result is a _integer_ scalar of the same kind as __i__.

## __Examples__

Sample program:

```fortran
program demo_bit_size
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
implicit none
integer(kind=int64)          :: answer
integer                      :: ilen
character(len=*),parameter   :: fmt='(*(g0,1x))'
    write(*,fmt)'default integer size is',bit_size(0),'bits'
    write(*,fmt)bit_size(bit_size(0_int8)), 'which is kind=',kind(0_int8)
    write(*,fmt)bit_size(bit_size(0_int16)),'which is kind=',kind(0_int16)
    write(*,fmt)bit_size(bit_size(0_int32)),'which is kind=',kind(0_int32)
    write(*,fmt)bit_size(bit_size(0_int64)),'which is kind=',kind(0_int64)

    ! Check size of value not explicitly defined.
    write(*,fmt) int(bit_size(answer))
end program demo_bit_size
```
  Typical Results:
```text
   default integer size is 32 bits
   8 which is kind= 1
   16 which is kind= 2
   32 which is kind= 4
   64 which is kind= 8
   64
```

## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions (license MIT) @urbanjost
# BTEST
## __Name__

__btest__(3) - \[BIT:INQUIRY\] Tests a bit of an _integer_ value.

## __Syntax__
```fortran
   result = btest(i, pos)

    integer(kind=KIND) elemental function btest(i,pos)
    integer,intent(in)  :: i
    logical,intent(out) :: pos
```
 where __KIND__ is any _integer_ kind supported by the programming environment.

## __Description__

__btest(i,pos)__ returns logical __.true.__ if the bit at __pos__ in __i__ is set.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __pos__
    : The bit position to query. it must be a valid position for the
    value __i__; ie.  __0 <= pos <= bit_size(i)__ .

    A value of zero refers to the least significant bit.

## __Returns__

   The result is a _logical_ that has the value __.true.__ if bit
   position __pos__ of __i__ has the value __1__ and the value
   __.false.__ if bit __pos__ of __i__ has the value __0__.

## __Examples__

Sample program:

```fortran
program demo_btest
implicit none
integer :: i, j, pos, a(2,2)
logical :: bool
character(len=*),parameter :: g='(*(g0))'

     i = 32768 + 1024 + 64
    write(*,'(a,i0,"=>",b32.32,/)')'Looking at the integer: ',i

    ! looking one bit at a time from LOW BIT TO HIGH BIT
    write(*,g)'from bit 0 to bit ',bit_size(i),'==>'
    do pos=0,bit_size(i)-1
        bool = btest(i, pos)
        write(*,'(l1)',advance='no')bool
    enddo
    write(*,*)

    ! a binary format the hard way. 
    ! Note going from bit_size(i) to zero.
    write(*,*)
    write(*,g)'so for ',i,' with a bit size of ',bit_size(i)
    write(*,'(b32.32)')i
    write(*,g)merge('^','_',[(btest(i,j),j=bit_size(i)-1,0,-1)])
    write(*,*)
    write(*,g)'and for ',-i,' with a bit size of ',bit_size(i)
    write(*,'(b32.32)')-i
    write(*,g)merge('^','_',[(btest(-i,j),j=bit_size(i)-1,0,-1)])

    ! elemental:
    !
    a(1,:)=[ 1, 2 ]
    a(2,:)=[ 3, 4 ]
    write(*,*)
    write(*,'(a,/,*(i2,1x,i2,/))')'given the array a ...',a
    ! the second bit of all the values in a
    write(*,'(a,/,*(l2,1x,l2,/))')'the value of btest (a, 2)',btest(a,2)
    ! bits 1,2,3,4 of the value 2
    write(*,'(a,/,*(l2,1x,l2,/))')'the value of btest (2, a)',btest(2,a)
end program demo_btest
```
Results:
```text
Looking at the integer: 33856=>11111111111111110111101111000000

00000000000000001000010001000000
11111111111111110111101111000000
1000010001000000
11111111111111110111101111000000
from bit 0 to bit 32==>
FFFFFFTFFFTFFFFTFFFFFFFFFFFFFFFF
 
so for 33856 with a bit size of 32
00000000000000001000010001000000
________________^____^___^______
 
and for -33856 with a bit size of 32
11111111111111110111101111000000
^^^^^^^^^^^^^^^^_^^^^_^^^^______
 
given the array a ...
 1  3
 2  4

the value of btest (a, 2)
 F  F
 F  T

the value of btest (2, a)
 T  F
 F  F
```
## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR),
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR),
[__mvbits__(3)](MVBITS)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# STORAGE_SIZE
## __Name__

__storage\_size__(3) - \[BIT:INQUIRY\] Storage size in bits


## __Syntax__
```fortran
result = storage_size(a, kind)
```
## __Description__

Returns the storage size of argument __a__ in bits.

## __Arguments__

  - __a__
    : Shall be a scalar or array of any type.

  - __kind__
    : (Optional) shall be a scalar integer constant expression.

## __Returns__

The result is a scalar integer with the kind type parameter specified by
__kind__ (or default integer type if __kind__ is missing). The result value is
the size expressed in bits for an element of an array that has the
dynamic type and type parameters of __a__.

## __Examples__

Sample program

```fortran
program demo_storage_size
implicit none
   write(*,*)'size of integer       ',storage_size(0)
   write(*,*)'size of real          ',storage_size(0.0)
   write(*,*)'size of logical       ',storage_size(.true.)
   write(*,*)'size of complex       ',storage_size((0.0,0.0))
   write(*,*)'size of integer array ',storage_size([0,1,2,3,4,5,6,7,8,9])
end program demo_storage_size
```
  Results:
```text
    size of integer                 32
    size of real                    32
    size of logical                 32
    size of complex                 64
    size of integer array           32
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__c\_sizeof__(3)](C_SIZEOF)

###### fortran-lang intrinsic descriptions
# LEADZ
## __Name__

__leadz__(3) - \[BIT:COUNT\] Number of leading zero bits of an integer


## __Syntax__
```fortran
result = leadz(i)
```
## __Description__

__leadz__ returns the number of leading zero bits of an integer.

## __Arguments__

  - __i__
    : Shall be of type _integer_.

## __Returns__

The type of the return value is the same as a default _integer_. If all
the bits of __i__ are zero, the result value is __bit\_size(i)__.

## __Examples__

Sample program:

```fortran
program demo_leadz
implicit none
integer :: value, i
character(len=80) :: f
  write(*,'(*(g0))')'BIT_SIZE=',bit_size(value)
  ! make a format statement for writing a value as a bit string
  write(f,'("(b",i0,".",i0,")")')bit_size(value),bit_size(value)
  ! show output for various integer values
  value=0
  do i=0,bit_size(value)-1
     write (*,'("LEADING ZERO BITS=",i3,1x)') leadz(value)
     write (*,'(" FOR VALUE ")',advance='no')
     write(*,f,advance='no') value
     write(*,'(*(1x,g0))') "OR",value
     value=value+2**(i)
  enddo
end program demo_leadz
```

Results:

```
   BIT_SIZE=32
   LEADING ZERO BITS= 32
    FOR VALUE 00000000000000000000000000000000 OR 0
   LEADING ZERO BITS= 31
    FOR VALUE 00000000000000000000000000000001 OR 1
   LEADING ZERO BITS= 30
    FOR VALUE 00000000000000000000000000000011 OR 3
   LEADING ZERO BITS= 29
    FOR VALUE 00000000000000000000000000000111 OR 7
   LEADING ZERO BITS= 28
    FOR VALUE 00000000000000000000000000001111 OR 15
   LEADING ZERO BITS= 27
    FOR VALUE 00000000000000000000000000011111 OR 31
   LEADING ZERO BITS= 26
    FOR VALUE 00000000000000000000000000111111 OR 63
   LEADING ZERO BITS= 25
    FOR VALUE 00000000000000000000000001111111 OR 127
   LEADING ZERO BITS= 24
    FOR VALUE 00000000000000000000000011111111 OR 255
   LEADING ZERO BITS= 23
    FOR VALUE 00000000000000000000000111111111 OR 511
   LEADING ZERO BITS= 22
    FOR VALUE 00000000000000000000001111111111 OR 1023
   LEADING ZERO BITS= 21
    FOR VALUE 00000000000000000000011111111111 OR 2047
   LEADING ZERO BITS= 20
    FOR VALUE 00000000000000000000111111111111 OR 4095
   LEADING ZERO BITS= 19
    FOR VALUE 00000000000000000001111111111111 OR 8191
   LEADING ZERO BITS= 18
    FOR VALUE 00000000000000000011111111111111 OR 16383
   LEADING ZERO BITS= 17
    FOR VALUE 00000000000000000111111111111111 OR 32767
   LEADING ZERO BITS= 16
    FOR VALUE 00000000000000001111111111111111 OR 65535
   LEADING ZERO BITS= 15
    FOR VALUE 00000000000000011111111111111111 OR 131071
   LEADING ZERO BITS= 14
    FOR VALUE 00000000000000111111111111111111 OR 262143
   LEADING ZERO BITS= 13
    FOR VALUE 00000000000001111111111111111111 OR 524287
   LEADING ZERO BITS= 12
    FOR VALUE 00000000000011111111111111111111 OR 1048575
   LEADING ZERO BITS= 11
    FOR VALUE 00000000000111111111111111111111 OR 2097151
   LEADING ZERO BITS= 10
    FOR VALUE 00000000001111111111111111111111 OR 4194303
   LEADING ZERO BITS=  9
    FOR VALUE 00000000011111111111111111111111 OR 8388607
   LEADING ZERO BITS=  8
    FOR VALUE 00000000111111111111111111111111 OR 16777215
   LEADING ZERO BITS=  7
    FOR VALUE 00000001111111111111111111111111 OR 33554431
   LEADING ZERO BITS=  6
    FOR VALUE 00000011111111111111111111111111 OR 67108863
   LEADING ZERO BITS=  5
    FOR VALUE 00000111111111111111111111111111 OR 134217727
   LEADING ZERO BITS=  4
    FOR VALUE 00001111111111111111111111111111 OR 268435455
   LEADING ZERO BITS=  3
    FOR VALUE 00011111111111111111111111111111 OR 536870911
   LEADING ZERO BITS=  2
    FOR VALUE 00111111111111111111111111111111 OR 1073741823
   LEADING ZERO BITS=  1
    FOR VALUE 01111111111111111111111111111111 OR 2147483647
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__bit\_size__(3)](BIT_SIZE),
[__popcnt__(3)](POPCNT),
[__poppar__(3)](POPPAR),
[__trailz__(3)](TRAILZ)

###### fortran-lang intrinsic descriptions
# POPCNT
## __Name__

__popcnt__(3) - \[BIT:COUNT\] Number of bits set


## __Syntax__
```fortran
result = popcnt(i)
```
## __Description__

Returns the number of bits set in the binary representation of an
_integer_.

## __Arguments__

  - __i__
    : Shall be of type _integer_.

## __Returns__

The return value is of type _integer_ and of the default integer kind.

## __Examples__

Sample program:

```fortran
program demo_popcnt
use, intrinsic :: iso_fortran_env, only : integer_kinds, &
   & int8, int16, int32, int64
implicit none
     print *, popcnt(127),       poppar(127)
     print *, popcnt(huge(0)), poppar(huge(0))
     print *, popcnt(huge(0_int8)), poppar(huge(0_int8))
     print *, popcnt(huge(0_int16)), poppar(huge(0_int16))
     print *, popcnt(huge(0_int32)), poppar(huge(0_int32))
     print *, popcnt(huge(0_int64)), poppar(huge(0_int64))
end program demo_popcnt
```
Results:
```text
        7           1
       31           1
        7           1
       15           1
       31           1
       63           1
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__poppar__(3)](POPPAR),
[__leadz__(3)](LEADZ),
[__trailz__(3)](TRAILZ)

###### fortran-lang intrinsic descriptions
# POPPAR
## __Name__

__poppar__(3) - \[BIT:COUNT\] Parity of the number of bits set


## __Syntax__
```fortran
result = poppar(i)
```
## __Description__

Returns the parity of an integer's binary representation (i.e., the
parity of the number of bits set).

## __Arguments__

  - __i__
    : Shall be of type _integer_.

## __Returns__

The return value is equal to __0__ if __i__ has an even number of bits set and 1 if an odd
number of bits are set.

It is of type _integer_ and of the default _integer_ kind.

## __Examples__

Sample program:

```fortran
program demo_popcnt
use, intrinsic :: iso_fortran_env, only : integer_kinds, &
   & int8, int16, int32, int64
implicit none
   print  *,  popcnt(127),            poppar(127)
   print  *,  popcnt(huge(0_int8)),   poppar(huge(0_int8))
   print  *,  popcnt(huge(0_int16)),  poppar(huge(0_int16))
   print  *,  popcnt(huge(0_int32)),  poppar(huge(0_int32))
   print  *,  popcnt(huge(0_int64)),  poppar(huge(0_int64))
end program demo_popcnt
```
  Results:
```text
              7           1
              7           1
             15           1
             31           1
             63           1
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__popcnt__(3)](POPCNT),
[__leadz__(3)](LEADZ),
[__trailz__(3)](TRAILZ)

###### fortran-lang intrinsic descriptions
# TRAILZ
## __Name__

__trailz__(3) - \[BIT:COUNT\] Number of trailing zero bits of an integer

## __Syntax__
```fortran
   result = trailz(i) integer :: result
   integer(kind=NNN),intent(in) :: i
```
## __Description__

__trailz(3)__ returns the number of trailing zero bits of an _integer_ value

## __Arguments__

  - __i__
    : Shall be of type _integer_.

## __Returns__

The type of the return value is the default _integer_. If all the bits of
I are zero, the result value is __bit\_size(i)__.

## __Examples__

Sample program:

```fortran
program demo_trailz
use, intrinsic :: iso_fortran_env, only : integer_kinds, &
& int8, int16, int32, int64
implicit none
integer(kind=int64) :: i, value
   write(*,*)'Default integer:'
   write(*,*)'bit_size=',bit_size(0)
   write(*,'(1x,i3,1x,i3,1x,b0)')-1,trailz(1),-1
   write(*,'(1x,i3,1x,i3,1x,b0)')0,trailz(0),0
   write(*,'(1x,i3,1x,i3,1x,b0)')1,trailz(1),1
   write(*,'(" huge(0)=",i0,1x,i0,1x,b0)') &
   & huge(0),trailz(huge(0)),huge(0)
   write(*,*)
   write(*,*)'integer(kind=int64):'

   do i=-1,62,5
      value=2**i
      write(*,'(1x,i19,1x,i3)')value,trailz(value)
   enddo
   value=huge(i)
   write(*,'(1x,i19,1x,i3,"(huge(0_int64))")')value,trailz(value)

   do i=-1,62,5
      value=2**i
      write(*,'(1x,i3,2x,b64.64)')i,value
   enddo
   value=huge(i)
   write(*,'(1x,a,1x,b64.64)') "huge",value

end program demo_trailz
```

Results:

```
 Default integer:
 bit_size=          32
  -1   0 11111111111111111111111111111111
   0  32 0
   1   0 1
 huge(0)=2147483647 0 1111111111111111111111111111111

 integer(kind=int64):
                   0  64
                  16   4
                 512   9
               16384  14
              524288  19
            16777216  24
           536870912  29
         17179869184  34
        549755813888  39
      17592186044416  44
     562949953421312  49
   18014398509481984  54
  576460752303423488  59
 9223372036854775807   0(huge(0_int64))
  -1  0000000000000000000000000000000000000000000000000000000000000000
   4  0000000000000000000000000000000000000000000000000000000000010000
   9  0000000000000000000000000000000000000000000000000000001000000000
  14  0000000000000000000000000000000000000000000000000100000000000000
  19  0000000000000000000000000000000000000000000010000000000000000000
  24  0000000000000000000000000000000000000001000000000000000000000000
  29  0000000000000000000000000000000000100000000000000000000000000000
  34  0000000000000000000000000000010000000000000000000000000000000000
  39  0000000000000000000000001000000000000000000000000000000000000000
  44  0000000000000000000100000000000000000000000000000000000000000000
  49  0000000000000010000000000000000000000000000000000000000000000000
  54  0000000001000000000000000000000000000000000000000000000000000000
  59  0000100000000000000000000000000000000000000000000000000000000000
 huge 0111111111111111111111111111111111111111111111111111111111111111
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__bit\_size__(3)](BIT_SIZE),
[__popcnt__(3)](POPCNT),
[__poppar__(3)](POPPAR),
[__leadz__(3)](LEADZ)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# DSHIFTL
## __Name__

__dshiftl__(3) - \[BIT:COPY\] combines bits of arguments __i__ and __j__


## __Syntax__
```fortran
result = dshiftl(i, j, shift)
```
## __Description__

__dshiftl(i, j, shift)__ combines bits of __i__ and __j__. The rightmost __shift__
bits of the result are the leftmost __shift__ bits of __j__, and the remaining
bits are the rightmost bits of __i__.

## __Arguments__

  - __i__
    : Shall be of type _integer_.

  - __j__
    : Shall be of type _integer_, and of the same kind as __i__.

  - __shift__
    : Shall be of type _integer_.

## __Returns__

The return value has same type and kind as __i__.

## __Standard__

Fortran 2008 and later

## __See Also__

[__dshiftr__(3)](DSHIFTR)

###### fortran-lang intrinsic descriptions
# DSHIFTR
## __Name__

__dshiftr__(3) - \[BIT:COPY\] combines bits of arguments __i__ and __j__


## __Syntax__
```fortran
result = dshiftr(i, j, shift)
```
## __Description__

__dshiftr(i, j, shift)__ combines bits of __i__ and __j__. The leftmost __shift__
bits of the result are the rightmost __shift__ bits of __i__, and the remaining
bits are the leftmost bits of __j__.

## __Arguments__

  - __i__
    : Shall be of type _integer_.

  - __j__
    : Shall be of type _integer_, and of the same kind as __i__.

  - __shift__
    : Shall be of type _integer_.

## __Returns__

The return value has same type and kind as __i__.

## __Standard__

Fortran 2008 and later

## __See Also__

[__dshiftl__(3)](DSHIFTL)

###### fortran-lang intrinsic descriptions
# MERGE_BITS
## __Name__

__merge\_bits__(3) - \[BIT:COPY\] Merge bits using a mask

## __Syntax__
```fortran
result = merge_bits(i, j, mask)

    elemental function merge_bits(i,j,mask) result(r)
    integer(kind=KIND) ,intent(in) :: i, j, mask
    integer(kind=KIND) :: r
```
where the result and all input values have the same _integer_ type and
KIND with the exception that the mask and either __i__ or __j__ may be
a BOZ constant.

## __Description__

A common graphics operation in Ternary Raster Operations is to combine
bits from two different sources, generally referred to as bit-blending.
__merge\_bits__ performs a masked bit-blend of __i__ and __j__ using
the bits of the __mask__ value to determine which of the input values
to copy bits from.

Specifically, The k-th bit of the result is equal to the k-th bit of
__i__ if the k-th bit of __mask__ is __1__; it is equal to the k-th bit
of __j__ otherwise (so all three input values must have the same number
of bits).

The resulting value is the same as would result from

    __ior (iand (i, mask),iand (j, not (mask)))__

An exception to all values being of the same _integer_ type is that __i__
or __j__ and/or the mask may be a BOZ constant (A BOZ constant means it is
either a Binary, Octal, or Hexadecimal literal constant).  The BOZ values
are converted to the _integer_ type of the non-BOZ value(s) as if called
by the intrinsic function __int()__ with the kind of the non-BOZ value(s),
so the BOZ values must be in the range of the type of the result.

## __Arguments__

  - __i__ 
  : value to select bits from when the associated bit in the mask is __1__.

  - __j__
  : value to select bits from when the associated bit in the mask is __0__.

  - __mask__
  : a value whose bits are used as a mask to select bits from __i__ and __j__

## __Returns__

The bits blended from __i__ and __j__ using the mask __mask__. It is the
same type as __i__ if __i__ is of type _integer_, otherwise the same type
as __j__.

## __Example__
```fortran
program demo_merge_bits
use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64
implicit none
integer(kind=int16) :: if_one,if_zero,msk
character(len=*),parameter :: fmt='(*(g0, 1X))'

   ! basic usage
   print *,'MERGE_BITS( 5,10,41) should be 3.=>',merge_bits(5,10,41)
   print *,'MERGE_BITS(13,18,22) should be 4.=>',merge_bits(13,18,22)

   ! use some values in base2 illustratively:
   if_one =int(b'1010101010101010',kind=int16)
   if_zero=int(b'0101010101010101',kind=int16)

   msk=int(b'0101010101010101',kind=int16)
   print '("should get all zero bits =>",b16.16)', &
   & merge_bits(if_one,if_zero,msk) 

   msk=int(b'1010101010101010',kind=int16)
   print '("should get all ones bits =>",b16.16)', &
   & merge_bits(if_one,if_zero,msk) 

   ! using BOZ values
   print fmt, &
   & merge_bits(32767_int16,    o'12345',         32767_int16), &
   & merge_bits(o'12345'   , 32767_int16, b'0000000000010101'), &
   & merge_bits(32767_int16,    o'12345',             z'1234')

   ! a do-it-yourself equivalent for comparison and validation
   print fmt, &
   & ior(iand(32767_int16, 32767_int16),                   &
   &   iand(o'12345', not(32767_int16))),                  &

   & ior(iand(o'12345', int(o'12345', kind=int16)),        &
   &   iand(32767_int16, not(int(o'12345', kind=int16)))), &

   & ior(iand(32767_int16, z'1234'),                       &
   &   iand(o'12345', not(int( z'1234', kind=int16))))

end program demo_merge_bits
```
  Results:
```text
    MERGE_BITS( 5,10,41) should be 3.=>           3
    MERGE_BITS(13,18,22) should be 4.=>           4
   should get all zero bits =>0000000000000000
   should get all ones bits =>1111111111111111
   32767 32751 5877
   32767 32767 5877
```

## __Standard__

Fortran 2008 and later

###### fortran-lang intrinsic descriptions (license MIT) @urbanjost
# MVBITS
## __Name__

__mvbits__(3) - \[BIT:COPY\] reproduce bit patterns found in one integer in another


## __Syntax__
```fortran
call mvbits(from, frompos, len, to, topos)
```
## __Description__

__mvbits(3f)__ copies a bit pattern found in a range of adjacent bits in
the _integer_ __from__ to a specified position in another integer __to__
(which is of the same kind as __from__).  It otherwise leaves the bits
in __to__ as-is.

The bit positions copied must exist within the value of __from__.
That is, the values of __frompos+len-1__ and __topos+len-1__ must be
nonnegative and less than __bit\_size__(from).

The bits are numbered __0__ to __bit_size(i)-1__, from right to left.

## __Arguments__

  - __from__
    : An _integer_ to read bits from.
  - __frompos__
    : __frompos__ is the position of the first bit to copy. It is a
    nonnegative _integer_ value < __bit_size(from)__.
  - __len__
    : A nonnegative _integer_ value that indicates how many bits to
    copy from __from__. It must not specify copying bits past the end
    of __from__. That is, __frompos + len__ must be less than or equal
    to __bit_size(from)__.
  - __to__
    : The _integer_ variable to place the copied bits into. It must
    be of the same kind as __from__ and may even be the same variable
    as __from__.

    __to__ 
    : is set by copying the sequence of bits of length __len__,
    starting at position __frompos__ of __from__ to position __topos__ of
    __to__. No other bits of __to__ are altered. On return, the __len__
    bits of __to__ starting at __topos__ are equal to the value that
    the __len__ bits of __from__ starting at __frompos__ had on entry.

  - __topos__
    : A nonnegative _integer_ value indicating the starting location in
    __to__ to place the specified copy of bits from __from__. 
    __topos + len__ must be less than or equal to __bit_size(to)__.

## __Example__
  Sample program that populates a new 32-bit integer with its bytes in
  reverse order (ie. changes the Endian of the integer).

      program demo_mvbits
      use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64
      implicit none
      integer(kind=int32) :: intfrom, intto, abcd_int
      character(len=*),parameter :: bits= '(g0,t30,b32.32)'
      character(len=*),parameter :: fmt= '(g0,t30,a,t40,b32.32)'

         intfrom=huge(0)  ! all bits are 1 accept the sign bit
	 intto=0          ! all bits are 0

         !! CHANGE BIT 0
         ! show the value and bit pattern
         write(*,bits)intfrom,intfrom
         write(*,bits)intto,intto

         ! copy bit 0 from intfrom to intto to show the rightmost bit changes
         !          (from,    frompos, len,    to, topos)
         call mvbits(intfrom,       0,   1, intto,     0) ! change bit 0
         write(*,bits)intto,intto

         !! COPY PART OF A VALUE TO ITSELF
	 call mvbits(intfrom,0,1,intfrom,31) ! can copy bit from a value to itself
         write(*,bits)intfrom,intfrom

         !! MOVING BYTES AT A TIME
         ! make native integer value with bit patterns
         ! that happen to be the same as the beginning of the alphabet
	 ! to make it easy to see the bytes are reversed
         abcd_int=transfer('abcd',0)
         ! show the value and bit pattern
         write(*,*)'native'
         write(*,fmt)abcd_int,abcd_int,abcd_int
      
         ! change endian of the value
         abcd_int=int_swap32(abcd_int)
         ! show the values and their bit pattern
         write(*,*)'non-native'
         write(*,fmt)abcd_int,abcd_int,abcd_int
      
      contains
      
      pure elemental function int_swap32(intin) result(intout)
      ! Convert a 32 bit integer from big Endian to little Endian, 
      ! or conversely from little Endian to big Endian.
      !               
      integer(kind=int32), intent(in)  :: intin
      integer(kind=int32) :: intout
         ! copy bytes from input value to new position in output value
         !          (from,  frompos, len,     to, topos)
         call mvbits(intin,       0,   8, intout,    24) ! byte1 to byte4
         call mvbits(intin,       8,   8, intout,    16) ! byte2 to byte3
         call mvbits(intin,      16,   8, intout,     8) ! byte3 to byte2
         call mvbits(intin,      24,   8, intout,     0) ! byte4 to byte1
      end function int_swap32
      
      end program demo_mvbits
```
  Results:
```text

   2147483647                   01111111111111111111111111111111
   0                            00000000000000000000000000000000
   1                            00000000000000000000000000000001
   -1                           11111111111111111111111111111111
    native
   1684234849                   abcd      01100100011000110110001001100001
    non-native
   1633837924                   dcba      01100001011000100110001101100100
================================================================================
```
## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR), 
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR)

###### fortran-lang intrinsic descriptions (license MIT) @urbanjost

# IBITS
## __Name__

__ibits__(3) - \[BIT:COPY\] Bit extraction


## __Syntax__
```fortran
result = ibits(i, pos, len)
```
## __Description__

__ibits__ extracts a field of length __len__ from __i__, starting from
bit position __pos__ and extending left for __len__ bits. The result is
right-justified and the remaining bits are zeroed. The value of pos+len
must be less than or equal to the value __bit\_size(i)__.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __pos__
    : The type shall be _integer_. A value of zero refers to the least
    significant bit.

  - __len__
    : The type shall be _integer_.

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR), 
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR),
[__mvbits__(3)](MVBITS)


###### fortran-lang intrinsic descriptions
# IBCLR
## __Name__

__ibclr__(3) - \[BIT:SET\] Clear bit


## __Syntax__
```fortran
result = ibclr(i, pos)
```
## __Description__

__ibclr__ returns the value of __i__ with the bit at position __pos__ set to zero.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __pos__
    : The type shall be _integer_. A value of zero refers to the least
    significant bit. __pos__ is an __intent(in)__ scalar or array of type
    _integer_. The value of __pos__ must be within the range zero to
    __(bit\_size(i)-1__).

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR), 
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR),
[__mvbits__(3)](MVBITS)

###### fortran-lang intrinsic descriptions
# IBSET
## __Name__

__ibset__(3) - \[BIT:SET\] Set bit


## __Syntax__
```fortran
result = ibset(i, pos)
```
## __Description__

__ibset__ returns the value of __i__ with the bit at position __pos__ set to one.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __pos__
    : The type shall be _integer_. A value of zero refers to the least
    significant bit. pos is an __intent(in)__ scalar or array of type
    _integer_. The value of pos must be within the range zero to
    __(bit\_size(i)-1__).

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR), 
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR),
[__mvbits__(3)](MVBITS)

###### fortran-lang intrinsic descriptions
# MASKL
## __Name__

__maskl__(3) - \[BIT:SET\] Generates a left justified mask

## __Syntax__
```fortran
result = maskl(i, kind)

  integer elemental function maskl(i,kind)
  integer,intent(in),optional :: kind
```
## __Description__

__maskl(i\[, *kind*\])__ has its leftmost __i__ bits set to __1__, and the
remaining bits set to __0__.

## __Arguments__

  - __i__
    : Shall be of type _integer_.
      Its value must be non-negative, and less than or equal to the
      number of bits for the kind of the result.

  - __kind__
    : Shall be a scalar constant expression of type _integer_.

## __Returns__

The return value is of type _integer_. If __kind__ is present, it specifies
the kind value of the return type; otherwise, it is of the default
integer kind.

The leftmost __i__ bits are set to 1 and the other bits are set to 0.

## __Examples__
Sample program:
```fortran
program demo_maskl
implicit none
integer :: i
   i=maskl(1)
   write(*,'(i0,1x,b0,/)') i,i
   ! elemental
   write(*,'(*(i11,1x,b0,1x,/))') maskl([(i,i,i=1,bit_size(0))])
end program demo_maskl
```
  Results:
```text
-2147483648 10000000000000000000000000000000

          0 0
-2147483648 10000000000000000000000000000000
-1073741824 11000000000000000000000000000000
 -536870912 11100000000000000000000000000000
 -268435456 11110000000000000000000000000000
 -134217728 11111000000000000000000000000000
  -67108864 11111100000000000000000000000000
  -33554432 11111110000000000000000000000000
  -16777216 11111111000000000000000000000000
   -8388608 11111111100000000000000000000000
   -4194304 11111111110000000000000000000000
   -2097152 11111111111000000000000000000000
   -1048576 11111111111100000000000000000000
    -524288 11111111111110000000000000000000
    -262144 11111111111111000000000000000000
    -131072 11111111111111100000000000000000
     -65536 11111111111111110000000000000000
     -32768 11111111111111111000000000000000
     -16384 11111111111111111100000000000000
      -8192 11111111111111111110000000000000
      -4096 11111111111111111111000000000000
      -2048 11111111111111111111100000000000
      -1024 11111111111111111111110000000000
       -512 11111111111111111111111000000000
       -256 11111111111111111111111100000000
       -128 11111111111111111111111110000000
        -64 11111111111111111111111111000000
        -32 11111111111111111111111111100000
        -16 11111111111111111111111111110000
         -8 11111111111111111111111111111000
         -4 11111111111111111111111111111100
         -2 11111111111111111111111111111110
         -1 11111111111111111111111111111111
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__maskr__(3)](MASKR)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# MASKR
## __Name__

__maskr__(3) - \[BIT:SET\] Generates a right-justified mask

## __Syntax__
```fortran
result = maskr(i, kind)

  integer elemental function maskr(i,kind)
  integer,intent(in),optional :: kind
```
## __Description__

__maskr(i\[, kind\])__ has its rightmost __i__ bits set to 1, and the
remaining bits set to 0.

## __Arguments__

  - __i__
    : Shall be of type _integer_.
      Its value must be non-negative, and less than or equal to the
      number of bits for the kind of the result.

  - __kind__
    : Shall be a scalar constant expression of type _integer_.

## __Returns__

The return value is of type _integer_. If __kind__ is present, it
specifies the kind value of the return type; otherwise, it is of the
default integer kind.

It has its rightmost __i__ bits set to 1, and the remaining bits set to 0.

##  __Example__

Sample program:
```fortrqn
program demo_maskr
implicit none
integer :: i
   i=maskr(1)
   write(*,'(i0,1x,b0,1x,b0/)') i,i, shiftl(7,bit_size(0)-1)
   i=maskr(5)
   write(*,'(i0,1x,b0,1x,b0/)') i,i, shiftl(7,bit_size(0)-5)
   i=maskr(11)
   write(*,'(i0,1x,b0,1x,b0/)') i,i, shiftl(7,bit_size(0)-11)
   ! elemental
   write(*,'(*(i11,1x,b0,1x,/))') maskr([(i,i,i=0,bit_size(0))])
end program demo_maskr
```
Results:
```text
1 1 10000000000000000000000000000000

31 11111 111000000000000000000000000000

2047 11111111111 111000000000000000000000

          0 0
          1 1
          3 11
          7 111
         15 1111
         31 11111
         63 111111
        127 1111111
        255 11111111
        511 111111111
       1023 1111111111
       2047 11111111111
       4095 111111111111
       8191 1111111111111
      16383 11111111111111
      32767 111111111111111
      65535 1111111111111111
     131071 11111111111111111
     262143 111111111111111111
     524287 1111111111111111111
    1048575 11111111111111111111
    2097151 111111111111111111111
    4194303 1111111111111111111111
    8388607 11111111111111111111111
   16777215 111111111111111111111111
   33554431 1111111111111111111111111
   67108863 11111111111111111111111111
  134217727 111111111111111111111111111
  268435455 1111111111111111111111111111
  536870911 11111111111111111111111111111
 1073741823 111111111111111111111111111111
 2147483647 1111111111111111111111111111111
         -1 11111111111111111111111111111111
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__maskl__(3)](MASKL)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# IPARITY
## __Name__

__iparity__(3) - \[BIT:LOGICAL\] Bitwise exclusive or of array elements


## __Syntax__
```fortran
  result = iparity(array, mask)

   or

  result = iparity(array, dim, mask)
```
## __Description__

Reduces with bitwise _xor_ (exclusive _or_) the elements of __array__ along
dimension __dim__ if the corresponding element in __mask__ is __.true.__.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_

  - __dim__
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from __"1" to "n"__, where __"n"__ equals the rank of __array__.

  - __mask__
    : (Optional) shall be of type _logical_ and either be a scalar or an
    array of the same shape as __array__.

## __Returns__

The result is of the same type as __array__.

If __dim__ is absent, a scalar with the bitwise _xor_ of all elements in __array__
is returned. Otherwise, an array of rank __n-1__, where __n__ equals the
rank of __array__, and a shape similar to that of __array__ with dimension __dim__
dropped is returned.

## __Examples__

Sample program:

```fortran
program demo_iparity
implicit none
integer, dimension(2) :: a
  a(1) = int(b'00100100')
  a(2) = int(b'01101010')
  print '(b8.8)', iparity(a)
end program demo_iparity
```

Results:

```
   01001110
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__iany__(3)](IANY),
[__iall__(3)](IALL),
[__ieor__(3)](IEOR),
[__parity__(3)](PARITY)

###### fortran-lang intrinsic descriptions
# IALL
## __Name__

__iall__(3) - \[BIT:LOGICAL\] Bitwise and of array elements


## __Syntax__
```fortran
  result = iall(array, mask)

    or

  result = iall(array, dim, mask)
```
## __Description__

Reduces with bitwise _and_ the elements of __array__ along dimension __dim__ if
the corresponding element in __mask__ is __.true.__.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_

  - __dim__
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from __1 to n__, where __n__ equals the rank of __array__.

  - __mask__
    : (Optional) shall be of type _logical_ and either be a scalar or an
    array of the same shape as __array__.

## __Returns__

The result is of the same type as __array__.

If __dim__ is absent, a scalar with the bitwise _all_ of all elements in __array__
is returned. Otherwise, an array of rank __n-1__, where __n__ equals the
rank of __array__, and a shape similar to that of __array__ with dimension __dim__
dropped is returned.

## __Examples__

Sample program:

```fortran
program demo_iall
use, intrinsic :: iso_fortran_env, only : integer_kinds, &
 & int8, int16, int32, int64
implicit none
integer(kind=int8) :: a(2)

   a(1) = int(b'00100100')
   a(2) = int(b'01101010')

   print '(b8.8)', iall(a)

end program demo_iall
```
  Results:
```text
   00100000
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__iany__(3)](IANY),
[__iparity__(3)](IPARITY),
[__iand__(3)](IAND)

###### fortran-lang intrinsic descriptions
# IAND
## __Name__

__iand__(3) - \[BIT:LOGICAL\] Bitwise logical and


## __Syntax__
```fortran
result = iand(i, j)
```
## __Description__

Bitwise logical __and__.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __j__
    : The type shall be _integer_, of the same kind as __i__.

## __Returns__

The return type is _integer_, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

## __Examples__

Sample program:

```fortran
program demo_iand
implicit none
integer :: a, b
      data a / z'f' /, b / z'3' /
      write (*,*) iand(a, b)
end program demo_iand
```
  Results:
```text
              3
```
## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR), 
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR),
[__mvbits__(3)](MVBITS)

###### fortran-lang intrinsic descriptions
# IANY
## __Name__

__iany__(3) - \[BIT:LOGICAL\] Bitwise or of array elements


## __Syntax__
```fortran
  result = iany(array, mask)

    or

  result = iany(array, dim, mask)
```
## __Description__

Reduces with bitwise or (inclusive or) the elements of __array__ along
dimension __dim__ if the corresponding element in __mask__ is __.true.__.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_

  - __dim__
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from __1 to n__, where __n__ equals the rank of __array__.

  - __mask__
    : (Optional) shall be of type _logical_ and either be a scalar or an
    array of the same shape as __array__.

## __Returns__

The result is of the same type as __array__.

If __dim__ is absent, a scalar with the bitwise _or_ of all elements in __array__
is returned. Otherwise, an array of rank __n-1__, where __n__ equals the
rank of __array__, and a shape similar to that of __array__ with dimension __dim__
dropped is returned.

## __Examples__

Sample program:

```fortran
program demo_iany
use, intrinsic :: iso_fortran_env, only : integer_kinds, &
 & int8, int16, int32, int64
implicit none
integer(kind=int8) :: a(2)
     a(1) = int(b'00100100')
     a(2) = int(b'01101010')
     print '(b8.8)', iany(a)
end program demo_iany
```
Results:

```
   01101110
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__iparity__(3)](IPARITY),
[__iall__(3)](IALL),
[__ior__(3)](IOR)

###### fortran-lang intrinsic descriptions
# IEOR
## __Name__

__ieor__(3) - \[BIT:LOGICAL\] Bitwise logical exclusive or


## __Syntax__
```fortran
result = ieor(i, j)
```
## __Description__

__ieor__ returns the bitwise Boolean exclusive-__or__ of __i__ and __j__.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __j__
    : The type shall be _integer_, of the same kind as __i__.

## __Returns__

The return type is _integer_, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR), 
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__mvbits__(3)](MVBITS)

###### fortran-lang intrinsic descriptions
# IOR
## __Name__

__ior__(3) - \[BIT:LOGICAL\] Bitwise logical inclusive or


## __Syntax__
```fortran
   result = ior(i, j)
    integer,intent(in) :: i
    integer,intent(in) :: j
```
## __Description__

__ior__ returns the bit-wise Boolean inclusive-__or__ of __i__ and __j__.

## __Arguments__

  - __i__
    : an _integer_ scalar or array.

  - __j__
    : _integer_ scalar or array, of the same kind as __i__.

## __Returns__

The return type is _integer_, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

## __Examples__

Sample program:

```fortran
program demo_ior
implicit none
integer :: i, j, k
   i=53       ! i=00110101 binary (lowest order byte)
   j=45       ! j=00101101 binary (lowest order byte)
   k=ior(i,j) ! k=00111101 binary (lowest order byte) , k=61 decimal
   write(*,'(i8,1x,b8.8)')i,i,j,j,k,k
end program demo_ior
```

Results:

```
         53 00110101
         45 00101101
         61 00111101
```

## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR), 
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ieor__(3)](IEOR),
[__mvbits__(3)](MVBITS)

###### fortran-lang intrinsic descriptions
# NOT
## __Name__
__not__(3) - \[BIT:LOGICAL\] Logical negation

## __Syntax__
```fortran
result = not(i)
```
## __Description__

NOT returns the bitwise Boolean inverse of I.

## __Arguments__

  - __i__
    : The type shall be _integer_.

## __Returns__

The return type is _integer_, of the same kind as the argument.

## __Examples__

Sample program

```fortran
program demo_not
implicit none
integer :: i

   i=13741
   write(*,'(b32.32,1x,i0)')i,i
   write(*,'(b32.32,1x,i0)')not(i),not(i)

end program demo_not
```

Results:

```
   00000000000000000011010110101101 13741
   11111111111111111100101001010010 -13742
```

## __Standard__

Fortran 95 and later

## __See Also__

[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),

[__ibclr__(3)](IBCLR)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# ISHFTC
## __Name__

__ishftc__(3) - \[BIT:SHIFT\] Shift bits circularly


## __Syntax__
```fortran
result = ishftc(i, shift, size)
```
## __Description__

__ishftc__(3) returns a value corresponding to __i__ with the rightmost __size__ bits
shifted circularly __shift__ places; that is, bits shifted out one end are
shifted into the opposite end. A value of __shift__ greater than zero
corresponds to a left shift, a value of zero corresponds to no shift,
and a value less than zero corresponds to a right shift. The absolute
value of __shift__ must be less than __size__. If the __size__ argument is omitted,
it is taken to be equivalent to __bit\_size(i)__.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __shift__
    : The type shall be _integer_.

  - __size__
    : (Optional) The type shall be _integer_; the value must be greater than
    zero and less than or equal to __bit\_size__(i).

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 95 and later

## __See Also__

[__ishft__(3)](ISHFT)

###### fortran-lang intrinsic descriptions
# ISHFT
## __Name__

__ishft__(3) - \[BIT:SHIFT\] Shift bits


## __Syntax__
```fortran
result = ishft(i, shift)
```
## __Description__

__ishft__(3) returns a value corresponding to __i__ with all of the bits shifted
__shift__ places. A value of __shift__ greater than zero corresponds to a left
shift, a value of zero corresponds to no shift, and a value less than
zero corresponds to a right shift. If the absolute value of __shift__ is
greater than __bit\_size(i)__, the value is undefined. Bits shifted out
from the left end or right end are lost; zeros are shifted in from the
opposite end.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __shift__
    : The type shall be _integer_.

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 95 and later

## __See Also__

[__ishftc__(3)](ISHFTC)

###### fortran-lang intrinsic descriptions
# SHIFTA
## __Name__

__shifta__(3) - \[BIT:SHIFT\] shift bits right with fill


## __Syntax__
```fortran
result = shifta(i, shift)
```
## __Description__

Returns a value corresponding to __i__ with all of the bits shifted right by
__shift__ places. If the absolute value of __shift__ is greater than
__bit\_size(i)__, the value is undefined. Bits shifted out from the
right end are lost. The fill is arithmetic: the bits shifted in from the
left end are equal to the leftmost bit, which in two's complement
representation is the sign bit.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __shift__
    : The type shall be _integer_.

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 2008 and later

## __See Also__

[__shiftl__(3)](SHIFTL),
[__shiftr__(3)](SHIFTR)

###### fortran-lang intrinsic descriptions
# SHIFTL
## __Name__

__shiftl__(3) - \[BIT:SHIFT\] shift bits left


## __Syntax__
```fortran
result = shiftl(i, shift)
```
## __Description__

Returns a value corresponding to __i__ with all of the bits shifted left by
__shift__ places. If the absolute value of __shift__ is greater than
__bit\_size(i)__, the value is undefined. Bits shifted out from the left
end are lost, and bits shifted in from the right end are set to __0__.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __shift__
    : The type shall be _integer_.

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 2008 and later

## __See Also__

[__shifta__(3)](SHIFTA),
[__shiftr__(3)](SHIFTR)

###### fortran-lang intrinsic descriptions
# SHIFTR
## __Name__

__shiftr__(3) - \[BIT:SHIFT\] shift bits right


## __Syntax__
```fortran
result = shiftr(i, shift)
```
## __Description__

Returns a value corresponding to __i__ with all of the bits shifted right by
__shift__ places. If the absolute value of __shift__ is greater than
__bit\_size(i)__, the value is undefined. Bits shifted out from the
right end are lost, and bits shifted in from the left end are set to 0.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __shift__
    : The type shall be _integer_.

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 2008 and later

## __See Also__

[__shifta__(3)](SHIFTA),
[__shiftl__(3)](SHIFTL)

###### fortran-lang intrinsic descriptions
