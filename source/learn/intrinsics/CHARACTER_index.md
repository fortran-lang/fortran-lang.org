# CHARACTER_index
### basic procedures specifically for manipulating _character_ variables

# LEN
## __Name__

__len__(3) - \[CHARACTER\] Length of a character entity

## __Syntax__
```fortran
   l = len(string, kind)

    integer(kind=KIND) function len(string,kind) result(value)
    character(len=*),intent(in) :: string
    integer,optional,intent(in) :: KIND
    integer(kind=KIND) :: value
```
where the returned value is the same kind as the __KIND__, or of
the default kind if __KIND__ is not specified.

## __Description__

__len(3)__ Returns the length of a _character_ string.

If __string__ is an array, the length of an element of __string__
is returned.

Note that __string__ need not be defined when this intrinsic is invoked,
as only the length (not the content) of __string__ is needed.

## __Arguments__

  - __string__
    : Shall be a scalar or array of type _character_.

  - __kind__
    : An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default integer kind.

## __Standard__

FORTRAN 77 and later; with __kind__ argument - Fortran 2003 and later

## __Examples__

Sample program

```fortran
program demo_len
implicit none
character(len=40) :: string
character(len=:),allocatable :: astring
character(len=:),allocatable :: many_strings(:)
integer :: ii

   ii=len(string)
  write(*,*)'length =',ii

  ! the string length will be constant for the fixed-length variable
  string=' How long is this string? '
  write(*,'(a)')' ',string,repeat('=',len(string))

  ! the allocatable string length will be the length of LHS expression
  astring=' How long is this string? '
  write(*,'(a)')' ',astring,repeat('=',len(astring))
   
   ! you can also query the length (and other attributes) of a string
   ! using a "type parameter inquiry:" (available since fortran 2018)
   write(*,*)'length from type parameter inquiry=',string%len

   ! a scalar is returned for an array, as all values in a Fortran
   ! character array must be of the same length:

   ! define an allocatable array with a constructor ...
     many_strings = [ character(len=7) :: 'Takata', 'Tanaka', 'Hayashi' ]
   write(*,*)
   write(*,*)'length of ALL elements of array=',len(many_strings)

   call proc_star(' how long? ')

contains

   subroutine proc_star(str)
   character(len=*),intent(in)  :: str
   character(len=:),allocatable :: str2
   ! the length of str can be used in the definitions of variables
   character(len=len(str))      :: str3

      if(allocated(str2))deallocate(str2)
      ! syntax for allocating a scalar string
      allocate(character(len=len(str)) :: str2)

      write(*,*)len(str),len(str2),len(str3)
      ! these are other allowable ways to define str2
      str2=str
      str2=repeat(' ',len(str))

   end subroutine proc_star

end program demo_len
```
Results:
```text
```
## __See Also__

len_trim(3), adjustr(3), trim(3), and adjustl(3) are related routines that
allow you to deal with leading and trailing blanks.

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL),
    [__adjustr__(3)](ADJUSTR),
    [__index__(3)](INDEX),
    [__scan__(3)](SCAN),
    [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT),
    [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# NEW_LINE
## __Name__

__new\_line__(3) - \[CHARACTER\] new-line character

## __Syntax__
```fortran
result = new_line(c)

   character(len=1,kind=kind(c)) :: new_line(c)
   character(len=1),intent(in) :: c(..)
```
## __Description__

__new\_line(c)__ returns the new-line character.

   Case (i)
   : If __a__ is default _character_ and the character in position __10__ of the
   ASCII collating sequence is representable in the default character set,
   then the result is __achar(10)__.

   Case (ii)
   : If __a__ is an ASCII character or an ISO 10646 character, then the
   result is __char(10, kind (a))__.

   Case (iii)
   : Otherwise, the result is a processor-dependent character that
   represents a newline in output to files connected for formatted
   stream output if there is such a character.

   Case (iv)
   : Otherwise, the result is the blank character.

## __Arguments__

  - __C__
    : The argument shall be a scalar or array of the type _character_.

## __Returns__

Returns a _character_ scalar of length one with the new-line character of
the same kind as parameter __c__.


## __Examples__

Sample program:

```fortran
program demo_new_line
implicit none
character,parameter :: nl=new_line('a')
character(len=:),allocatable :: string

   string='This is record 1.'//nl//'This is record 2.'
   write(*,'(a)') string

   write(*,'(*(a))',advance='no') &
      nl,'This is record 1.',nl,'This is record 2.',nl

end program demo_new_line
```
  Results:
```text
   This is record 1.
   This is record 2.

   This is record 1.
   This is record 2.
```
## __Standard__

Fortran 2003 and later

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# REPEAT
## __Name__

__repeat__(3) - \[CHARACTER\] Repeated string concatenation


## __Syntax__
```fortran
result = repeat(string, ncopies)

   character(len=len(string)*ncopies) :: repeat
   character(len=*),intent(in)        :: string
   integer,intent(in)                 :: ncopies
```
## __Description__

Concatenates __ncopies__ copies of a string.

## __Arguments__

  - __string__
    : The input string to repeatedly generate.
    Shall be scalar and of type _character_.

  - __ncopies__
    : Number of copies to make of _string_, greater than or equal to zero (0).
    Shall be scalar and of type _integer_.

## __Returns__

A new scalar of type _character_ built up from __ncopies__ copies of __string__.

## __Examples__

Sample program:

```fortran
program demo_repeat
implicit none
integer :: i, j
    write(*,'(a)') repeat("^v", 36)         ! line break
    write(*,'(a)') repeat("_", 72)          ! line break
    write(*,'(a)') repeat("1234567890", 7)  ! number line
    do i=80,0,-1 ! a simple progress bar
        write(*,'(a)',advance='no') &
        & repeat("#", i)//repeat(' ',80-i)//char(13)
        !do something slow
    enddo
end program demo_repeat
```
  Results:
```
   ^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v
   ________________________________________________________________________
   1234567890123456789012345678901234567890123456789012345678901234567890
```
## __Standard__

Fortran 95 and later

## __See Also__

Functions that perform operations on character strings:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL),
    [__adjustr__(3)](ADJUSTR),
    [__index__(3)](INDEX),
    [__scan__(3)](SCAN),
    [__verify__(3)](VERIFY)

  - __Non-elemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT),
    [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# ACHAR
## __Name__

__achar__(3) - \[CHARACTER:CONVERSION\] returns a character in a specified position in the ASCII collating sequence

## __Syntax__
```fortran
  result = achar(i,kind=KIND)

    character(len=1) elemental function :: achar(i,kind=KIND)

    integer(kind=KIND),intent(in) :: i
    integer(kind=KIND),intent(in),optional :: kind
```
where KIND may be any supported kind value for _integer_ types.

## __Description__

__achar(i)__ returns the character located at position __i__ (commonly called the
_ADE_ or ASCII Decimal Equivalent) in the ASCII collating sequence.

The __achar__(3) function is often used for generating in-band escape
sequences to control terminal attributes.
```fortran
   write(*,'(*(a))')achar(27),'[2J'
```
will clear the screen on an ANSI-compatible terminal display, for
example.

## __Arguments__

  - __i__
    : the _integer_ value to convert to an ASCII character, in the range
    0 to 127.

  - __kind__
    : (optional) an _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is the requested character of type _character_ with a
length of one. If the __kind__ argument is present, the return value is of
the specified kind and of the default kind otherwise.

## __Examples__

```fortran
program demo_achar
use,intrinsic::iso_fortran_env,only:int8,int16,int32,int64
implicit none
integer :: i
   i=65
   write(*,'("decimal     =",i0)')i
   write(*,'("character   =",a1)')achar(i)
   write(*,'("binary      =",b0)')achar(i)
   write(*,'("octal       =",o0)')achar(i)
   write(*,'("hexadecimal =",z0)')achar(i)

   write(*,'(8(i3,1x,a,1x),/)')(i,achar(i), i=32,126)

   write(*,'(a)')upper('Mixed Case')
contains
! a classic use of achar(3) is to convert the case of a string

elemental pure function upper(str) result (string)
!
!$@(#) upper(3f): function to return a trimmed uppercase-only string
!
! input string to convert to all uppercase
character(*), intent(in)      :: str
! output string that contains no miniscule letters
character(len(str))           :: string
integer                       :: i, iend
integer,parameter             :: toupper = iachar('A')-iachar('a')
   iend=len_trim(str)
   ! initialize output string to trimmed input string
   string = str(:iend)
   ! process each letter in the string
   do concurrent (i = 1:iend)
       select case (str(i:i))
       ! located miniscule letter
       case ('a':'z')
          ! change miniscule to majuscule letter
          string(i:i) = achar(iachar(str(i:i))+toupper)
       end select
   enddo
end function upper
end program demo_achar
```
Results:
```
   decimal     =65
   character   =A
   binary      =1000001
   octal       =101
   hexadecimal =41
    32    33 !  34 "  35 #  36 $  37 %  38 &  39 '

    40 (  41 )  42 *  43 +  44 ,  45 -  46 .  47 /

    48 0  49 1  50 2  51 3  52 4  53 5  54 6  55 7

    56 8  57 9  58 :  59 ;  60 <  61 =  62 >  63 ?

    64 @  65 A  66 B  67 C  68 D  69 E  70 F  71 G

    72 H  73 I  74 J  75 K  76 L  77 M  78 N  79 O

    80 P  81 Q  82 R  83 S  84 T  85 U  86 V  87 W

    88 X  89 Y  90 Z  91 [  92 \  93 ]  94 ^  95 _

    96 `  97 a  98 b  99 c 100 d 101 e 102 f 103 g

   104 h 105 i 106 j 107 k 108 l 109 m 110 n 111 o

   112 p 113 q 114 r 115 s 116 t 117 u 118 v 119 w

   120 x 121 y 122 z 123 { 124 | 125 } 126 ~
   MIXED CASE
```
## __Note__

The ADEs (ASCII Decimal Equivalents) for ASCII are

```
*-------*-------*-------*-------*-------*-------*-------*-------*
| 00 nul| 01 soh| 02 stx| 03 etx| 04 eot| 05 enq| 06 ack| 07 bel|
| 08 bs | 09 ht | 10 nl | 11 vt | 12 np | 13 cr | 14 so | 15 si |
| 16 dle| 17 dc1| 18 dc2| 19 dc3| 20 dc4| 21 nak| 22 syn| 23 etb|
| 24 can| 25 em | 26 sub| 27 esc| 28 fs | 29 gs | 30 rs | 31 us |
| 32 sp | 33  ! | 34  " | 35  # | 36  $ | 37  % | 38  & | 39  ' |
| 40  ( | 41  ) | 42  * | 43  + | 44  , | 45  - | 46  . | 47  / |
| 48  0 | 49  1 | 50  2 | 51  3 | 52  4 | 53  5 | 54  6 | 55  7 |
| 56  8 | 57  9 | 58  : | 59  ; | 60  < | 61  = | 62  > | 63  ? |
| 64  @ | 65  A | 66  B | 67  C | 68  D | 69  E | 70  F | 71  G |
| 72  H | 73  I | 74  J | 75  K | 76  L | 77  M | 78  N | 79  O |
| 80  P | 81  Q | 82  R | 83  S | 84  T | 85  U | 86  V | 87  W |
| 88  X | 89  Y | 90  Z | 91  [ | 92  \ | 93  ] | 94  ^ | 95  _ |
| 96  ` | 97  a | 98  b | 99  c |100  d |101  e |102  f |103  g |
|104  h |105  i |106  j |107  k |108  l |109  m |110  n |111  o |
|112  p |113  q |114  r |115  s |116  t |117  u |118  v |119  w |
|120  x |121  y |122  z |123  { |124  | |125  } |126  ~ |127 del|
*-------*-------*-------*-------*-------*-------*-------*-------*
```

## __Standard__

FORTRAN 77 and later, with KIND argument Fortran 2003 and later

## __See Also__

[__char__(3)](CHAR),
[__iachar__(3)](IACHAR),
[__ichar__(3)](ICHAR)

## __Resources__

- [ANSI escape sequences](https://en.wikipedia.org/wiki/ANSI_escape_code)
- [M_attr module](https://github.com/urbanjost/M_attr) for controlling ANSI-compatible terminals


###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# CHAR
## __Name__

__char__(3) - \[CHARACTER\] Character conversion function


## __Syntax__
```fortran
result = char(i, kind)
   elemental integer function char(i,kind)

    integer(kind=KIND),intent(in) :: c
    integer,intent(in),optional :: KIND
```
## __Description__

__char(i, kind)__ returns the character represented by the integer __i__.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _character_

## __Examples__

Sample program:

```fortran
program demo_char
implicit none
integer :: i = 74
character(1) :: c
    c = char(i)
    print *, i, c ! returns 'J'
end program demo_char
```
  Results:
```text
             74 J
```

## __Note__

See [__ichar__(3)](CHAR) for a discussion of converting between numerical
values and formatted string representations.

## __Standard__

FORTRAN 77 and later

## __See Also__

[__achar__(3)](ACHAR),
[__iachar__(3)](IACHAR),
[__ichar__(3)](ICHAR)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL), [__adjustr__(3)](ADJUSTR), [__index__(3)](INDEX), 
    [__scan__(3)](SCAN), [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT), [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# IACHAR
## __Name__

__iachar__(3) - \[CHARACTER:CONVERSION\] Code in ASCII collating sequence


## __Syntax__
```fortran
result = iachar(c, kind)
```
## __Description__

__iachar__(c) returns the code for the ASCII character in the first
character position of C.

## __Arguments__

  - __c__
    : Shall be a scalar _character_, with _intent(in)_

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default integer kind.

## __Examples__

Sample program:

```fortran
program demo_iachar
implicit none
! create function to convert uppercase letters to lowercase
   write(*,'(a)')lower('abcdefg ABCDEFG')
contains
!
elemental pure function lower(str) result (string)
! Changes a string to lowercase
character(*), intent(In)     :: str
character(len(str))          :: string
integer                      :: i
   string = str
   ! step thru each letter in the string in specified range
   do i = 1, len(str)
      select case (str(i:i))
      case ('A':'Z') ! change letter to miniscule
         string(i:i) = char(iachar(str(i:i))+32)
      case default
      end select
   end do
end function lower
!
end program demo_iachar
```
  Results:
```text
   abcdefg abcdefg
```
## __Note__

See [__ichar__(3)](ICHAR) for a discussion of converting between numerical
values and formatted string representations.

## __Standard__

Fortran 95 and later, with KIND argument - Fortran 2003 and later

## __See Also__

[__achar__(3)](ACHAR),
[__char__(3)](CHAR),
[__ichar__(3)](ICHAR)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL), [__adjustr__(3)](ADJUSTR), [__index__(3)](INDEX), 
    [__scan__(3)](SCAN), [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT), [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# ICHAR
## __Name__

__ichar__(3) - \[CHARACTER:CONVERSION\] Character-to-integer conversion function


## __Syntax__
```fortran
   elemental function ichar(c,kind)

    character(len=1),intent(in) :: c
    integer,intent(in),optional :: kind
```
## __Description__

__ichar(c)__ returns the code for the character in the system's native
character set. The correspondence between characters and their codes is
not necessarily the same across different Fortran implementations. For
example, a platform using EBCDIC would return different values than an
ASCII platform.

See __iachar__(3) for specifically working with the ASCII character
set.

## __Arguments__

  - __c__
    : Shall be a scalar _character_, with __intent(in)__

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default _integer_ kind.

## __Examples__

Sample program:

```fortran
program demo_ichar
implicit none
integer i

   write(*,*)ichar(['a','z','A','Z'])
   do i=0,127
      call printme()
   enddo

contains

subroutine printme()
character(len=1) :: letter

   letter=char(i)
   select case(i)
   case (:31,127:)
      write(*,'(1x,i0.3,1x,"HEX=",z2.2,1x,i0)')i,letter,ichar(letter)
   case default
      write(*,'(1x,i0.3,1x,a,1x,i0)')i,letter,ichar(letter)
   end select

end subroutine printme

end program demo_ichar
```

## __Note__

No intrinsic exists to convert between a numeric value and a formatted
character string representation -- for instance, given the _character_
value '154', obtaining an _integer_ or _real_ value with the value 154, or
vice versa. Instead, this functionality is provided by internal-file
I/O, as in the following example:

```
program read_val
integer value
character(len=10) string, string2
   string = '154'

   ! Convert a string to a numeric value
   read (string,'(I10)') value
   print *, value

   ! Convert a value to a formatted string
   write (string2,'(I10)') value
   print *, string2
end program read_val
```
  Results:
```text
            154
           154
```

## __Standard__

Fortran 95 and later, with KIND argument -Fortran 2003 and later

## __See Also__

[__achar__(3)](ACHAR),
[__char__(3)](CHAR),
[__iachar__(3)](IACHAR)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
 [__adjustl__(3)](ADJUSTL),
 [__adjustr__(3)](ADJUSTR),
 [__index__(3)](INDEX),

 [__scan__(3)](SCAN),
 [__verify__(3)](VERIFY)

  - __Nonelemental:__
 [__len\_trim__(3)](LEN_TRIM),
 [__len__(3)](LEN),
 [__repeat__(3)](REPEAT),
 [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# INDEX
## __Name__

__index__(3) - \[CHARACTER:SEARCH\] Position of a substring within a string


## __Syntax__
```fortran
   index(string, substring, back, kind) result(start)

     character(len=*),intent(in) :: string
     character(len=*),intent(in) :: substring
     logical,intent(in),optional :: back
     integer,intent(in),optional :: kind
     integer(kind=KIND)          :: start
```
## __Description__

Returns the position of the start of the leftmost or rightmost
occurrence of string __substring__ in __string__, counting from one. If
__substring__ is not present in __string__, zero is returned.

## __Arguments__

  - __string__
    : string to be searched

  - __substring__
    : string to attempt to locate in __string__

  - __back__
    : If the __back__ argument is present and true, the return value is the
    start of the rightmost occurrence rather than the leftmost.

  - __kind__
    : An _integer_ initialization expression indicating the kind parameter
    of the result.

## __Returns__

  - __START__
    : The return value is of type _integer_ and of kind __kind__. If __kind__ is
    absent, the return value is of default integer kind.

## __Examples__

Example program

```fortran
program demo_index
implicit none
character(len=*),parameter :: str=&
   'Search this string for this expression'
   !1234567890123456789012345678901234567890
   write(*,*)&
      index(str,'this').eq.8,              &
      ! return value is counted from the left end even if BACK=.TRUE.
      index(str,'this',back=.true.).eq.24, &
      ! INDEX is case-sensitive
      index(str,'This').eq.0
end program demo_index
```
Expected Results:
```text
   T T T
```
## __Standard__

FORTRAN 77 and later, with KIND argument Fortran 2003
and later

## __See Also__

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL), [__adjustr__(3)](ADJUSTR), [__index__(3)](INDEX),
    [__scan__(3)](SCAN), [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT), [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# SCAN
## __Name__

__scan__(3) - \[CHARACTER:SEARCH\] Scan a string for the presence of a set of characters


## __Syntax__
```fortran
result = scan(string, set[, back [, kind]])
```
## __Description__

Scans a __string__ for any of the characters in a __set__ of characters.

If __back__ is either absent or equals __.false.__, this function returns the
position of the leftmost character of __STRING__ that is in __set__. If __back__
equals __.true.__, the rightmost position is returned. If no character of __set__
is found in __string__, the result is zero.

## __Arguments__

  - __string__
    : Shall be of type _character_.

  - __set__
    : Shall be of type _character_.

  - __back__
    : (Optional) shall be of type _logical_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default integer kind.

## __Examples__

Sample program:

```fortran
program demo_scan
implicit none
   write(*,*) scan("fortran", "ao")          ! 2, found 'o'
   write(*,*) scan("fortran", "ao", .true.)  ! 6, found 'a'
   write(*,*) scan("fortran", "c++")         ! 0, found none
end program demo_scan
```
  Results:
```text
              2
              6
              0
```
## __Standard__

Fortran 95 and later, with KIND argument - Fortran 2003 and later

## __See Also__

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL), [__adjustr__(3)](ADJUSTR), [__index__(3)](INDEX),
    [__scan__(3)](SCAN), [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT), [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# VERIFY
## __Name__

__verify__(3) - \[CHARACTER:SEARCH\] Scan a string for the absence of a set of characters

## __Syntax__
```fortran
result = verify(string, set, back, kind)

  integer(kind=KIND) elemental function verify(string,set,back,kind)

   character(len=*),intent(in) :: string
   character(len=*),intent(in) :: set
   logical,intent(in),optional :: back
   integer,intent(in),optional :: KIND
```
## __Description__

Verifies that all the characters in __string__ belong to the set of
characters in __set__ by identifying the first character in the string(s)
that is not in the set(s).

If __back__ is either absent or equals __.false.__, this function
returns the position of the leftmost character of __string__ that is
not in __set__.

If __back__ equals __.true.__, the rightmost position is returned.

If all characters of __string__ are found in __set__, the result is zero.

This makes it easy to verify strings are all uppercase or lowercase, 
follow a basic syntax, only contain printable characters, and many of the
conditions tested for with the C routines
__isalnum__(3c), __isalpha__(3c), __isascii__(3c), __isblank__(3c),
__iscntrl__(3c), __isdigit__(3c), __isgraph__(3c), __islower__(3c),
__isprint__(3c), __ispunct__(3c), __isspace__(3c), __isupper__(3c),
and __isxdigit__(3c); but for a string as well an an array of characters.

## __Arguments__

  - __string__
    : Shall be of type _character_.

  - __set__
    : Shall be of type _character_.

  - __back__
    : shall be of type _logical_.

  - __kind__
    : An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__
is absent, the return value is of default integer kind.

## __Examples__

Sample program I:

```fortran
program demo_verify
implicit none
character(len=*),parameter :: int='0123456789'
character(len=*),parameter :: hex='abcdef0123456789'
character(len=*),parameter :: low='abcdefghijklmnopqrstuvwxyz'
character(len=*),parameter :: upp='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
character(len=20):: string='   Howdy There!'
character(len=6) :: strings(2)=["Howdy ","there!"]
character(len=2) :: sets(2)=["de","gh"]
    
   write(*,*)'first non-blank character ',verify(string, ' ')
   ! NOTE: same as len_trim(3)
   write(*,*)'last non-blank character',verify(string, ' ',back=.true.)

   ! first non-lowercase non-blank character
   write(*,*) verify(string,low//' ')          

   !! elemental -- using arrays for both strings and for sets
   
   ! first character in "Howdy" not in "de", and first letter in "there!"
   ! not in "gh"
   write(*,*) verify(strings,sets)                       

   ! check each string from right to left for non-letter
   write(*,*) 'last non-letter',verify(strings,upp//low,back=.true.)         

   ! note character variables in an array have to be of same length
   ! find last non-uppercase character in "Howdy" 
   ! and first non-lowercase in "There!"
   write(*,*) verify(strings,[upp,low],back=[.true.,.false.]) 

   write(*,*) verify("fortran", "", .true.)  ! 7, found 'n'
   ! 0' found none unmatched
   write(*,*) verify("fortran", "nartrof")      


    !! CHECK IF STRING IS OF FORM NN-HHHHH
    CHECK : block
       logical                    :: lout
       character(len=80)          :: chars
   
       chars='32-af43d'
       lout=.true.

       ! are the first two characters integer characters?
       lout = lout.and.(verify(chars(1:2), int) == 0)

       ! is the third character a dash?
       lout = lout.and.(verify(chars(3:3), '-') == 0)

       ! is remaining string a valid representation of a hex value?
       lout = lout.and.(verify(chars(4:8), hex) == 0)

       if(lout)then
          write(*,*)trim(chars),' passed'
       endif

    endblock CHECK
end program demo_verify
```
  Results:
```text
    first non-blank character            4
    last non-blank character          15
              4
              1           1
    last non-letter           6           6
              6           6
              7
              0
    32-af43d passed
```
Sample program II:

Determine if strings are valid integer representations
```fortran
program fortran_ints
implicit none
integer :: i
character(len=*),parameter :: ints(*)=[character(len=10) :: &
 '+1 ', &
 '3044848 ', &
 '30.40 ', &
 'September ', &
 '1 2 3', &
 '  -3000 ', &
 ' ']

   write(*,'("|",*(g0,"|"))') ints
   write(*,'("|",*(1x,l1,8x,"|"))') isint(ints)

contains

elemental function isint(line) result (lout)
!
! determine if string is a valid integer representation 
! ignoring trailing spaces and leading spaces
!
character(len=*),parameter   :: digits='0123456789'
character(len=*),intent(in)  :: line
character(len=:),allocatable :: name
logical                      :: lout
   lout=.false.
   ! make sure at least two characters long to simplify tests
   name=adjustl(line)//'  ' 
   ! blank string
   if( name .eq. '' )return     
   ! allow one leading sign
   if( verify(name(1:1),'+-') == 0 ) name=name(2:) 
   ! was just a sign
   if( name .eq. '' )return 
   lout=verify(trim(name), digits)  == 0  
end function isint

end program fortran_ints
```
Results:
```text
|+1       |3044848  |30.40    |September|1 2 3    |  -3000  |         |
| T       | T       | F       | F       | F       | T       | F       |
```

Sample program III:

Determine if strings represent valid Fortran symbol names
```fortran
program fortran_symbol_name
implicit none
integer :: i
character(len=*),parameter :: symbols(*)=[character(len=10) :: &
 'A_ ', &
 '10 ', &
 'September ', &
 'A B', &
 '_A ', &
 ' ']
   
   write(*,'("|",*(g0,"|"))') symbols
   write(*,'("|",*(1x,l1,8x,"|"))') fortran_name(symbols)

contains

elemental function fortran_name(line) result (lout)
!
! determine if a string is a valid Fortran name 
! ignoring trailing spaces (but not leading spaces)
!
character(len=*),parameter   :: int='0123456789'
character(len=*),parameter   :: lower='abcdefghijklmnopqrstuvwxyz'
character(len=*),parameter   :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
character(len=*),parameter   :: allowed=upper//lower//int//'_'

character(len=*),intent(in)  :: line
character(len=:),allocatable :: name
logical                      :: lout
   name=trim(line)
   if(len(name).ne.0)then
      ! first character is alphameric
      lout = verify(name(1:1), lower//upper) == 0  &
       ! other characters are allowed in a symbol name
       & .and. verify(name,allowed) == 0           &
       ! allowable length
       & .and. len(name) <= 63
   else
      lout = .false.
   endif
end function fortran_name

end program fortran_symbol_name
```

Results:

```text
|A_        |10        |September |A B       |_A        |          |
| T        | F        | T        | F        | F        | F        |
```

## __Standard__

Fortran 95 and later, with __kind__ argument - Fortran 2003 and later

## __See Also__

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL),
    [__adjustr__(3)](ADJUSTR),
    [__index__(3)](INDEX),
    [__scan__(3)](SCAN), 
    [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT), 
    [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# LGE
## __Name__

__lge__(3) - \[CHARACTER:COMPARE\] Lexical greater than or equal


## __Syntax__
```fortran
result = lge(string_a, string_b)
```
## __Description__

Determines whether one string is lexically greater than or equal to
another string, where the two strings are interpreted as containing
ASCII character codes. If the String __a__ and String __b__ are not the same
length, the shorter is compared as if spaces were appended to it to form
a value that has the same length as the longer.

In general, the lexical comparison intrinsics __lge__(3), __lgt__(3), __lle__(3), and __llt__(3)
differ from the corresponding intrinsic operators .ge., .gt., .le., and
.lt., in that the latter use the processor's character ordering (which
is not ASCII on some targets), whereas the former always use the ASCII
ordering.

## __Arguments__

  - __string\_a__
    : Shall be of default _character_ type.

  - __string\_b__
    : Shall be of default _character_ type.

## __Returns__

Returns .true. if string\_a \>= string\_b, and .false. otherwise, based
on the ASCII ordering.

## __Standard__

FORTRAN 77 and later

## __See Also__

__\[\[lgt__(3), __\[\[lle__(3), __\[\[llt__(3)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
 [__adjustl__(3)](ADJUSTL),
 [__adjustr__(3)](ADJUSTR),
 [__index__(3)](INDEX),

 [__scan__(3)](SCAN),
 [__verify__(3)](VERIFY)

  - __Nonelemental:__
 [__len\_trim__(3)](LEN_TRIM),
 [__len__(3)](LEN),
 [__repeat__(3)](REPEAT),
 [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# LGT
## __Name__

__lgt__(3) - \[CHARACTER:COMPARE\] Lexical greater than


## __Syntax__
```fortran
result = lgt(string_a, string_b)
```
## __Description__

Determines whether one string is lexically greater than another string,
where the two strings are interpreted as containing ASCII character
codes. If the String __a__ and String __b__ are not the same length, the shorter
is compared as if spaces were appended to it to form a value that has
the same length as the longer.

In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT
differ from the corresponding intrinsic operators .ge., .gt., .le., and
.lt., in that the latter use the processor's character ordering (which
is not ASCII on some targets), whereas the former always use the ASCII
ordering.

## __Arguments__

  - __string\_a__
    : Shall be of default _character_ type.

  - __string\_b__
    : Shall be of default _character_ type.

## __Returns__

Returns .true. if string\_a \> string\_b, and .false. otherwise, based
on the ASCII ordering.

## __Standard__

FORTRAN 77 and later

## __See Also__

[__lge__(3)](LGE),
[__lle__(3)](LLE),
[__llt__(3)](LLT)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

- __Elemental:__
[__adjustl__(3)](ADJUSTL),
[__adjustr__(3)](ADJUSTR),
[__index__(3)](INDEX),

[__scan__(3)](SCAN),
[__verify__(3)](VERIFY)

- __Nonelemental:__
[__len\_trim__(3)](LEN_TRIM),
[__len__(3)](LEN),
[__repeat__(3)](REPEAT),
[__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# LLE
## __Name__

__lle__(3) - \[CHARACTER:COMPARE\] Lexical less than or equal


## __Syntax__
```fortran
result = lle(str_a, str_b)

   character(len=*),intent(in) :: str_a, str_b

      or

   character(len=*),intent(in) :: str_a, str_b(*) logical :: result
```
## __Description__

Determines whether one string is lexically less than or equal to another
string, where the two strings are interpreted as containing ASCII
character codes. if the __string\_a__ and __string\_b__ are not the same length,
the shorter is compared as if spaces were appended to it to form a value
that has the same length as the longer. Leading spaces are significant.

In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT
differ from the corresponding intrinsic operators .ge., .gt., .le., and
.lt., in that the latter use the processor's character ordering (which
is not ASCII on some targets), whereas the former always use the ASCII
ordering.

## __Arguments__

  - __str\_a__
    : variable or array of default _character_ type.

  - __str\_b__
    : variable or array of default _character_ type.

    if __str_a__ and __str_b__ are both arrays they must be of the
    same shape.

## __Returns__

  - __result__
    Returns __.true.__ if __STR\_A \<= STR\_B__, and __.false.__ otherwise, based on
    the ASCII ordering.

## __Examples__

Sample program:

```fortran
program demo_lle
implicit none
integer             :: i
   write(*,'(*(a))')(char(i),i=32,126)
     write(*,*) lle('abc','ABC')              ! F lowercase is > uppercase
     write(*,*) lle('abc','abc  ')            ! T trailing spaces
     ! If both strings are of zero length the result is true.
     write(*,*) lle('','')                    ! T
     write(*,*) lle('','a')                   ! T the null string is padded
     write(*,*) lle('a','')                   ! F
     write(*,*) lle('abc',['abc','123'])      ! [T,F] scalar and array
     write(*,*) lle(['cba', '123'],'abc')     ! [F,T]
     write(*,*) lle(['abc','123'],['cba','123']) ! [T,T] both arrays
end program demo_lle
```

Results:

```text
  !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ
  [\]^_`abcdefghijklmnopqrstuvwxyz{|}~
  F
  T
  T
  T
  F
  T F
  F T
  T T
```

## __Standard__

FORTRAN 77 and later

## __See Also__

[__lge__(3)](LGE),
[__lgt__(3),](LGT),
[__llt__(3)](LLT)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

 - __Elemental:__
 [__adjustl__(3)](ADJUSTL),
 [__adjustr__(3)](ADJUSTR),
 [__index__(3)](INDEX),

 [__scan__(3)](SCAN),
 [__verify__(3)](VERIFY)

 - __Nonelemental:__
 [__len\_trim__(3)](LEN_TRIM),
 [__len__(3)](LEN),
 [__repeat__(3)](REPEAT),
 [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# LLT
## __Name__

__llt__(3) - \[CHARACTER:COMPARE\] Lexical less than


## __Syntax__
```fortran
result = llt(string_a, string_b)
```
## __Description__

Determines whether one string is lexically less than another string,
where the two strings are interpreted as containing ASCII character
codes. If the __string\_a__ and __string\_b__ are not the same length, the shorter
is compared as if spaces were appended to it to form a value that has
the same length as the longer.

In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT
differ from the corresponding intrinsic operators .ge., .gt., .le., and
.lt., in that the latter use the processor's character ordering (which
is not ASCII on some targets), whereas the former always use the ASCII
ordering.

## __Arguments__

  - __string\_a__
    : Shall be of default _character_ type.

  - __string\_b__
    : Shall be of default _character_ type.

## __Returns__

Returns .true. if string\_a \<= string\_b, and .false. otherwise, based
on the ASCII ordering.

## __Standard__

FORTRAN 77 and later

## __See Also__

[__lge__(3)](LGE),
[__lgt__(3)](LGT),
[__lle__(3](LLE))

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL), [__adjustr__(3)](ADJUSTR), [__index__(3)](INDEX),
    [__scan__(3)](SCAN), [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__len\_trim__(3)](LEN_TRIM),
    [__len__(3)](LEN),
    [__repeat__(3)](REPEAT), [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
# ADJUSTL
## __Name__

__adjustl__(3) - \[CHARACTER:WHITESPACE\] Left-adjust a string

## __Syntax__
```fortran
    result = adjustl(string)

     character(len=(len(string)) elemental function adjustr(a)

     character(len=*),intent(in) :: string
```
## __Description__

__adjustl(string)__ will left-adjust a string by removing leading
spaces. Spaces are inserted at the end of the string as needed.

## __Arguments__

  - __string__
    : the type shall be _character_.

## __Returns__

The return value is of type _character_ and of the same kind as __string__
where leading spaces are removed and the same number of spaces are
inserted on the end of __string__.

## __Examples__

Sample program:

```fortran
program demo_adjustl
implicit none
character(len=20) :: str = '   sample string'
character(len=:),allocatable :: astr
    !
    ! basic use
    str = adjustl(str)
    write(*,'("[",a,"]")') str, trim(str)
    !
    ! an allocatable string stays the same length
    ! and is not trimmed.
    astr='    allocatable string   '
    write(*,'("[",a,"]")') adjustl(astr)
    !
end program demo_adjustl
```
Results:
```text
   [sample string       ]
   [sample string]
   [allocatable string       ]
```
## __Standard__

Fortran 95 and later

## __See Also__

[__adjustr__(3)](ADJUSTR)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# ADJUSTR
## __Name__

__adjustr__(3) - \[CHARACTER:WHITESPACE\] Right-adjust a string

## __Syntax__
```fortran
    result = adjustr(string)

     elemental function adjustr(a)
     character(len=(len(string)) :: adjustr
     character(len=*),intent(in) :: string
```
## __Description__

__adjustr(string)__ right-adjusts a string by removing trailing
spaces. Spaces are inserted at the start of the string as needed to
retain the original length.

## __Arguments__

  - __string__
    : the type shall be _character_.

## __Returns__

The return value is of type _character_ and of the same kind as __string__
where trailing spaces are removed and the same number of spaces are
inserted at the start of __string__.

## __Examples__

Sample program:

```fortran
program demo_adjustr
implicit none
integer :: right
character(len=20) :: str = ' sample string '
character(len=:),allocatable :: str2
   ! print a short number line
   write(*,'(a)')repeat('1234567890',5)

   !
   ! basic usage
   !
   str = adjustr(str)
   write(*,'(a)') str

   !
   ! elemental
   !
   write(*,'(a)')adjustr([character(len=50) :: &
   '  first           ', &
   '     second       ', &
   '         third    ' ])
    
   write(*,'(a)')repeat('1234567890',5)
end program demo_adjustr
```
Results:
```text
   12345678901234567890123456789012345678901234567890
          sample string
                                                first
                                               second
                                                third
   12345678901234567890123456789012345678901234567890
```
## __Standard__

Fortran 95 and later

## __See Also__

[__adjustl__(3)](ADJUSTL)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# LEN_TRIM
## __Name__

__len\_trim__(3) - \[CHARACTER:WHITESPACE\] Length of a character entity without trailing blank characters

## __Syntax__
```fortran
   result = len_trim(string, kind)

    integer(kind=KIND) function len_trim(string,KIND) result (value)
    character(len=*),intent(in) :: string
    integer,optional,intent(in) :: KIND
    integer(kind=KIND) :: value
```
## __Description__

Returns the length of a character string, ignoring any trailing blanks.

## __Arguments__

  - __string__
    : The input string whose length is to be measured.
    Shall be a scalar of type _character_

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default _integer_ kind.

## __Examples__

Sample program

```fortran
program demo_len_trim
implicit none
character(len=:),allocatable :: string
   string=' how long is this string?     '
   write(*,*)'LENGTH=',len(string)
   write(*,*)'TRIMMED LENGTH=',len_trim(string)
   !
   ELE:block ! elemental example
   character(len=:),allocatable :: tablet(:)
   tablet=[character(len=256) :: &
   & ' how long is this string?     ',&
   & 'and this one?']
      write(*,*)'LENGTH=            ',len(tablet)
      write(*,*)'TRIMMED LENGTH=    ',len_trim(tablet)
      write(*,*)'SUM TRIMMED LENGTH=',sum(len_trim(tablet))
   endblock ELE
   !
end program demo_len_trim
```
Results:
```
    LENGTH=          30
    TRIMMED LENGTH=          25
    LENGTH=                     256
    TRIMMED LENGTH=              25          13
    SUM TRIMMED LENGTH=          38
```
## __Standard__

Fortran 95 and later, with __kind__ argument - Fortran 2003
and later

## __See Also__

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    [__adjustl__(3)](ADJUSTL),
    [__adjustr__(3)](ADJUSTR),
    [__index__(3)](INDEX),
    [__scan__(3)](SCAN),
    [__verify__(3)](VERIFY)

  - __Nonelemental:__
    [__repeat__(3)](REPEAT),
    [__len__(3)](LEN),
    [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
# TRIM
## __Name__

__trim__(3) - \[CHARACTER:WHITESPACE\] Remove trailing blank characters of a string


## __Syntax__
```fortran
result = trim(string)
```
## __Description__

Removes trailing blank characters of a string.

## __Arguments__

  - __string__
    : Shall be a scalar of type _character_.

## __Returns__

A scalar of type _character_ which length is that of __string__ less the
number of trailing blanks.

## __Examples__

Sample program:

```fortran
program demo_trim
implicit none
character(len=10), parameter :: s = "gfortran  "
   write(*,*) len(s), len(trim(s))  ! "10 8", with/without trailing blanks

   ! with/without trailing blanks
   write(*,*) len(s), len(trim('   leading'))  
   write(*,*) len(s), len(trim('   trailing    ')) 
   write(*,*) len(s), len(trim('               ')) 

end program demo_trim
```
Results:
```text
      10           8
      10          10
      10          11
      10           0
```
## __Standard__

Fortran 95 and later

## __See Also__

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
 [__adjustl__(3)](ADJUSTL),
 [__adjustr__(3)](ADJUSTR),
 [__index__(3)](INDEX),

 [__scan__(3)](SCAN),
 [__verify__(3)](VERIFY)

  - __Nonelemental:__
 [__len\_trim__(3)](LEN_TRIM),
 [__len__(3)](LEN),
 [__repeat__(3)](REPEAT),
 [__trim__(3)](TRIM)

###### fortran-lang intrinsic descriptions
