---
layout: book
title: achar
permalink: /learn/intrinsics/ACHAR
---
### NAME

__achar__(3f) - \[CHARACTER\] returns a character in a specified position in the ASCII collating sequence

### SYNTAX

    result = achar(i [, kind])

     character(len=1),elemental :: achar
     integer(kind=KIND),intent(in) :: i
     integer(kind=KIND),intent(in),optional :: kind

where KIND may be any supported kind value for INTEGER types.

### DESCRIPTION

__achar__(I) returns the character located at position I (often called the 
_ADE_ or ASCII Decimal Equivalent) in the ASCII collating sequence.

The __achar(3f)__ function is often used for generating in-band escape
sequences to control terminal attributes.
```fortran
   write(*,'(*(a))')achar(27),'[2J'
```
will clear the screen on an ANSI-compatible terminal display, for
example.

### ARGUMENTS

  - __I__
    the INTEGER value to convert to an ASCII character, therefore in
    the range 0 to 127.

  - __KIND__
    (optional) an INTEGER initialization expression indicating the kind
    parameter of the result.

### RETURN VALUE

The return value is of type CHARACTER with a length of one. If the KIND
argument is present, the return value is of the specified kind and of
the default kind otherwise.

### EXAMPLE

Sample program:

```fortran
program demo_achar
use,intrinsic::iso_fortran_env,only:int8,int16,int32,int64 
implicit none
integer,kind=int8) :: byte
integer :: i
   i=65
   write(*,'("decimal     =",i0)')i
   write(*,'("character   =",a1)')achar(i)
   write(*,'("binary      =",b0)')achar(i)
   write(*,'("octal       =",o0)')achar(i)
   write(*,'("hexadecimal =",z0)')achar(i)

   write(*,'(8(i3,1x,a,1x),/)')(i,achar(i), i=32,126)
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
```
### NOTE

see \[\[ichar\]\] for a discussion of converting between numerical
values and formatted string representations.

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

### STANDARD

FORTRAN 77 and later, with KIND argument Fortran 2003 and later

### SEE ALSO

[__char__(3)](CHAR.md),
[__iachar__(3)](IACHAR.md),
[__ichar__(3)](ICHAR.md)

### RESOURCES

- [ANSI escape sequences](https://en.wikipedia.org/wiki/ANSI_escape_code)
- [M_attr module](https://github.com/urbanjost/M_attr) for controlling ANSI-compatible terminals


###### fortran-lang intrinsic descriptions
