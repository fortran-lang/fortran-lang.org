---
layout: book
title: achar
permalink: /learn/intrinsics/ACHAR
---
### NAME

**achar**(3f) - \[CHARACTER\] returns a character in a specified position in the ASCII collating sequence
(GFDL)

### SYNTAX

result = **ACHAR**(I \[, KIND\])

### DESCRIPTION

**achar**(I) returns the character located at position I in the ASCII
collating sequence.

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

### ARGUMENTS

  - **I**
    the type shall be INTEGER.

  - **KIND**
    (optional) an INTEGER initialization expression indicating the kind
    parameter of the result.

### RETURN VALUE

The return value is of type CHARACTER with a length of one. If the KIND
argument is present, the return value is of the specified kind and of
the default kind otherwise.

### EXAMPLE

Sample program:

```
    program demo_achar
    implicit none
    character(len=1) :: c
    integer,parameter :: blank=32
    integer,parameter :: horizonal_tab=11
    integer,parameter :: escape=27
    integer :: i
      c = achar(blank)
      write(*,'(i0,1x,a,1x,b0,1x,o0,1x,z0)')blank,c,c,c,c
      write(*,'(32(a))') (achar(i),i=32,126)
    end program demo_achar
```

Results:

```
   32   100000 40 20
    !"#$%&'()*+,-./0123456789:;<=>?
   @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_
   `abcdefghijklmnopqrstuvwxyz{|}~
```

### NOTE

see \[\[ichar\]\] for a discussion of converting between numerical
values and formatted string representations.

### STANDARD

FORTRAN 77 and later, with KIND argument Fortran 2003 and later

### CLASS

Elemental function

### SEE ALSO

**char**(3), **iachar**(3), **ichar**(3)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - **elemental:**
    **adjustl**(3), **adjustr**(3), **index**(3), **len\_trim**(3),
    **scan**(3), **verify**(3)

  - **nonelemental:**
    **repeat**(3), **trim**(3)
