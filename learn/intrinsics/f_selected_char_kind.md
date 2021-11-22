---
layout: book
title: selected_char_kind
permalink: /learn/intrinsics/f_selected_char_kind
---
### NAME

**selected\_char\_kind**(3f) - \[KIND\] Choose
character kind such as "Unicode"

### SYNTAX

result = **selected\_char\_kind**(name)

### DESCRIPTION

**selected\_char\_kind**(name) returns the kind value for the character
set named NAME, if a character set with such a name is supported, or
**-1** otherwise. Currently, supported character sets include "ASCII"
and "DEFAULT" (iwhich are equivalent), and "ISO\_10646" (Universal
Character Set, UCS-4) which is commonly known as "Unicode".

### ARGUMENTS

  - **NAME**
    Shall be a scalar and of the default character type.

### EXAMPLE

Sample program:

```
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

### STANDARD

Fortran 2003 and later

### CLASS

Transformational function
