## selected\_char\_kind
### __Name__

__selected\_char\_kind__(3) - \[KIND\] Choose character kind such as "Unicode"


### __Syntax__
```fortran
result = selected_char_kind(name)
```
### __Description__

__selected\_char\_kind(name)__ returns the kind value for the character
set named NAME, if a character set with such a name is supported, or
__-1__ otherwise. Currently, supported character sets include "ASCII"
and "DEFAULT" (iwhich are equivalent), and "ISO\_10646" (Universal
Character Set, UCS-4) which is commonly known as "Unicode".

### __Arguments__

  - __name__
    : Shall be a scalar and of the default character type.

### __Examples__

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
### __Standard__

Fortran 2003 and later

####### fortran-lang intrinsic descriptions
