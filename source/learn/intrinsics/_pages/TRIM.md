## trim
### __Name__

__trim__(3) - \[CHARACTER:WHITESPACE\] Remove trailing blank characters of a string


### __Syntax__
```fortran
result = trim(string)
```
### __Description__

Removes trailing blank characters of a string.

### __Arguments__

  - __string__
    : Shall be a scalar of type _character_.

### __Returns__

A scalar of type _character_ which length is that of __string__ less the
number of trailing blanks.

### __Examples__

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
### __Standard__

Fortran 95 and later

### __See Also__

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

####### fortran-lang intrinsic descriptions
