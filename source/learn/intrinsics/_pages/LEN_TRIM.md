## len\_trim
### __Name__

__len\_trim__(3) - \[CHARACTER:WHITESPACE\] Length of a character entity without trailing blank characters

### __Syntax__
```fortran
   result = len_trim(string, kind)

    integer(kind=KIND) function len_trim(string,KIND) result (value)
    character(len=*),intent(in) :: string
    integer,optional,intent(in) :: KIND
    integer(kind=KIND) :: value
```
### __Description__

Returns the length of a character string, ignoring any trailing blanks.

### __Arguments__

  - __string__
    : The input string whose length is to be measured.
    Shall be a scalar of type _character_

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

### __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default _integer_ kind.

### __Examples__

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
### __Standard__

Fortran 95 and later, with __kind__ argument - Fortran 2003
and later

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
    [__repeat__(3)](REPEAT),
    [__len__(3)](LEN),
    [__trim__(3)](TRIM)

####### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
