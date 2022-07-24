## char
### __Name__

__char__(3) - \[CHARACTER\] Character conversion function


### __Syntax__
```fortran
result = char(i, kind)
   elemental integer function char(i,kind)

    integer(kind=KIND),intent(in) :: c
    integer,intent(in),optional :: KIND
```
### __Description__

__char(i, kind)__ returns the character represented by the integer __i__.

### __Arguments__

  - __i__
    : The type shall be _integer_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

### __Returns__

The return value is of type _character_

### __Examples__

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

### __Note__

See [__ichar__(3)](CHAR) for a discussion of converting between numerical
values and formatted string representations.

### __Standard__

FORTRAN 77 and later

### __See Also__

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

####### fortran-lang intrinsic descriptions
