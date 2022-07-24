## new\_line
### __Name__

__new\_line__(3) - \[CHARACTER\] new-line character

### __Syntax__
```fortran
result = new_line(c)

   character(len=1,kind=kind(c)) :: new_line(c)
   character(len=1),intent(in) :: c(..)
```
### __Description__

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

### __Arguments__

  - __C__
    : The argument shall be a scalar or array of the type _character_.

### __Returns__

Returns a _character_ scalar of length one with the new-line character of
the same kind as parameter __c__.


### __Examples__

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
### __Standard__

Fortran 2003 and later

####### fortran-lang intrinsic descriptions (license: MIT) @urbanjost
