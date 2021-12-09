program demo_len
use,intrinsic :: iso_fortran_env, only : stdout=>output_unit
implicit none
character(len=:),allocatable :: string
character(len=:),allocatable :: many_strings(:)
integer :: ii

   string=' How long is this string?     '
   ii=len(string)
   ! note when adjacent strings are printed no space is inserted
   ! between them
   write(*,*)'[',string,']',' length=',ii

   ! Related Matters:

   write(*,*)
   ! you can also query the length (and other attributes) of a string
   ! using a
   write(*,*) "type parameter inquiry:"
   write(*,*)'length=',string%len,'kind=',string%kind
   ! note a type parameter inquiry of an intrinsic requires Fortran 2018+ 

   ! note that all that is required is an A descriptor in a format, a
   ! numeric length is not required. If a length IS provided the string
   ! will be trimmed or blank padded ON THE LEFT to the specified length
   write(*,*)
   write(*,'(" ",a," ")')repeat('=',ii)
   write(*,'("[",a,"]")')string
   write(*,'(" ",a," ")')repeat('=',ii)

   write(*,'("[",a10,"]")')string  ! TRUNCATED!
   write(*,'("[",a40,"]")')string  ! PADDED!
   ! you can specify the length at run time:
   ii=40
   write(*,'("[",a,"]")')[character(len=ii) :: string]  ! RIGHT JUSTIFIED!

   ! a scalar is returned for an array, as all values in a Fortran
   ! character array must be of the same length:

   ! stepping aside to define an allocatable array with a constructor ...
   ! (that MUST specify a LEN= length type parameter if all values are
   ! not the same length):
     many_strings = [ character(len=7) :: 'Takata', 'Tanaka', 'Hayashi' ]
   ! if the length specified is too short the strings will be truncated

   ! In that constructor, without the LEN= type specification, it would
   ! have been necessary to specify all of the constants with the same
   ! character length.

   write(*,*)
   write(*,*)'length of ALL elements of array=',len(many_strings)
   write(*,'("[",a,"]")')many_strings

   ! Note in the following the result is always scalar, even if the
   ! object is an array.

   write(*,*) &
   & 'length=', many_strings%len, &
   & 'kind=', many_strings%kind
   ! which is the same as
   write(*,*) &
   & 'length=', len(many_strings), &
   & 'kind=', kind(many_strings)

   ! you should also be careful when printing strings that they do not
   ! exceed the current record length of your output file, although that
   ! usually is very large
   inquire(unit=stdout,recl=ii)
   write(*,*)'line length=',ii

end program demo_len
