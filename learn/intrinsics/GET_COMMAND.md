---
layout: book
title: get_command
permalink: /learn/intrinsics/GET_COMMAND
---
## __Name__

__get\_command__(3) - \[SYSTEM:COMMAND LINE\] Get the entire command line

## __Syntax__
```fortran
   call get_command(command, length, status)

    subroutine get_command(command,length,status)
    character(len=*),intent(out),optional :: command
    integer,intent(out),optional :: length
    integer,intent(out),optional :: status
```
## __Description__

Retrieve the entire command line that was used to invoke the program.

Note that what is typed on the command line is often processed by
a shell. The shell typically processes special characters and white
space before passing it to the program. The processing can typically be
turned off by turning off globbing or quoting the command line arguments
and/or changing the default field separators, but this should rarely
be necessary.

## __Returns__

  - __command__
    : Shall be of type _character_ and of default kind. If
    __command__ is present, stores the entire command line that was used to
    invoke the program in __command__.

  - __length__
    : Shall be of type _integer_ and of default kind. If __length__
    is present, it is assigned the length of the command line.

  - __status__
    : Shall be of type _integer_ and of default kind. If __status__
    is present, it is assigned 0 upon success of the command, __-1__ if
    __command__ is too short to store the command line, or a positive value
    in case of an error.

## __Examples__

Sample program:

```fortran
program demo_get_command
implicit none
integer                      :: COMMAND_LINE_LENGTH
character(len=:),allocatable :: COMMAND_LINE
   ! get command line length
   call get_command(length=COMMAND_LINE_LENGTH)
   ! allocate string big enough to hold command line
   allocate(character(len=COMMAND_LINE_LENGTH) :: COMMAND_LINE)
   ! get command line as a string
   call get_command(command=COMMAND_LINE)
   ! trim leading spaces just in case
   COMMAND_LINE=adjustl(COMMAND_LINE)
   write(*,'("OUTPUT:",a)')COMMAND_LINE
end program demo_get_command
```

Sample execution:

```bash
     # note that shell expansion removes some of the whitespace
     # without quotes
     ./test_get_command  arguments    on command   line to   echo

     OUTPUT:./test_get_command arguments on command line to echo

     # using the bash shell with single quotes
     ./test_get_command  'arguments  *><`~[]!{}?"\'| '

     OUTPUT:./test_get_command arguments  *><`~[]!{}?"'|
```

## __Standard__

Fortran 2003 and later

## __See Also__

[__get\_command\_argument__(3)](GET_COMMAND_ARGUMENT),
[__command\_argument\_count__(3)](COMMAND_ARGUMENT_COUNT)

###### fortran-lang intrinsic descriptions (@urbanjost)
