---
layout: book
title: get_command
permalink: /learn/intrinsics/GET_COMMAND
---
### NAME

**get\_command**(3f) - \[SYSTEM ENVIRONMENT\] Get the entire command line
(MIT)

### SYNTAX


```fortran
   call get_command([command, length, status])
    character(len=*),intent(out),optional :: command
    integer,intent(out),optional :: length
    integer,intent(out),optional :: status
```

### DESCRIPTION

Retrieve the entire command line that was used to invoke the program.

Note that what is typed on the command line is often processed by a
shell. The shell often processes special characters and white space
before passing it to the program. The processing can typically be turned
off by turning off globbing or quoting the command line arguments with
quote characters and/or changing the default field separators, but this
should rarely be necessary.

### RETURNS

  - **COMMAND**
    (Optional) shall be of type CHARACTER and of default kind. If
    COMMAND is present, stores the entire command line that was used to
    invoke the program in COMMAND.

  - **LENGTH**
    (Optional) Shall be of type INTEGER and of default kind. If LENGTH
    is present, it is assigned the length of the command line.

  - **STATUS**
    (Optional) Shall be of type INTEGER and of default kind. If STATUS
    is present, it is assigned 0 upon success of the command, **-1** if
    COMMAND is too short to store the command line, or a positive value
    in case of an error.

### EXAMPLE

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

```
     # note that shell expansion removes some of the whitespace
     # without quotes
     ./test_get_command  arguments    on the    command   line to   echo

     OUTPUT:./test_get_command arguments on the command line to echo

     # using the bash shell with single quotes
     ./test_get_command  'arguments  *><`~[]!{}?"\'| on the    command   line '

     OUTPUT:./test_get_command arguments  *><`~[]!{}?"'| on the   command   line
```

### STANDARD

Fortran 2003 and later

### CLASS

Subroutine

### SEE ALSO

**get\_command\_argument**(3), **command\_argument\_count**(3)

#### @urbanjost
