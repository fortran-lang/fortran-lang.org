---
layout: book
title: get_command_argument
permalink: /learn/intrinsics/GET_COMMAND_ARGUMENT
---
### NAME

__get\_command\_argument__(3f) - \[SYSTEM ENVIRONMENT\] Get command line arguments

### SYNTAX
```fortran
     call get_command_argument__(number [, value, length, status])
     integer,intent(in)                    :: number
     character(len=*),intent(out),optional :: value
     integer,intent(out),optional          :: length
     integer,intent(out),optional          :: status
```

### DESCRIPTION

Retrieve the NUMBER-th argument that was passed on the command line when
the containing program was invoked.

There is not anything specifically stated about what an argument is but
in practice the arguments are split on whitespace unless the arguments
are quoted and IFS values (Internal Field Separators) used by common
shells are ignored.

### OPTIONS

  - __NUMBER__
    Shall be a scalar of type __integer__(4), NUMBER \>= 0. If NUMBER =
    0, VALUE is set to the name of the program (on systems that support
    this feature).

### RETURNS

  - __VALUE__
    Shall be a scalar of type CHARACTER and of default kind. After
    get\_command\_argument returns, the VALUE argument holds the
    NUMBER-th command line argument. If VALUE can not hold the argument,
    it is truncated to fit the length of VALUE. If there are less than
    NUMBER arguments specified at the command line, VALUE will be filled
    with blanks.

  - __LENGTH__
    (Optional) Shall be a scalar of type __integer__(4). The LENGTH
    argument contains the length of the NUMBER-th command line argument.

  - __STATUS__
    (Optional) Shall be a scalar of type __integer__(4). If the argument
    retrieval fails, STATUS is a positive number; if VALUE contains a
    truncated command line argument, STATUS is __-1__; and otherwise the
    STATUS is zero.

### EXAMPLE

Sample program:

```fortran
   program demo_get_command_argument
   implicit none
   character(len=255)           :: progname
   integer                      :: stat
   integer                      :: count,i, longest, argument_length
   integer,allocatable          :: istat(:), ilen(:)
   character(len=:),allocatable :: arguments(:)
     ! get number of arguments
     count = command_argument_count()
     write(*,*)'The number of arguments is ',count
     ! simple usage
     call get_command_argument (0, progname, status=stat)
     if (stat == 0) then
        print *, "The program's name is " // trim (progname)
     endif
     ! showing how to make an array to hold any argument list
     ! find longest argument
     longest=0
     do i=0,count
        call get_command_argument(number=i,length=argument_length)
        longest=max(longest,argument_length)
     enddo
     ! allocate string array big enough to hold command line argument strings
     ! and related information
     allocate(character(len=longest) :: arguments(0:count))
     allocate(istat(0:count))
     allocate(ilen(0:count))
     ! read the arguments into the array
     do i=0,count
       call get_command_argument(i, arguments(i),status=istat(i),length=ilen(i))
     enddo
     ! show the results
     write (*,'(i3.3,1x,i0.5,1x,i0.5,1x,"[",a,"]")') &
     & (i,istat(i),ilen(i),arguments(i)(:ilen(i)),i=0,count)
   end program demo_get_command_argument
```

Sample output:

```
   ./test_get_command_argument a    test  'of getting   arguments  ' "  leading"

   > The number of arguments is            5
   > The program's name is xxx
   >000 00000 00003 [./test_get_command_argument]
   >001 00000 00001 [a]
   >003 00000 00004 [test]
   >004 00000 00024 [of getting   arguments  ]
   >005 00000 00018 [  leading]
```

### STANDARD

Fortran 2003 and later

### CLASS

Subroutine

### SEE ALSO

__get\_command__(3), __command\_argument\_count__(3)

###### fortran-lang intrinsic descriptions
