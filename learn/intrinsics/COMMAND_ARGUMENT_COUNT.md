---
layout: book
title: count
permalink: /learn/intrinsics/COUNT
---
### NAME

__command\_argument\_count__(3f) - \[SYSTEM ENVIRONMENT\] Get number of command line arguments

### SYNTAX


```fortran
    result = command_argument_count()
     integer :: result
```

### DESCRIPTION

command\_argument\_count returns the number of arguments passed on the
command line when the containing program was invoked.

### ARGUMENTS

None

### RETURN VALUE

  - __RESULT__
    The return value is of type default integer. It is the number of
    arguments passed on the command line when the program was invoked.

### EXAMPLE

Sample program:

```fortran
   program demo_command_argument_count
   implicit none
   integer :: count
       count = command_argument_count()
       print *, count
   end program demo_command_argument_count
```

Sample output:

```
   # the command verb does not count
   ./test_command_argument_count
       0
   # quoted strings may count as one argument
   ./test_command_argument_count count arguments
       2
   ./test_command_argument_count 'count arguments'
       1
```

### STANDARD

Fortran 2003 and later

### CLASS

Inquiry function

### SEE ALSO

__get\_command__(3), __get\_command\_argument__(3)

#### @urbanjost
