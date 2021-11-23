---
layout: book
title: count
permalink: /learn/intrinsics/COUNT
---
### NAME

**command\_argument\_count**(3f) - \[SYSTEM ENVIRONMENT\] Get number of command line arguments
(MIT)

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

  - **RESULT**
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

**get\_command**(3), **get\_command\_argument**(3)

#### @urbanjost
