# Types and kinds

These intrinsics allow for explicitly casting one type of variable to
another or can be used to conditionally execute code blocks based on
variable types when working with polymorphic variables.

## Fortran Data Types

Fortran provides five basic intrinsic data types:

  * Integer type
    : The integer types can hold only whole number values.
  * Real type
    : Stores floating point numbers, such as 2.0, 3.1415, -100.876, etc.
  * Complex type
    : A complex number has two parts,
      the real part and the imaginary part. Two consecutive floating
      point storage units store the two parts.
  * Logical type
    : There are only two logical values: .true. and .false.
  * Character type
    : The character type stores strings. The length of the string
    can be specified by the __len__ specifier. If no length is specified, it is 1.

These "types" can be of many "kinds". Often different numeric kinds
take up different storage sizes and therefore can represent
different ranges; but a different kind can have other meanings.
A _character_ variable might represent ASCII characters or UTF-8 or
Unicode characters, for example.

You can derive your own data types from these fundamental types as well.

## Implicit Typing

Fortran allows a feature called implicit typing, i.e., you do not have
to declare some variables before use. By default if a variable is not declared,
then the first letter of its name will determine its type:

1. Variable names starting with __i-n__ (the first two letters of
   "integer") specify _integer_ variables.

2. All other variable names default to _real_.


However, in most circles it is considered good programming practice to declare all the
variables. For that to be enforced, you start your variable declaration section with 
a statement that turns off implicit typing:
the statement
```fortran
implicit none
```
For more information refer to the __implicit__ statement.

```{include} _pages/AIMAG.md
```

```{include} _pages/CMPLX.md
```

```{include} _pages/INT.md
```

```{include} _pages/NINT.md
```

```{include} _pages/REAL.md
```

```{include} _pages/DBLE.md
```

```{include} _pages/TRANSFER.md
```

```{include} _pages/LOGICAL.md
```

```{include} _pages/KIND.md
```

```{include} _pages/SELECTED_CHAR_KIND.md
```

```{include} _pages/SELECTED_INT_KIND.md
```

```{include} _pages/SELECTED_REAL_KIND.md
```
