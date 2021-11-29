---
layout: book
title: Fortran Intrinsics
permalink: /learn/intrinsics
---
This is a collection of extended descriptions of the Fortran intrinsics
based on the reference document
[Current F2018 Working Document as of April 2018](http://isotc.iso.org/livelink/livelink?func=ll&objId=19442438&objAction=Open)

with an emphasis on including a complete working example program that
describes common use cases. Vendor-specific extensions are not included.

The standard documents and most vendor-supplied descriptions of the
intrinsics are often very brief and concise; often to the point
of the functionality of the intrinsics being obscure, particulary to
someone new to the procedure.

By describing the procedures here in greater detail, including a working
example, and being able to provide links to additional resources
(including additional documents at fortran-lang.org and related
discussions in Fortran Discourse) these documents will hopefully be a
valuable asset for Fortran programmers.

This is a community-driven resource and everyone is encouraged to improve
on the documents. For contribution guidelines see 
[MINIBOOKS](https://github.com/fortran-lang/fortran-lang.org/blob/master/MINIBOOKS.md).


| -------------------------------------------------------------------------------- | ------  | ------------------------------------------------------------ |
| category                  | page                                                 | license | description                                                  |
| -------------------------------------------------------------------------------- | ------  | ------------------------------------------------------------ |
| ARRAY CONSTRUCTION        | [merge](MERGE)                                       |         | Merge variables                                              |
| ARRAY CONSTRUCTION        | [pack](PACK)                                         | (GFDL)  | Pack an array into an array of rank one                      |
| ARRAY CONSTRUCTION        | [spread](SPREAD)                                     | (GFDL)  | Add a dimension to an array                                  |
| ARRAY CONSTRUCTION        | [unpack](UNPACK)                                     | (GFDL)  | Store the elements of a vector in an array of higher rank    |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| ARRAY INQUIRY             | [allocated](ALLOCATED)                               | (GFDL)  | Status of an allocatable entity                              |
| ARRAY INQUIRY             | [is\_contiguous](IS_CONTIGUOUS)                      | (GFDL)  | test if object is contiguous                                 |
| ARRAY INQUIRY             | [lbound](LBOUND)                                     | (GFDL)  | Lower dimension bounds of an array                           |
| ARRAY INQUIRY             | [rank](RANK)                                         | (GFDL)  | Rank of a data object                                        |
| ARRAY INQUIRY             | [shape](SHAPE)                                       | (GFDL)  | Determine the shape of an array                              |
| ARRAY INQUIRY             | [size](SIZE)                                         | (GFDL)  | Determine the size of an array                               |
| ARRAY INQUIRY             | [ubound](UBOUND)                                     | (GFDL)  | Upper dimension bounds of an array                           |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| ARRAY LOCATION            | [maxloc](MAXLOC)                                     | (GFDL)  | Location of the maximum value within an array                |
| ARRAY LOCATION            | [minloc](MINLOC)                                     | (GFDL)  | Location of the minimum value within an array                |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| ARRAY MANIPULATION        | [transpose](TRANSPOSE)                               | (GFDL)  | Transpose an array of rank two                               |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| ARRAY REDUCTION           | [all](ALL)                                           | (GFDL)  | determines if all the values are true                        |
| ARRAY REDUCTION           | [any](ANY)                                           | (GFDL)  | determines if any of the values in the logical array are true. |
| ARRAY REDUCTION           | [count](COUNT)                                       | (GFDL)  | Count function                                               |
| ARRAY REDUCTION           | [maxval](MAXVAL)                                     | (GFDL)  | determines the maximum value in an array or row              |
| ARRAY REDUCTION           | [minval](MINVAL)                                     | (GFDL)  | Minimum value of an array                                    |
| ARRAY REDUCTION           | [product](PRODUCT)                                   | (GFDL)  | Product of array elements                                    |
| ARRAY REDUCTION           | [sum](SUM)                                           | (GFDL)  | sum the elements of an array                                 |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| ARRAY RESHAPE             | [reshape](RESHAPE)                                   | (GFDL)  | Function to reshape an array                                 |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| ATOMIC:BIT MANIPULATION   | [atomic\_and](ATOMIC_AND)                            | (GFDL)  | Atomic bitwise AND operation                                 |
| ATOMIC:BIT MANIPULATION   | [atomic\_fetch\_and](ATOMIC_FETCH_AND)               | (GFDL)  | Atomic bitwise AND operation with prior fetch                |
| ATOMIC:BIT MANIPULATION   | [atomic\_fetch\_or](ATOMIC_FETCH_OR)                 | (GFDL)  | Atomic bitwise OR operation with prior fetch                 |
| ATOMIC:BIT MANIPULATION   | [atomic\_fetch\_xor](ATOMIC_FETCH_XOR)               | (GFDL)  | Atomic bitwise XOR operation with prior fetch                |
| ATOMIC:BIT MANIPULATION   | [atomic\_or](ATOMIC_OR)                              | (GFDL)  | Atomic bitwise OR operation                                  |
| ATOMIC:BIT MANIPULATION   | [atomic\_xor](ATOMIC_XOR)                            | (GFDL)  | Atomic bitwise OR operation                                  |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| ATOMIC                    | [atomic\_add](ATOMIC_ADD)                            | (GFDL)  | Atomic ADD operation                                         |
| ATOMIC                    | [atomic\_cas](ATOMIC_CAS)                            | (GFDL)  | Atomic compare and swap                                      |
| ATOMIC                    | [atomic\_define](ATOMIC_DEFINE)                      | (GFDL)  | Setting a variable atomically                                |
| ATOMIC                    | [atomic\_fetch\_add](ATOMIC_FETCH_ADD)               | (GFDL)  | Atomic ADD operation with prior fetch                        |
| ATOMIC                    | [atomic\_ref](ATOMIC_REF)                            | (GFDL)  | Obtaining the value of a variable atomically                 |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| BIT COMPARE               | [bge](BGE)                                           | (GFDL)  | Bitwise greater than or equal to                             |
| BIT COMPARE               | [bgt](BGT)                                           | (GFDL)  | Bitwise greater than                                         |
| BIT COMPARE               | [ble](BLE)                                           | (GFDL)  | Bitwise less than or equal to                                |
| BIT COMPARE               | [blt](BLT)                                           | (GFDL)  | Bitwise less than                                            |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| BIT INQUIRY               | [bit\_size](BIT_SIZE)                                | (GFDL)  | Bit size inquiry function                                    |
| BIT INQUIRY               | [leadz](LEADZ)                                       | (GFDL)  | Number of leading zero bits of an integer                    |
| BIT INQUIRY               | [popcnt](POPCNT)                                     | (GFDL)  | Number of bits set                                           |
| BIT INQUIRY               | [poppar](POPPAR)                                     | (GFDL)  | Parity of the number of bits set                             |
| BIT INQUIRY               | [storage\_size](STORAGE_SIZE)                        | (GFDL)  | Storage size in bits                                         |
| BIT INQUIRY               | [trailz](TRAILZ)                                     |         | Number of trailing zero bits of an integer                   |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| BIT MANIPULATION          | [btest](BTEST)                                       | (GFDL)  | Bit test function                                            |
| BIT MANIPULATION          | [dshiftl](DSHIFTL)                                   | (GFDL)  | combines bits of arguments I and J                           |
| BIT MANIPULATION          | [dshiftr](DSHIFTR)                                   | (GFDL)  | combines bits of arguments I and J                           |
| BIT MANIPULATION          | [iall](IALL)                                         | (GFDL)  | Bitwise and of array elements                                |
| BIT MANIPULATION          | [iand](IAND)                                         | (GFDL)  | Bitwise logical and                                          |
| BIT MANIPULATION          | [iany](IANY)                                         | (GFDL)  | Bitwise or of array elements                                 |
| BIT MANIPULATION          | [ibclr](IBCLR)                                       | (GFDL)  | Clear bit                                                    |
| BIT MANIPULATION          | [ibits](IBITS)                                       | (GFDL)  | Bit extraction                                               |
| BIT MANIPULATION          | [ibset](IBSET)                                       | (GFDL)  | Set bit                                                      |
| BIT MANIPULATION          | [ieor](IEOR)                                         | (GFDL)  | Bitwise logical exclusive or                                 |
| BIT MANIPULATION          | [ior](IOR)                                           | (GFDL)  | Bitwise logical inclusive or                                 |
| BIT MANIPULATION          | [iparity](IPARITY)                                   | (GFDL)  | Bitwise exclusive or of array elements                       |
| BIT MANIPULATION          | [ishft](ISHFT)                                       | (GFDL)  | Shift bits                                                   |
| BIT MANIPULATION          | [ishftc](ISHFTC)                                     | (GFDL)  | Shift bits circularly                                        |
| BIT MANIPULATION          | [logical](LOGICAL)                                   | (GFDL)  | Converts one kind of _logical_ variable to another             |
| BIT MANIPULATION          | [merge\_bits](MERGE_BITS)                            | (GFDL)  | Merge of bits under mask                                     |
| BIT MANIPULATION          | [mvbits](MVBITS)                                     | (GFDL)  | Move bits from one integer to another                        |
| BIT MANIPULATION          | [not](NOT)                                           |         | Logical negation                                             |
| BIT MANIPULATION          | [shifta](SHIFTA)                                     | (GFDL)  | shift bits right with fill                                   |
| BIT MANIPULATION          | [shiftl](SHIFTL)                                     | (GFDL)  | shift bits left                                              |
| BIT MANIPULATION          | [shiftr](SHIFTR)                                     | (GFDL)  | shift bits right                                             |
| BIT MANIPULATION          | [transfer](TRANSFER)                                 | (GFDL)  | Transfer bit patterns                                        |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| CHARACTER                 | [achar](ACHAR)                                       |         | returns a character in a specified position in the ASCII collating sequence              |
| CHARACTER                 | [adjustl](ADJUSTL)                                   |         | Left-adjust a string                                         |
| CHARACTER                 | [adjustr](ADJUSTR)                                   |         | Right-adjust a string                                        |
| CHARACTER                 | [char](CHAR)                                         | (GFDL)  | Character conversion function                                |
| CHARACTER                 | [iachar](IACHAR)                                     | (GFDL)  | Code in ASCII collating sequence                             |
| CHARACTER                 | [ichar](ICHAR)                                       | (GFDL)  | Character-to-integer conversion function                     |
| CHARACTER                 | [index](INDEX)                                       | (GFDL)  | Position of a substring within a string                      |
| CHARACTER                 | [len](LEN)                                           | (GFDL)  | Length of a character entity                                 |
| CHARACTER                 | [len\_trim](LEN_TRIM)                                |         | Length of a character entity without trailing blank characters |
| CHARACTER                 | [lge](LGE)                                           | (GFDL)  | Lexical greater than or equal                                |
| CHARACTER                 | [lgt](LGT)                                           | (GFDL)  | Lexical greater than                                         |
| CHARACTER                 | [lle](LLE)                                           | (GFDL)  | Lexical less than or equal                                   |
| CHARACTER                 | [llt](LLT)                                           | (GFDL)  | Lexical less than                                            |
| CHARACTER                 | [new\_line](NEW_LINE)                                | (GFDL)  | New line character                                           |
| CHARACTER                 | [repeat](REPEAT)                                     |         | Repeated string concatenation                                |
| CHARACTER                 | [scan](SCAN)                                         | (GFDL)  | Scan a string for the presence of a set of characters        |
| CHARACTER                 | [trim](TRIM)                                         | (GFDL)  | Remove trailing blank characters of a string                 |
| CHARACTER                 | [verify](VERIFY)                                     | (GFDL)  | Scan a string for the absence of a set of characters         |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| COLLECTIVE                | [co\_broadcast](CO_BROADCAST)                        | (GFDL)  | Copy a value to all images the current set of images         |
| COLLECTIVE                | [co\_lbound](CO_LBOUND)                              | (GFDL)  | Lower codimension bounds of an array                         |
| COLLECTIVE                | [co\_max](CO_MAX)                                    | (GFDL)  | Maximal value on the current set of images                   |
| COLLECTIVE                | [co\_min](CO_MIN)                                    | (GFDL)  | Minimal value on the current set of images                   |
| COLLECTIVE                | [co\_reduce](CO_REDUCE)                              | (GFDL)  | Reduction of values on the current set of images             |
| COLLECTIVE                | [co\_sum](CO_SUM)                                    | (GFDL)  | Sum of values on the current set of images                   |
| COLLECTIVE                | [co\_ubound](CO_UBOUND)                              | (GFDL)  | Upper codimension bounds of an array                         |
| COLLECTIVE                | [event\_query](EVENT_QUERY)                          | (GFDL)  | Query whether a coarray event has occurred                   |
| COLLECTIVE                | [image\_index](IMAGE_INDEX)                          | (GFDL)  | Cosubscript to image index conversion                        |
| COLLECTIVE                | [num\_images](NUM_IMAGES)                            | (GFDL)  | Number of images                                             |
| COLLECTIVE                | [this\_image](THIS_IMAGE)                            | (GFDL)  | Cosubscript index of this image                              |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| COMPILER INQUIRY          | [compiler\_options](COMPILER_OPTIONS)                | (GFDL)  | Options passed to the compiler                               |
| COMPILER INQUIRY          | [compiler\_version](COMPILER_VERSION)                | (GFDL)  | Compiler version string                                      |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| ISO\_C\_BINDING           | [c\_associated](C_ASSOCIATED)                        | (GFDL)  | Status of a C pointer                                        |
| ISO\_C\_BINDING           | [c\_f\_pointer](C_F_POINTER)                         | (GFDL)  | Convert C into Fortran pointer                               |
| ISO\_C\_BINDING           | [c\_f\_procpointer](C_F_PROCPOINTER)                 | (GFDL)  | Convert C into Fortran procedure pointer                     |
| ISO\_C\_BINDING           | [c\_funloc](C_FUNLOC)                                | (GFDL)  | Obtain the C address of a procedure                          |
| ISO\_C\_BINDING           | [c\_loc](C_LOC)                                      | (GFDL)  | Obtain the C address of an object                            |
| ISO\_C\_BINDING           | [c\_sizeof](C_SIZEOF)                                | (GFDL)  | Size in bytes of an expression                               |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| KIND INQUIRY              | [kind](KIND)                                         | (GFDL)  | Kind of an entity                                            |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| KIND                      | [selected\_char\_kind](SELECTED_CHAR_KIND)           | (GFDL)  | Choose character kind such as "Unicode"                      |
| KIND                      | [selected\_int\_kind](SELECTED_INT_KIND)             | (GFDL)  | Choose integer kind                                          |
| KIND                      | [selected\_real\_kind](SELECTED_REAL_KIND)           | (GFDL)  | Choose real kind                                             |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| MATHEMATICS:RANDOM        | [random\_number](RANDOM_NUMBER)                      | (GFDL)  | Pseudo-random number                                         |
| MATHEMATICS:RANDOM        | [random\_seed](RANDOM_SEED)                          | (GFDL)  | Initialize a pseudo-random number sequence                   |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| MATHEMATICS:TRIGONOMETRIC | [acos](ACOS)                                         | (GFDL)  | arccosine function                                           |
| MATHEMATICS:TRIGONOMETRIC | [acosh](ACOSH)                                       | (GFDL)  | Inverse hyperbolic cosine function                           |
| MATHEMATICS:TRIGONOMETRIC | [asin](ASIN)                                         |         | Arcsine function                                             |
| MATHEMATICS:TRIGONOMETRIC | [asinh](ASINH)                                       | (GFDL)  | Inverse hyperbolic sine function                             |
| MATHEMATICS:TRIGONOMETRIC | [atan](ATAN)                                         | (GFDL)  | Arctangent function                                          |
| MATHEMATICS:TRIGONOMETRIC | [atan2](ATAN2)                                       | (GFDL)  | Arctangent function                                          |
| MATHEMATICS:TRIGONOMETRIC | [atanh](ATANH)                                       | (GFDL)  | Inverse hyperbolic tangent function                          |
| MATHEMATICS:TRIGONOMETRIC | [cos](COS)                                           | (GFDL)  | Cosine function                                              |
| MATHEMATICS:TRIGONOMETRIC | [cosh](COSH)                                         | (GFDL)  | Hyperbolic cosine function                                   |
| MATHEMATICS:TRIGONOMETRIC | [sin](SIN)                                           |         | Sine function                                                |
| MATHEMATICS:TRIGONOMETRIC | [sinh](SINH)                                         | (GFDL)  | Hyperbolic sine function                                     |
| MATHEMATICS:TRIGONOMETRIC | [tan](TAN)                                           | (GFDL)  | Tangent function                                             |
| MATHEMATICS:TRIGONOMETRIC | [tanh](TANH)                                         | (GFDL)  | Hyperbolic tangent function                                  |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| MATHEMATICS               | [bessel\_j0](BESSEL_J0)                              | (GFDL)  | Bessel function of the first kind of order 0                 |
| MATHEMATICS               | [bessel\_j1](BESSEL_J1)                              | (GFDL)  | Bessel function of the first kind of order 1                 |
| MATHEMATICS               | [bessel\_jn](BESSEL_JN)                              | (GFDL)  | Bessel function of the first kind                            |
| MATHEMATICS               | [bessel\_y0](BESSEL_Y0)                              | (GFDL)  | Bessel function of the second kind of order 0                |
| MATHEMATICS               | [bessel\_y1](BESSEL_Y1)                              | (GFDL)  | Bessel function of the second kind of order 1                |
| MATHEMATICS               | [bessel\_yn](BESSEL_YN)                              | (GFDL)  | Bessel function of the second kind                           |
| MATHEMATICS               | [erf](ERF)                                           | (GFDL)  | Error function                                               |
| MATHEMATICS               | [er](ERFC)                                           | (GFDL)  | Complementary error function                                 |
| MATHEMATICS               | [er\_scaled](ERFC_SCALED)                            | (GFDL)  | Error function                                               |
| MATHEMATICS               | [exp](EXP)                                           | (GFDL)  | Exponential function                                         |
| MATHEMATICS               | [gamma](GAMMA)                                       | (GFDL)  | Gamma function                                               |
| MATHEMATICS               | [hypot](HYPOT)                                       | (GFDL)  | Euclidean distance function                                  |
| MATHEMATICS               | [log](LOG)                                           | (GFDL)  | Logarithm function                                           |
| MATHEMATICS               | [log10](LOG10)                                       | (GFDL)  | Base 10 logarithm function                                   |
| MATHEMATICS               | [log\_gamma](LOG_GAMMA)                              | (GFDL)  | Logarithm of the Gamma function                              |
| MATHEMATICS               | [norm2](NORM2)                                       | (GFDL)  | Euclidean vector norm                                        |
| MATHEMATICS               | [sqrt](SQRT)                                         | (GFDL)  | Square-root function                                         |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| MODEL\_COMPONENTS         | [exponent](EXPONENT)                                 | (GFDL)  | Exponent function                                            |
| MODEL\_COMPONENTS         | [fraction](FRACTION)                                 | (GFDL)  | Fractional part of the model representation                  |
| MODEL\_COMPONENTS         | [nearest](NEAREST)                                   | (GFDL)  | Nearest representable number                                 |
| MODEL\_COMPONENTS         | [rrspacing](RRSPACING)                               | (GFDL)  | Reciprocal of the relative spacing                           |
| MODEL\_COMPONENTS         | [scale](SCALE)                                       | (GFDL)  | Scale a real value                                           |
| MODEL\_COMPONENTS         | [set\_exponent](SET_EXPONENT)                        | (GFDL)  | Set the exponent of the model                                |
| MODEL\_COMPONENTS         | [spacing](SPACING)                                   | (GFDL)  | Smallest distance between two numbers of a given type        |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| NUMERIC MODEL             | [digits](DIGITS)                                     | (GFDL)  | Significant digits function                                  |
| NUMERIC MODEL             | [epsilon](EPSILON)                                   | (GFDL)  | Epsilon function                                             |
| NUMERIC MODEL             | [huge](HUGE)                                         |         | Largest number of a kind                                     |
| NUMERIC MODEL             | [maxexponent](MAXEXPONENT)                           | (GFDL)  | Maximum exponent of a real kind                              |
| NUMERIC MODEL             | [minexponent](MINEXPONENT)                           | (GFDL)  | Minimum exponent of a real kind                              |
| NUMERIC MODEL             | [precision](PRECISION)                               | (GFDL)  | Decimal precision of a real kind                             |
| NUMERIC MODEL             | [radix](RADIX)                                       | (GFDL)  | Base of a model number                                       |
| NUMERIC MODEL             | [range](RANGE)                                       | (GFDL)  | Decimal exponent range of a real kind                        |
| NUMERIC MODEL             | [tiny](TINY)                                         | (GFDL)  | Smallest positive number of a real kind                      |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| NUMERIC:TYPE              | [aimag](AIMAG)                                       | (GFDL)  | Imaginary part of complex number                             |
| NUMERIC:TYPE              | [cmplx](CMPLX)                                       | (GFDL)  | Complex conversion function                                  |
| NUMERIC:TYPE              | [dble](DBLE)                                         | (GFDL)  | Double conversion function                                   |
| NUMERIC:TYPE              | [float](FLOAT)                                       | (GFDL)  | Convert integer to default real                              |
| NUMERIC:TYPE              | [int](INT)                                           | (GFDL)  | Convert to integer type                                      |
| NUMERIC:TYPE              | [nint](NINT)                                         |         | Nearest whole number                                         |
| NUMERIC:TYPE              | [real](_real_)                                         | (GFDL)  | Convert to real type                                         |
| NUMERIC:TYPE              | [sngl](SNGL)                                         | (GFDL)  | Convert double precision real to default real                |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| NUMERIC                   | [abs](ABS)                                           |         | Absolute value                                               |
| NUMERIC                   | [aint](AINT)                                         | (GFDL)  | Truncate to a whole number                                   |
| NUMERIC                   | [anint](ANINT)                                       | (GFDL)  | Nearest whole number                                         |
| NUMERIC                   | [ceiling](CEILING)                                   | (GFDL)  | Integer ceiling function                                     |
| NUMERIC                   | [conjg](CONJG)                                       | (GFDL)  | Complex conjugate function                                   |
| NUMERIC                   | [dim](DIM)                                           | (GFDL)  | Positive difference                                          |
| NUMERIC                   | [dprod](DPROD)                                       | (GFDL)  | Double product function                                      |
| NUMERIC                   | [floor](FLOOR)                                       | (GFDL)  | Integer floor function                                       |
| NUMERIC                   | [max](MAX)                                           | (GFDL)  | Maximum value of an argument list                            |
| NUMERIC                   | [min](MIN)                                           | (GFDL)  | Minimum value of an argument list                            |
| NUMERIC                   | [mod](MOD)                                           | (GFDL)  | Remainder function                                           |
| NUMERIC                   | [modulo](MODULO)                                     | (GFDL)  | Modulo function                                              |
| NUMERIC                   | [sign](SIGN)                                         | (GFDL)  | Sign copying function                                        |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| SYSTEM ENVIRONMENT        | [command\_argument\_count](COMMAND_ARGUMENT_COUNT)   |         | Get number of command line arguments                         |
| SYSTEM ENVIRONMENT        | [cpu\_time](CPU_TIME)                                |         | return CPU processor time in seconds                         |
| SYSTEM ENVIRONMENT        | [date\_and\_time](DATE_AND_TIME)                     |         | gets current time                                            |
| SYSTEM ENVIRONMENT        | [execute\_command\_line](EXECUTE_COMMAND_LINE)       | (GFDL)  | Execute a shell command                                      |
| SYSTEM ENVIRONMENT        | [get\_command](GET_COMMAND)                          |         | Get the entire command line                                  |
| SYSTEM ENVIRONMENT        | [get\_command\_argument](GET_COMMAND_ARGUMENT)       |         | Get command line arguments                                   |
| SYSTEM ENVIRONMENT        | [get\_environment\_variable](GET_ENVIRONMENT_VARIABLE)|         | Get an environmental variable                               |
| SYSTEM ENVIRONMENT        | [system\_clock](SYSTEM_CLOCK)                        | (GFDL)  | Return numeric data from a real-time clock.                  |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| TRANSFORMATIONAL          | [cshift](CSHIFT)                                     | (GFDL)  | Circular shift elements of an array                          |
| TRANSFORMATIONAL          | [dot\_product](DOT_PRODUCT)                          | (GFDL)  | Dot product function                                         |
| TRANSFORMATIONAL          | [eoshift](EOSHIFT)                                   | (GFDL)  | End-off shift elements of an array                           |
| TRANSFORMATIONAL          | [matmul](MATMUL)                                     | (GFDL)  | matrix multiplication                                        |
| TRANSFORMATIONAL          | [null](NULL)                                         | (GFDL)  | Function that returns a disassociated pointer                |
| TRANSFORMATIONAL          | [parity](PARITY)                                     | (GFDL)  | Reduction with exclusive OR                                  |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |
| TBD                       | [associated](ASSOCIATED)                             | (GFDL)  | Status of a pointer or pointer/target pair                   |
| TBD                       | [extends\_type\_of](EXTENDS_TYPE_OF)                 | (GFDL)  | determine if the dynamic type of A is an extension of the dynamic type of MOLD.          |
| TBD                       | [findloc](FINDLOC)                                   | (GFDL)  | Location of first element of ARRAY identified by MASK along dimension DIM having a value |
| TBD                       | [is\_iostat\_end](IS_IOSTAT_END)                     | (GFDL)  | Test for end-of-file value                                   |
| TBD                       | [is\_iostat\_eor](IS_IOSTAT_EOR)                     | (GFDL)  | Test for end-of-record value                                 |
| TBD                       | [maskl](MASKL)                                       | (GFDL)  | Left justified mask                                          |
| TBD                       | [maskr](MASKR)                                       | (GFDL)  | Right justified mask                                         |
| TBD                       | [move\_alloc](MOVE_ALLOC)                            | (GFDL)  | Move allocation from one object to another                   |
| TBD                       | [present](PRESENT)                                   | (GFDL)  | Determine whether an optional dummy argument is specified    |
| TBD                       | [same\_type\_as](SAME_TYPE_AS)                       | (GFDL)  | Query dynamic types for equality                             |
| ------------------------- | ---------------------------------------------------- | ------  | ------------------------------------------------------------ |

## Text Content Copyrights
The above table and the documents themselves indication the license under which
the document falls.

Many of the documents presented here are modified versions of man-pages from the
[Fortan Wiki](https://fortranwiki.org) 
and as such are available under the terms of the GNU
Free Documentation License [(GFDL)](GNU_Free_Documentation_License.md)
with no invariant sections, front-cover texts, or back-cover texts.


If you contribute to this site by modifying the files marked as GFDL,
you thereby agree to license the contributed material to the public
under the GFDL (version 1.2 or any later version published by the Free
Software Foundation, with no invariant sections, front-cover texts,
or back-cover texts).

If you contribute new material you thereby agree to release it under
the MIT license.

###### Written in [Markdown](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet) [kramdown](https://kramdown.gettalong.org/syntax.html)
