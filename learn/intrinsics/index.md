---
layout: book
title: Fortran Intrinsics
permalink: /learn/intrinsics
---
| grouping                  | page                                                   | description                                                  |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| ARRAY CONSTRUCTION        | [merge](f_merge)                                       | Merge variables                                              |
| ARRAY CONSTRUCTION        | [pack](f_pack)                                         | Pack an array into an array of rank one                      |
| ARRAY CONSTRUCTION        | [spread](f_spread)                                     | Add a dimension to an array                                  |
| ARRAY CONSTRUCTION        | [unpack](f_unpack)                                     | Store the elements of a vector in an array of higher rank    |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| ARRAY INQUIRY             | [allocated](f_allocated)                               | Status of an allocatable entity                              |
| ARRAY INQUIRY             | [is\_contiguous](f_is_contiguous)                      | test if object is contiguous                                 |
| ARRAY INQUIRY             | [lbound](f_lbound)                                     | Lower dimension bounds of an array                           |
| ARRAY INQUIRY             | [rank](f_rank)                                         | Rank of a data object                                        |
| ARRAY INQUIRY             | [shape](f_shape)                                       | Determine the shape of an array                              |
| ARRAY INQUIRY             | [size](f_size)                                         | Determine the size of an array                               |
| ARRAY INQUIRY             | [ubound](f_ubound)                                     | Upper dimension bounds of an array                           |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| ARRAY LOCATION            | [maxloc](f_maxloc)                                     | Location of the maximum value within an array                |
| ARRAY LOCATION            | [minloc](f_minloc)                                     | Location of the minimum value within an array                |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| ARRAY MANIPULATION        | [transpose](f_transpose)                               | Transpose an array of rank two                               |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| ARRAY REDUCTION           | [all](f_all)                                           | determines if all the values are true                        |
| ARRAY REDUCTION           | [any](f_any)                                           | determines if any of the values in the logical array are true.                           |
| ARRAY REDUCTION           | [count](f_count)                                       | Count function                                               |
| ARRAY REDUCTION           | [maxval](f_maxval)                                     | determines the maximum value in an array or row              |
| ARRAY REDUCTION           | [minval](f_minval)                                     | Minimum value of an array                                    |
| ARRAY REDUCTION           | [product](f_product)                                   | Product of array elements                                    |
| ARRAY REDUCTION           | [sum](f_sum)                                           | sum the elements of an array                                 |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| ARRAY RESHAPE             | [reshape](f_reshape)                                   | Function to reshape an array                                 |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| ATOMIC:BIT MANIPULATION   | [atomic\_and](f_atomic_and)                            | Atomic bitwise AND operation                                 |
| ATOMIC:BIT MANIPULATION   | [atomic\_fetch\_and](f_atomic_fetch_and)               | Atomic bitwise AND operation with prior fetch                |
| ATOMIC:BIT MANIPULATION   | [atomic\_fetch\_or](f_atomic_fetch_or)                 | Atomic bitwise OR operation with prior fetch                 |
| ATOMIC:BIT MANIPULATION   | [atomic\_fetch\_xor](f_atomic_fetch_xor)               | Atomic bitwise XOR operation with prior fetch                |
| ATOMIC:BIT MANIPULATION   | [atomic\_or](f_atomic_or)                              | Atomic bitwise OR operation                                  |
| ATOMIC:BIT MANIPULATION   | [atomic\_xor](f_atomic_xor)                            | Atomic bitwise OR operation                                  |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| ATOMIC                    | [atomic\_add](f_atomic_add)                            | Atomic ADD operation                                         |
| ATOMIC                    | [atomic\_cas](f_atomic_cas)                            | Atomic compare and swap                                      |
| ATOMIC                    | [atomic\_define](f_atomic_define)                      | Setting a variable atomically                                |
| ATOMIC                    | [atomic\_fetch\_add](f_atomic_fetch_add)               | Atomic ADD operation with prior fetch                        |
| ATOMIC                    | [atomic\_ref](f_atomic_ref)                            | Obtaining the value of a variable atomically                 |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| BIT COMPARE               | [bge](f_bge)                                           | Bitwise greater than or equal to                             |
| BIT COMPARE               | [bgt](f_bgt)                                           | Bitwise greater than                                         |
| BIT COMPARE               | [ble](f_ble)                                           | Bitwise less than or equal to                                |
| BIT COMPARE               | [blt](f_blt)                                           | Bitwise less than                                            |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| BIT INQUIRY               | [bit\_size](f_bit_size)                                | Bit size inquiry function                                    |
| BIT INQUIRY               | [leadz](f_leadz)                                       | Number of leading zero bits of an integer                    |
| BIT INQUIRY               | [popcnt](f_popcnt)                                     | Number of bits set                                           |
| BIT INQUIRY               | [poppar](f_poppar)                                     | Parity of the number of bits set                             |
| BIT INQUIRY               | [storage\_size](f_storage_size)                        | Storage size in bits                                         |
| BIT INQUIRY               | [trailz](f_trailz)                                     | Number of trailing zero bits of an integer                   |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| BIT MANIPULATION          | [btest](f_btest)                                       | Bit test function                                            |
| BIT MANIPULATION          | [dshiftl](f_dshiftl)                                   | combines bits of arguments I and J                           |
| BIT MANIPULATION          | [dshiftr](f_dshiftr)                                   | combines bits of arguments I and J                           |
| BIT MANIPULATION          | [iall](f_iall)                                         | Bitwise and of array elements                                |
| BIT MANIPULATION          | [iand](f_iand)                                         | Bitwise logical and                                          |
| BIT MANIPULATION          | [iany](f_iany)                                         | Bitwise or of array elements                                 |
| BIT MANIPULATION          | [ibclr](f_ibclr)                                       | Clear bit                                                    |
| BIT MANIPULATION          | [ibits](f_ibits)                                       | Bit extraction                                               |
| BIT MANIPULATION          | [ibset](f_ibset)                                       | Set bit                                                      |
| BIT MANIPULATION          | [ieor](f_ieor)                                         | Bitwise logical exclusive or                                 |
| BIT MANIPULATION          | [ior](f_ior)                                           | Bitwise logical inclusive or                                 |
| BIT MANIPULATION          | [iparity](f_iparity)                                   | Bitwise exclusive or of array elements                       |
| BIT MANIPULATION          | [ishft](f_ishft)                                       | Shift bits                                                   |
| BIT MANIPULATION          | [ishftc](f_ishftc)                                     | Shift bits circularly                                        |
| BIT MANIPULATION          | [logical](f_logical)                                   | Converts one kind of LOGICAL variable to another             |
| BIT MANIPULATION          | [merge\_bits](f_merge_bits)                            | Merge of bits under mask                                     |
| BIT MANIPULATION          | [mvbits](f_mvbits)                                     | Move bits from one integer to another                        |
| BIT MANIPULATION          | [not](f_not)                                           | Logical negation                                             |
| BIT MANIPULATION          | [shifta](f_shifta)                                     | shift bits right with fill                                   |
| BIT MANIPULATION          | [shiftl](f_shiftl)                                     | shift bits left                                              |
| BIT MANIPULATION          | [shiftr](f_shiftr)                                     | shift bits right                                             |
| BIT MANIPULATION          | [transfer](f_transfer)                                 | Transfer bit patterns                                        |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| CHARACTER                 | [achar](f_achar)                                       | returns a character in a specified position in the ASCII collating sequence              |
| CHARACTER                 | [adjustl](f_adjustl)                                   | Left-adjust a string                                         |
| CHARACTER                 | [adjustr](f_adjustr)                                   | Right-adjust a string                                        |
| CHARACTER                 | [char](f_char)                                         | Character conversion function                                |
| CHARACTER                 | [iachar](f_iachar)                                     | Code in ASCII collating sequence                             |
| CHARACTER                 | [ichar](f_ichar)                                       | Character-to-integer conversion function                     |
| CHARACTER                 | [index](f_index)                                       | Position of a substring within a string                      |
| CHARACTER                 | [len](f_len)                                           | Length of a character entity                                 |
| CHARACTER                 | [len\_trim](f_len_trim)                                | Length of a character entity without trailing blank characters                           |
| CHARACTER                 | [lge](f_lge)                                           | Lexical greater than or equal                                |
| CHARACTER                 | [lgt](f_lgt)                                           | Lexical greater than                                         |
| CHARACTER                 | [lle](f_lle)                                           | Lexical less than or equal                                   |
| CHARACTER                 | [llt](f_llt)                                           | Lexical less than                                            |
| CHARACTER                 | [new\_line](f_new_line)                                | New line character                                           |
| CHARACTER                 | [repeat](f_repeat)                                     | Repeated string concatenation                                |
| CHARACTER                 | [scan](f_scan)                                         | Scan a string for the presence of a set of characters        |
| CHARACTER                 | [trim](f_trim)                                         | Remove trailing blank characters of a string                 |
| CHARACTER                 | [verify](f_verify)                                     | Scan a string for the absence of a set of characters         |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| COLLECTIVE                | [co\_broadcast](f_co_broadcast)                        | Copy a value to all images the current set of images         |
| COLLECTIVE                | [co\_lbound](f_co_lbound)                              | Lower codimension bounds of an array                         |
| COLLECTIVE                | [co\_max](f_co_max)                                    | Maximal value on the current set of images                   |
| COLLECTIVE                | [co\_min](f_co_min)                                    | Minimal value on the current set of images                   |
| COLLECTIVE                | [co\_reduce](f_co_reduce)                              | Reduction of values on the current set of images             |
| COLLECTIVE                | [co\_sum](f_co_sum)                                    | Sum of values on the current set of images                   |
| COLLECTIVE                | [co\_ubound](f_co_ubound)                              | Upper codimension bounds of an array                         |
| COLLECTIVE                | [event\_query](f_event_query)                          | Query whether a coarray event has occurred                   |
| COLLECTIVE                | [image\_index](f_image_index)                          | Cosubscript to image index conversion                        |
| COLLECTIVE                | [num\_images](f_num_images)                            | Number of images                                             |
| COLLECTIVE                | [this\_image](f_this_image)                            | Cosubscript index of this image                              |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| COMPILER INQUIRY          | [compiler\_options](f_compiler_options)                | Options passed to the compiler                               |
| COMPILER INQUIRY          | [compiler\_version](f_compiler_version)                | Compiler version string                                      |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| ISO\_C\_BINDING           | [c\_associated](f_c_associated)                        | Status of a C pointer                                        |
| ISO\_C\_BINDING           | [c\_f\_pointer](f_c_f_pointer)                         | Convert C into Fortran pointer                               |
| ISO\_C\_BINDING           | [c\_f\_procpointer](f_c_f_procpointer)                 | Convert C into Fortran procedure pointer                     |
| ISO\_C\_BINDING           | [c\_funloc](f_c_funloc)                                | Obtain the C address of a procedure                          |
| ISO\_C\_BINDING           | [c\_loc](f_c_loc)                                      | Obtain the C address of an object                            |
| ISO\_C\_BINDING           | [c\_sizeof](f_c_sizeof)                                | Size in bytes of an expression                               |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| KIND INQUIRY              | [kind](f_kind)                                         | Kind of an entity                                            |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| KIND                      | [selected\_char\_kind](f_selected_char_kind)           | Choose character kind such as "Unicode"                      |
| KIND                      | [selected\_int\_kind](f_selected_int_kind)             | Choose integer kind                                          |
| KIND                      | [selected\_real\_kind](f_selected_real_kind)           | Choose real kind                                             |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| MATHEMATICS:RANDOM        | [random\_number](f_random_number)                      | Pseudo-random number                                         |
| MATHEMATICS:RANDOM        | [random\_seed](f_random_seed)                          | Initialize a pseudo-random number sequence                   |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| MATHEMATICS:TRIGONOMETRIC | [acos](f_acos)                                         | arccosine function                                           |
| MATHEMATICS:TRIGONOMETRIC | [acosh](f_acosh)                                       | Inverse hyperbolic cosine function                           |
| MATHEMATICS:TRIGONOMETRIC | [asin](f_asin)                                         | Arcsine function                                             |
| MATHEMATICS:TRIGONOMETRIC | [asinh](f_asinh)                                       | Inverse hyperbolic sine function                             |
| MATHEMATICS:TRIGONOMETRIC | [atan](f_atan)                                         | Arctangent function                                          |
| MATHEMATICS:TRIGONOMETRIC | [atan2](f_atan2)                                       | Arctangent function                                          |
| MATHEMATICS:TRIGONOMETRIC | [atanh](f_atanh)                                       | Inverse hyperbolic tangent function                          |
| MATHEMATICS:TRIGONOMETRIC | [cos](f_cos)                                           | Cosine function                                              |
| MATHEMATICS:TRIGONOMETRIC | [cosh](f_cosh)                                         | Hyperbolic cosine function                                   |
| MATHEMATICS:TRIGONOMETRIC | [sin](f_sin)                                           | Sine function                                                |
| MATHEMATICS:TRIGONOMETRIC | [sinh](f_sinh)                                         | Hyperbolic sine function                                     |
| MATHEMATICS:TRIGONOMETRIC | [tan](f_tan)                                           | Tangent function                                             |
| MATHEMATICS:TRIGONOMETRIC | [tanh](f_tanh)                                         | Hyperbolic tangent function                                  |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| MATHEMATICS               | [bessel\_j0](f_bessel_j0)                              | Bessel function of the first kind of order 0                 |
| MATHEMATICS               | [bessel\_j1](f_bessel_j1)                              | Bessel function of the first kind of order 1                 |
| MATHEMATICS               | [bessel\_jn](f_bessel_jn)                              | Bessel function of the first kind                            |
| MATHEMATICS               | [bessel\_y0](f_bessel_y0)                              | Bessel function of the second kind of order 0                |
| MATHEMATICS               | [bessel\_y1](f_bessel_y1)                              | Bessel function of the second kind of order 1                |
| MATHEMATICS               | [bessel\_yn](f_bessel_yn)                              | Bessel function of the second kind                           |
| MATHEMATICS               | [erf](f_erf)                                           | Error function                                               |
| MATHEMATICS               | [erfc](f_erfc)                                         | Complementary error function                                 |
| MATHEMATICS               | [erfc\_scaled](f_erfc_scaled)                          | Error function                                               |
| MATHEMATICS               | [exp](f_exp)                                           | Exponential function                                         |
| MATHEMATICS               | [gamma](f_gamma)                                       | Gamma function                                               |
| MATHEMATICS               | [hypot](f_hypot)                                       | Euclidean distance function                                  |
| MATHEMATICS               | [log](f_log)                                           | Logarithm function                                           |
| MATHEMATICS               | [log10](f_log10)                                       | Base 10 logarithm function                                   |
| MATHEMATICS               | [log\_gamma](f_log_gamma)                              | Logarithm of the Gamma function                              |
| MATHEMATICS               | [norm2](f_norm2)                                       | Euclidean vector norm                                        |
| MATHEMATICS               | [sqrt](f_sqrt)                                         | Square-root function                                         |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| MODEL\_COMPONENTS         | [exponent](f_exponent)                                 | Exponent function                                            |
| MODEL\_COMPONENTS         | [fraction](f_fraction)                                 | Fractional part of the model representation                  |
| MODEL\_COMPONENTS         | [nearest](f_nearest)                                   | Nearest representable number                                 |
| MODEL\_COMPONENTS         | [rrspacing](f_rrspacing)                               | Reciprocal of the relative spacing                           |
| MODEL\_COMPONENTS         | [scale](f_scale)                                       | Scale a real value                                           |
| MODEL\_COMPONENTS         | [set\_exponent](f_set_exponent)                        | Set the exponent of the model                                |
| MODEL\_COMPONENTS         | [spacing](f_spacing)                                   | Smallest distance between two numbers of a given type        |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| NUMERIC MODEL             | [digits](f_digits)                                     | Significant digits function                                  |
| NUMERIC MODEL             | [epsilon](f_epsilon)                                   | Epsilon function                                             |
| NUMERIC MODEL             | [huge](f_huge)                                         | Largest number of a kind                                     |
| NUMERIC MODEL             | [maxexponent](f_maxexponent)                           | Maximum exponent of a real kind                              |
| NUMERIC MODEL             | [minexponent](f_minexponent)                           | Minimum exponent of a real kind                              |
| NUMERIC MODEL             | [precision](f_precision)                               | Decimal precision of a real kind                             |
| NUMERIC MODEL             | [radix](f_radix)                                       | Base of a model number                                       |
| NUMERIC MODEL             | [range](f_range)                                       | Decimal exponent range of a real kind                        |
| NUMERIC MODEL             | [tiny](f_tiny)                                         | Smallest positive number of a real kind                      |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| NUMERIC:TYPE              | [aimag](f_aimag)                                       | Imaginary part of complex number                             |
| NUMERIC:TYPE              | [cmplx](f_cmplx)                                       | Complex conversion function                                  |
| NUMERIC:TYPE              | [dble](f_dble)                                         | Double conversion function                                   |
| NUMERIC:TYPE              | [float](f_float)                                       | Convert integer to default real                              |
| NUMERIC:TYPE              | [int](f_int)                                           | Convert to integer type                                      |
| NUMERIC:TYPE              | [nint](f_nint)                                         | Nearest whole number                                         |
| NUMERIC:TYPE              | [real](f_real)                                         | Convert to real type                                         |
| NUMERIC:TYPE              | [sngl](f_sngl)                                         | Convert double precision real to default real                |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| NUMERIC                   | [abs](f_abs)                                           | Absolute value                                               |
| NUMERIC                   | [aint](f_aint)                                         | Truncate to a whole number                                   |
| NUMERIC                   | [anint](f_anint)                                       | Nearest whole number                                         |
| NUMERIC                   | [ceiling](f_ceiling)                                   | Integer ceiling function                                     |
| NUMERIC                   | [conjg](f_conjg)                                       | Complex conjugate function                                   |
| NUMERIC                   | [dim](f_dim)                                           | Positive difference                                          |
| NUMERIC                   | [dprod](f_dprod)                                       | Double product function                                      |
| NUMERIC                   | [floor](f_floor)                                       | Integer floor function                                       |
| NUMERIC                   | [max](f_max)                                           | Maximum value of an argument list                            |
| NUMERIC                   | [min](f_min)                                           | Minimum value of an argument list                            |
| NUMERIC                   | [mod](f_mod)                                           | Remainder function                                           |
| NUMERIC                   | [modulo](f_modulo)                                     | Modulo function                                              |
| NUMERIC                   | [sign](f_sign)                                         | Sign copying function                                        |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| SYSTEM ENVIRONMENT        | [command\_argument\_count](f_command_argument_count)   | Get number of command line arguments                         |
| SYSTEM ENVIRONMENT        | [cpu\_time](f_cpu_time)                                | return CPU processor time in seconds                         |
| SYSTEM ENVIRONMENT        | [date\_and\_time](f_date_and_time)                     | gets current time                                            |
| SYSTEM ENVIRONMENT        | [execute\_command\_line](f_execute_command_line)       | Execute a shell command                                      |
| SYSTEM ENVIRONMENT        | [get\_command](f_get_command)                          | Get the entire command line                                  |
| SYSTEM ENVIRONMENT        | [get\_command\_argument](f_get_command_argument)       | Get command line arguments                                   |
| SYSTEM ENVIRONMENT        | [get\_environment\_variable](f_get_environment_variable)| Get an environmental variable                               |
| SYSTEM ENVIRONMENT        | [system\_clock](f_system_clock)                        | Return numeric data from a real-time clock.                  |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| TRANSFORMATIONAL FUNCTION | [cshift](f_cshift)                                     | Circular shift elements of an array                          |
| TRANSFORMATIONAL FUNCTION | [dot\_product](f_dot_product)                          | Dot product function                                         |
| TRANSFORMATIONAL FUNCTION | [eoshift](f_eoshift)                                   | End-off shift elements of an array                           |
| TRANSFORMATIONAL FUNCTION | [matmul](f_matmul)                                     | matrix multiplication                                        |
| TRANSFORMATIONAL FUNCTION | [null](f_null)                                         | Function that returns a disassociated pointer                |
| TRANSFORMATIONAL FUNCTION | [parity](f_parity)                                     | Reduction with exclusive OR                                  |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
| TBD                       | [associated](f_associated)                             | Status of a pointer or pointer/target pair                   |
| TBD                       | [extends\_type\_of](f_extends_type_of)                 | determine if the dynamic type of A is an extension of the dynamic type of MOLD.          |
| TBD                       | [findloc](f_findloc)                                   | Location of first element of ARRAY identified by MASK along dimension DIM having a value |
| TBD                       | [is\_iostat\_end](f_is_iostat_end)                     | Test for end-of-file value                                   |
| TBD                       | [is\_iostat\_eor](f_is_iostat_eor)                     | Test for end-of-record value                                 |
| TBD                       | [maskl](f_maskl)                                       | Left justified mask                                          |
| TBD                       | [maskr](f_maskr)                                       | Right justified mask                                         |
| TBD                       | [move\_alloc](f_move_alloc)                            | Move allocation from one object to another                   |
| TBD                       | [present](f_present)                                   | Determine whether an optional dummy argument is specified    |
| TBD                       | [same\_type\_as](f_same_type_as)                       | Query dynamic types for equality                             |
| ------------------------- | ------------------------------------------------------ | ------------------------------------------------------------ |
