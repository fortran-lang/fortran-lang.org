---
layout: post
date: 2022-06-09
title: "Fortran newsletter: June 2022"
category: newsletter
author: Milan Curcic, Alexis Perry-Holby, Ondřej Čertík, Henil Panchal
---

Welcome to the June edition of the monthly Fortran newsletter.
The newsletter comes out at the beginning of every month and details
Fortran news from the previous month.

<ul id="page-nav"></ul>

# fortran-lang.org

Here's what's new in the fortran-lang.org repo:

* [#401](https://github.com/fortran-lang/fortran-lang.org/pull/401):
  Newsletter May 2022
* [#403](https://github.com/fortran-lang/fortran-lang.org/pull/403):
  Add SeisSol to package index

Work in progress:

* [#397](https://github.com/fortran-lang/fortran-lang.org/pull/397) (WIP):
  Add NUFFT to package index
* [#396](https://github.com/fortran-lang/fortran-lang.org/pull/396) (WIP):
  Add OpenFFT to package index
* [#395](https://github.com/fortran-lang/fortran-lang.org/pull/395) (WIP):
  Add 2DECOMP&FFT to package index
* [#394](https://github.com/fortran-lang/fortran-lang.org/pull/394) (WIP):
  Add SLICOT to package index
* [#393](https://github.com/fortran-lang/fortran-lang.org/pull/393) (WIP):
  Add FATODE to package index
* [#347](https://github.com/fortran-lang/fortran-lang.org/pull/347) (WIP):
  Fortran Intrinsics

[Let us know](https://github.com/fortran-lang/fortran-lang.org/issues)
if you have any suggestions for the website and its content.
We welcome any new contributors to the website and the tutorials page in particular - see the
[contributor guide](https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/CONTRIBUTING.md)
for how to get started.

## Fortran Standard Library

Here's what's new in stdlib:

* [#656](https://github.com/fortran-lang/stdlib/pull/656):
  Add hint for building error with make
* [#655](https://github.com/fortran-lang/stdlib/pull/655):
  fixed 32-bit integer overflow in stdlib_io_npy
* [#657](https://github.com/fortran-lang/stdlib/pull/657):
  Remove support for manual make builds

Work in progress:

* [#660](https://github.com/fortran-lang/stdlib/pull/660) (WIP):
  Fix erroneous gaussian quadrature points in gauss_legendre
* [#659](https://github.com/fortran-lang/stdlib/pull/659) (WIP):
  Readme update
* [#652](https://github.com/fortran-lang/stdlib/pull/652) (WIP):
  Feature: loadtxt skiprows and max_rows
* [#625](https://github.com/fortran-lang/stdlib/pull/625) (WIP):
  Gamma special function
* [#611](https://github.com/fortran-lang/stdlib/pull/611) (WIP):
  Hash maps
* [#604](https://github.com/fortran-lang/stdlib/pull/604) (WIP):
  Add get_argument, get_variable and set_variable
* [#580](https://github.com/fortran-lang/stdlib/pull/580) (WIP):
  Add terminal and color escape sequences
* [#552](https://github.com/fortran-lang/stdlib/pull/552) (WIP):
  fixed bug in stringlist
* [#536](https://github.com/fortran-lang/stdlib/pull/536) (WIP):
  Fix conversion warnings
* [#520](https://github.com/fortran-lang/stdlib/pull/520) (WIP):
  [stdlib_io] add `disp`(display variable values formatted).
* [#517](https://github.com/fortran-lang/stdlib/pull/517) (WIP):
  adding SPEC_TEMPLATE.md #504
* [#514](https://github.com/fortran-lang/stdlib/pull/514) (WIP):
  pop, drop & get with basic range feature for stringlist
* [#491](https://github.com/fortran-lang/stdlib/pull/491) (WIP):
  Stdlib linked list
* [#473](https://github.com/fortran-lang/stdlib/pull/473) (WIP):
  Error stop improvements
* [#363](https://github.com/fortran-lang/stdlib/pull/363) (WIP):
  Sorting string's characters according to their ASCII values
* [#286](https://github.com/fortran-lang/stdlib/pull/286) (WIP):
  Probability Distribution and Statistical Functions -- Beta Distribution Module
* [#278](https://github.com/fortran-lang/stdlib/pull/278) (WIP):
  Probability Distribution and Statistical Functions -- Gamma Distribution Module
* [#189](https://github.com/fortran-lang/stdlib/pull/189) (WIP):
  Initial implementation of COO / CSR sparse format



Please help improve stdlib by testing and [reviewing pull requests](https://github.com/fortran-lang/stdlib/issues?q=is%3Apr+is%3Aopen+label%3A%22reviewers+needed%22)!

The candidate for file system operations to be included in stdlib is being developed by
[@MarDiehl](https://github.com/MarDiehl) and [@arjenmarkus](https://github.com/arjenmarkus)
in [this repository](https://github.com/MarDiehl/stdlib_os).
Please try it out and let us know how it works, if there are any issues, or if the API can be improved.

## Fortran Package Manager

Here's what's new in fpm:

* [#692](https://github.com/fortran-lang/fpm/pull/692):
  Fix for non-portable GFortran `-J` flag in install script
* [#693](https://github.com/fortran-lang/fpm/pull/693):
  Fix show-model option

Work in progress:

* [#701](https://github.com/fortran-lang/fpm/pull/701) (WIP):
  Some cleanups and minor fixes
* [#686](https://github.com/fortran-lang/fpm/pull/686) (WIP):
  fix: remove extra space from help-test cmd
* [#685](https://github.com/fortran-lang/fpm/pull/685) (WIP):
  fix: function for getting executable path
* [#653](https://github.com/fortran-lang/fpm/pull/653) (WIP):
  Enable profiles in toml
* [#608](https://github.com/fortran-lang/fpm/pull/608) (WIP):
  --env switch lets you specify the prefix of the compiler-related environment variables
* [#539](https://github.com/fortran-lang/fpm/pull/539) (WIP):
  Add parent packages into dependency tree
* [#498](https://github.com/fortran-lang/fpm/pull/498) (WIP):
  Compiler flags profiles

`fpm` is still in early development and we need as much help as we can get.
Here's how you can help today:

* Use it and let us know what you think! Read the [fpm packaging guide](https://fpm.fortran-lang.org/en/tutorial)
to learn how to build your package with fpm, and the [manifest reference](https://fpm.fortran-lang.org/en/spec/manifest.html)
to learn what are all the things that you can specify in the fpm.toml file.

* Browse existing *fpm* packages on the [fortran-lang website](https://fortran-lang.org/packages/fpm)
* Browse the [open issues](https://github.com/fortran-lang/fpm/issues) and see if you can help implement any fixes or features.
* Adapt your Fortran package for fpm and submit it to the [Registry](https://github.com/fortran-lang/fpm-registry).
* Improve the documentation.

The short-term goal of fpm is to make development and installation of Fortran packages with dependencies easier.
Its long term goal is to build a rich and decentralized ecosystem of Fortran packages and create a healthy
environment in which new open source Fortran projects are created and published with ease.

## Compilers

### Flang

* FIR
    * Initial lowering of the Fortran Do loop
    * Lower Unstructured do loops
* Driver
    * Define the default frontend driver triple
    * Add support for consuming LLVM IR/BC files
    * Add support for -save-temps
    * Switch to the MLIR coding style in the driver
    * Fix driver method names overridden by the plugins
    * Support parsing response files
    * Make driver accept -module-dir<value>
    * Add support for generating executables on MacOSX/Darwin
* OpenMP
    * Add lowering stubs for OpenMP/OpenACC declarative constructs
    * Added tests for taskwait and taskyield translation
    * Restrict types for omp.parallel args
    * Add omp.cancel and omp.cancellationpoint.
    * Initial lowering of the OpenMP worksharing loop
    * Lowering for task construct
    * Support lowering to MLIR for ordered clause
    * Support for Collapse
    * Upstream the lowering of the parallel do combined construct
    * Fix the types of worksharing-loop variables
    * Change the OpenMP atomic read/write test cases
* Runtime
    * Correct emission & reading of unterminated final records
    * Support B/O/Z editing of CHARACTER
    * Use 1-based dim in transformational runtime error msg
    * Change "unsupported" messages in the runtime to "not yet implemented"
    * Fix input of NAN(...) on non-fast path
    * Don't pad CHARACTER input at end of record unless PAD='YES'
    * Enforce restrictions on unlimited format repetition
    * (G0) for CHARACTER means (A), not (A0)
    * BACKSPACE after non-advancing I/O
    * Use proper prototypes in Fortran_main. NFCI
    * Clean up asynchronous I/O APIs
    * INQUIRE(UNIT=666,NUMBER=n) must set n=666
    * Handle BACKSPACE after reading past EOF
* Fix MAXLOC/MINLOC when MASK is scalar .FALSE. 
* Fix UBOUND() constant folding for parentheses expr
* Support FINDLOC/MAXLOC/MINLOC with scalar mask
* Handle common block with different sizes in same file
* Add one semantic check for implicit interface
* Fix semantics check for RETURN statement
* Fix ICE for passing a label for non alternate return arguments
* Add ExternalNameConversionPass to pass pipeline
* Fix AllocaOp/AllocMemOp type conversion
* Support external procedure passed as actual argument with implicit character type
* Fix internal error with DATA-statement style initializers
* Upstream support for POINTER assignment in FORALL
* Enforce a program not including more than one main program
* Retain binding label of entry subprograms
* Fold intrinsic inquiry functions SAME_TYPE_AS() and EXTENDS_TYPE_OF()
* Fold intrinsic functions SPACING() and RRSPACING()
* Operands of SIGN() need not have same kind
* Correct folding of SPREAD() for higher ranks
* Refine handling of short character actual arguments
* Ensure that structure constructors fold parameter references
* Correct actual/dummy procedure compatibility for ALLOCATABLE/POINTER functions
* Allow PDTs with LEN parameters in REDUCE()
* Allow NULL() actual argument for optional dummy procedure
* Allow implicit declaration of DATA objects in inner procedures
* Refine error checking in specification expressions
* Reverse a reversed type compatibility check
* Accept POINTER followed by INTERFACE
* Allow ENTRY function result symbol usage before the ENTRY
* Fold real-valued DIM(), MODULO() and MOD()
* Enforce limit on rank + corank
* Allow local variables and function result inquiries in specification expressions
* Change "bad kind" messages in the runtime to "not yet implemented"
* Fold complex component references
* Fix check for assumed-size arguments to SHAPE() & al.
* Fix a performance problem with lowering of forall loops and creating too many temporaries
* Warn for the limit on name length
* Install Fortran_main library
* test conforming & non-conforming lcobound
* Fix use-associated false-positive error
* Fix character length calculation for Unicode component
* Allow global scope names that clash with intrinsic modules
* Ignore BIND(C) binding name conflicts of inner procedures
* Allow more forward references to ENTRY names
* Extension: Accept Hollerith actual arguments as if they were BOZ
* Alternate entry points with unused arguments
* Fix crash in semantics after PDT instantiation


### LFortran

* Gagandeep Singh (106):
  - Factored out visit_Declaration to visit_DeclarationUtil in CommonVisitor
  - Added test for kwargs in class procedure
  - Added support for kwargs in class procedures
  - Updated reference tests
  - Added support for i32 and i64 in repeat
  - Added mergechar in merge interface
  - Added is_iostat_eor
  - Removed compulsory evaluation of ishft
  - Updated reference tests
  - Use intrinsic type checking in assignment only when operator overloading fails
  - Perform casting in Compare only when overloaded is not available
  - Set dest_type and source_type even though casting doesn't happen
  - Use kind_value to generate type in ArraySize
  - Added test for verifying SemanticError in case of non-constant kind input
  - Updated reference tests
  - Add AssociateBlock and Block in serialization.cpp
  - Include associate_06 in the integration_tests/CMakeLists.txt
  - Take into account output kind in LLVM's ArraySize visitor
  - Added test for different output kinds in ArraySize
  - Updated reference tests
  - Import procedures for overloaded operators as well
  - Fixed tests for compiling correctly with gfortran
  - Mangle name before importing procedures under generic procs
  - Merged master into sprint_6
  - Avoid manual imports while using overloaded symbols
  - Remove symbol from to_be_imported_later
  - Added support for keyword arguments in generic procedures and fix total arguments
  - Added test for generic procedures with keyword arguments
  - Updated reference tests
  - Added intrinsics: congjz, dotproduct and updated: merge
  - Added matmul, transpose as ASR nodes
  - Added tests for matmul and transpose
  - Updated reference tests
  - Added merge, dotproduct procedures for complex type
  - Updated reference tests
  - Add support for source kwarg in allocate
  - Updated test for verifying source argument
  - Updated reference tests
  - Registered shiftr, shiftl, adjustr, lgt, llt, lge, lle, count in comptime_eval.h
  - Added shiftl, shiftr and count
  - Added more implementations for abs, mod
  - Added adjustr, lgt, llt, lle, lge for string type
  - Updated reference tests
  - Registered ieee_is_nan in comptime_eval.h
  - Added support for pack intrinsic
  - Added support for transfer intrinsic
  - Use modern Fortran syntax for array constants
  - Updated reference tests
  - ArrayTransfer -> Transfer rename
  - Added generation code for expression replacer
  - Added ReplaceArgVisitor and generalised handle_return_type
  - Added tests for verifying arg replacer in return types
  - Updated reference tests
  - Added cmplx via ComplexConstructor node
  - Updated reference tests
  - Import via use inside Function
  - Added support for matching Derived/ClassType
  - Added support for falling back to intrinsic
  - Added to test verify importing procedures inside function
  - Updated reference tests
  - Added support for passing kind parameter to floor intrinsic
  - 1. Use CPtr for variables declared with type(c_ptr) 2. Set Module_t.m_intrinsic in set_intrinsic 3. Add CPtr in extract_dimensions_from_ttype
  - Updated reference tests
  - Added LLVM support for CLoc, CPtr
  - Updated reference tests
  - Added integration test for c_f_pointer
  - Added support for c_f_pointer
  - Updated reference tests
  - Added error checking for presence of shape argument in c_f_pointer call
  - Fixed ArrayConstant type and raise error is shape is not rank 1
  - Updated reference tests
  - Implemented c_f_pointer for non-array variables
  - Updated reference tests
  - Added test with pointer array variables
  - Shifted type generation to a function in LLVM backend
  - Corrected llvm::Type* for array pointer variables
  - Updated reference tests
  - 1. Fixed ArrayBound for array pointers 2. Shifted argument type generation to a function and use recursion for Pointer
  - Added support for printing Pointer type
  - 1. Added AssociateBlock symbol in PassVisitor 2. Fixed get_bound to return ArrayBound instead of a function call
  - Use element type in ArrayRef instead of pointer
  - Syntax improvement
  - Updated reference tests
  - Removed warnings
  - Adjust ArrayBound for ArrayConstant
  - Corrected arrays_13 by making iv, rv as target
  - Stronger verification checks for CFPointer creation
  - Support for array inputs in CFPointer
  - Improved bindc2 for array inputs in c_f_pointer
  - 1. Fixed ArraySize for array pointer variables in LLVM backend. 2. Improved CFPointer in LLVM backend to not interfere with already stored array in array pointer variables 3. Improved bindc2.f90 and made it robust to cover more cases. 4. Updated reference tests
  - Fixed unused variable warnings in llvm_utils.cpp
  - Use abstract methods in CFPointer for accessing array descriptor data
  - Added test for ArrayRef in c_loc
  - Minor update in bindc2 and bindc3
  - Fixed Complex case in duplicate type and intialise type at declaration
  - Added support ArrayRef in CLoc in LLVM backend
  - Updated reference tests
  - 1. Added support for CPtr in arguments and fixed llvm::Type for intent(out) for CPtr 2. Added support for constant arrays as shape in c_f_pointer
  - Updated bindc4 for verifying constant arrays in c_f_pointer
  - Updated reference tests

* Naman Gera (1):
  - Update the C runtime library

* Ondřej Čertík (136):
  - AST->ASR: Remove current_body
  - Fix a bug in a test
  - Update tests
  - bind(c): Add tests for i64, f32, f64
  - Update modules_18b.f90 to compile
  - Add tests for the other types
  - Comment out a non-working case
  - Update tests
  - bind(c): Fix call_fortran_i64
  - Update tests
  - bind(c): Add a test for i32 by value
  - LLVM: implement value arguments in bind(c) procs
  - Update tests
  - Make modules_18b.f90 compile
  - bind(c): test i64, f32, f64 by value
  - Update modules_18b to compile
  - Update tests
  - ASR: Bring updates from LPython
  - Update tests
  - ASR: Updates from LPython
  - Update the rest of the code to compile
  - Update tests
  - C++ backend: implement ComplexConstructor
  - Add the simplest test for submodules
  - Add Logical to nested_vars
  - Workaround a cmake bug
  - Add a test for bind(c) with pointers
  - Rework the AST->ASR handling of floor()
  - Implement is_intrinsic_symbol()
  - Refactor floor() into lfortran_intrinsic_math3
  - Update tests
  - Add a test case for imported derived type
  - Add value to all expr nodes except Constant
  - Update tests
  - Implement expr_type() using ExprTypeVisitor
  - Implement expr_value() using ExprValueVisitor
  - iso_c_binding: add the c_loc() function
  - Allow derived types as return values
  - ASR: Represent c_loc()
  - Add ASR test for c_loc()
  - Update tests
  - ASR: Add string conversion for derived type
  - Add a CPtr() type
  - Make c_loc() return CPtr
  - LLVM: handle CPtr in convert_args()
  - LLVM: Comment out visit_CFPointer (WIP)
  - CI: pin mkdocs-material and mkdocs versions
  - Fix spelling
  - Update tests
  - Shorten the help for --show-wat to fit 80 columns
  - Git ignore wasm_visitor.h
  - Move emit_wat out of LLVM ifdef
  - Generate wasm_visitor.h in ci/build.xsh
  - Enable WAT tests in run_tests.py
  - Add a test for WASM
  - Update reference tests
  - Implement FortranEvaluator::get_wasm()
  - Use asr_to_wasm_bytes_stream() in asr_to_wasm
  - WASM: Add 64 bit BinOp operations
  - WASM: handle i64 arguments
  - WASM: Add a test for i64
  - Move the wasm_instructions_visitor.py to libasr
  - Update ASR from LPython
  - Update tests
  - Bring more ASR improvements from LPython
  - Bring in name mangling
  - Update tests
  - Add a test for passing through pointers via C
  - LLVM: implement debug ASR printing
  - LLVM: Use `deftype` to implement interfaces
  - LLVM: Pass type(c_ptr) by value properly
  - Pass "n" by reference for now
  - Pass arguments by value for bind(c)
  - Only do the load if it is a pointer
  - LLVM: Use an ASR condition instead of LLVM one
  - lfortran_intrinsic_string: depend iso_fortran_env
  - Enable bindc_01 LLVM test
  - Add a test for pointer argument
  - Add suffixes 1/2
  - Add a callback2b test
  - Add a test for callback1b()
  - Get value to reference argument working
  - Port ASR improvements from LPython
  - Update the rest of the code to compile
  - Update tests
  - ASR sync with LPython

* Tapasweni Pathak (9):
  - Add markdown sample for intrinsic:math:asin
  - Add doxygen docstring in lfortran intrinsic runtime asin
  - Add comments for interface asin
  - Add doxygen comment for lfortran_sasin_api
  - Enhance presentation of information
  - use retval for return values variable
  - delete fortran doxygen comments docs
  - mkdocs: LFortran Intrinsics: asin
  - add: mkdocs: code syntax highlighting

* Ubaid (24):
  - Improve ceiling() test case
  - Update reference tests
  - Specifying constants as double precision
  - Update error condition check as suggested
  - Update reference tests
  - Code formatting
  - Add and improve namespace ending comments
  - Add wasm_instructions list
  - Comment out few instructions that have temporary variables
  - Add wasm_instructions_visitor script
  - Add namespace related info and pass code as function parameter in wasm_insts_visitor script
  - Add command in build0.sh to generate wasm_visitor.h
  - Add utility struct and functions defined in wasm_utils
  - Add wasm_to_wat converter
  - Include wasm_to_wat and wasm_utils in CMakeLists.txt
  - Define vector.resize()
  - Add --show-wat flag and emit_wat() function
  - Declare and define get_wat() in fortran_evaluator
  - Declare and define asr_to_wasm_bytes_stream() which stores wasm binary to memory
  - Fix warning by adding U and add comment
  - Fix missing parameters bug
  - Switch off WAT_DEBUG macro
  - Remove debugging cout statements
  - Move load_file() to top

We are looking for new contributors. Please do not hesitate to contact us if you are interested. We will help you get up to speed.

## Events

* The Fortran-lang Google Summer of Code 2022 program began on May 23.
  We welcome five contributors: Arteev Raina, Ashirwad Mishra,
  Henil Shalin Panchal, Mohd Ubaid Shaikh, and Oshanath Rajawasam.
  They will be working on exciting projects from fpm and the Fortran
  website to improving the LFortran compiler.
  Read more about their projects [here](https://summerofcode.withgoogle.com/programs/2022/organizations/fortran-lang).
* We had our 26th Fortran Monthly call on May 16.
  Watch the recording below:
  <iframe width="560" height="315" src="https://www.youtube.com/embed/apuldZF_r2I" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

Join and follow the [Fortran Discourse](https://fortran-lang.discourse.group)
to stay tuned with the future meetings.

## Contributors

We thank everybody who contributed to fortran-lang in the past month by
commenting in any of these repositories:

* [fortran-lang/stdlib](https://github.com/fortran-lang/stdlib)
* [fortran-lang/stdlib-cmake-example](https://github.com/fortran-lang/stdlib-cmake-example)
* [fortran-lang/fpm](https://github.com/fortran-lang/fpm)
* [fortran-lang/fpm-registry](https://github.com/fortran-lang/fpm-registry)
* [fortran-lang/fpm-docs](https://github.com/fortran-lang/fpm-docs)
* [fortran-lang/setup-fpm](https://github.com/fortran-lang/setup-fpm)
* [fortran-lang/fpm-haskell](https://github.com/fortran-lang/fpm-haskell)
* [fortran-lang/fortran-lang.org](https://github.com/fortran-lang/fortran-lang.org)
* [fortran-lang/benchmarks](https://github.com/fortran-lang/benchmarks)
* [fortran-lang/fortran-forum-article-template](https://github.com/fortran-lang/fortran-forum-article-template)
* [fortran-lang/fftpack](https://github.com/fortran-lang/fftpack)
* [fortran-lang/minpack](https://github.com/fortran-lang/minpack)
* [fortran-lang/test-drive](https://github.com/fortran-lang/test-drive)
* [fortran-lang/vscode-fortran-support](https://github.com/fortran-lang/vscode-fortran-support)
* [j3-fortran/fortran\_proposals](https://github.com/j3-fortran/fortran_proposals)

<div id="gh-contributors" data-startdate="May 01 2022" data-enddate="May 31 2022" height="500px"></div>
