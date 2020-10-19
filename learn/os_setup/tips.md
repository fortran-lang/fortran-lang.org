---
layout: book
title: Smart Tips
permalink: /learn/os_setup/tips
---

To conclude, we give a few tips that may help you to choose a compiler and an editor or an IDE.

- Check [this site](https://www.fortran.uk/fortran-compiler-comparisons/) for a comprehensive comparison of Fortran compilers.
- Since NVFortran version 17.1, the support for compute capability 2.0 (Fermi) and lower has been removed. This is important because, at any time, only the latest compiler version is free to download. Users with older GPUs will not be able to run CUDA Fortran with the current (20.7) or future versions. Read [this wiki](https://www.wikiwand.com/en/CUDA#/GPUs_supported) to find your GPU's compute capability and cross-check with the latest CUDA SDK that supports it.
- There are many online Fortran compilers. These are websites that offer a basic text editor and allow you to compile and run your code on a remote server. They are particularly useful for quick prototyping and testing. Although we promised neutrality, we recommend that you check out the open-source and free [Compiler Explorer](https://godbolt.org/), an amazing tool with unique features.
- Arguably the most popular text editor, according to recent online surveys, is the Visual Studio Code, developed by Microsoft.
- The most popular free IDEs are the Code::Blocks and Geany. Many commercial IDEs give up to 30 days of free trial. Keep in mind that the prices of commercial IDEs may vary, and some may be quite affordable. Finally, if you are a student, an open-source developer, or a …hobbyist *Fortraner* do not hesitate to contact those companies and request a discount. There have been cases, e.g., in r/fortran, where representatives, from at least one company, have offered discount codes to individuals looking for affordable complete solutions (IDE + compiler).
- Finally, please join us on [Fortran Discourse](https://fortran-lang.discourse.group/) and do not hesitate to post your questions and request for further information.
