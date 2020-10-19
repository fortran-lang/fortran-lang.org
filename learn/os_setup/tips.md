---
layout: book
title: Smart Tips
permalink: /learn/os_setup/tips
---

Closing this mini-tutorial we give a few tips and share some thoughts that will, hopefully, help you in choosing a compiler and an editor or an IDE.

- Check [this](https://www.fortran.uk/fortran-compiler-comparisons/) site for a comprehensive Fortran Compiler Comparisons.
- Since NVFortran version 17.1, the support for compute capability 2.0 (Fermi) and lower has been removed. This is important because, at any time, only the latest compiler version is free to download. Users with older GPUs will not be able to run CUDA Fortran with the current (20.7) or future versions. Read [this wiki](https://www.wikiwand.com/en/CUDA#/GPUs_supported) to find your GPU's compute capability and cross-check with the latest CUDA SDK that supports it.
- There are many online Fortran compilers. These are websites that offer a basic text editor and allow you to compile and run your code on a remote server. They are particularly useful for quick prototyping and testing. Although we promised neutrality, we recommend that you check out the open-source and free [Compiler Explorer](https://godbolt.org/), an amazing tool with unique features.
- Arguably the most popular Text Editor, according to recent online surveys, as of the latest update of this tutorial, is the Visual Studio Code, developed by Microsoft.
- Regarding IDEs, the most common free choices are the CODE::BLOCKS and Geany. Many of the commercial IDEs give up to 30 days of free trial period. Also, keep in mind that among the commercial IDEs the prices may vary greatly, some might be quite affordable. Finally, if you are a student, an open-source developer, or a …hobbyist *Fortraner* do not hesitate to contact those companies and request for a discount. There have been cases, e.g., in r/fortran, where representatives, from at least one company, have offered to individuals, looking for affordable complete solutions (IDE + compiler), discount codes.
- Finally, please join us in [Fortran Discourse](https://fortran-lang.discourse.group/) and do not hesitate to post your questions and request for further information.
