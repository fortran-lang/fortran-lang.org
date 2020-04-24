# Fortran-lang.org package index


## Package criteria

The following criteria are required of packages to be indexed:

- __Relevance__: the package must be primarily implemented in Fortran or provide
a complete Fortran interface to an existing package or be purposed solely towards
software development in Fortran.

- __Maturity__: the primary functionality of the package shall be implemented.
No prototype, testing or partially complete packages will be accepted.
If the package is hosted on github or similar, it should have at least 5 'stars'.

- __Availability__: the package source shall be freely available for browsing online
or cloning or downloading

- __Open source__: the package shall be licensed under an appropriate [open-source license](https://opensource.org/licenses)
with the license file clearly included with the source code

- __Uniqueness__: the package shall not be a fork or minor revision of existing packages

- __README__: the package shall have some form of README or landing-page clearly
stating the package purpose and functionality. This should also contain information
on the package dependencies and the steps required to build and run.


The following criteria are not required but are recommended:

- __Documentation__: any form of written documentation aimed at users of the package. Ideally
this should cover:
  - Supported / tested compilers
  - Dependencies
  - Build and install process
  - Modules contained within the package
  - Procedures made available and their interfaces
  - Example code

- __Contributing__: details on how users may submit issues and contribute to the development of the
package

- __Tests__: any form of executable test(s) that can be used to verify the functionality of the package

- __Portability__: no non-standard language extensions or proprietary dependencies

- __FPM__: support installation by the Fortran Package Manager [fpm](https://github.com/fortran-lang/fpm)


## Process for adding packages

1. Users should confirm that their project meets the minimum requirements for listing in the 
Fortran-lang package index, as written in this document

2. Users should open an issue using the 'Package index request' issue template

3. At least three Fortran-lang community members shall review the request against the criteria above

4. If three or more Fortran-lang community members agree that the package should be listed and there is no significant objection, then a pull
request can be opened updating `_data/package_index.yml` with the new package metadata


## Package index requests

Package details are to be supplied in the issue in the following YAML format:

```
name: <Package_name>
url: <repository_url>
description: <single_line_description>
tags: [tag] [tag] [tag]
version: [version]
license: [license]
```

- Package description should clearly describe the functionality of the package in a single sentence.

- Tags (optional) should contain any terms not already contained in the name or description that 
users may search directly for.

If the project is hosted on github:

- Package version
  - this can be determined automatically if a versioned release has been created on github
  - if version is specified, it will override any detect github version
  - if version is 'none', then no version information will be displayed

- Package license
  - this can be determined automatically if github is able to detect a known license
  - if license is specified, it will override any detected github license
  - if license is 'none', then no license information will be displayed

If the project is not hosted on github:

- License and version information cannot be detected automatically for non-github repositories
- if version is not specified, then no version information will be displayed
- if license is not specified, then no license information will be displayed


