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

2. Users should open a pull request using the 'Package index request' template

3. At least three Fortran-lang community members shall review the request against the criteria above

4. If three or more Fortran-lang community members agree that the package should be listed and there is no significant objection, then the pull request will be merged


## Package index requests

Package index requests are made by pull requests against the [fortran-lang.org repository](https://github.com/fortran-lang/fortran-lang.org/).
See [this guide](https://guides.github.com/activities/forking/) for guidance on forking and making pull requests.

Package details are listed in the `_data/package_index.yml` data file.

To add a package simply create a new entry within this file.
The data file is ordered by high-level categories merely to aid in navigation;
find the appropriate category for your package and create a new entry.

### Github hosted packages

```
  - name: <Package_name>
    github: <github_user>/<repository_name>
    description: <single_line_description>
    categories: <category1> [category2]
    tags: [tag1] [tag2] [tag3]
    version: [version]
    license: [license]
```

Valid categories:
- `libraries`: general libraries
- `interfaces`: libraries that provide interfaces to other libraries, software or devices
- `programming`: general programming utilities: errors, logging, testing, documentation _etc._
- `data-types`: libraries providing advanced data types: containers, datetime, resizable arrays _etc._
- `strings`: string handling libraries
- `io`: libraries that parse and generate various file formats
- `graphics`: plotting and GUIs
- `numerical`: matrices, linear algebra, solvers, root-finding, interpolation, optimization, differential eqns, statistics, machine learning, random numbers _etc._
- `scientific`: domain-specific scientific libraries or applications
- `examples`: repositories offering language feature demonstrations, tutorials and benchmarks

__Projects listing more than one category must provide good justification thereof 
in the pull request.__

__Notes:__

- The package description should clearly describe the functionality of the package in a single sentence.

- Tags (optional) should contain any terms not already contained in the name or description that users may search directly for. Tags should be separate by spaces.

- Package version
  - this can be determined automatically if a versioned release has been created on github
  - if version is specified, it will override any detected github version
  - if version is 'none', then no version information will be displayed. (Use this if
  your package has no version.)

- Package license
  - this can be determined automatically if github is able to detect a known license
  - license must be specified if github is unable to detect a known license

### Non-github hosted packages

```
  - name: <Package_name>
    url: <repo_url>
    description: <single_line_description>
    categories: <category1> [category2]
    tags: [tag1] [tag2] [tag3]
    version: [version]
    license: <license>
```

__Notes:__

- License and version information cannot be detected automatically for non-github repositories
- if your package has no version, then omit the version field
- a license must be specified for non-github repositories


### Member review checklist

Community members reviewing packages should cover the following points:

1. Ensure the package meets the minimum criteria as written in this document

2. Check the package metadata
    - Repository exists and is accessible
    - Description clearly and concisely describes the package
    - Assigned category is appropriate

3. Check license information
    - If license field has been omitted: check that github has detected the license
    - If license field is included: check that it matches repository license file

After merge:
  - Check that package is available in expected category and search
