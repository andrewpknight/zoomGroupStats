## Resubmission
This is a resubmission. Thank you for your helpful feedback. In this version, I have:

* Edited title and description to ensure that package names, software names and API names are in single quotes.
* Changed the function grabVideoStills() to ensure that it does not write by default to the user's home filespace (including the package directory and getwd()). To generate output, the user must now supply a path for the output as an argument in the function call.
* Reviewed all functions to ensure that any file writing activity is performed only at the explicit request of the user and using an explicitly provided file path for the written file(s). No functions write to the user's home filespace unless explicitly requested.
* Changed all examples that write output to do so to tempdir().

## Test environments

* local OS X install, R 4.0.4
* win-builder (devel and release)

## R CMD check results

* There were no ERRORs or WARNINGs. 
* This is my first submission, so there is a first release NOTE. 

## Downstream dependencies

* This is a new release.
* There are no downstream dependencies. 
