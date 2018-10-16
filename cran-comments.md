## Test environments

* local OS X install, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.5.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Feedback

The previously submitted version of this package had a CRAN maintainer
request that instances of `\dontrun{}` were substituted with `\donttest{}`. 
This has been done.

Regarding feedback about the missing author, the code snippet was taken from
an answer on StackOverflow. As the IP rights were not explicit, the routine has
been removed from the package. Thus, we've opted to avoid adding another author
or contributor to the package via `Authors@R`.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.
