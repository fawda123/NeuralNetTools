## Resubmission
This is an update to v1.5.3

## Test environments
* Ubuntu 20.04.3 devel and release (on GitHub Actions), R 4.1.2
* OS X 10.15.7 (on GitHub Actions), R 4.1.2
* local Windows 10 install, R 4.1.2
* win-builder [http://win-builder.r-project.org/](http://win-builder.r-project.org/) (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE, for an email change to one that is active:

* checking DESCRIPTION meta-information ... NOTE
Maintainer field differs from that derived from Authors@R
  Maintainer: 'Marcus W. Beck <marcusb@sccwrp.org>'
  Authors@R:  'Marcus W. Beck <mbafs2012@gmail.com>'

## Downstream dependencies
We checked 6 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages