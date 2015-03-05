## Resubmission for package update
This is a resubmission for an update that was submitted earlier today (Jan. 27 2015).

I think I have fixed the NOTE that was received on submission regarding the export of unregistered S3 methods.  This included changes to NAMESPACE by adding S3method(print, foo) for a print method for class foo, repeated for each S3 method in my package.

All test environments were also checked again before resubmission. 

## Test environments
* local Windows 7 install, R 3.1.2 
* local Windows 7 install, Current r-devel (2015-01-27 r67627)
* ubuntu 12.04 (on travis-ci), R 3.1.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Marcus W. Beck <mbafs2012@gmail.com>'

## Downstream dependencies
None.