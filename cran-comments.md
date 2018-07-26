## Resubmission
This is an update to v1.5.2 with new CITATION file for JSS article

## Test environments
* Ubuntu 14.04.5 LTS (on travis-ci), R 3.4.4
* local Windows 7 install, R 3.4.4
* local Windows 7 install, Current r-devel (2018-07-25 r75005)
* Windows install (on AppVeyor), R 3.5.1 Patched (2018-07-24 r75006)
* win-builder [http://win-builder.r-project.org/](http://win-builder.r-project.org/) (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE, the DOI will be registered by JSS when this version is on CRAN: 

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Marcus W. Beck <marcusb@sccwrp.org>'

Found the following (possibly) invalid DOIs:
  DOI: 10.18637/jss.v085.i11
    From: inst/CITATION
    Status: Not Found
    Message: 404

## Downstream dependencies
I have also run R CMD check on the radiant.model and RSNNS downstream dependencies for NeuralNetTools.  There were no ERRORs, WARNINGs, or NOTEs.