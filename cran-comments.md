## Resubmission
This is an update to v1.5.3

## Test environments
* Ubuntu 20.04.3 LTS (on travis-ci), R 3.4.4
* local Windows 10 install, R 4.1.2
* Windows install (on AppVeyor), R 3.5.1 Patched (2018-07-24 r75006)
* win-builder [http://win-builder.r-project.org/](http://win-builder.r-project.org/) (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE, for an email change for one that is no longer valid:

* checking DESCRIPTION meta-information ... NOTE
Maintainer field differs from that derived from Authors@R
  Maintainer: 'Marcus W. Beck <marcusb@sccwrp.org>'
  Authors@R:  'Marcus W. Beck <mbafs2012@gmail.com>'

## Downstream dependencies
I have also run R CMD check on the radiant.model and RSNNS downstream dependencies for NeuralNetTools.  There were no ERRORs, WARNINGs, or NOTEs.