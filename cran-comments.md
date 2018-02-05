## Resubmission
This is a version update to 1.5.1. 

## Test environments
* ubuntu 14.04.5 LTS (on travis-ci), R 3.4.2
* Windows install (on AppVeyor), R 3.4.3 Patched (2018-02-03 r74202)
* win-builder [http://win-builder.r-project.org/](http://win-builder.r-project.org/) (devel and release)
* local Windows 7 install, R 3.4.3

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE, related to a change in my contact email: 

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Marcus W. Beck <marcusb@sccwrp.org>'

New maintainer:
  Marcus W. Beck <marcusb@sccwrp.org>
Old maintainer(s):
  Marcus W. Beck <mbafs2012@gmail.com>

## Downstream dependencies
I have also run R CMD check on the darch, radiant.model, and RSNNS downstream dependencies for NeuralNetTools.  There were no ERRORs, WARNINGs, or NOTEs.