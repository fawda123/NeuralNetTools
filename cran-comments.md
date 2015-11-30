## Resubmission
This is a resubmission.  In this version I have removed the NOTE by adding the following to the NAMESPACE: 

importFrom(graphics,par)
importFrom(graphics,plot)
importFrom(graphics,points)
importFrom(graphics,segments)
importFrom(graphics,text)
importFrom(stats,coef)
importFrom(stats,formula)
importFrom(stats,kmeans)
importFrom(stats,model.frame)
importFrom(stats,predict)
importFrom(stats,quantile)
importFrom(stats,terms)
importFrom(utils,capture.output)

I have also reduced all examples in the documentation to run in less than five seconds.  

## Test environments
* local Windows 7 install, R 3.2.2 
* local Windows 7 install, Current r-devel (2015-11-30 r69717)
* Windows install (on AppVeyor), r-devel (2015-11-28 r69714)
* ubuntu 12.04 (on travis-ci), R 3.2.2
* CRAN win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs, NOTEs.

## Downstream dependencies
I have also run R CMD check on the RSNNS downstream dependency for NeuralNetTools.  There were no ERRORs, WARNINGs, or NOTEs.