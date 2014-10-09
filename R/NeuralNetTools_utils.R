#' Get weights for a neural network
#'
#' @param mod.in
#' @param nid 
#' @param rel.rsc
#' @param struct.out
#' @return Returns \code{list} of weight values for the input model
neuralweights<-function(mod.in,nid,rel.rsc,struct.out){
  
  if('numeric' %in% class(mod.in)){
    wts<-mod.in
  }
  
  #neuralnet package
  if('nn' %in% class(mod.in)){
    struct.out<-unlist(lapply(mod.in$weights[[1]],ncol))
    struct.out<-struct.out[-length(struct.out)]
    struct.out<-c(
      length(mod.in$model.list$variables),
      struct.out,
      length(mod.in$model.list$response)
    )        
    wts<-unlist(mod.in$weights[[1]])   
  }
  
  #nnet package
  if('nnet' %in% class(mod.in)){
    struct.out<-mod.in$n
    wts<-mod.in$wts
  }
  
  #RSNNS package
  if('mlp' %in% class(mod.in)){
    struct.out<-c(mod.in$nInputs,mod.in$archParams$size,mod.in$nOutputs)
    hid.num<-length(struct.out)-2
    wts<-mod.in$snnsObject$getCompleteWeightMatrix()
    
    #get all input-hidden and hidden-hidden wts
    inps<-wts[grep('Input',row.names(wts)),grep('Hidden_2',colnames(wts)),drop=F]
    inps<-melt(rbind(rep(NA,ncol(inps)),inps))$value
    uni.hids<-paste0('Hidden_',1+seq(1,hid.num))
    for(i in 1:length(uni.hids)){
      if(is.na(uni.hids[i+1])) break
      tmp<-wts[grep(uni.hids[i],rownames(wts)),grep(uni.hids[i+1],colnames(wts)),drop=F]
      inps<-c(inps,melt(rbind(rep(NA,ncol(tmp)),tmp))$value)
    }
    
    #get connections from last hidden to output layers
    outs<-wts[grep(paste0('Hidden_',hid.num+1),row.names(wts)),grep('Output',colnames(wts)),drop=F]
    outs<-rbind(rep(NA,ncol(outs)),outs)
    
    #weight vector for all
    wts<-c(inps,melt(outs)$value)
    assign('bias',F,envir=environment(nnet.vals))
  }
  
  if(nid) wts<-rescale(abs(wts),c(1,rel.rsc))
  
  #convert wts to list with appropriate names 
  hid.struct<-struct.out[-c(length(struct.out))]
  row.nms<-NULL
  for(i in 1:length(hid.struct)){
    if(is.na(hid.struct[i+1])) break
    row.nms<-c(row.nms,rep(paste('hidden',i,seq(1:hid.struct[i+1])),each=1+hid.struct[i]))
  }
  row.nms<-c(
    row.nms,
    rep(paste('out',seq(1:struct.out[length(struct.out)])),each=1+struct.out[length(struct.out)-1])
  )
  out.ls<-data.frame(wts,row.nms)
  out.ls$row.nms<-factor(row.nms,levels=unique(row.nms),labels=unique(row.nms))
  out.ls<-split(out.ls$wts,f=out.ls$row.nms)
  
  assign('struct',struct.out,envir=environment(nnet.vals))
  
  out.ls
  
}