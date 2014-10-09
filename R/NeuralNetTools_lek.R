#' Sensitivity analysis based on Lek's profile method
#'
#' @param mod.in
#' @param var.sens
#' @param resp.name 
#' @param exp.in
#' @param steps
#' @param split.vals
#' @param val.out
#' @return what it returns
lekprofile<-function(mod.in,var.sens=NULL,resp.name=NULL,exp.in=NULL,
                  steps=100,split.vals=seq(0,1,by=0.2),val.out=F){
  
  ##
  #sort out exp and resp names based on object type of call to mod.in
  #get matrix for exp vars
  
  #for nnet
  if('nnet' %in% class(mod.in) | !'mlp' %in% class(mod.in)){
    if(is.null(mod.in$call$formula)){
      if(is.null(resp.name)) resp.name<-colnames(eval(mod.in$call$y))
      if(is.null(var.sens)) var.sens<-colnames(eval(mod.in$call$x))
      mat.in<-eval(mod.in$call$x)
    }
    else{
      forms<-eval(mod.in$call$formula)
      dat.names<-model.frame(forms,data=eval(mod.in$call$data))
      if(is.null(resp.name)) resp.name<-as.character(forms)[2]
      if(is.null(var.sens)) 
        var.sens<-names(dat.names)[!names(dat.names) %in% as.character(forms)[2]]
      mat.in<-dat.names[,!names(dat.names) %in% as.character(forms)[2]]
    }
  }
  
  #for rsnns
  if('mlp' %in% class(mod.in)){
    if(is.null(exp.in)) stop('Must include matrix or data frame of input variables')
    
    if(is.null(resp.name)) resp.name<-paste0('Y',seq(1,mod.in$nOutputs))
    mat.in<-data.frame(exp.in)
    names(mat.in)<-paste0('X',seq(1,mod.in$nInputs))
    if(is.null(var.sens)) var.sens<-names(mat.in)
    
  }
  
  ##
  #gets predicted output for nnet based on matrix of explanatory variables
  #selected explanatory variable is sequenced across range of values
  #all other explanatory variables are held constant at value specified by 'fun.in'
  pred.sens<-function(mat.in,mod.in,var.sel,step.val,fun.in,resp.name){
    
    mat.out<-matrix(nrow=step.val,ncol=ncol(mat.in),dimnames=list(c(1:step.val),colnames(mat.in)))
    
    mat.cons<-mat.in[,!names(mat.in) %in% var.sel]
    mat.cons<-apply(mat.cons,2,fun.in)
    mat.out[,!names(mat.in) %in% var.sel]<-t(sapply(1:step.val,function(x) mat.cons))
    
    mat.out[,var.sel]<-seq(min(mat.in[,var.sel]),max(mat.in[,var.sel]),length=step.val)
    
    out<-data.frame(predict(mod.in,new=as.data.frame(mat.out)))
    names(out)<-paste0('Y',seq(1,ncol(out)))
    out<-out[,resp.name,drop=F]
    x.vars<-mat.out[,var.sel]
    data.frame(out,x.vars)
    
  }
  
  #use 'pred.fun' to get pred vals of response across range of vals for an exp vars
  #loops over all explanatory variables of interest and all split values
  lek.vals<-sapply(
    var.sens,
    function(vars){
      sapply(
        split.vals,
        function(splits){
          pred.sens(
            mat.in,
            mod.in,
            vars,
            steps,
            function(val) quantile(val,probs=splits),
            resp.name
          )
        },
        simplify=F
      )
    },
    simplify=F  
  )
  
  #melt lek.val list for use with ggplot
  lek.vals<-melt.list(lek.vals,id.vars='x.vars')
  lek.vals$L2<-factor(lek.vals$L2,labels=split.vals)
  names(lek.vals)<-c('Explanatory','resp.name','Response','Splits','exp.name')
  
  #return only values if val.out = T
  if(val.out) return(lek.vals)
  
  #ggplot object
  p<-ggplot(lek.vals,aes(x=Explanatory,y=Response,group=Splits)) + 
    geom_line(aes(colour=Splits,linetype=Splits,size=Splits)) + 
    facet_grid(resp.name~exp.name) +
    scale_linetype_manual(values=rep('solid',length(split.vals))) +
    scale_size_manual(values=rep(1,length(split.vals)))
  
  return(p)
  
}
