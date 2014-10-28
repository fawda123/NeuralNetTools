#' Get weights for a neural network
#'
#' Get weights for a neural network in an organized list by extracting values from a neural network object.  This function is generally not called by itself.  
#'
#' @param mod_in input object for which an organized model list is desired.  The input can be an object of class \code{numeric}, \code{nnet}, \code{mlp}, or \code{nn} 
#' @param ... arguments passed to other methods
#' 
#' @export neuralweights neuralnet nnet mlp
#' 
#' @import neuralnet nnet RSNNS
#' 
#' @return Returns a named \code{list} of weight values for the input model.  
#' 
#' @details Each element of the returned list is named using the construct 'layer node', e.g. 'out 1' is the first node of the output layer.  Hidden layers are named using three values for instances with more than one hidden layer, e.g., 'hidden 1 1' is the first node in the first hidden layer, 'hidden 1 2' is the second node in the first hidden layer, 'hidden 2 1' is the first node in the second hidden layer, etc.  The values in each element of the list represent the weights entering the specific node from the preceding layer in sequential order, starting with the bias layer if applicable.  
#' 
#' @examples
#' 
#' data(neuraldat)
#' set.seed(123)
#' 
#' ## using numeric input
#' 
#' wts_in <- c(13.12, 1.49, 0.16, -0.11, -0.19, -0.16, 0.56, -0.52, 0.81)
#' struct <- c(2, 2, 1) #two inputs, two hidden, one output 
#' 
#' neuralweights(wts_in, struct = struct)
#' 
#' ## using nnet
#' 
#' library(nnet)
#' 
#' mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 10, linout = T)
#'  
#' neuralweights(mod)  
#' 
#' ## using RSNNS, no bias layers
#' 
#' library(RSNNS)
#' 
#' x <- neuraldat[, c('X1', 'X2', 'X3')]
#' y <- neuraldat[, 'Y1']
#' mod <- mlp(x, y, size = 10, linOut = T)
#' 
#' neuralweights(mod)
#' 
#' ## using neuralnet
#' 
#' library(neuralnet)
#' 
#' mod <- neuralnet(Y1 ~ X1 + X2 + X3, data = neuraldat, hidden = 10)
#' 
#' neuralweights(mod)
neuralweights <-  function(mod_in, ...) UseMethod('neuralweights')

#' @rdname neuralweights
#' 
#' @param rel_rsc numeric vector of length two indicating minimum and maximum values to rescale weights for plotting in a neural interpretation diagram. 
#' @param struct numeric vector equal in length to the number of layers in the network.  Each number indicates the number of nodes in each layer starting with the input and ending with the output.  An arbitrary number of hidden layers can be included.
#' 
#' @import scales
#' 
#' @export neuralweights.numeric
#' 
#' @method neuralweights numeric
neuralweights.numeric <-  function(mod_in, rel_rsc = NULL, struct){
  
  wts <-  mod_in
  
  # sanity check
  if(length(wts) != struct[1]*struct[2]+struct[2]*struct[3]+struct[3]+struct[2])
    stop('Incorrect length of weight matrix for given network structure')
  
  if(!is.null(rel_rsc)) wts <- scales::rescale(abs(wts), c(1, rel_rsc))
  
  #convert wts to list with appropriate names 
  hid_struct <-  struct[ -c(length(struct))]
  row_nms <-  NULL
  for(i in 1:length(hid_struct)){
    if(is.na(hid_struct[i + 1])) break
    row_nms <-  c(row_nms, rep(paste('hidden', i, seq(1:hid_struct[i + 1])), each = 1 + hid_struct[i]))
  }
  row_nms <-  c(
    row_nms, 
    rep(paste('out', seq(1:struct[length(struct)])), each = 1 + struct[length(struct) - 1])
  )
  out_ls <-  data.frame(wts, row_nms)
  out_ls$row_nms <-  factor(row_nms, levels = unique(row_nms), labels = unique(row_nms))
  out_ls <-  split(out_ls$wts, f = out_ls$row_nms)
  
  assign('struct', struct, envir = environment(neuralweights))
  
  out_ls
  
}

#' @rdname neuralweights
#' 
#' @import scales
#'
#' @export neuralweights.nnet
#'  
#' @method neuralweights nnet
neuralweights.nnet <-  function(mod_in, rel_rsc = NULL){
  
  struct <-  mod_in$n
  wts <-  mod_in$wts
  
  if(!is.null(rel_rsc)) wts <-  scales::rescale(abs(wts), c(1, rel_rsc))
  
  #convert wts to list with appropriate names 
  hid_struct <-  struct[ -c(length(struct))]
  row_nms <-  NULL
  for(i in 1:length(hid_struct)){
    if(is.na(hid_struct[i + 1])) break
    row_nms <-  c(row_nms, rep(paste('hidden', i, seq(1:hid_struct[i + 1])), each = 1 + hid_struct[i]))
  }
  row_nms <-  c(
    row_nms, 
    rep(paste('out', seq(1:struct[length(struct)])), each = 1 + struct[length(struct) - 1])
  )
  out_ls <-  data.frame(wts, row_nms)
  out_ls$row_nms <-  factor(row_nms, levels = unique(row_nms), labels = unique(row_nms))
  out_ls <-  split(out_ls$wts, f = out_ls$row_nms)
  
  assign('struct', struct, envir = environment(neuralweights))
  
  out_ls
  
}

#' @rdname neuralweights
#' 
#' @import scales reshape2
#'
#' @export neuralweights.mlp
#'
#' @method neuralweights mlp
neuralweights.mlp <-  function(mod_in, rel_rsc = NULL){
  
  struct <-  c(mod_in$nInputs, mod_in$archParams$size, mod_in$nOutputs)
  hid.num <-  length(struct) - 2
  wts <-  mod_in$snnsObject$getCompleteWeightMatrix()
    
  #get all input - hidden and hidden - hidden wts
  inps <-  wts[grep('Input', row.names(wts)), grep('Hidden_2', colnames(wts)), drop = F]
  inps <-  melt(rbind(rep(NA, ncol(inps)), inps))$value
  uni.hids <-  paste0('Hidden_', 1 + seq(1, hid.num))
  for(i in 1:length(uni.hids)){
    if(is.na(uni.hids[i + 1])) break
    tmp <-  wts[grep(uni.hids[i], rownames(wts)), grep(uni.hids[i + 1], colnames(wts)), drop = F]
    inps <-  c(inps, melt(rbind(rep(NA, ncol(tmp)), tmp))$value)
  }
    
  #get connections from last hidden to output layers
  outs <-  wts[grep(paste0('Hidden_', hid.num + 1), row.names(wts)), grep('Output', colnames(wts)), drop = F]
  outs <-  rbind(rep(NA, ncol(outs)), outs)
    
  #weight vector for all
  wts <-  c(inps, melt(outs)$value)
  assign('bias', F, envir = environment(neuralweights))
  
  if(!is.null(rel_rsc)) wts <-  scales::rescale(abs(wts), c(1, rel_rsc))
  
  #convert wts to list with appropriate names 
  hid_struct <-  struct[ -c(length(struct))]
  row_nms <-  NULL
  for(i in 1:length(hid_struct)){
    if(is.na(hid_struct[i + 1])) break
    row_nms <-  c(row_nms, rep(paste('hidden', i, seq(1:hid_struct[i + 1])), each = 1 + hid_struct[i]))
  }
  row_nms <-  c(
    row_nms, 
    rep(paste('out', seq(1:struct[length(struct)])), each = 1 + struct[length(struct) - 1])
  )
  out_ls <-  data.frame(wts, row_nms)
  out_ls$row_nms <-  factor(row_nms, levels = unique(row_nms), labels = unique(row_nms))
  out_ls <-  split(out_ls$wts, f = out_ls$row_nms)
  
  assign('struct', struct, envir = environment(neuralweights))
  
  out_ls
  
}

#' @rdname neuralweights
#'   
#' @import scales
#'   
#' @export neuralweights.nn
#'
#' @method neuralweights nn
neuralweights.nn <- function(mod_in, rel_rsc = NULL){
  
  struct <-  unlist(lapply(mod_in$weights[[1]], ncol))
  struct <-  struct[ - length(struct)]
  struct <-  c(
    length(mod_in$model.list$variables), 
    struct, 
    length(mod_in$model.list$response)
  )        
  wts <-  unlist(mod_in$weights[[1]])   
  
  if(!is.null(rel_rsc)) wts <-  scales::rescale(abs(wts), c(1, rel_rsc))
  
  #convert wts to list with appropriate names 
  hid_struct <-  struct[ -c(length(struct))]
  row_nms <-  NULL
  for(i in 1:length(hid_struct)){
    if(is.na(hid_struct[i + 1])) break
    row_nms <-  c(row_nms, rep(paste('hidden', i, seq(1:hid_struct[i + 1])), each = 1 + hid_struct[i]))
  }
  row_nms <-  c(
    row_nms, 
    rep(paste('out', seq(1:struct[length(struct)])), each = 1 + struct[length(struct) - 1])
  )
  out_ls <-  data.frame(wts, row_nms)
  out_ls$row_nms <-  factor(row_nms, levels = unique(row_nms), labels = unique(row_nms))
  out_ls <-  split(out_ls$wts, f = out_ls$row_nms)
  
  assign('struct', struct, envir = environment(neuralweights))
  
  out_ls
  
}