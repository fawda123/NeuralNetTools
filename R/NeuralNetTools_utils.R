#' Get weights for a neural network
#'
#' Get weights for a neural network in an organized list by extracting values from a neural network object.  This function is generally not called by itself.  
#'
#' @param mod_in input object for which an organized model list is desired.  The input can be an object of class \code{numeric}, \code{nnet}, \code{mlp}, or \code{nn} 
#' @param ... arguments passed to other methods
#' 
#' @export
#' 
#' @import neuralnet nnet RSNNS
#' 
#' @return Returns a two-element list with the first element being a vector indicating the number of nodes in each layer of the neural network and the second element being a named list of weight values for the input model.  
#' 
#' @details Each element of the returned list is named using the construct 'layer node', e.g. 'out 1' is the first node of the output layer.  Hidden layers are named using three values for instances with more than one hidden layer, e.g., 'hidden 1 1' is the first node in the first hidden layer, 'hidden 1 2' is the second node in the first hidden layer, 'hidden 2 1' is the first node in the second hidden layer, etc.  The values in each element of the list represent the weights entering the specific node from the preceding layer in sequential order, starting with the bias layer if applicable.  For example, the elements in a list item for 'hidden 1 1' of a neural network with a 3 5 1 structure (3 inputs, 5 hidden nodes, 1 output) would have four values indicating the weights in sequence from the bias layer, first input layer, second input layer, and third input layer going to the first hidden node.    
#' 
#' The function will remove direct weight connections between input and output layers if the neural network was created with a skip-layer using \code{skip = TRUE} with the \code{\link[nnet]{nnet}}  or \code{\link[caret]{train}} functions.  This may produce misleading results when evaluating variable performance with the \code{\link{garson}} function.  
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
#' mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5, linout = TRUE)
#'  
#' neuralweights(mod)  
#' 
#' ## using RSNNS, no bias layers
#' 
#' library(RSNNS)
#' 
#' x <- neuraldat[, c('X1', 'X2', 'X3')]
#' y <- neuraldat[, 'Y1']
#' mod <- mlp(x, y, size = 5, linOut = TRUE)
#' 
#' neuralweights(mod)
#' 
#' \dontrun{
#' # pruned model using code from RSSNS pruning demo
#' pruneFuncParams <- list(max_pr_error_increase = 10.0, pr_accepted_error = 1.0, 
#'  no_of_pr_retrain_cycles = 1000, min_error_to_stop = 0.01, init_matrix_value = 1e-6, 
#'  input_pruning = TRUE, hidden_pruning = TRUE)
#' mod <- mlp(x, y, size = 5, pruneFunc = "OptimalBrainSurgeon", 
#'  pruneFuncParams = pruneFuncParams)
#' 
#' neuralweights(mod)
#' 
#' }
#' 
#' ## using neuralnet
#' 
#' library(neuralnet)
#' 
#' mod <- neuralnet(Y1 ~ X1 + X2 + X3, data = neuraldat, hidden = 5)
#' 
#' neuralweights(mod)
neuralweights <-  function(mod_in, ...) UseMethod('neuralweights')

#' @rdname neuralweights
#' 
#' @param rel_rsc numeric value indicating maximum to rescale weights for plotting in a neural interpretation diagram. Default is \code{NULL} for no rescaling.
#' @param struct numeric vector equal in length to the number of layers in the network.  Each number indicates the number of nodes in each layer starting with the input and ending with the output.  An arbitrary number of hidden layers can be included.
#' 
#' @import scales
#' 
#' @export
#' 
#' @method neuralweights numeric
neuralweights.numeric <-  function(mod_in, rel_rsc = NULL, struct, ...){
  
  wts <-  mod_in
  
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
  
  return(list(struct = struct, wts = out_ls))
  
}

#' @rdname neuralweights
#' 
#' @import scales
#'
#' @export
#'  
#' @method neuralweights nnet
neuralweights.nnet <-  function(mod_in, rel_rsc = NULL, ...){
  
  struct <-  mod_in$n
  wts <-  mod_in$wts
  
  if(!is.null(rel_rsc)) wts <- scales::rescale(abs(wts), c(1, rel_rsc))
  
  # remove wts from input to output if skip layers present
  chk <- grepl('skip-layer', capture.output(mod_in))
  if(any(chk)){
    coefs <- coef(mod_in)
    rems <- grepl('^i.*>o', names(coefs))
    wts <- wts[!rems]
  }
  
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
  
  return(list(struct = struct, wts = out_ls))
  
}

#' @rdname neuralweights
#' 
#' @import scales reshape2 tidyr
#'
#' @export
#'
#' @method neuralweights mlp
neuralweights.mlp <-  function(mod_in, rel_rsc = NULL, ...){
  
  struct <-  c(mod_in$nInputs, mod_in$archParams$size, mod_in$nOutputs)
  hid.num <-  length(struct) - 2
  wts <-  mod_in$snnsObject$getCompleteWeightMatrix()
  
  # recreate weight matrix if pruned network
  if('pruneFunc' %in% names(mod_in)){
    
    # get all node names using naming convention from mlp
    inp_nms <- grep('^Input', colnames(wts), value = TRUE)
    out_nms <- grep('^Output', colnames(wts), value = TRUE)
    uni.hids <-  paste0('Hidden_', 1 + seq(1, hid.num))
    hid_nms <- NULL
    for(i in 1:length(uni.hids)){ 
      hid_nms <- c(hid_nms, paste0(uni.hids[i], '_', 1:struct[i + 1]))
    }
    all_nms <- c(inp_nms, hid_nms, out_nms)
    all_nms <- expand.grid(all_nms, all_nms)
    all_nms <- data.frame(all_nms)
    names(all_nms) <- c('keyrow', 'key')
    
    # wts in long form, merge with all names, NA to zero, back to matrix
    wts <- data.frame(wts, keyrow = row.names(wts))
    wts <- gather(wts, 'key', 'value',-ncol(wts))
    wts <- merge(all_nms, wts, by = c('key', 'keyrow'), all.x = TRUE)
    wts[is.na(wts$value), 'value'] <- 0
    wts <- spread(wts, 'keyrow', 'value')
    wts$key <- NULL
    wts <- as.matrix(wts)
    rownames(wts) <- colnames(wts)
    wts <- t(wts)  
  }

  #get all input - hidden and hidden - hidden wts
  inps <-  wts[grep('Input', row.names(wts)), grep('Hidden_2', colnames(wts)), drop = FALSE]
  inps <-  melt(rbind(rep(NA, ncol(inps)), inps))$value
  uni.hids <-  paste0('Hidden_', 1 + seq(1, hid.num))
  for(i in 1:length(uni.hids)){
    if(is.na(uni.hids[i + 1])) break
    tmp <-  wts[grep(uni.hids[i], rownames(wts)), grep(uni.hids[i + 1], colnames(wts)), drop = FALSE]
    inps <-  c(inps, melt(rbind(rep(NA, ncol(tmp)), tmp))$value)
  }
    
  #get connections from last hidden to output layers
  outs <-  wts[grep(paste0('Hidden_', hid.num + 1), row.names(wts)), grep('Output', colnames(wts)), drop = FALSE]
  outs <-  rbind(rep(NA, ncol(outs)), outs)
    
  #weight vector for all
  wts <-  c(inps, melt(outs)$value)
  assign('bias', FALSE)
  
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
  
  return(list(struct = struct, wts = out_ls))
  
}

#' @rdname neuralweights
#'   
#' @import scales
#'   
#' @export
#'
#' @method neuralweights nn
neuralweights.nn <- function(mod_in, rel_rsc = NULL, ...){
  
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
  
  return(list(struct = struct, wts = out_ls))
  
}

#' Predicted values for Lek profile method
#'
#' Get predicted values for Lek Profile method, used iteratively in \code{\link{lekprofile}}
#' 
#' @param mat_in \code{data.frame} of only the explanatory variables used to create model
#' @param mod_in any model object with a predict method
#' @param var_sel chr string of explanatory variable to select
#' @param step_val number of values to sequence range of selected explanatory variable
#' @param fun_in function defining the method of holding explanatory variables constant
#' @param resp_name chr string of response variable names for correct labelling
#'
#'@details
#' Gets predicted output for a model's response variable based on matrix of explanatory variables that are restricted following Lek's profile method. The selected explanatory variable is sequenced across a range of values. All other explanatory variables are held constant at the value specified by \code{fun_in}.
#' 
#' @seealso lekprofile
#' 
#' @return A \code{\link{data.frame}} of predictions and the sequence values of the selected explanatory variable
#' 
#' @export
#' 
#' @examples
#' 
#' ## using nnet
#' 
#' library(nnet)
#' 
#' data(neuraldat) 
#' set.seed(123)
#' 
#' mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5)
#' 
#' mat_in <- neuraldat[, c('X1', 'X2', 'X3')]
#' pred_sens(mat_in, mod, 'X1', 100, function(x) quantile(x, 0.5), 'Y1')
pred_sens <- function(mat_in, mod_in, var_sel, step_val, fun_in, resp_name){
  
  mat_out <- matrix(nrow = step_val, ncol = ncol(mat_in), dimnames = list(c(1:step_val)))
  mat_out <- data.frame(mat_out)
  names(mat_out) <- names(mat_in)
  
  mat_cons <- mat_in[, !names(mat_in) %in% var_sel, drop = F]
  mat_cons <- apply(mat_cons, 2, fun_in)
  mat_cons <- sapply(1:step_val, function(x) mat_cons)
  if(!'numeric' %in% class(mat_cons)) mat_cons <- t(mat_cons)
  mat_out[, !names(mat_in) %in% var_sel] <- mat_cons
  
  mat_out[, var_sel] <- seq(min(mat_in[, var_sel]), max(mat_in[, var_sel]), length = step_val)
  
  out <- data.frame(predict(mod_in, newdata = as.data.frame(mat_out)))
  names(out) <- resp_name
  x_vars <- mat_out[, var_sel]
  data.frame(out, x_vars)
  
}

#' Get weights for the skip layer in a neural network
#'
#' Get weights for the skip layer in a neural network, only valid for networks created using \code{skip = TRUE} with the \code{\link[nnet]{nnet}} function.
#'
#' @param mod_in input object for which an organized model list is desired. 
#' @param ... arguments passed to other methods
#' 
#' @export
#' 
#' @return return a numeric vector in sequential order of the weights for the direct connection between input and output layers
#' 
#' @details This function is similar to \code{\link{neuralweights}} except only the skip layer weights are returned.
#' 
#' @examples
#' 
#' data(neuraldat)
#' set.seed(123)
#' 
#' ## using nnet
#' 
#' library(nnet)
#' 
#' mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5, linout = TRUE, 
#'  skip = TRUE)
#'  
#' neuralskips(mod)  
#' 
neuralskips <-  function(mod_in, ...) UseMethod('neuralskips')

#' @rdname neuralskips
#' 
#' @import scales
#'
#' @export
#'  
#' @method neuralskips nnet
neuralskips.nnet <-  function(mod_in, ...){
  
  wts <-  mod_in$wts
  
  # get skip layer weights if present, otherwise exit
  chk <- grepl('skip-layer', capture.output(mod_in))
  if(any(chk)){
    
    coefs <- coef(mod_in)
    skips <- grepl('^i.*>o', names(coefs))
    skips <- wts[skips]
    
  } else {
    
    stop('No skip layer')
    
  }
  
  return(skips)
  
}
