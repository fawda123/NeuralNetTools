#' Get weights for a neural network
#'
#' Get weights for a neural network in an organized list by extracting values from a neural network object.  This function is generally not called by itself.  
#'
#' @param mod_in input object for which an organized model list is desired.  The input can be an object of class \code{numeric}, \code{nnet}, \code{mlp}, or \code{nn} 
#' @param ... arguments passed to other methods
#' 
#' @export
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
  if(hid_struct[2] != 0) # only get hidden layer names if hidden layer exists
    for(i in 1:length(hid_struct)){
      if(is.na(hid_struct[i + 1])) break
      row_nms <-  c(row_nms, rep(paste('hidden', i, seq(1:hid_struct[i + 1])), each = 1 + hid_struct[i]))
    }
  # always get output names
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
#' @param grps matrix of values for holding explanatory values constant, one column per variable and one row per split
#' @param ynms chr string of response variable names for correct labelling
#'
#'@details
#' Gets predicted output for a model's response variable based on matrix of explanatory variables that are restricted following Lek's profile method. The selected explanatory variable is sequenced across a range of values. All other explanatory variables are held constant at the values in \code{grps}.
#' 
#' @seealso lekprofile
#' 
#' @return A \code{\link[base]{list}} of predictions where each element is a \code{\link[base]{data.frame}} with the predicted value of the response and the values of the explanatory variable defined by \code{var_sel}.  Each element of the list corresponds to a group defined by the rows in \code{grps} at which the other explanatory variables were held constant.
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
#' grps <- apply(mat_in, 2, quantile, seq(0, 1, by = 0.2))
#' 
#' pred_sens(mat_in, mod, 'X1', 100, grps, 'Y1')
pred_sens <- function(mat_in, mod_in, var_sel, step_val, grps, ynms){

  # exp variable to evaluate across its range
  chngs <- range(mat_in[, var_sel, drop = FALSE], na.rm = TRUE)
  chngs <- data.frame(seq(chngs[1], chngs[2], length = step_val))
  names(chngs) <- var_sel

  # constant values exp variables not to evaluate
  const <- grps[, !names(mat_in) %in% var_sel]
  rownames(const) <- 1:nrow(const)
   
  # iterate across rows of const, combine with chngs, get preds
  out <- apply(const, 1, function(x) {
    
    topred <- as.data.frame(rbind(x))[rep(1, step_val), ]
    topred <- cbind(chngs, topred)
    row.names(topred) <- 1:nrow(topred)
    topred <- topred[, names(mat_in)] # this has to be in correct order....
  
    preds <- data.frame(predict(mod_in, newdata = topred))
    names(preds) <- ynms

    x_vars <- topred[, var_sel]
    preds <- data.frame(preds, x_vars)
    rownames(preds) <- 1:step_val
    
    return(preds)
  
  })
   
  return(out)
  
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
#' @return Returns a list of connections for each output node, where each element of the list is the connection for each input node in sequential order to the respective output node.  The first weight in each element is not the bias connection, unlike the results for \code{\link{neuralweights}}.
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
#' @param rel_rsc numeric value indicating maximum to rescale weights for plotting in a neural interpretation diagram. Default is \code{NULL} for no rescaling.  Scaling is relative to all weights, not just those in the primary network. 
#'
#' @export
#'  
#' @method neuralskips nnet
neuralskips.nnet <-  function(mod_in, rel_rsc = NULL, ...){
  
  struct <- mod_in$n
  wts <-  mod_in$wts
  
  if(!is.null(rel_rsc)) wts <- scales::rescale(abs(wts), c(1, rel_rsc))
  
  # get skip layer weights if present, otherwise exit
  chk <- grepl('skip-layer', capture.output(mod_in))
  if(any(chk)){
    
    coefs <- coef(mod_in)
    skips <- grepl('^i.*>o', names(coefs))
    skips <- wts[skips]
    
  } else {
    
    stop('No skip layer')
    
  }
  
  # assign names and as list, otherwise weights not plotted correctly for multiple outputs
  row_nms <- rep(paste('out', seq(1:struct[length(struct)])), each = struct[1])
  
  out_ls <-  data.frame(skips, row_nms)
  out_ls$row_nms <-  factor(row_nms, levels = unique(row_nms), labels = unique(row_nms))
  out_ls <-  split(out_ls$skips, f = out_ls$row_nms)
  
  return(out_ls)
  
}

#' Get y locations for layers in \code{\link{plotnet}}
#' 
#' Get y locations for input, hidden, output layers in \code{\link{plotnet}} 
#'
#' @param lyr numeric indicating layer for getting y locations
#' @param max_sp logical indicating if space is maximized in plot
#' @param struct numeric vector for network structure
#' @param y_range numeric vector indicating limits of y axis
#' 
get_ys <- function(lyr, max_sp, struct, y_range){
  
  if(max_sp){ 
    spacing <- diff(c(0 * diff(y_range), 0.9 * diff(y_range)))/lyr
  } else {
    spacing <- diff(c(0 * diff(y_range), 0.9 * diff(y_range)))/max(struct)
  }
  
  out <- seq(0.5 * (diff(y_range) + spacing * (lyr - 1)), 0.5 * (diff(y_range) - spacing * (lyr - 1)), 
      length = lyr)
  
  return(out)
  
}

#' Plot neural network nodes
#' 
#' Plot neural network nodes in \code{\link{plotnet}}
#'
#' @param layer specifies which layer, integer from \code{struct}
#' @param x_loc indicates x location for layer, integer from \code{layer_x}
#' @param x_range numeric for total range of x-axis
#' @param layer_name string indicating text to put in node
#' @param cex_val numeric indicating size of point text
#' @param circle_cex numeric indcating size of circles
#' @param bord_col chr string indicating border color around nodes, default \code{lightblue}
#' @param in_col chr string indicating interior color of nodes
#' @param node_labs logical indicating if node labels are to be plotted
#' @param line_stag numeric indicating distance between of text from nodes
#' @param var_labs chr string for variable labels
#' @param x_names chr string for alternative names of input nodes
#' @param y_names chr string for alternative names of output nodes
#' @param ... values passed to \code{\link{get_ys}}
#' 
layer_points <- function(layer, x_loc, x_range, layer_name, cex_val, circle_cex, bord_col, in_col, node_labs, line_stag, var_labs, x_names, y_names, ...){
  
  x <- rep(x_loc * diff(x_range), layer)
  y <- get_ys(layer, ...)
  points(x, y, pch = 21, cex = circle_cex, col = bord_col, bg = in_col)
  if(node_labs) text(x, y, paste(layer_name, 1:layer, sep = ''), cex = cex_val)
  if(layer_name == 'I' & var_labs) text(x - line_stag * diff(x_range), y, x_names, pos = 2, cex = cex_val)      
  if(layer_name == 'O' & var_labs) text(x + line_stag * diff(x_range), y, y_names, pos = 4, cex = cex_val)
  
}

#' Plot bias points
#' 
#' Plot bias points in \code{\link{plotnet}}
#' 
#' @param bias_x numeric vector of values for x locations
#' @param bias_y numeric vector for y location
#' @param layer_name string indicating text to put in node
#' @param node_labs logical indicating of node labels are included
#' @param x_range numeric of x axis range for base plot
#' @param y_range numeric of y axis range for base plot
#' @param circle_cex numeric value indicating size of nodes, default 5
#' @param cex_val numeric value indicating size of text labels, default 1
#' @param bord_col chr string indicating border color around nodes, default \code{'lightblue'}
#' @param circle_col chr string indicating color of nodes
#' 
bias_points <- function(bias_x, bias_y, layer_name, node_labs, x_range, y_range, circle_cex, cex_val, bord_col, circle_col){
  for(val in 1:length(bias_x)){
    points(
      diff(x_range) * bias_x[val], 
      bias_y * diff(y_range), 
      pch = 21, col = bord_col, bg = circle_col, cex = circle_cex
    )
    if(node_labs)
      text(
        diff(x_range) * bias_x[val], 
        bias_y * diff(y_range), 
        paste(layer_name, val, sep = ''), 
        cex = cex_val
      )
  }
}

#' Plot connection weights
#' 
#' Plot connection weights in \code{\link{plotnet}}
#'  
#' @param mod_in neural network model object
#' @param h_layer numeric indicating which connections to plot for the layer
#' @param layer1 numeric indicating order of first layer (for multiple hiden layers)
#' @param layer2 numeric indicating order of second layer (for multiple hiden layers)
#' @param out_layer logical indicating if the lines are for the output layer
#' @param nid logical value indicating if neural interpretation diagram is plotted, default \code{TRUE}
#' @param rel_rsc numeric value indicating maximum to rescale weights for plotting in a neural interpretation diagram. Default is \code{NULL} for no rescaling.
#' @param all_in chr string indicating names of input variables for which connections are plotted, default all
#' @param pos_col chr string indicating color of positive connection weights, default \code{'black'}
#' @param neg_col chr string indicating color of negative connection weights, default \code{'grey'}
#' @param x_range numeric of x axis range for base plot
#' @param y_range numeric of y axis range for base plot
#' @param line_stag numeric value that specifies distance of connection weights from nodes
#' @param x_names chr string for names of input variables
#' @param layer_x numeric indicating locations of layers on x axis
#' @param struct numeric vector for network structure
#' @param max_sp logical indicating if space is maximized in plot
#' @param prune_col chr string indicating color of pruned connections, otherwise not shown
#' @param prune_lty line type for pruned connections, passed to \code{\link[graphics]{segments}}
#' @param skip logical to plot connections for skip layer
#' 
layer_lines <- function(mod_in, h_layer, layer1 = 1, layer2 = 2, out_layer = FALSE, nid, rel_rsc, all_in, pos_col, neg_col, x_range, y_range, line_stag, x_names, layer_x, struct, max_sp, prune_col = NULL, prune_lty = 'dashed', skip){

  x0 <- rep(layer_x[layer1] * diff(x_range) + line_stag * diff(x_range), struct[layer1])
  x1 <- rep(layer_x[layer2] * diff(x_range) - line_stag * diff(x_range), struct[layer1])
  
  # see if skip layer is presnet in nnet
  chk <- grepl('skip-layer', capture.output(mod_in))
  
  if(out_layer == TRUE){
    
    y0 <- get_ys(struct[layer1], max_sp, struct, y_range)
    y1 <- rep(get_ys(struct[layer2], max_sp, struct, y_range)[h_layer], struct[layer1])
    src_str <- paste('out', h_layer)
    
    # get weights for numeric, otherwise use different method for neuralweights
    if(inherits(mod_in, c('numeric', 'integer'))){
      
      wts <- neuralweights(mod_in, struct = struct)$wts
      wts_rs <- neuralweights(mod_in, rel_rsc, struct = struct)$wts
      wts <- wts[grep(src_str, names(wts))][[1]][-1]
      wts_rs <- wts_rs[grep(src_str, names(wts_rs))][[1]][-1]
      
    } else {
      
      # get skip weights if both TRUE
      if(skip & any(chk)){
        
        wts <- neuralskips(mod_in)
        wts_rs <- neuralskips(mod_in, rel_rsc)
        wts <- wts[grep(src_str, names(wts))][[1]] # these do not include bias 
        wts_rs <- wts_rs[grep(src_str, names(wts_rs))][[1]]
      
      # otherwise get normal connects
      } else {
        
        wts <- neuralweights(mod_in)$wts
        wts_rs <- neuralweights(mod_in, rel_rsc)$wts
        wts <- wts[grep(src_str, names(wts))][[1]][-1]
        wts_rs <- wts_rs[grep(src_str, names(wts_rs))][[1]][-1]
        
      }
      
    }

    cols <- rep(pos_col, struct[layer1])
    cols[wts<0] <- neg_col
    
    # remove pruned connections or color of prune_col not null, linetype dashed
    ltype <- rep(par('lty'), length(wts))
    if('pruneFunc' %in% names(mod_in)){
      if(is.null(prune_col)) cols[wts == 0] <- NA
      else cols[wts == 0] <- prune_col
      ltype[wts == 0] <- prune_lty
    }
    
  }
  
  else{
    
    if(is.logical(all_in)) all_in <- h_layer
    else all_in <- which(x_names == all_in)
    
    y0 <- rep(get_ys(struct[layer1], max_sp, struct, y_range)[all_in], struct[2])
    y1 <- get_ys(struct[layer2], max_sp, struct, y_range)
    src_str <- paste('hidden', layer1)
    
    if(inherits(mod_in, c('numeric', 'integer'))){
      wts <- neuralweights(mod_in, struct = struct)$wts
      wts <- unlist(lapply(wts[grep(src_str, names(wts))], function(x) x[all_in + 1]))
      wts_rs <- neuralweights(mod_in, rel_rsc, struct = struct)$wts
      wts_rs <- unlist(lapply(wts_rs[grep(src_str, names(wts_rs))], function(x) x[all_in + 1]))
    } else {
      wts <- neuralweights(mod_in)$wts
      wts <- unlist(lapply(wts[grep(src_str, names(wts))], function(x) x[all_in + 1]))
      wts_rs <- neuralweights(mod_in, rel_rsc)$wts
      wts_rs <- unlist(lapply(wts_rs[grep(src_str, names(wts_rs))], function(x) x[all_in + 1]))
    }
    
    cols <- rep(pos_col, struct[layer2])
    cols[wts<0] <- neg_col
    
    # remove pruned connections or color of prune_col not null, linetype dashed
    ltype <- rep(par('lty'), length(wts))
    if('pruneFunc' %in% names(mod_in)){
      if(is.null(prune_col)) cols[wts == 0] <- NA
      else cols[wts == 0] <- prune_col
      ltype[wts == 0] <- prune_lty
    }
          
  }
  
  if(nid) segments(x0, y0, x1, y1, col = cols, lwd = wts_rs, lty = ltype)
  else segments(x0, y0, x1, y1, lty = ltype)
  
}

#' Plot connection weights for bias lines
#' 
#' Plot connection weights for bias lines in \code{\link{plotnet}}
#'
#' @param bias_x numeric vector x axis locations for bias lines
#' @param bias_y numeric vector y axis locations for bias lines
#' @param mod_in neural network model object
#' @param nid logical value indicating if neural interpretation diagram is plotted, default \code{TRUE}
#' @param rel_rsc numeric value indicating maximum to rescale weights for plotting in a neural interpretation diagram. Default is \code{NULL} for no rescaling.
#' @param all_out chr string indicating names of response variables for which connections are plotted, default all
#' @param pos_col chr string indicating color of positive connection weights, default \code{'black'}
#' @param neg_col chr string indicating color of negative connection weights, default \code{'grey'}
#' @param struct numeric vector for network structure
#' @param y_names chr string for names of output variables
#' @param x_range numeric of x axis range for base plot
#' @param y_range numeric of x axis range for base plot
#' @param layer_x numeric indicating locations of layers on x axis
#' @param line_stag numeric value that specifies distance of connection weights from nodes
#' @param max_sp logical indicating if space is maximized in plot
bias_lines <- function(bias_x, bias_y, mod_in, nid, rel_rsc, all_out, pos_col, neg_col, struct, y_names, x_range, y_range, layer_x, line_stag, max_sp){
  
  if(is.logical(all_out)) all_out <- 1:struct[length(struct)]
  else all_out <- which(y_names == all_out)
  
  for(val in 1:length(bias_x)){
    
    if(inherits(mod_in, c('numeric', 'integer'))){
      wts <- neuralweights(mod_in, struct = struct)$wts
      wts_rs <- neuralweights(mod_in, rel_rsc, struct = struct)$wts
    } else {
      wts <- neuralweights(mod_in)$wts
      wts_rs <- neuralweights(mod_in, rel_rsc)$wts
    }
    
    if(val != length(bias_x)){
      wts <- wts[grep('out', names(wts), invert = TRUE)]
      wts_rs <- wts_rs[grep('out', names(wts_rs), invert = TRUE)]
      sel_val <- grep(val, substr(names(wts_rs), 8, 8))
      wts <- wts[sel_val]
      wts_rs <- wts_rs[sel_val]
    }
    
    else{
      wts <- wts[grep('out', names(wts))]
      wts_rs <- wts_rs[grep('out', names(wts_rs))]
    }
    
    cols <- rep(pos_col, length(wts))
    cols[unlist(lapply(wts, function(x) x[1]))<0] <- neg_col
    wts_rs <- unlist(lapply(wts_rs, function(x) x[1]))
    
    if(nid == FALSE){
      wts_rs <- rep(1, struct[val + 1])
      cols <- rep('black', struct[val + 1])
    }
    
    if(val != length(bias_x)){
      segments(
        rep(diff(x_range) * bias_x[val] + diff(x_range) * line_stag, struct[val + 1]), 
        rep(bias_y * diff(y_range), struct[val + 1]), 
        rep(diff(x_range) * layer_x[val + 1] - diff(x_range) * line_stag, struct[val + 1]), 
        get_ys(struct[val + 1], max_sp, struct, y_range), 
        lwd = wts_rs, 
        col = cols
      )
    }
    
    else{
      segments(
        rep(diff(x_range) * bias_x[val] + diff(x_range) * line_stag, struct[val + 1]), 
        rep(bias_y * diff(y_range), struct[val + 1]), 
        rep(diff(x_range) * layer_x[val + 1] - diff(x_range) * line_stag, struct[val + 1]), 
        get_ys(struct[val + 1], max_sp, struct, y_range)[all_out], 
        lwd = wts_rs[all_out], 
        col = cols[all_out]
      )
    }
    
  }
}

######
#' Create optional barplot for \code{\link{lekprofile}} splits
#' 
#' Create optional barplot of constant values of each variable for each split used with \code{\link{lekprofile}}
#'
#' @param grps \code{\link[base]{data.frame}} of values for each variable in each group used to create splits in \code{\link{lekprofile}}
#' 
#' @import ggplot2
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object
#'
#' @export
#' 
#' @examples 
#' ## enters used with kmeans clustering
#' x <- neuraldat[, c('X1', 'X2', 'X3')]
#' grps <- kmeans(x, 6)$center
#' 
#' lekgrps(grps)
lekgrps <- function(grps){
  
  # add split columns, make long form
  grps <- as.data.frame(grps)
  grps$Splits <- factor(1:nrow(grps))
  grps <- tidyr::gather(grps, 'variable', 'value', -ncol(grps))

  p <- ggplot(grps, aes_string(x = 'Splits', y = 'value', fill = 'variable')) +
    geom_bar(stat = 'identity') + 
    theme(legend.title = element_blank()) + 
    scale_y_continuous('Constant values')
  
  return(p)
  
}
