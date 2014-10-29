#' Variable importance for neural networks
#' 
#' Variable importance for neural networks using a modification of Garson's algorithm
#' 
#' @param mod_in input object for which an organized model list is desired.  The input can be an object of class \code{numeric}, \code{nnet}, \code{mlp}, or \code{nn}
#' @param out_var chr string indicating the response variable in the neural network object to be evaluated.  Only one input is allowed for models with more than one response.  Names must be of the form \code{'Y1'}, \code{'Y2'}, etc. if using numeric values as weight inputs for \code{mod_in}.
#' @param ... arguments passed to other methods
#' 
#' @details
#' The weights that connect variables in a neural network are partially analogous to parameter coefficients in a standard regression model and can be used to describe relationships between variables. The weights dictate the relative influence of information that is processed in the network such that input variables that are not relevant in their correlation with a response variable are suppressed by the weights. The opposite effect is seen for weights assigned to explanatory variables that have strong, positive associations with a response variable. An obvious difference between a neural network and a regression model is that the number of weights is excessive in the former case. This characteristic is advantageous in that it makes neural networks very flexible for modeling non-linear functions with multiple interactions, although interpretation of the effects of specific variables is of course challenging.
#'
#' A method described in Garson 1991 (also see Goh 1995) identifies the relative importance of explanatory variables for specific response variables in a supervised neural network by deconstructing the model weights. The basic idea is that the relative importance (or strength of association) of a specific explanatory variable for a specific response variable can be determined by identifying all weighted connections between the nodes of interest. That is, all weights connecting the specific input node that pass through the hidden layer to the specific response variable are identified. This is repeated for all other explanatory variables until the analyst has a list of all weights that are specific to each input variable. The connections are tallied for each input node and scaled relative to all other inputs. A single value is obtained for each explanatory variable that describes the relationship with response variable in the model (see the appendix in Goh 1995 for a more detailed description). The original algorithm presented in Garson 1991 indicated relative importance as the absolute magnitude from zero to one such the direction of the response could not be determined. This function modifies the original method to preserve the sign, such that relative importance values for input variables can be ranked continuous from -1 (negative relation) to 1 (positive relation).
#' 
#' @export garson neuralnet nnet mlp ggplot aes geom_bar scale_x_discrete scale_y_continuous
#' 
#' @import ggplot2 neuralnet nnet RSNNS
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object for plotting if \code{bar_plot = F}, otherwise a \code{data.frame} of relative importance values for each input variable.
#' 
#' @references
#' Garson, G.D. 1991. Interpreting neural network connection weights. Artificial Intelligence Expert. 6(4):46-51.
#' 
#' Goh, A.T.C. 1995. Back-propagation neural networks for modeling complex systems. Artificial Intelligence in Engineering. 9(3):143-151.
#' 
#' Olden, J.D., Jackson, D.A. 2002. Illuminating the 'black-box': a randomization approach for understanding variable contributions in artificial neural networks. Ecological Modelling. 154:135-150.
#' 
#' @examples
#' 
#' ## using numeric input
#' 
#' wts_in <- c(13.12, 1.49, 0.16, -0.11, -0.19, -0.16, 0.56, -0.52, 0.81)
#' struct <- c(2, 2, 1) #two inputs, two hidden, one output 
#' 
#' garson(wts_in, 'Y1', struct)
#' 
#' ## using nnet
#' 
#' library(nnet)
#' 
#' data(neuraldat) 
#' set.seed(123)
#' 
#' mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 10)
#'  
#' garson(mod, 'Y1')  
#' 
#' ## using RSNNS, no bias layers
#' 
#' library(RSNNS)
#' 
#' x <- neuraldat[, c('X1', 'X2', 'X3')]
#' y <- neuraldat[, 'Y1']
#' mod <- mlp(x, y, size = 10)
#' 
#' garson(mod, 'Y1')
#' 
#' ## using neuralnet
#' 
#' library(neuralnet)
#' 
#' mod <- neuralnet(Y1 ~ X1 + X2 + X3, data = neuraldat, hidden = 10)
#' 
#' garson(mod, 'Y1')
garson <- function(mod_in, out_var, ...) UseMethod('garson')
 
#' @rdname garson
#'
#' @param bar_plot logical indicating if a \code{ggplot} object is returned (default \code{T}), otherwise numeric values are returned
#' @param struct numeric vector equal in length to the number of layers in the network.  Each number indicates the number of nodes in each layer starting with the input and ending with the output.  An arbitrary number of hidden layers can be included.
#' @param x_lab chr string of alternative names to be used for explanatory variables in the figure, default is taken from \code{mod_in}
#' @param y_lab chr string of alternative names to be used for response variable in the figure, default is taken from \code{out_var}
#' @param wts_only logical passed to \code{\link{neuralweights}}, default \code{F}
#' 
#' @import ggplot2 scales
#' 
#' @export garson.numeric
#' 
#' @method garson numeric
garson.numeric <- function(mod_in, out_var, struct, bar_plot = T, x_lab = NULL, y_lab = NULL, wts_only = F){
  
  # get model weights
  best_wts <- neuralweights(mod_in, struct = struct)
  
  # weights only if T
  if(wts_only) return(best_wts)
  
  #get variable names from mod_in object
  #change to user input if supplied
  x_names <- paste0(rep('X', struct[1]), seq(1:struct[1]))
  y_names <- paste0(rep('Y', struct[3]), seq(1:struct[3]))
  
  # get index value for response variable to measure
  out_ind <- as.numeric(gsub('^[A-Z]', '', out_var))

  #change variables names to user sub 
  if(!is.null(x_lab)){
    if(length(x_names) != length(x_lab)) stop('x_lab length not equal to number of input variables')
    else x_names <- x_lab
  }
  if(!is.null(y_lab)){
    y_names <- y_lab
  } else {
    y_names <- y_names[grep(out_var, y_names)]
  }
  
  # organize hidden layer weights for matrix mult
  inp_hid <- best_wts[grep('hidden', names(best_wts))]
  split_vals <- substr(names(inp_hid), 1, 8)
  inp_hid <- split(inp_hid, split_vals)
  inp_hid <- lapply(inp_hid, function(x) t(do.call('rbind', x))[-1, ])
  
  # final layer weights for output
  hid_out <- best_wts[[grep(paste('out', out_ind), names(best_wts))]][-1]
  
  # matrix multiplication of output layer with connecting hidden layer
  max_i <- length(inp_hid)
  sum_in <- as.matrix(inp_hid[[max_i]]) %*% matrix(hid_out)
  
  # recursive matrix multiplication for all remaining hidden layers
  # only for multiple hidden layers
  if(max_i != 1){ 
    
    for(i in (max_i - 1):1) sum_in <- as.matrix(inp_hid[[i]]) %*% sum_in
    
    # final contribution vector for all inputs
    inp_cont <- sum_in    
    
  } else {
    
    inp_cont <- sum_in
    
  }
  
  #get relative contribution
  #inp_cont/sum(inp_cont)
  rel_imp <- { inp_cont
#     signs <- sign(inp_cont)
#     signs*scales::rescale(abs(inp_cont), c(0, 1))
  }
  
  if(!bar_plot){
    out <- data.frame(rel_imp)
    row.names(out) <- x_names
    return(out)
  }
  
  to_plo <- data.frame(rel_imp, x_names)[order(rel_imp), , drop = F]
  to_plo$x_names <- factor(x_names[order(rel_imp)], levels = x_names[order(rel_imp)])
  out_plo <- ggplot2::ggplot(to_plo, aes(x = x_names, y = rel_imp, fill = rel_imp,
                                colour = rel_imp)) + 
    geom_bar(stat = 'identity') + 
    scale_x_discrete(element_blank()) +
    scale_y_continuous(y_names)
  
  return(out_plo)
  
}

#' @rdname garson
#' 
#' @import ggplot2 scales
#' 
#' @export garson.nnet
#' 
#' @method garson nnet
garson.nnet <- function(mod_in, out_var, bar_plot = T, x_lab = NULL, y_lab = NULL, wts_only = F){
  
  # get model weights
  best_wts <- neuralweights(mod_in)
  
  # weights only if T
  if(wts_only) return(best_wts)
  
  # get variable names from mod_in object
  # separate methdos if nnet called with formula 
  if(is.null(mod_in$call$formula)){
    x_names <- colnames(eval(mod_in$call$x))
    y_names <- colnames(eval(mod_in$call$y))
  }
  else{
    forms <- eval(mod_in$call$formula)
    x_names <- mod_in$coefnames
    facts <- attr(terms(mod_in), 'factors')
    y_check <- mod_in$fitted
    if(ncol(y_check)>1) y_names <- colnames(y_check)
    else y_names <- as.character(forms)[2]
  } 
  
  # get index value for response variable to measure
  out_ind <- grep(out_var, y_names)
  
  #change variables names to user sub 
  if(!is.null(x_lab)){
    if(length(x_names) != length(x_lab)) stop('x_lab length not equal to number of input variables')
    else x_names <- x_lab
  }
  if(!is.null(y_lab)){
    y_names <- y_lab
  } else {
    y_names <- y_names[grep(out_var, y_names)]
  }
  
  # organize hidden layer weights for matrix mult
  inp_hid <- best_wts[grep('hidden', names(best_wts))]
  split_vals <- substr(names(inp_hid), 1, 8)
  inp_hid <- split(inp_hid, split_vals)
  inp_hid <- lapply(inp_hid, function(x) t(do.call('rbind', x))[-1, ])
  
  # final layer weights for output
  hid_out <- best_wts[[grep(paste('out', out_ind), names(best_wts))]][-1]
  
  # matrix multiplication of output layer with connecting hidden layer
  max_i <- length(inp_hid)
  sum_in <- as.matrix(inp_hid[[max_i]]) %*% matrix(hid_out)
  
  # recursive matrix multiplication for all remaining hidden layers
  # only for multiple hidden layers
  if(max_i != 1){ 
    
    for(i in (max_i - 1):1) sum_in <- as.matrix(inp_hid[[i]]) %*% sum_in
    
    # final contribution vector for all inputs
    inp_cont <- sum_in    
    
  } else {
    
    inp_cont <- sum_in
    
  }
  
  #get relative contribution
  #inp_cont/sum(inp_cont)
  rel_imp <- { inp_cont
#     signs <- sign(inp_cont)
#     signs*scales::rescale(abs(inp_cont), c(0, 1))
  }
  
  if(!bar_plot){
    out <- data.frame(rel_imp)
    row.names(out) <- x_names
    return(out)
  }
  
  to_plo <- data.frame(rel_imp, x_names)[order(rel_imp), , drop = F]
  to_plo$x_names <- factor(x_names[order(rel_imp)], levels = x_names[order(rel_imp)])
  out_plo <- ggplot2::ggplot(to_plo, aes(x = x_names, y = rel_imp, fill = rel_imp,
                                colour = rel_imp)) + 
    geom_bar(stat = 'identity') + 
    scale_x_discrete(element_blank()) +
    scale_y_continuous(y_names)
  
  return(out_plo)
  
}

#' @rdname garson
#'
#' @import ggplot2 scales
#' 
#' @export garson.mlp
#' 
#' @method garson mlp
garson.mlp <- function(mod_in, out_var, bar_plot = T, x_lab = NULL, y_lab = NULL, wts_only = F){
  
  # exception of train class for mlp
  if('train' %in% class(mod_in)){
    if('nnet' %in% class(mod_in$finalModel)){
      mod_in <- mod_in$finalModel
      warning('Using best nnet model from train output')
    }
    else stop('Only nnet method can be used with train object')
  }
  
  # get model weights
  best_wts <- neuralweights(mod_in, rel_rsc = 5)
  
  # weights only if T
  if(wts_only) return(best_wts)
  
  #get variable names from mod_in object
  #change to user input if supplied
  all_names <- mod_in$snnsObject$getUnitDefinitions()
  x_names <- all_names[grep('Input', all_names$unitName), 'unitName']
  y_names <- all_names[grep('Output', all_names$unitName), 'unitName']

  # get index value for response variable to measure
  out_ind <- grep(out_var, y_names)
  
  #change variables names to user sub 
  if(!is.null(x_lab)){
    if(length(x_names) != length(x_lab)) stop('x_lab length not equal to number of input variables')
    else x_names <- x_lab
  }
  if(!is.null(y_lab)){
    y_names <- y_lab
  } else {
    y_names <- y_names[grep(out_var, y_names)]
  }
  
  # organize hidden layer weights for matrix mult
  inp_hid <- best_wts[grep('hidden', names(best_wts))]
  split_vals <- substr(names(inp_hid), 1, 8)
  inp_hid <- split(inp_hid, split_vals)
  inp_hid <- lapply(inp_hid, function(x) t(do.call('rbind', x))[-1, ])
  
  # final layer weights for output
  hid_out <- best_wts[[grep(paste('out', out_ind), names(best_wts))]][-1]
  
  # matrix multiplication of output layer with connecting hidden layer
  max_i <- length(inp_hid)
  sum_in <- as.matrix(inp_hid[[max_i]]) %*% matrix(hid_out)
  
  # recursive matrix multiplication for all remaining hidden layers
  # only for multiple hidden layers
  if(max_i != 1){ 
    
    for(i in (max_i - 1):1) sum_in <- as.matrix(inp_hid[[i]]) %*% sum_in
    
    # final contribution vector for all inputs
    inp_cont <- sum_in    
    
  } else {
    
    inp_cont <- sum_in
    
  }
  
  #get relative contribution
  #inp_cont/sum(inp_cont)
  rel_imp <- { inp_cont
#     signs <- sign(inp_cont)
#     signs*scales::rescale(abs(inp_cont), c(0, 1))
  }
  
  if(!bar_plot){
    out <- data.frame(rel_imp)
    row.names(out) <- x_names
    return(out)
  }
  
  to_plo <- data.frame(rel_imp, x_names)[order(rel_imp), , drop = F]
  to_plo$x_names <- factor(x_names[order(rel_imp)], levels = x_names[order(rel_imp)])
  out_plo <- ggplot2::ggplot(to_plo, aes(x = x_names, y = rel_imp, fill = rel_imp,
                                colour = rel_imp)) + 
    geom_bar(stat = 'identity') + 
    scale_x_discrete(element_blank()) +
    scale_y_continuous(y_names)
  
  return(out_plo)
  
}

#' @rdname garson
#' 
#' @import ggplot2 scales
#' 
#' @export garson.nn
#'  
#' @method garson nn
garson.nn <- function(mod_in, out_var, bar_plot = T, x_lab = NULL, y_lab = NULL, wts_only = F){
  
  # get model weights
  best_wts <- neuralweights(mod_in, rel_rsc = 5)
  
  # weights only if T
  if(wts_only) return(best_wts)
  
  #get variable names from mod_in object
  #change to user input if supplied
  x_names <- mod_in$model.list$variables
  y_names <- mod_in$model.list$response

  # get index value for response variable to measure
  out_ind <- grep(out_var, y_names)
  
  #change variables names to user sub 
  if(!is.null(x_lab)){
    if(length(x_names) != length(x_lab)) stop('x_lab length not equal to number of input variables')
    else x_names <- x_lab
  }
  if(!is.null(y_lab)){
    y_names <- y_lab
  } else {
    y_names <- y_names[grep(out_var, y_names)]
  }
  
  # organize hidden layer weights for matrix mult
  inp_hid <- best_wts[grep('hidden', names(best_wts))]
  split_vals <- substr(names(inp_hid), 1, 8)
  inp_hid <- split(inp_hid, split_vals)
  inp_hid <- lapply(inp_hid, function(x) t(do.call('rbind', x))[-1, ])
  
  # final layer weights for output
  hid_out <- best_wts[[grep(paste('out', out_ind), names(best_wts))]][-1]
  
  # matrix multiplication of output layer with connecting hidden layer
  max_i <- length(inp_hid)
  sum_in <- as.matrix(inp_hid[[max_i]]) %*% matrix(hid_out)
  
  # recursive matrix multiplication for all remaining hidden layers
  # only for multiple hidden layers
  if(max_i != 1){ 
    
    for(i in (max_i - 1):1) sum_in <- as.matrix(inp_hid[[i]]) %*% sum_in
    
    # final contribution vector for all inputs
    inp_cont <- sum_in    
    
  } else {
    
    inp_cont <- sum_in
    
  }
  
  #get relative contribution
  #inp_cont/sum(inp_cont)
  rel_imp <- { inp_cont
#     signs <- sign(inp_cont)
#     signs*scales::rescale(abs(inp_cont), c(0, 1))
  }
  
  if(!bar_plot){
    out <- data.frame(rel_imp)
    row.names(out) <- x_names
    return(out)
  }
  
  to_plo <- data.frame(rel_imp, x_names)[order(rel_imp), , drop = F]
  to_plo$x_names <- factor(x_names[order(rel_imp)], levels = x_names[order(rel_imp)])
  out_plo <- ggplot2::ggplot(to_plo, aes(x = x_names, y = rel_imp, fill = rel_imp,
                                colour = rel_imp)) + 
    geom_bar(stat = 'identity') + 
    scale_x_discrete(element_blank()) +
    scale_y_continuous(y_names)
  
  return(out_plo)
  
}
