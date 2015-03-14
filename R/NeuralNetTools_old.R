#' Variable importance using connection weights
#' 
#' Relative importance of input variables in neural networks as the sum of the product of raw input-hidden, hidden-output connection weights, proposed by Olden et al. 2004. 
#' 
#' @param mod_in input object for which an organized model list is desired.  The input can be an object of class \code{numeric}, \code{nnet}, \code{mlp}, or \code{nn}
#' @param out_var chr string indicating the response variable in the neural network object to be evaluated.  Only one input is allowed for models with more than one response.  Names must be of the form \code{'Y1'}, \code{'Y2'}, etc. if using numeric values as weight inputs for \code{mod_in}.
#' @param ... arguments passed to other methods
#' 
#' @details
#' This method is similar to Garson's algorithm (Garson 1991, modified by Goh 1995) in that the connection weights between layers of a neural network form the basis for determining variabile importance.  However, Olden et al. 2004 describe a connection weights algorithm that consistently out-performed Garson's algorithm in representing the true variable importance in simulated datasets.  This `Olden' method calculates variable importance as the product of the raw input-hidden and hidden-output connection weights between each input and output neuron and sums the product across all hidden neurons.  An advantage of this approach is the relative contributions of each connection weight are maintained as compared to Garson's algorithm which only considers the absolute magnitude. For example, connection weights that change sign (e.g., positive to negative) between the input-hidden to hidden-output layers would have a cancelling effect whereas Garson's algorithm may provide misleading results based on the absolute magnitude.  
#' 
#' @export
#' 
#' @import ggplot2 neuralnet nnet RSNNS
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object for plotting if \code{bar_plot = FALSE}, otherwise a \code{data.frame} of relative importance values for each input variable.
#' 
#' @references
#' 
#' Garson, G.D. 1991. Interpreting neural network connection weights. Artificial Intelligence Expert. 6(4):46-51.
#' 
#' Goh, A.T.C. 1995. Back-propagation neural networks for modeling complex systems. Artificial Intelligence in Engineering. 9(3):143-151.
#' 
#' Olden, J.D., Jackson, D.A. 2002. Illuminating the 'black-box': a randomization approach for understanding variable contributions in artificial neural networks. Ecological Modelling. 154:135-150.
#' 
#' Olden, J.D., Joy, M.K., Death, R.G. 2004. An accurate comparison of methods for quantifying variable importance in artificial neural networks using simulated data. Ecological Modelling. 178:389-397.
#' 
#' @examples
#' 
#' ## using numeric input
#' 
#' wts_in <- c(13.12, 1.49, 0.16, -0.11, -0.19, -0.16, 0.56, -0.52, 0.81)
#' struct <- c(2, 2, 1) #two inputs, two hidden, one output 
#' 
#' olden(wts_in, 'Y1', struct)
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
#' olden(mod, 'Y1')  
#' 
#' ## using RSNNS, no bias layers
#' 
#' library(RSNNS)
#' 
#' x <- neuraldat[, c('X1', 'X2', 'X3')]
#' y <- neuraldat[, 'Y1']
#' mod <- mlp(x, y, size = 5)
#' 
#' olden(mod, 'Y1')
#' 
#' ## using neuralnet
#' 
#' library(neuralnet)
#' 
#' mod <- neuralnet(Y1 ~ X1 + X2 + X3, data = neuraldat, hidden = 5)
#' 
#' olden(mod, 'Y1')
#' 
#' ## using caret
#' 
#' \dontrun{
#' library(caret)
#' 
#' mod <- train(Y1 ~ X1 + X2 + X3, method = 'nnet', data = neuraldat, linout = TRUE)
#' 
#' olden(mod, 'Y1')
#' 
#' }
olden <- function(mod_in, out_var, ...) UseMethod('olden')
 
#' @rdname olden
#'
#' @param bar_plot logical indicating if a \code{ggplot} object is returned (default \code{T}), otherwise numeric values are returned
#' @param struct numeric vector equal in length to the number of layers in the network.  Each number indicates the number of nodes in each layer starting with the input and ending with the output.  An arbitrary number of hidden layers can be included.
#' @param x_lab chr string of alternative names to be used for explanatory variables in the figure, default is taken from \code{mod_in}
#' @param y_lab chr string of alternative names to be used for response variable in the figure, default is taken from \code{out_var}
#' @param wts_only logical passed to \code{\link{neuralweights}}, default \code{FALSE}
#' 
#' @import ggplot2 scales
#' 
#' @export
#' 
#' @method olden numeric
olden.numeric <- function(mod_in, out_var, struct, bar_plot = TRUE, x_lab = NULL, y_lab = NULL, wts_only = FALSE, ...){
  
  # get model weights
  best_wts <- neuralweights(mod_in, struct = struct)
  struct <- best_wts$struct
  best_wts <- best_wts$wts
  
  # weights only if TRUE
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
  sum_in <- inp_hid[[max_i]] %*% matrix(hid_out)
  
  # recursive matrix multiplication for all remaining hidden layers
  # only for multiple hidden layers
  if(max_i != 1){ 
    
    for(i in (max_i - 1):1) sum_in <- as.matrix(inp_hid[[i]]) %*% sum_in
    
    # final contribution vector for all inputs
    importance <- sum_in    
    
  } else {
    
    importance <- sum_in
    
  }
  
  if(!bar_plot){
    out <- data.frame(importance)
    row.names(out) <- x_names
    return(out)
  }
  
  to_plo <- data.frame(importance, x_names)[order(importance), , drop = FALSE]
  to_plo$x_names <- factor(x_names[order(importance)], levels = x_names[order(importance)])
  out_plo <- ggplot2::ggplot(to_plo, aes(x = x_names, y = importance, fill = importance,
                                colour = importance)) + 
    geom_bar(stat = 'identity') + 
    scale_x_discrete(element_blank()) +
    scale_y_continuous(y_names)
  
  return(out_plo)
  
}

#' @rdname olden
#' 
#' @import ggplot2 scales
#' 
#' @export
#' 
#' @method olden nnet
olden.nnet <- function(mod_in, out_var, bar_plot = TRUE, x_lab = NULL, y_lab = NULL, wts_only = FALSE, ...){
  
  # get model weights
  best_wts <- neuralweights(mod_in)
  struct <- best_wts$struct
  best_wts <- best_wts$wts
  
  # check for skip layers
  chk <- grepl('skip-layer', capture.output(mod_in))
  if(any(chk))
    warning('Skip layer used, results may be inaccurate because input and output connections are removed')
  
  # weights only if TRUE
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
  sum_in <- inp_hid[[max_i]] %*% matrix(hid_out)
  
  # recursive matrix multiplication for all remaining hidden layers
  # only for multiple hidden layers
  if(max_i != 1){ 
    
    for(i in (max_i - 1):1) sum_in <- as.matrix(inp_hid[[i]]) %*% sum_in
    
    # final contribution vector for all inputs
    importance <- sum_in    
    
  } else {
    
    importance <- sum_in
    
  }
  
  if(!bar_plot){
    out <- data.frame(importance)
    row.names(out) <- x_names
    return(out)
  }
  
  to_plo <- data.frame(importance, x_names)[order(importance), , drop = FALSE]
  to_plo$x_names <- factor(x_names[order(importance)], levels = x_names[order(importance)])
  out_plo <- ggplot2::ggplot(to_plo, aes(x = x_names, y = importance, fill = importance,
                                colour = importance)) + 
    geom_bar(stat = 'identity') + 
    scale_x_discrete(element_blank()) +
    scale_y_continuous(y_names)
  
  return(out_plo)
  
}

#' @rdname olden
#'
#' @import ggplot2 scales
#' 
#' @export
#' 
#' @method olden mlp
olden.mlp <- function(mod_in, out_var, bar_plot = TRUE, x_lab = NULL, y_lab = NULL, wts_only = FALSE, ...){
  
  # get model weights
  best_wts <- neuralweights(mod_in)
  struct <- best_wts$struct
  best_wts <- best_wts$wts
  
  # weights only if TRUE
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
  sum_in <- inp_hid[[max_i]] %*% matrix(hid_out)
  
  # recursive matrix multiplication for all remaining hidden layers
  # only for multiple hidden layers
  if(max_i != 1){ 
    
    for(i in (max_i - 1):1) sum_in <- as.matrix(inp_hid[[i]]) %*% sum_in
    
    # final contribution vector for all inputs
    importance <- sum_in    
    
  } else {
    
    importance <- sum_in
    
  }
  
  if(!bar_plot){
    out <- data.frame(importance)
    row.names(out) <- x_names
    return(out)
  }
  
  to_plo <- data.frame(importance, x_names)[order(importance), , drop = FALSE]
  to_plo$x_names <- factor(x_names[order(importance)], levels = x_names[order(importance)])
  out_plo <- ggplot2::ggplot(to_plo, aes(x = x_names, y = importance, fill = importance,
                                colour = importance)) + 
    geom_bar(stat = 'identity') + 
    scale_x_discrete(element_blank()) +
    scale_y_continuous(y_names)
  
  return(out_plo)
  
}

#' @rdname olden
#' 
#' @import ggplot2 scales
#' 
#' @export
#'  
#' @method olden nn
olden.nn <- function(mod_in, out_var, bar_plot = TRUE, x_lab = NULL, y_lab = NULL, wts_only = FALSE, ...){
  
  # get model weights
  best_wts <- neuralweights(mod_in)
  struct <- best_wts$struct
  best_wts <- best_wts$wts
  
  # weights only if TRUE
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
  sum_in <- inp_hid[[max_i]] %*% matrix(hid_out)
  
  # recursive matrix multiplication for all remaining hidden layers
  # only for multiple hidden layers
  if(max_i != 1){ 
    
    for(i in (max_i - 1):1) sum_in <- as.matrix(inp_hid[[i]]) %*% sum_in
    
    # final contribution vector for all inputs
    importance <- sum_in    
    
  } else {
    
    importance <- sum_in
    
  }
  
  if(!bar_plot){
    out <- data.frame(importance)
    row.names(out) <- x_names
    return(out)
  }
  
  to_plo <- data.frame(importance, x_names)[order(importance), , drop = FALSE]
  to_plo$x_names <- factor(x_names[order(importance)], levels = x_names[order(importance)])
  out_plo <- ggplot2::ggplot(to_plo, aes(x = x_names, y = importance, fill = importance,
                                colour = importance)) + 
    geom_bar(stat = 'identity') + 
    scale_x_discrete(element_blank()) +
    scale_y_continuous(y_names)
  
  return(out_plo)
  
}

#' @rdname olden
#' 
#' @import ggplot2 scales
#' 
#' @export
#' 
#' @method olden train
olden.train <- function(mod_in, out_var, bar_plot = TRUE, x_lab = NULL, y_lab = NULL, wts_only = FALSE, ...){
  
  y_names <- strsplit(as.character(mod_in$terms[[2]]), ' + ', fixed = TRUE)[[1]]
  mod_in <- mod_in$finalModel
  x_names <- mod_in$xNames
  
  # get model weights
  best_wts <- neuralweights(mod_in)
  struct <- best_wts$struct
  best_wts <- best_wts$wts
  
  # check for skip layers
  chk <- grepl('skip-layer', capture.output(mod_in))
  if(any(chk))
    warning('Skip layer used, results may be inaccurate because input and output connections are removed')

  # weights only if TRUE
  if(wts_only) return(best_wts)
  
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
  sum_in <- inp_hid[[max_i]] %*% matrix(hid_out)
  
  # recursive matrix multiplication for all remaining hidden layers
  # only for multiple hidden layers
  if(max_i != 1){ 
    
    for(i in (max_i - 1):1) sum_in <- as.matrix(inp_hid[[i]]) %*% sum_in
    
    # final contribution vector for all inputs
    importance <- sum_in    
    
  } else {
    
    importance <- sum_in
    
  }
  
  if(!bar_plot){
    out <- data.frame(importance)
    # row.names(out) <- x_names
    return(out)
  }
  
  to_plo <- data.frame(importance, x_names)[order(importance), , drop = FALSE]
  to_plo$x_names <- factor(x_names[order(importance)], levels = x_names[order(importance)])
  out_plo <- ggplot2::ggplot(to_plo, aes(x = x_names, y = importance, fill = importance,
                                         colour = importance)) + 
    geom_bar(stat = 'identity') + 
    scale_x_discrete(element_blank()) +
    scale_y_continuous(y_names)
  
  return(out_plo)
  
}
