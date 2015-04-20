#' Variable importance using Garson's algorithm
#' 
#' Relative importance of input variables in neural networks using Garson's algorithm
#' 
#' @param mod_in input object for which an organized model list is desired.  The input can be an object of class \code{numeric}, \code{nnet}, \code{mlp}, or \code{nn}
#' @param ... arguments passed to other methods
#' 
#' @details
#' The weights that connect variables in a neural network are partially analogous to parameter coefficients in a standard regression model and can be used to describe relationships between variables. The weights dictate the relative influence of information that is processed in the network such that input variables that are not relevant in their correlation with a response variable are suppressed by the weights. The opposite effect is seen for weights assigned to explanatory variables that have strong positive or negative associations with a response variable. An obvious difference between a neural network and a regression model is that the number of weights is excessive in the former case. This characteristic is advantageous in that it makes neural networks very flexible for modeling non-linear functions with multiple interactions, although interpretation of the effects of specific variables is of course challenging.
#'
#' A method described in Garson 1991 (also see Goh 1995) identifies the relative importance of explanatory variables for a single response variables in a supervised neural network by deconstructing the model weights. The relative importance (or strength of association) of a specific explanatory variable for the response variable can be determined by identifying all weighted connections between the nodes of interest. That is, all weights connecting the specific input node that pass through the hidden layer to the response variable are identified. This is repeated for all other explanatory variables until a list of all weights that are specific to each input variable is obtained. The connections are tallied for each input node and scaled relative to all other inputs. A single value is obtained for each explanatory variable that describes the relationship with the response variable in the model (see the appendix in Goh 1995 for a more detailed description). The original algorithm indicates relative importance as the absolute magnitude from zero to one such the direction of the response cannot be determined.  
#' 
#' Misleading results may be produced if the neural network was created with a skip-layer using \code{skip = TRUE} with the \code{\link[nnet]{nnet}} or \code{\link[caret]{train}} functions.  Garson's algorithm does not describe the effects of skip layer connections on estimates of variable importance.  As such, these values are removed prior to estimating variable importance.  
#' 
#' The algorithm currently only works for neural networks with one hidden layer and one response variable.  
#' 
#' @export
#' 
#' @import ggplot2 neuralnet nnet RSNNS
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object for plotting if \code{bar_plot = FALSE}, otherwise a \code{data.frame} of relative importance values for each input variable.
#' 
#' @references
#' Garson, G.D. 1991. Interpreting neural network connection weights. Artificial Intelligence Expert. 6(4):46-51.
#' 
#' Goh, A.T.C. 1995. Back-propagation neural networks for modeling complex systems. Artificial Intelligence in Engineering. 9(3):143-151.
#' 
#' Olden, J.D., Jackson, D.A. 2002. Illuminating the 'black-box': a randomization approach for understanding variable contributions in artificial neural networks. Ecological Modelling. 154:135-150.
#' 
#' Olden, J.D., Joy, M.K., Death, R.G. 2004. An accurate comparison of methods for quantifying variable importance in artificial neural networks using simulated data. Ecological Modelling. 178:389-397.
#' 
#' @seealso \code{\link{olden}} for a more flexible approach for variable importance
#' 
#' @examples
#' 
#' ## using numeric input
#' 
#' wts_in <- c(13.12, 1.49, 0.16, -0.11, -0.19, -0.16, 0.56, -0.52, 0.81)
#' struct <- c(2, 2, 1) #two inputs, two hidden, one output 
#' 
#' garson(wts_in, struct)
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
#' garson(mod)  
#' 
#' ## using RSNNS, no bias layers
#' 
#' library(RSNNS)
#' 
#' x <- neuraldat[, c('X1', 'X2', 'X3')]
#' y <- neuraldat[, 'Y1']
#' mod <- mlp(x, y, size = 5)
#' 
#' garson(mod)
#' 
#' ## using neuralnet
#' 
#' library(neuralnet)
#' 
#' mod <- neuralnet(Y1 ~ X1 + X2 + X3, data = neuraldat, hidden = 5)
#' 
#' garson(mod)
#' 
#' ## using caret
#' 
#' \dontrun{
#' library(caret)
#' 
#' mod <- train(Y1 ~ X1 + X2 + X3, method = 'nnet', data = neuraldat, linout = TRUE)
#' 
#' garson(mod)
#' 
#' }
garson <- function(mod_in, ...) UseMethod('garson')
 
#' @rdname garson
#'
#' @param bar_plot logical indicating if a \code{ggplot} object is returned (default \code{T}), otherwise numeric values are returned
#' @param struct numeric vector equal in length to the number of layers in the network.  Each number indicates the number of nodes in each layer starting with the input and ending with the output.  An arbitrary number of hidden layers can be included.
#' @param x_lab chr string of alternative names to be used for explanatory variables in the figure, default is taken from \code{mod_in}
#' @param y_lab chr string of alternative names to be used for response variable in the figure, otherwise it is taken from the input model
#' @param wts_only logical passed to \code{\link{neuralweights}}, default \code{FALSE}
#' 
#' @import ggplot2 scales
#' 
#' @export
#' 
#' @method garson numeric
garson.numeric <- function(mod_in, struct, bar_plot = TRUE, x_lab = NULL, y_lab = NULL, wts_only = FALSE, ...){
  
  # get model weights
  best_wts <- neuralweights(mod_in, struct = struct)
  struct <- best_wts$struct
  best_wts <- best_wts$wts
  
  # weights only if TRUE
  if(wts_only) return(best_wts)
  
  #get variable names from mod_in object
  x_names <- paste0(rep('X', struct[1]), seq(1:struct[1]))
  y_names <- paste0(rep('Y', struct[3]), seq(1:struct[3]))
  
  # stop if more than one y output
  if(length(y_names) > 1) 
    stop('Garson only applies to neural networks with one output node')
  
  #change variables names to user sub 
  if(!is.null(x_lab)){
    if(length(x_names) != length(x_lab)) stop('x_lab length not equal to number of input variables')
    else x_names <- x_lab
  }
  if(!is.null(y_lab)){
    y_names <- y_lab
  } 
  
  # organize hidden layer weights for matrix mult
  inp_hid <- best_wts[grep('hidden', names(best_wts))]
  split_vals <- substr(names(inp_hid), 1, 8)
  inp_hid <- split(inp_hid, split_vals)
  inp_hid <- lapply(inp_hid, function(x) t(do.call('rbind', x))[-1, ])
  
  # final layer weights for output
  hid_out <- best_wts[[grep('out 1', names(best_wts))]][-1]
  
  # stop if multiple hidden layers
  max_i <- length(inp_hid)
  if(max_i > 1) stop('Garsons algorithm not applicable for multiple hidden layers')
  
  # use garson's algorithm
  sum_in <- t(inp_hid[[max_i]])
  sum_in <- apply(sum_in, 2, function(x){
  
    abs(x) * abs(hid_out)
    
  })
  sum_in <- sum_in/rowSums(sum_in)
  sum_in <- colSums(sum_in)
  
  # get relative contribution
  rel_imp <- sum_in/sum(sum_in)
  
  if(!bar_plot){
    out <- data.frame(rel_imp)
    row.names(out) <- x_names
    return(out)
  }
  
  to_plo <- data.frame(rel_imp, x_names)[order(rel_imp), , drop = FALSE]
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
#' @export
#' 
#' @method garson nnet
garson.nnet <- function(mod_in, bar_plot = TRUE, x_lab = NULL, y_lab = NULL, wts_only = FALSE, ...){
  
  # get model weights
  best_wts <- neuralweights(mod_in)
  struct <- best_wts$struct
  best_wts <- best_wts$wts
  
  # check for skip layers
  chk <- grepl('skip-layer', capture.output(mod_in))
  if(any(chk))
    stop("Garson's algorithm not applicable for networks with skip layers, use Olden's method")
  
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
  
  # stop if more than one y output
  if(length(y_names) > 1) 
    stop('Garson only applies to neural networks with one output node')
  
  #change variables names to user sub 
  if(!is.null(x_lab)){
    if(length(x_names) != length(x_lab)) stop('x_lab length not equal to number of input variables')
    else x_names <- x_lab
  }
  if(!is.null(y_lab)){
    y_names <- y_lab
  }
  
  # organize hidden layer weights for matrix mult
  inp_hid <- best_wts[grep('hidden', names(best_wts))]
  split_vals <- substr(names(inp_hid), 1, 8)
  inp_hid <- split(inp_hid, split_vals)
  inp_hid <- lapply(inp_hid, function(x) t(do.call('rbind', x))[-1, ])
  
  # final layer weights for output
  hid_out <- best_wts[[grep(paste('out', 1), names(best_wts))]][-1]
  
  # stop if multiple hidden layers
  max_i <- length(inp_hid)
  if(max_i > 1) stop('Garsons algorithm not applicable for multiple hidden layers')
  
  # use garson's algorithm
  sum_in <- t(inp_hid[[max_i]])
  sum_in <- apply(sum_in, 2, function(x){
  
    abs(x) * abs(hid_out)
    
  })
  
  sum_in <- sum_in/rowSums(sum_in)
  sum_in <- colSums(sum_in)
  
  # get relative contribution
  rel_imp <- sum_in/sum(sum_in)
  
  if(!bar_plot){
    out <- data.frame(rel_imp)
    row.names(out) <- x_names
    return(out)
  }
  
  to_plo <- data.frame(rel_imp, x_names)[order(rel_imp), , drop = FALSE]
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
#' @export
#' 
#' @method garson mlp
garson.mlp <- function(mod_in, bar_plot = TRUE, x_lab = NULL, y_lab = NULL, wts_only = FALSE, ...){
  
  # get model weights
  best_wts <- neuralweights(mod_in)
  struct <- best_wts$struct
  best_wts <- best_wts$wts
  
  # weights only if TRUE
  if(wts_only) return(best_wts)
  
  #get variable names from mod_in object
  all_names <- mod_in$snnsObject$getUnitDefinitions()
  x_names <- all_names[grep('Input', all_names$unitName), 'unitName']
  y_names <- all_names[grep('Output', all_names$unitName), 'unitName']

  # stop if more than one y output
  if(length(y_names) > 1) 
    stop('Garson only applies to neural networks with one output node')
  
  #change variables names to user sub 
  if(!is.null(x_lab)){
    if(length(x_names) != length(x_lab)) stop('x_lab length not equal to number of input variables')
    else x_names <- x_lab
  }
  if(!is.null(y_lab)){
    y_names <- y_lab
  }
  
  # organize hidden layer weights for matrix mult
  inp_hid <- best_wts[grep('hidden', names(best_wts))]
  split_vals <- substr(names(inp_hid), 1, 8)
  inp_hid <- split(inp_hid, split_vals)
  inp_hid <- lapply(inp_hid, function(x) t(do.call('rbind', x))[-1, ])
  
  # final layer weights for output
  hid_out <- best_wts[[grep('out 1', names(best_wts))]][-1]
  
  # stop if multiple hidden layers
  max_i <- length(inp_hid)
  if(max_i > 1) stop('Garsons algorithm not applicable for multiple hidden layers')
  
  # use garson's algorithm
  sum_in <- t(inp_hid[[max_i]])
  sum_in <- apply(sum_in, 2, function(x){
  
    abs(x) * abs(hid_out)
    
  })
  sum_in <- sum_in/rowSums(sum_in)
  sum_in <- colSums(sum_in)
  
  # get relative contribution
  rel_imp <- sum_in/sum(sum_in)
  
  if(!bar_plot){
    out <- data.frame(rel_imp)
    row.names(out) <- x_names
    return(out)
  }
  
  to_plo <- data.frame(rel_imp, x_names)[order(rel_imp), , drop = FALSE]
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
#' @export
#'  
#' @method garson nn
garson.nn <- function(mod_in, bar_plot = TRUE, x_lab = NULL, y_lab = NULL, wts_only = FALSE, ...){
  
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

  # stop if more than one y output
  if(length(y_names) > 1) 
    stop('Garson only applies to neural networks with one output node')
  
  #change variables names to user sub 
  if(!is.null(x_lab)){
    if(length(x_names) != length(x_lab)) stop('x_lab length not equal to number of input variables')
    else x_names <- x_lab
  }
  if(!is.null(y_lab)){
    y_names <- y_lab
  } 
  
  # organize hidden layer weights for matrix mult
  inp_hid <- best_wts[grep('hidden', names(best_wts))]
  split_vals <- substr(names(inp_hid), 1, 8)
  inp_hid <- split(inp_hid, split_vals)
  inp_hid <- lapply(inp_hid, function(x) t(do.call('rbind', x))[-1, ])
  
  # final layer weights for output
  hid_out <- best_wts[[grep('out 1', names(best_wts))]][-1]

  # stop if multiple hidden layers
  max_i <- length(inp_hid)
  if(max_i > 1) stop('Garsons algorithm not applicable for multiple hidden layers')
  
  # use garson's algorithm
  sum_in <- t(inp_hid[[max_i]])
  sum_in <- apply(sum_in, 2, function(x){
  
    abs(x) * abs(hid_out)
    
  })
  sum_in <- sum_in/rowSums(sum_in)
  sum_in <- colSums(sum_in)
  
  # get relative contribution
  rel_imp <- sum_in/sum(sum_in)
  
  if(!bar_plot){
    out <- data.frame(rel_imp)
    row.names(out) <- x_names
    return(out)
  }
  
  to_plo <- data.frame(rel_imp, x_names)[order(rel_imp), , drop = FALSE]
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
#' @export
#' 
#' @method garson train
garson.train <- function(mod_in, bar_plot = TRUE, x_lab = NULL, y_lab = NULL, wts_only = FALSE, ...){
  
  y_names <- strsplit(as.character(mod_in$terms[[2]]), ' + ', fixed = TRUE)[[1]]
  mod_in <- mod_in$finalModel
  x_names <- mod_in$xNames
  
  # stop if more than one y output
  if(length(y_names) > 1) 
    stop('Garson only applies to neural networks with one output node')
  
  # get model weights
  best_wts <- neuralweights(mod_in)
  struct <- best_wts$struct
  best_wts <- best_wts$wts
  
  # check for skip layers
  chk <- grepl('skip-layer', capture.output(mod_in))
  if(any(chk))
    stop("Garson's algorithm not applicable for networks with skip layers, use Olden's method")
  
  # weights only if TRUE
  if(wts_only) return(best_wts)
  
  #change variables names to user sub 
  if(!is.null(x_lab)){
    if(length(x_names) != length(x_lab)) stop('x_lab length not equal to number of input variables')
    else x_names <- x_lab
  }
  if(!is.null(y_lab)){
    y_names <- y_lab
  }
  
  # organize hidden layer weights for matrix mult
  inp_hid <- best_wts[grep('hidden', names(best_wts))]
  split_vals <- substr(names(inp_hid), 1, 8)
  inp_hid <- split(inp_hid, split_vals)
  inp_hid <- lapply(inp_hid, function(x) t(do.call('rbind', x))[-1, ])
  
  # final layer weights for output
  hid_out <- best_wts[[grep('out 1', names(best_wts))]][-1]
  
  # stop if multiple hidden layers
  max_i <- length(inp_hid)
  if(max_i > 1) stop('Garsons algorithm not applicable for multiple hidden layers')
  
  # use garson's algorithm
  sum_in <- t(inp_hid[[max_i]])
  sum_in <- apply(sum_in, 2, function(x){
  
    abs(x) * abs(hid_out)
    
  })
  sum_in <- sum_in/rowSums(sum_in)
  sum_in <- colSums(sum_in)
  
  # get relative contribution
  rel_imp <- sum_in/sum(sum_in)
  
  if(!bar_plot){
    out <- data.frame(rel_imp)
    row.names(out) <- x_names
    return(out)
  }
  
  to_plo <- data.frame(rel_imp, x_names)[order(rel_imp), , drop = FALSE]
  to_plo$x_names <- factor(x_names[order(rel_imp)], levels = x_names[order(rel_imp)])
  out_plo <- ggplot2::ggplot(to_plo, aes(x = x_names, y = rel_imp, fill = rel_imp,
                                         colour = rel_imp)) + 
    geom_bar(stat = 'identity') + 
    scale_x_discrete(element_blank()) +
    scale_y_continuous(y_names)
  
  return(out_plo)
  
}

