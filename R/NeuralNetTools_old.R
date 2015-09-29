#' Variable importance using connection weights
#' 
#' Relative importance of input variables in neural networks as the sum of the product of raw input-hidden, hidden-output connection weights, proposed by Olden et al. 2004. 
#' 
#' @param mod_in input model object or a list of model weights as returned from \code{\link{neuralweights}} if using the default method
#' @param x_names chr string of input variable names, obtained from the model object
#' @param y_names chr string of response variable names, obtained from the model object
#' @param out_var chr string indicating the response variable in the neural network object to be evaluated.  Only one input is allowed for models with more than one response.  Names must be of the form \code{'Y1'}, \code{'Y2'}, etc. if using numeric values as weight inputs for \code{mod_in}.
#' @param bar_plot logical indicating if a \code{ggplot} object is returned (default \code{T}), otherwise numeric values are returned
#' @param x_lab chr string of alternative names to be used for explanatory variables in the figure, default is taken from \code{mod_in}
#' @param y_lab chr string of alternative names to be used for response variable in the figure, default is taken from \code{out_var}
#' @param skip_wts vector from \code{\link{neuralskips}} for \code{\link[nnet]{nnet}} models with skip-layer connections 
#' @param ... arguments passed to or from other methods
#' 
#' @details
#' This method is similar to Garson's algorithm (Garson 1991, modified by Goh 1995) in that the connection weights between layers of a neural network form the basis for determining variable importance.  However, Olden et al. 2004 describe a connection weights algorithm that consistently out-performed Garson's algorithm in representing the true variable importance in simulated datasets.  This `Olden' method calculates variable importance as the product of the raw input-hidden and hidden-output connection weights between each input and output neuron and sums the product across all hidden neurons.  An advantage of this approach is the relative contributions of each connection weight are maintained in terms of both magnitude and sign as compared to Garson's algorithm which only considers the absolute magnitude. For example, connection weights that change sign (e.g., positive to negative) between the input-hidden to hidden-output layers would have a cancelling effect whereas Garson's algorithm may provide misleading results based on the absolute magnitude.  An additional advantage is that Olden's algorithm is capable of evaluating neural networks with multiple hidden layers wheras Garson's was developed for networks with a single hidden layer.   
#' 
#' The importance values assigned to each variable are in units that are based directly on the summed product of the connection weights.  The actual values should only be interpreted based on relative sign and magnitude between explanatory variables.  Comparisons between different models should not be made.
#' 
#' The Olden function also works with networks that have skip layers by adding the input-output connection weights to the final summed product of all input-hidden and hidden-output connections.  This was not described in the original method so interpret with caution. 
#' 
#' @export
#' 
#' @import ggplot2
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
#' olden(wts_in, struct)
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
#' olden(mod)  
#' 
#' ## View the difference for a model w/ skip layers
#' 
#' set.seed(123)
#' 
#' mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5, skip = TRUE)
#' 
#' olden(mod)
#' 
#' ## using RSNNS, no bias layers
#' 
#' library(RSNNS)
#' 
#' x <- neuraldat[, c('X1', 'X2', 'X3')]
#' y <- neuraldat[, 'Y1']
#' mod <- mlp(x, y, size = 5)
#' 
#' olden(mod)
#' 
#' ## using neuralnet
#' 
#' library(neuralnet)
#' 
#' mod <- neuralnet(Y1 ~ X1 + X2 + X3, data = neuraldat, hidden = 5)
#' 
#' olden(mod)
#' 
#' ## using caret
#' 
#' \dontrun{
#' library(caret)
#'
#' mod <- train(Y1 ~ X1 + X2 + X3, method = 'nnet', data = neuraldat, linout = TRUE)
#' 
#' olden(mod)
#' 
#' }
#' 
#' ## multiple hidden layers
#'
#' x <- neuraldat[, c('X1', 'X2', 'X3')]
#' y <- neuraldat[, 'Y1']
#' mod <- mlp(x, y, size = c(5, 7, 6), linOut = TRUE)
#' 
#' olden(mod)
olden <- function(mod_in, ...) UseMethod('olden')

#' @rdname olden
#'
#' @export
#' 
#' @method olden default
olden.default <- function(mod_in, x_names, y_names, out_var = NULL, bar_plot = TRUE, x_lab = NULL, y_lab = NULL, skip_wts = NULL, ...){
  
  # get index value for response variable to measure
  if(is.null(out_var)) out_var <- y_names[1]
  
  # stop if out_var is not a named variable
  if(!out_var %in% y_names) stop(paste('out_var must match one:', paste(y_names, collapse = ', ')))
  else out_ind <- grep(out_var, y_names)
  
  #change variables names to user sub 
  if(!is.null(x_lab)){
    if(length(x_names) != length(x_lab)) stop('x_lab length not equal to number of input variables')
    else x_names <- x_lab
  }
  if(!is.null(y_lab)){
    y_names <- y_lab
  } else {
    y_names <- 'Importance' 
  }
  
  # the default method works with weight list
  wts_in <- mod_in
  
  # organize hidden layer weights for matrix mult
  inp_hid <- wts_in[grep('hidden', names(wts_in))]
  split_vals <- substr(names(inp_hid), 1, 8)
  inp_hid <- split(inp_hid, split_vals)
  inp_hid <- lapply(inp_hid, function(x) t(do.call('rbind', x))[-1, ])
  
  # final layer weights for output
  hid_out <- wts_in[[grep(paste('out', out_ind), names(wts_in))]][-1]
  
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
  
  # add skip_wts
  if(!is.null(skip_wts)) sum_in <- unlist(skip_wts) + sum_in
  
  if(!bar_plot){
    out <- data.frame(importance)
    row.names(out) <- x_names
    return(out)
  }

  to_plo <- data.frame(importance, x_names)[order(importance), , drop = FALSE]
  to_plo$x_names <- factor(x_names[order(importance)], levels = x_names[order(importance)])
  out_plo <- ggplot2::ggplot(to_plo, aes(x = x_names, y = importance, fill = importance,
                                colour = importance)) + 
    geom_bar(stat = 'identity', position = 'identity') + 
    scale_x_discrete(element_blank()) +
    scale_y_continuous(y_names) +
    theme_bw() +
    theme(legend.position = 'none')

  
  return(out_plo)
  
  
}
  
#' @rdname olden
#'
#' @param struct numeric vector equal in length to the number of layers in the network.  Each number indicates the number of nodes in each layer starting with the input and ending with the output.  An arbitrary number of hidden layers can be included.
#' 
#' @export
#' 
#' @method olden numeric
olden.numeric <- function(mod_in, struct, ...){
  
  #get variable names from mod_in object
  x_names <- paste0(rep('X', struct[1]), seq(1:struct[1]))
  y_names <- paste0(rep('Y', struct[3]), seq(1:struct[3]))
  
  # get model weights
  wts_in <- neuralweights(mod_in, struct = struct)
  struct <- wts_in$struct
  wts_in <- wts_in$wts
  
  olden.default(wts_in, x_names, y_names, ...)

}

#' @rdname olden
#' 
#' @export
#' 
#' @method olden nnet
olden.nnet <- function(mod_in, ...){
  
  # check for skip layers
  skip_wts <- try(neuralskips(mod_in), silent = TRUE)
  if(inherits(skip_wts, 'try-error')) skip_wts <- NULL
  
  # get variable names from mod_in object
  # separate methods if nnet called with formula 
  if(is.null(mod_in$call$formula)){
    x_names <- 
      colnames(eval(mod_in$call$x))
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
  
  # get model weights
  wts_in <- neuralweights(mod_in)
  struct <- wts_in$struct
  wts_in <- wts_in$wts
  
  olden.default(wts_in, x_names, y_names, skip_wts = skip_wts, ...)
  
}

#' @rdname olden
#' 
#' @export
#' 
#' @method olden mlp
olden.mlp <- function(mod_in, ...){
  
  # get variable names from mod_in object
  all_names <- mod_in$snnsObject$getUnitDefinitions()
  x_names <- all_names[grep('Input', all_names$unitName), 'unitName']
  y_names <- all_names[grep('Output', all_names$unitName), 'unitName']

  # get model weights
  wts_in <- neuralweights(mod_in)
  struct <- wts_in$struct
  wts_in <- wts_in$wts
  
  olden.default(wts_in, x_names, y_names, ...)
  
}

#' @rdname olden
#' 
#' @export
#'  
#' @method olden nn
olden.nn <- function(mod_in, ...){
  
  # get variable names from mod_in object
  x_names <- mod_in$model.list$variables
  y_names <- mod_in$model.list$response

  # get model weights
  wts_in <- neuralweights(mod_in)
  struct <- wts_in$struct
  wts_in <- wts_in$wts
  
  olden.default(wts_in, x_names, y_names, ...)
  
}

#' @rdname olden
#' 
#' @export
#' 
#' @method olden train
olden.train <- function(mod_in, ...){

  # check for skip layers
  skip_wts <- try(neuralskips(mod_in), silent = TRUE)
  if(inherits(skip_wts, 'try-error')) skip_wts <- NULL
  
  # get variable names from mod_in object  
  y_names <- strsplit(as.character(mod_in$terms[[2]]), ' + ', fixed = TRUE)[[1]]
  mod_in <- mod_in$finalModel
  x_names <- mod_in$xNames
  
  # get model weights
  wts_in <- neuralweights(mod_in)
  struct <- wts_in$struct
  wts_in <- wts_in$wts
  
  olden.default(wts_in, x_names, y_names, skip_wts = skip_wts, ...)
  
}
