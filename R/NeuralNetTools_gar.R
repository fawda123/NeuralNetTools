#' Variable importance using Garson's algorithm
#' 
#' Relative importance of input variables in neural networks using Garson's algorithm
#' 
#' @param mod_in input model object or a list of model weights as returned from \code{\link{neuralweights}} if using the default method
#' @param x_names chr string of input variable names, obtained from the model object
#' @param y_names chr string of response variable names, obtained from the model object
#' @param bar_plot logical indicating if a \code{ggplot} object is returned (default \code{T}), otherwise numeric values are returned
#' @param x_lab chr string of alternative names to be used for explanatory variables in the figure, default is taken from \code{mod_in}
#' @param y_lab chr string of alternative name to be used for the y-axis in the figure
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
#' @import ggplot2
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object for plotting if \code{bar_plot = FALSE}, otherwise a \code{data.frame} of relative importance values for each input variable.  The default aesthetics for \code{\link[ggplot2]{ggplot}} can be further modified, as shown with the examples.
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
#' \dontrun{
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
#' library(caret)
#' 
#' mod <- train(Y1 ~ X1 + X2 + X3, method = 'nnet', data = neuraldat, linout = TRUE)
#' 
#' garson(mod)
#' 
#' ## modify the plot using ggplot2 syntax
#' library(ggplot2)
#' 
#' mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5)
#' 
#' cols <- heat.colors(10)
#' garson(mod) +
#'   scale_y_continuous('Rel. Importance', limits = c(-1, 1)) + 
#'   scale_fill_gradientn(colours = cols) + 
#'   scale_colour_gradientn(colours = cols)
#'}
garson <- function(mod_in, ...) UseMethod('garson')
 
#' @rdname garson
#'
#' @export
#' 
#' @method garson default
garson.default <- function(mod_in, x_names, y_names, bar_plot = TRUE, x_lab = NULL, y_lab = NULL, ...){
  
  # stop if more than one y output
  if(length(y_names) > 1) 
    stop('Garson only applies to neural networks with one output node')
  
  # change variables names to user sub 
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
  
  # stop if multiple hidden layers
  max_i <- length(inp_hid)
  if(max_i > 1) stop('Garsons algorithm not applicable for multiple hidden layers')
  
  # final layer weights for output
  hid_out <- wts_in[[grep('out 1', names(wts_in))]][-1]
  
  # use garson's algorithm
  sum_in <- t(inp_hid[[max_i]])
  dimsum <- dim(sum_in)
  sum_in <- apply(sum_in, 2, function(x){
  
    abs(x) * abs(hid_out)
    
  })
  sum_in <- matrix(sum_in, nrow = dimsum[1], ncol = dimsum[2], byrow = FALSE)
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
    geom_bar(stat = 'identity', position = 'identity') + 
    scale_x_discrete(element_blank()) +
    scale_y_continuous(y_names) +
    theme_bw() +
    theme(legend.position = 'none')
  
  return(out_plo)
  
}

#' @rdname garson
#' 
#' @param struct numeric vector equal in length to the number of layers in the network.  Each number indicates the number of nodes in each layer starting with the input and ending with the output.  An arbitrary number of hidden layers can be included.
#' 
#' @export
#' 
#' @method garson numeric
garson.numeric <- function(mod_in, struct, ...){
  
  # get variable names from mod_in object
  x_names <- paste0(rep('X', struct[1]), seq(1:struct[1]))
  y_names <- paste0(rep('Y', struct[3]), seq(1:struct[3]))
  
  # get model weights
  wts_in <- neuralweights(mod_in, struct = struct)
  struct <- wts_in$struct
  wts_in <- wts_in$wts
  
  garson.default(wts_in, x_names, y_names, ...)
    
}

#' @rdname garson
#' 
#' @export
#' 
#' @method garson nnet
garson.nnet <- function(mod_in, ...){
  
  # check for skip layers
  chk <- grepl('skip-layer', capture.output(mod_in))
  if(any(chk))
    stop("Garson's algorithm not applicable for networks with skip layers, use Olden's method")

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

  # get model weights
  wts_in <- neuralweights(mod_in)
  struct <- wts_in$struct
  wts_in <- wts_in$wts
  
  garson.default(wts_in, x_names, y_names, ...)
  
}

#' @rdname garson
#' 
#' @export
#' 
#' @method garson mlp
garson.mlp <- function(mod_in, ...){
  
  #get variable names from mod_in object
  all_names <- mod_in$snnsObject$getUnitDefinitions()
  x_names <- all_names[grep('Input', all_names$unitName), 'unitName']
  y_names <- all_names[grep('Output', all_names$unitName), 'unitName']
  
  # get model weights
  wts_in <- neuralweights(mod_in)
  struct <- wts_in$struct
  wts_in <- wts_in$wts
  
  garson.default(wts_in, x_names, y_names, ...)
  
}

#' @rdname garson
#' 
#' @export
#'  
#' @method garson nn
garson.nn <- function(mod_in, ...){
  
  # get variable names from mod_in object
  # change to user input if supplied
  x_names <- mod_in$model.list$variables
  y_names <- mod_in$model.list$response
  
  # get model weights
  wts_in <- neuralweights(mod_in)
  struct <- wts_in$struct
  wts_in <- wts_in$wts
  
  garson.default(wts_in, x_names, y_names, ...)
  
}

#' @rdname garson
#' 
#' @export
#' 
#' @method garson train
garson.train <- function(mod_in, ...){
  
  y_names <- strsplit(as.character(mod_in$terms[[2]]), ' + ', fixed = TRUE)[[1]]
  mod_in <- mod_in$finalModel
  x_names <- mod_in$xNames
  
  # check for skip layers
  chk <- grepl('skip-layer', capture.output(mod_in))
  if(any(chk))
    stop("Garson's algorithm not applicable for networks with skip layers, use Olden's method")
  
  # get model weights
  wts_in <- neuralweights(mod_in)
  struct <- wts_in$struct
  wts_in <- wts_in$wts
  
  garson.default(wts_in, x_names, y_names, ...)
  
}

