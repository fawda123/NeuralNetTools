#' Sensitivity analysis based on Lek's profile method
#' 
#' Conduct a sensitivity analysis of model responses in a neural network to input variables using Lek's profile method
#' 
#' @param mod_in input object for which an organized model list is desired.  The input can be an object of class \code{nnet} or \code{mlp}
#' @param xvars \code{\link[base]{data.frame}} of explanatory variables used to create the input model
#' @param ynms chr string of response variable names for model
#' @param xsel chr string of names of explanatory variables to plot, defaults to all
#' @param steps numeric value indicating number of observations to evaluate for each explanatory variable from minimum to maximum value, default 100
#' @param split_vals numeric vector indicating quantile values at which to hold other explanatory variables constant
#' @param val_out logical value indicating if actual sensitivity values are returned rather than a plot, default \code{FALSE}
#' @param ... arguments passed to other methods
#' 
#' @details
#' The Lek profile method is described briefly in Lek et al. 1996 and in more detail in Gevrey et al. 2003. The profile method is fairly generic and can be extended to any statistical model in R with a predict method.  However, it is one of few methods used to evaluate sensitivity in neural networks.   The default method of this function attempts to find variables names from a generic model object.  
#' 
#' The profile method begins by obtaining model predictions of the response variable across the range of values for the given explanatory variable. All other explanatory variables are held constant at set values (e.g., minimum, 20th percentile, maximum). The final result is a set of response curves for one response variable across the range of values for one explanatory variable, while holding all other explanatory variables constant. This is implemented in in the function by creating a matrix of values for explanatory variables where the number of rows is the number of observations and the number of columns is the number of explanatory variables. All explanatory variables are held at their mean (or other constant value) while the variable of interest is sequenced from its minimum to maximum value across the range of observations. This matrix (or data frame) is then used to predict values of the response variable from a fitted model object. This is repeated for each explanatory variable to obtain all response curves.
#' 
#' Note that there is no predict method for neuralnet objects from the nn package.  The lekprofile method for nn objects uses the nnet package to recreate the input model, which is then used for the sensitivity predictions.  This approach only works for networks with one hidden layer. 
#' 
#' @export
#' 
#' @import ggplot2 nnet
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object for plotting if \code{val_out  =  FALSE}, otherwise a \code{data.frame} in long form showing the predicted responses at different values of the explanatory varibales. 
#' 
#' @references
#' Lek, S., Delacoste, M., Baran, P., Dimopoulos, I., Lauga, J., Aulagnier, S. 1996. Application of neural networks to modelling nonlinear relationships in Ecology. Ecological Modelling. 90:39-52.
#' 
#' Gevrey, M., Dimopoulos, I., Lek, S. 2003. Review and comparison of methods to study the contribution of variables in artificial neural network models. Ecological Modelling. 160:249-264.
#' 
#' Olden, J.D., Joy, M.K., Death, R.G. 2004. An accurate comparison of methods for quantifying variable importance in artificial neural networks using simulated data. Ecological Modelling. 178:389-397.
#' 
#' @examples
#' 
#' ## using nnet
#' 
#' library(nnet)
#' 
#' set.seed(123)
#' 
#' mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5)
#'  
#' lekprofile(mod)  
#' 
#' ## using RSNNS, no bias layers
#' 
#' library(RSNNS)
#' 
#' x <- neuraldat[, c('X1', 'X2', 'X3')]
#' y <- neuraldat[, 'Y1', drop = FALSE]
#' 
#' mod <- mlp(x, y, size = 5)
#' 
#' lekprofile(mod, xvars = x)
#' 
#' ## using neuralnet
#' 
#' library(neuralnet)
#' 
#' mod <- neuralnet(Y1 ~ X1 + X2 + X3, data = neuraldat, hidden = 5)
#' 
#' lekprofile(mod)
#' 
#' ## back to nnet, not using formula to create model
#' ## y variable must a name attribute
#' 
#' mod <- nnet(x, y, data = neuraldat, size = 5)
#' 
#' lekprofile(mod)
#' 
#' ## using caret
#' 
#' \dontrun{
#' library(caret)
#' 
#' mod <- train(Y1 ~ X1 + X2 + X3, method = 'nnet', data = neuraldat, linout = TRUE)
#' 
#' lekprofile(mod)
#' 
#' }
lekprofile <- function(mod_in, ...) UseMethod('lekprofile')

#' @rdname lekprofile
#'
#' @import ggplot2 
#' 
#' @export
#' 
#' @method lekprofile default
lekprofile.default <- function(mod_in, xvars, ynms, xsel = NULL, steps = 100, split_vals = seq(0, 1, by = 0.2), val_out = FALSE, ...){
  
  
  # subset xall if xsel is not empy
  if(is.null(xsel)) xsel <- names(xvars)
  
  # stop if only one input variable
  if(ncol(xvars) == 1) stop('Lek profile requires greater than one input variable')
  
  #use 'pred_fun' to get pred vals of response across range of vals for an exp vars
  #loops over all explanatory variables of interest and all split values
  lek_vals <- sapply(
    xsel, 
    function(vars){
      sapply(
        split_vals, 
        function(splits){
          pred_sens(
            xvars, 
            mod_in, 
            vars, 
            steps, 
            function(val) quantile(val, probs = splits), 
            ynms
          )
        }, 
        simplify = FALSE
      )
    }, 
    simplify = FALSE  
  )
  
  #melt lek_val list for use with ggplot
  lek_vals <- melt(lek_vals, id.vars = 'x_vars')
  lek_vals$L2 <- factor(lek_vals$L2, labels = split_vals)
  names(lek_vals) <- c('Explanatory', 'resp_name', 'Response', 'Splits', 'exp_name')
  
  #return only values if val_out = TRUE
  if(val_out) return(lek_vals)
  
  #ggplot object
  p <- ggplot2::ggplot(lek_vals, aes_string(x = 'Explanatory', y = 'Response', group = 'Splits')) + 
    geom_line(aes_string(colour = 'Splits', linetype = 'Splits', size = 'Splits')) + 
    facet_grid(resp_name ~ exp_name, scales = 'free_x') +
    scale_linetype_manual(values = rep('solid', length(split_vals))) +
    scale_size_manual(values = rep(1, length(split_vals)))
  
  return(p)
  
}

#' @rdname lekprofile
#'
#' @import ggplot2 
#' 
#' @export
#' 
#' @method lekprofile nnet
lekprofile.nnet <- function(mod_in, ...){
  
  # get exp and resp names from mod_in
  # get matrix for exp vars
  if(is.null(mod_in$call$formula)){
    
    ynms <- colnames(eval(mod_in$call$y))
    if(is.null(ynms)) stop('Response variables must have names attribute') 
    xall <- colnames(eval(mod_in$call$x))
    if(is.null(xall)) stop('Input variables must have names attribute')
    xvars <- eval(mod_in$call$x)
    
  } else {
    
    forms <- eval(mod_in$call$formula)
    dat_names <- try(model.frame(forms,data = eval(mod_in$call$data)))
    ynms <- as.character(forms)[2]
    xall <- names(dat_names)[!names(dat_names) %in% as.character(forms)[2]]
    xvars <- dat_names[, !names(dat_names) %in% as.character(forms)[2], drop = F]
    
  }

  lekprofile.default(mod_in, xvars = xvars, ynms = ynms, ...)

}

#' @rdname lekprofile
#' 
#' @import ggplot2 
#' 
#' @export
#' 
#' @method lekprofile mlp
lekprofile.mlp <- function(mod_in, xvars, ...){

  # getexp and resp names from mod_in
  # get matrix for exp vars
  ynms <- paste0('Y', seq(1, mod_in$nOutputs))
  xvars <- data.frame(xvars)
  names(xvars) <- paste0('X', seq(1, mod_in$nInputs))
  xall <- names(xvars)

  lekprofile.default(mod_in, xvars = xvars, ynms = ynms, ...)
  
}

#' @rdname lekprofile
#'
#' @import ggplot2 
#' 
#' @export
#' 
#' @method lekprofile train
lekprofile.train <- function(mod_in, ...){
  
  # input data, x_names, and y_names
  xvars <- mod_in$trainingData
  xvars <- xvars[, !names(xvars) %in% '.outcome']
  ynms <- strsplit(as.character(mod_in$terms[[2]]), ' + ', fixed = TRUE)[[1]]
  mod_in <- mod_in$finalModel
  x_names <- mod_in$xNames
  xvars <- xvars[, x_names]
  
  lekprofile.default(mod_in, xvars = xvars, ynms = ynms, ...)
  
}

#' @rdname lekprofile
#'
#' @import ggplot2 nnet
#' 
#' @export
#' 
#' @method lekprofile nn
lekprofile.nn <- function(mod_in, ...){

  # recreate the model using nnet (no predict method for nn)
  moddat <- mod_in$data
  modwts <- neuralweights(mod_in)
  modwts <- unlist(modwts$wts)
  modsz <- mod_in$call$hidden
  modfrm <- mod_in$call$formula
  modlin <- mod_in$call$linear.output
  modlin2 <- TRUE
  if(!is.null(modlin)) modlin2 <- modlin

  # stop if multiple hidden layers - nnet can only do one input
  # mlp can do this but does not accept starting weights
  if(length(modsz) > 1) stop('Cannot use lekprofile with multiple hidden layers')
  
  # create call for nnet model
  mod_in <- substitute(
    nnet(formin, data = moddat, size = modsz, 
      Wts = modwts, maxit = 0, linout = modlin2, trace = FALSE), 
    list(formin = formula(modfrm), moddat = moddat, modsz = modsz, modwts = modwts, 
      modlin2 = modlin2)
  )
  
  # eval call
  mod_in <- eval(mod_in)
  
  # pass to lekprofile.nnet
  lekprofile(mod_in, ...)
  
}

    