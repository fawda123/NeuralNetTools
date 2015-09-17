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
#' @param split_show logical if a barplot is returned that shows the values at which explanatory variables were held constant while not being evaluated
#' @param ... arguments passed to other methods
#' 
#' @details
#' The Lek profile method is described briefly in Lek et al. 1996 and in more detail in Gevrey et al. 2003. The profile method is fairly generic and can be extended to any statistical model in R with a predict method.  However, it is one of few methods used to evaluate sensitivity in neural networks.
#' 
#' The profile method can be used to evaluate the effect of explanatory variables by returning a plot of the predicted response across the range of values for each separate variable.  The original profile method evaluated the effects of each variable while holding the remaining expalanatory variables at different quantiles (e.g., minimum, 20th percentile, maximum).  This is implemented in in the function by creating a matrix of values for explanatory variables where the number of rows is the number of observations and the number of columns is the number of explanatory variables. All explanatory variables are held at their mean (or other constant value) while the variable of interest is sequenced from its minimum to maximum value across the range of observations. This matrix (or data frame) is then used to predict values of the response variable from a fitted model object. This is repeated for each explanatory variable to obtain all response curves.  Values passed to \code{split_vals} must range from zero to one to define the quantiles for holding unevalauted explanatory variables. 
#' 
#' An alternative implementation of the profile method is to group the unevaluated explanatory variables using groupings defined by the statistical properties of the data.  Covariance among predictors may present unlikely scenarios if holding all unevaluated variables at the same level.  To address this issue, the function provides an option to hold unevalutaed variable at mean values defined by natural clusters in the data.  \code{\link[stats]{kmeans}} clustering is used on the input \code{data.frame} of explanatory variables if the argument passed to \code{split_vals} is an integer value greater than one.  The centers of the clusters are then used as constant values for the unevaluated variables.  An arbitrary grouping scheme can also be passed to \code{split_vals} as a \code{data.frame} where the user can specify exact values for holding each value constant (see the examples). 
#' 
#' For all plots, the legend with the 'splits' label indicates the colors that correspond to each group.  The groups describe the values at which unevaluated explanatory variables were held constant, either as specific quantiles, group assignments based on clustering, or in the arbitrary grouping defined by the user.  The constant values of each explanatory variable for each split can be viewed as a barplot by using \code{split_show = TRUE}.
#' 
#' Note that there is no predict method for neuralnet objects from the nn package.  The lekprofile method for nn objects uses the nnet package to recreate the input model, which is then used for the sensitivity predictions.  This approach only works for networks with one hidden layer. 
#' 
#' @export
#' 
#' @import ggplot2 nnet
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object for plotting if \code{val_out  =  FALSE}, otherwise a two-element \code{list} is returned with a \code{data.frame} in long form showing the predicted responses at different values of the explanatory variables and the grouping scheme that was used to hold unevaluated variables constant. 
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
#' ## y variable must have a name attribute
#' 
#' mod <- nnet(x, y, size = 5)
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
#' 
#' ## group by clusters instead of sequencing by quantiles
#' 
#' mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5)
#'  
#' lekprofile(mod, split_vals = 6) # six clusters
#' 
#' ## enter an arbitrary grouping scheme for the split values
#' ## i.e. hold all values at 0.5
#' split_vals <- rbind(rep(0.5, length = ncol(x)))
#' split_vals <- data.frame(split_vals)
#' names(split_vals) <- names(split_vals)
#' 
#' lekprofile(mod, split_vals = split_vals, xsel = 'X3')
lekprofile <- function(mod_in, ...) UseMethod('lekprofile')

#' @rdname lekprofile
#'
#' @import ggplot2 
#' 
#' @export
#' 
#' @method lekprofile default
lekprofile.default <- function(mod_in, xvars, ynms, xsel = NULL, steps = 100, split_vals = seq(0, 1, by = 0.2), val_out = FALSE, split_show = FALSE, ...){
  
  # subset xall if xsel is not empy
  if(is.null(xsel)) xsel <- names(xvars)
  
  # stop if only one input variable
  if(ncol(xvars) == 1) stop('Lek profile requires greater than one input variable')
  
  # standard lekprofile method using quantile splits or clusters
  if(inherits(split_vals, c('numeric', 'integer'))){
  
    # quantile approach
    if(all(split_vals <= 1)){
      
      grps <- apply(xvars, 2, quantile, split_vals)
      grps <- as.data.frame(rbind(grps))

    # kmeans approach      
    } else {
      
      # sanity checks for integer, one value
      if(length(split_vals) > 1) stop('split_vals must have length equal to one if an integer')
      if(split_vals%%1 != 0) stop('split_vals must be an integer greater than one')
    
      # get means of cluster centers
      grps <- kmeans(xvars, centers = split_vals)$centers
        
    }
    
  # use matrix or data.frame input for constant values
  } else {
    
    if(ncol(split_vals) != ncol(xvars)) stop('split_vals as matrix must have ncol same as xvars')
    grps <- split_vals
    names(grps) <- names(xvars)
    
  }

  # return bar plot for split values
  if(split_show) return(lekgrps(grps))
  
  #use 'pred_fun' to get pred vals of response across range of vals for an exp vars
  #loops over all explanatory variables of interest and all split values
  lek_vals <- sapply(
    xsel, 
    function(vars) pred_sens(xvars, mod_in, vars, steps, grps, ynms),
    simplify = FALSE  
  )

  #melt lek_val list for use with ggplot
  lek_vals <- melt(lek_vals, id.vars = 'x_vars')
  lek_vals$L2 <- factor(lek_vals$L2)#, labels = 1:nrow(grps))
  names(lek_vals) <- c('Explanatory', 'resp_name', 'Response', 'Splits', 'exp_name')

  #return only values if val_out = TRUE
  if(val_out) return(list(lek_vals, grps))
  
  #ggplot object
  p <- ggplot2::ggplot(lek_vals, aes_string(x = 'Explanatory', y = 'Response', group = 'Splits')) + 
    geom_line(aes_string(colour = 'Splits')) + 
    facet_grid(resp_name ~ exp_name, scales = 'free_x')
  
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

    