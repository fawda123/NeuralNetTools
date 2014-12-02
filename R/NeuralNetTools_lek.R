#' Sensitivity analysis based on Lek's profile method
#' 
#' Conduct a sensitivity analysis of model responses in a neural network to input variables using Lek's profile method
#' 
#' @param mod_in input object for which an organized model list is desired.  The input can be an object of class \code{nnet} or \code{mlp}
#' @param steps numeric value indicating number of observations to evaluate for each explanatory variable from minimum to maximum value, default 100
#' @param split_vals numeric vector indicating quantile values at which to hold other explanatory variables constant
#' @param val_out logical value indicating if actual sensitivity values are returned rather than a plot, default \code{F}
#' @param ... arguments passed to other methods
#' 
#' @details
#' The Lek profile method is described briefly in Lek et al. 1996 and in more detail in Gevrey et al. 2003. The profile method is fairly generic and can be extended to any statistical model in R with a predict method.  However, it is one of few methods used to evaluate sensitivity in neural networks.  Note that there is no predict method for neuralnet objects from the nn package, so a profile method is not available.  Currently, the defualt method of this function attempts to find variables names from a generic model object.  The default method of this function has only been tested with \code{\link[stats]{lm}} and may not work with other model types if the variable names cannot be found.
#' 
#' The profile method begins by obtaining model predictions of the response variable across the range of values for the given explanatory variable. All other explanatory variables are held constant at set values (e.g., minimum, 20th percentile, maximum). The final result is a set of response curves for one response variable across the range of values for one explanatory variable, while holding all other explanatory variables constant. This is implemented in in the functoin by creating a matrix of values for explanatory variables where the number of rows is the number of observations and the number of columns is the number of explanatory variables. All explanatory variables are held at their mean (or other constant value) while the variable of interest is sequenced from its minimum to maximum value across the range of observations. This matrix (or data frame) is then used to predict values of the response variable from a fitted model object. This is repeated for each explanatory variable to obtain all response curves.
#' 
#' @export lekprofile
#' 
#' @import ggplot2 neuralnet nnet RSNNS 
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object for plotting if \code{val_out  =  F}, otherwise a \code{data.frame} in long form showing the predicted responses at different values of the explanatory varibales. 
#' 
#' @references
#' Lek, S., Delacoste, M., Baran, P., Dimopoulos, I., Lauga, J., Aulagnier, S. 1996. Application of neural networks to modelling nonlinear relationships in Ecology. Ecological Modelling. 90:39-52.
#' 
#' Gevrey, M., Dimopoulos, I., Lek, S. 2003. Review and comparison of methods to study the contribution of variables in artificial neural network models. Ecological Modelling. 160:249-264.
#' 
#' @examples
#' 
#' ## a simple lm 
#' 
#' data(neuraldat) 
#' 
#' mod <- lm(Y1 ~ X1 + X2 + X3, data = neuraldat)
#' 
#' lekprofile(mod)
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
#' y <- neuraldat[, 'Y1', drop = F]
#' 
#' mod <- mlp(x, y, size = 5)
#' 
#' lekprofile(mod, exp_in = x)
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
#' @export lekprofile.default
#' 
#' @method lekprofile default
lekprofile.default <- function(mod_in, steps = 100, split_vals = seq(0, 1, by = 0.2), val_out = F, ...){
  
  ##
  #sort out exp and resp names based on object type of call to mod_in
  #get matrix for exp vars
  if('nnet' %in% class(mod_in)| !'mlp' %in% class(mod_in)){
    if(is.null(mod_in$call$formula)){
      resp_name <- colnames(eval(mod_in$call$y))
      if(is.null(resp_name)) stop('Response variables must have names attribute') 
      var_sens <- colnames(eval(mod_in$call$x))
      if(is.null(var_sens)) stop('Input variables must have names attribute')
      mat_in<-eval(mod_in$call$x)
    }
    else{
      forms <- eval(mod_in$call$formula)
      dat_names <- try(model.frame(forms,data = eval(mod_in$call$data)))
      resp_name <- as.character(forms)[2]
      var_sens <- names(dat_names)[!names(dat_names) %in% as.character(forms)[2]]
      mat_in <- dat_names[,!names(dat_names) %in% as.character(forms)[2]]
    }
  }
  
  #use 'pred_fun' to get pred vals of response across range of vals for an exp vars
  #loops over all explanatory variables of interest and all split values
  lek_vals <- sapply(
    var_sens, 
    function(vars){
      sapply(
    
        split_vals, 
        function(splits){
          pred_sens(
            mat_in, 
            mod_in, 
            vars, 
            steps, 
            function(val) quantile(val, probs = splits), 
            resp_name
          )
        }, 
        simplify = F
      )
    }, 
    simplify = F  
  )
  
  #melt lek_val list for use with ggplot
  lek_vals <- melt(lek_vals, id.vars = 'x_vars')
  lek_vals$L2 <- factor(lek_vals$L2, labels = split_vals)
  names(lek_vals) <- c('Explanatory', 'resp_name', 'Response', 'Splits', 'exp_name')
  
  #return only values if val_out = T
  if(val_out) return(lek_vals)
  
  #ggplot object
  p <- ggplot(lek_vals, aes(x = Explanatory, y = Response, group = Splits)) + 
    geom_line(aes(colour = Splits, linetype = Splits, size = Splits)) + 
    facet_grid(resp_name ~ exp_name) +
    scale_linetype_manual(values = rep('solid', length(split_vals))) +
    scale_size_manual(values = rep(1, length(split_vals)))
  
  return(p)
  
}

#' @rdname lekprofile
#'
#' @import ggplot2 
#' 
#' @export lekprofile.nnet
#' 
#' @method lekprofile nnet
lekprofile.nnet <- function(mod_in,steps = 100, split_vals = seq(0, 1, by = 0.2), val_out = F, ...){
  
  lekprofile.default(mod_in, steps, split_vals, val_out)

}

#' @rdname lekprofile
#'
#' @param exp_in \code{matrix} or \code{data.frame} of input variables used to create the model 
#' 
#' @import ggplot2 
#' 
#' @export lekprofile.mlp
#' 
#' @method lekprofile mlp
lekprofile.mlp <- function(mod_in, exp_in, steps = 100, split_vals = seq(0, 1, by = 0.2), val_out = F, ...){

  ##
  #sort out exp and resp names based on object type of call to mod_in
  #get matrix for exp vars
  resp_name <- paste0('Y', seq(1, mod_in$nOutputs))
  mat_in <- data.frame(exp_in)
  names(mat_in) <- paste0('X', seq(1, mod_in$nInputs))
  var_sens <- names(mat_in)

  #use 'pred_fun' to get pred vals of response across range of vals for an exp vars
  #loops over all explanatory variables of interest and all split values
  lek_vals <- sapply(
    var_sens, 
    function(vars){
      sapply(
        split_vals, 
        function(splits){
          pred_sens(
            mat_in, 
            mod_in, 
            vars,
            steps, 
            function(val) quantile(val, probs = splits)
          )
        }, 
        simplify = F
      )
    }, 
    simplify = F  
  )
  
  #melt lek_val list for use with ggplot
  lek_vals <- melt(lek_vals, id.vars = 'x_vars')
  lek_vals$L2 <- factor(lek_vals$L2, labels = split_vals)
  names(lek_vals) <- c('Explanatory', 'resp_name', 'Response', 'Splits', 'exp_name')
  
  #return only values if val_out = T
  if(val_out) return(lek_vals)
  
  #ggplot object
  p <- ggplot(lek_vals, aes(x = Explanatory, y = Response, group = Splits)) + 
    geom_line(aes(colour = Splits, linetype = Splits, size = Splits)) + 
    facet_grid(resp_name ~ exp_name) +
    scale_linetype_manual(values = rep('solid', length(split_vals))) +
    scale_size_manual(values = rep(1, length(split_vals)))
  
  return(p)
  
}

#' @rdname lekprofile
#'
#' @import ggplot2 
#' 
#' @export lekprofile.train
#' 
#' @method lekprofile train
lekprofile.train <- function(mod_in, steps = 100, split_vals = seq(0, 1, by = 0.2), val_out = F, ...){
  
  # input data, x_names, and y_names
  mat_in <- mod_in$trainingData
  mat_in <- mat_in[, !names(mat_in) %in% '.outcome']
  
  y_names <- strsplit(as.character(mod_in$terms[[2]]), ' + ', fixed = T)[[1]]
  
  mod_in <- mod_in$finalModel
  x_names <- mod_in$xNames
  
  mat_in <- mat_in[, x_names]
  
  ##
  #sort out exp and resp names based on object type of call to mod_in
  #get matrix for exp vars
  resp_name <- y_names
  var_sens <- names(mat_in)
  
  #use 'pred_fun' to get pred vals of response across range of vals for an exp vars
  #loops over all explanatory variables of interest and all split values
  lek_vals <- sapply(
    var_sens, 
    function(vars){
      sapply(
        split_vals, 
        function(splits){
          pred_sens(
            mat_in, 
            mod_in, 
            vars, 
            steps, 
            function(val) quantile(val, probs = splits)
          )
        }, 
        simplify = F
      )
    }, 
    simplify = F  
  )
  
  #melt lek_val list for use with ggplot
  lek_vals <- melt(lek_vals, id.vars = 'x_vars')
  lek_vals$L2 <- factor(lek_vals$L2, labels = split_vals)
  names(lek_vals) <- c('Explanatory', 'resp_name', 'Response', 'Splits', 'exp_name')
  
  #return only values if val_out = T
  if(val_out) return(lek_vals)
  
  #ggplot object
  p <- ggplot(lek_vals, aes(x = Explanatory, y = Response, group = Splits)) + 
    geom_line(aes(colour = Splits, linetype = Splits, size = Splits)) + 
    facet_grid(resp_name ~ exp_name) +
    scale_linetype_manual(values = rep('solid', length(split_vals))) +
    scale_size_manual(values = rep(1, length(split_vals)))
  
  return(p)
  
}

    