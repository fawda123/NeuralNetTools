#' Plot a neural network model
#' 
#' Plot a neural interpretation diagram for a neural network object
#' 
#' @param mod_in neural network object or numeric vector of weights
#' @param x_names chr string indicating names for input variables, default from model object
#' @param y_names	chr string indicating names for output variables, default from model object
#' @param nid	logical value indicating if neural interpretation diagram is plotted, default \code{TRUE}
#' @param all_out	chr string indicating names of response variables for which connections are plotted, default all
#' @param all_in	chr string indicating names of input variables for which connections are plotted, default all
#' @param bias	logical value indicating if bias nodes and connections are plotted, default \code{TRUE}
#' @param rel_rsc	numeric indicating the scaling range for the width of connection weights
#' @param circle_cex	numeric value indicating size of nodes, default 5
#' @param node_labs	logical value indicating if labels are plotted directly on nodes, default \code{TRUE}
#' @param var_labs logical value indicating if variable names are plotted next to nodes, default \code{TRUE}
#' @param line_stag	numeric value that specifies distance of connection weights from nodes
#' @param cex_val	numeric value indicating size of text labels, default 1
#' @param alpha_val	numeric value (0-1) indicating transparency of connections, default 1
#' @param circle_col	chr string indicating color of nodes, default \code{'lightblue'}, or two element list with first element indicating color of input nodes and second indicating color of remaining nodes
#' @param pos_col	chr string indicating color of positive connection weights, default \code{'black'}
#' @param neg_col	chr string indicating color of negative connection weights, default \code{'grey'}
#' @param bord_col chr string indicating border color around nodes, default \code{'lightblue'}
#' @param prune_col chr string indicating color of pruned connections, otherwise not shown
#' @param prune_lty line type for pruned connections, passed to \code{\link[graphics]{segments}}
#' @param max_sp logical value indicating if space between nodes in each layer is maximized, default \code{FALSE}
#' @param skip logical if skip layer connections are plotted instead of the primary network
#' @param ...	additional arguments passed to plot
#' 
#' @import ggplot2
#' 
#' @export
#' 
#' @references
#' Ozesmi, S.L., Ozesmi, U. 1999. An artificial neural network approach to spatial habitat modeling with interspecific interaction. Ecological Modelling. 116:15-31.
#' 
#' @return A graphics object unless \code{wts_only = TRUE}, then neural network weights from \code{\link{neuralweights}}.
#' 
#' @details
#'  This function plots a neural network as a neural interpretation diagram as in Ozesmi and Ozesmi (1999). Options to plot without color-coding or shading of weights are also provided.  The default settings plot positive weights between layers as black lines and negative weights as grey lines. Line thickness is in proportion to relative magnitude of each weight. The first layer includes only input variables with nodes labelled arbitrarily as I1 through In for n input variables.  One through many hidden layers are plotted with each node in each layer labelled as H1 through Hn.  The output layer is plotted last with nodes labeled as O1 through On.  Bias nodes connected to the hidden and output layers are also shown.  Neural networks created using \code{\link[RSNNS]{mlp}} do not show bias layers.
#' 
#' A primary network and a skip layer network can be plotted for \code{\link[nnet]{nnet}} models with a skip layer connection.  The default is to plot the primary network, whereas the skip layer network can be viewed with \code{skip = TRUE}.  If \code{nid = TRUE}, the line widths for both the primary and skip layer plots are relative to all weights.  Viewing both plots is recommended to see which network has larger relative weights.  Plotting a network with only a skip layer (i.e., no hidden layer, \code{size = 0}) will include bias connections to the output layer, whereas these are not included in the plot of the skip layer if \code{size} is greater than zero.
#'  
#' The numeric method for plotting requires the input weights to be in a specific order given the structure of the network.  An additional argument \code{struct} (from \code{\link{neuralweights}} is also required that lists the number of nodes in the input, hidden, and output layers.  The example below for the numeric input shows the correct weight vector for a simple neural network model with two input variables, one output variable, and one hidden layer with two nodes.  Bias nodes are also connected to the hidden and output layer.  Using the plot syntax of I, H, O, and B for input, hidden, output, and bias to indicate weighted connections between layers, the correct weight order for the \code{mod_in} vector is B1-H1, I1-H1, I2-H1, B1-H2, I1-H2, I2-H2, B2-O1, H1-O1, H2-O1.
#' 
#' @examples 
#' ## using numeric input
#' 
#' # B1-H1, I1-H1, I2-H1, B1-H2, I1-H2, I2-H2, B2-O1, H1-O1, H2-O1.
#' wts_in <- c(13.12, 1.49, 0.16, -0.11, -0.19, -0.16, 0.56, -0.52, 0.81)
#' struct <- c(2, 2, 1) #two inputs, two hidden, one output 
#' 
#' plotnet(wts_in, struct = struct)
#' 
#' # numeric input, two hidden layers
#' 
#' # B1-H11, I1-H11, I2-H11, B1-H12, I1-H12, I2-H12, B2-H21, H11-H21, H12-H21, 
#' # B2-H22, H11-H22, H12-H22, B3-O1, H21-O1, H22-O1 
#' wts_in <- c(1.12, 1.49, 0.16, -0.11, -0.19, -0.16, 0.5, 0.2, -0.12, -0.1, 0.89, 0.9, 0.56, -0.52, 0.81)
#' struct <- c(2, 2, 2, 1) # two inputs, two (two nodes each), one output 
#'
#' plotnet(wts_in, struct = struct)
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
#' plotnet(mod)  
#' 
#' ## plot the skip layer from nnet model
#'
#' mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5, skip = TRUE)
#'
#' plotnet(mod, skip = TRUE)  
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
#' plotnet(mod)
#'
#' # pruned model using code from RSSNS pruning demo
#' pruneFuncParams <- list(max_pr_error_increase = 10.0, pr_accepted_error = 1.0, 
#'  no_of_pr_retrain_cycles = 1000, min_error_to_stop = 0.01, init_matrix_value = 1e-6, 
#'  input_pruning = TRUE, hidden_pruning = TRUE)
#' mod <- mlp(x, y, size = 5, pruneFunc = "OptimalBrainSurgeon", 
#'  pruneFuncParams = pruneFuncParams)
#' 
#' plotnet(mod)
#' plotnet(mod, prune_col = 'lightblue')
#' 
#' ## using neuralnet
#' 
#' library(neuralnet)
#' 
#' mod <- neuralnet(Y1 ~ X1 + X2 + X3, data = neuraldat, hidden = 5)
#' 
#' plotnet(mod)
#' 
#' ## using caret
#'
#' library(caret)
#' 
#' mod <- train(Y1 ~ X1 + X2 + X3, method = 'nnet', data = neuraldat, linout = TRUE)
#' 
#' plotnet(mod)
#' 
#' ## a more complicated network with categorical response
#' AND <- c(rep(0, 7), 1)
#' OR <- c(0, rep(1, 7))
#'  
#' binary_data <- data.frame(expand.grid(c(0, 1), c(0, 1), c(0, 1)), AND, OR)
#'  
#' mod <- neuralnet(AND + OR ~ Var1 + Var2 + Var3, binary_data, 
#'  hidden = c(6, 12, 8), rep = 10, err.fct = 'ce', linear.output = FALSE)
#'  
#' plotnet(mod)
#' 
#' ## recreate the previous example with numeric inputs
#' 
#' # get the weights and structure in the right format
#' wts <- neuralweights(mod)
#' struct <- wts$struct
#' wts <- unlist(wts$wts)
#'
#' # plot
#' plotnet(wts, struct = struct)
#' 
#' ## color input nodes by relative importance
#' mod <- nnet(Y1 ~ X1 + X2 + X3, data = neuraldat, size = 5)
#'  
#' rel_imp <- garson(mod, bar_plot = FALSE)$rel_imp
#' cols <- colorRampPalette(c('lightgreen', 'darkgreen'))(3)[rank(rel_imp)]
#'  
#' plotnet(mod, circle_col = list(cols, 'lightblue'))
#' }
plotnet <- function(mod_in, ...) UseMethod('plotnet')

#' @rdname plotnet
#'
#' @export
#'
#' @method plotnet default
plotnet.default <- function(mod_in, x_names, y_names, struct = NULL, nid = TRUE, all_out = TRUE, all_in = TRUE, bias = TRUE, rel_rsc = c(1, 7), circle_cex = 5, node_labs = TRUE, var_labs = TRUE, line_stag = NULL, cex_val = 1, alpha_val = 1, circle_col = 'lightblue', pos_col = 'black', neg_col = 'grey', bord_col = 'lightblue', max_sp = FALSE, prune_col = NULL, prune_lty = 'dashed', skip = NULL, ...){

  wts <- neuralweights(mod_in, struct = struct)
  struct <- wts$struct
  wts <- wts$wts
  
  #circle colors for input, if desired, must be two-vector list, first vector is for input layer
  if(is.list(circle_col)){
    circle_col_inp <- circle_col[[1]]
    circle_col <- circle_col[[2]]
  }
  else circle_col_inp <- circle_col
  
  #initiate plotting
  x_range <- c(0, 100)
  y_range <- c(0, 100)
  #these are all proportions from 0-1
  if(is.null(line_stag)) line_stag <- 0.011 * circle_cex/2
  layer_x <- seq(0.17, 0.9, length = length(struct))
  bias_x <- layer_x[-length(layer_x)] + diff(layer_x)/2
  bias_y <- 0.95
  circle_cex <- circle_cex
  
  #initiate plot
  plot(x_range, y_range, type = 'n', axes = FALSE, ylab = '', xlab = '')
  
  # warning if nnet hidden is zero
  if(struct[2] == 0){
    warning('No hidden layer, plotting skip layer only with bias connections')
    skip <- TRUE
  }
  
  # subroutine for skip layer connections in nnet
  if(any(skip)){
    
    return({ # use this to exit
      
      # plot connections usign layer lines with skip TRUE
      mapply(
        function(x) layer_lines(mod_in, x, layer1 = 1, layer2 = length(struct), out_layer = TRUE, nid = nid, rel_rsc = rel_rsc, all_in = all_in, pos_col = alpha(pos_col, alpha_val), neg_col = alpha(neg_col, alpha_val), x_range = x_range, y_range = y_range, line_stag = line_stag, x_names = x_names, layer_x = layer_x, max_sp = max_sp, struct = struct, prune_col = prune_col, prune_lty = prune_lty, skip = skip), 
        1:struct[length(struct)]
      )
      
      # plot only input, output nodes
      for(i in c(1, length(struct))){
        in_col <- circle_col
        if(i == 1) { layer_name <- 'I'; in_col <- circle_col_inp}
        if(i == length(struct)) layer_name <- 'O'
        layer_points(struct[i], layer_x[i], x_range, layer_name, cex_val, circle_cex, bord_col, in_col, 
          node_labs, line_stag, var_labs, x_names, y_names, max_sp = max_sp, struct = struct, 
          y_range = y_range
        )
        
      }
      
      # bias node
      if(bias & struct[2] == 0){
        layer_x <- rep(layer_x[length(layer_x)], length(layer_x)) # repeat this for last layer
        bias_points(max(bias_x), bias_y, 'B', node_labs, x_range, 
          y_range, circle_cex, cex_val, bord_col, circle_col)
        bias_lines(max(bias_x), bias_y, mod_in, nid = nid, rel_rsc = rel_rsc, all_out = all_out, pos_col = alpha(pos_col, alpha_val), neg_col = alpha(neg_col, alpha_val), y_names = y_names, x_range = x_range, max_sp = max_sp, struct = struct[c(1, length(struct))], y_range = y_range, layer_x = layer_x, line_stag = line_stag)
      }
      
    })
    
  }
  
  #use functions to plot connections between layers
  #bias lines
  if(bias) bias_lines(bias_x, bias_y, mod_in, nid = nid, rel_rsc = rel_rsc, all_out = all_out, pos_col = alpha(pos_col, alpha_val), neg_col = alpha(neg_col, alpha_val), y_names = y_names, x_range = x_range, max_sp = max_sp, struct = struct, y_range = y_range, layer_x = layer_x, line_stag = line_stag)

  #layer lines,  makes use of arguments to plot all or for individual layers
  #starts with input - hidden
  #uses 'all_in' argument to plot connection lines for all input nodes or a single node
  if(is.logical(all_in)){  
    mapply(
      function(x) layer_lines(mod_in, x, layer1 = 1, layer2 = 2, nid = nid, rel_rsc = rel_rsc, 
        all_in = all_in, pos_col = alpha(pos_col, alpha_val), 
        neg_col = alpha(neg_col, alpha_val), x_range = x_range, y_range = y_range, 
        line_stag = line_stag, x_names = x_names, layer_x = layer_x, max_sp = max_sp, struct = struct, 
        prune_col = prune_col, prune_lty = prune_lty),
      1:struct[1]
    )
  }
  else{
    node_in <- which(x_names == all_in)
    layer_lines(mod_in, node_in, layer1 = 1, layer2 = 2, nid = nid, rel_rsc = rel_rsc, all_in = all_in, 
      pos_col = alpha(pos_col, alpha_val), neg_col = alpha(neg_col, alpha_val), 
      x_range = x_range, y_range = y_range, line_stag = line_stag, x_names = x_names, layer_x = layer_x,
      max_sp = max_sp, struct = struct, prune_col = prune_col, prune_lty = prune_lty)
  }
  
  #connections between hidden layers
  lays <- split(c(1, rep(2:(length(struct) - 1), each = 2), length(struct)), 
              f = rep(1:(length(struct) - 1), each = 2))
  lays <- lays[-c(1, (length(struct) - 1))]
  for(lay in lays){
    for(node in 1:struct[lay[1]]){
      layer_lines(mod_in, node, layer1 = lay[1], layer2 = lay[2], nid = nid, rel_rsc = rel_rsc, all_in = TRUE, 
        pos_col = alpha(pos_col, alpha_val), neg_col = alpha(neg_col, alpha_val), 
        x_range = x_range, y_range = y_range, line_stag = line_stag, x_names = x_names, layer_x = layer_x,
        max_sp = max_sp, struct = struct, prune_col = prune_col, prune_lty = prune_lty, skip = skip)
    }
  }
  #lines for hidden - output
  #uses 'all_out' argument to plot connection lines for all output nodes or a single node
  if(is.logical(all_out))
    mapply(
      function(x) layer_lines(mod_in, x, layer1 = length(struct) - 1, layer2 = length(struct), out_layer = TRUE, nid = nid, rel_rsc = rel_rsc, all_in = all_in, pos_col = alpha(pos_col, alpha_val), neg_col = alpha(neg_col, alpha_val), x_range = x_range, y_range = y_range, line_stag = line_stag, x_names = x_names, layer_x = layer_x, max_sp = max_sp, struct = struct, prune_col = prune_col, prune_lty = prune_lty, skip = skip), 
      1:struct[length(struct)]
    )
  else{
    node_in <- which(y_names == all_out)
    layer_lines(mod_in, node_in, layer1 = length(struct) - 1, layer2 = length(struct), out_layer = TRUE, nid = nid, rel_rsc = rel_rsc, pos_col = pos_col, neg_col = neg_col,  x_range = x_range, y_range = y_range, line_stag = line_stag, x_names = x_names, layer_x = layer_x, max_sp = max_sp, struct = struct, prune_col = prune_col, prune_lty = prune_lty, skip = skip)
  }
  
  #use functions to plot nodes
  for(i in 1:length(struct)){
    in_col <- circle_col
    layer_name <- 'H'
    if(i == 1) { layer_name <- 'I'; in_col <- circle_col_inp}
    if(i == length(struct)) layer_name <- 'O'
    layer_points(struct[i], layer_x[i], x_range, layer_name, cex_val, circle_cex, bord_col, in_col, 
      node_labs, line_stag, var_labs, x_names, y_names, max_sp = max_sp, struct = struct, 
      y_range = y_range
      )
  }
  
  if(bias) bias_points(bias_x, bias_y, 'B', node_labs, x_range, 
    y_range, circle_cex, cex_val, bord_col, circle_col)
  
}

#' @rdname plotnet
#' 
#' @export
#' 
#' @method plotnet nnet
plotnet.nnet <- function(mod_in, x_names = NULL, y_names = NULL, skip = FALSE, ...){
  
  # check for skip layers
  chk <- grepl('skip-layer', capture.output(mod_in))
  if(!any(chk)) skip <- FALSE
  
  #get variable names from mod_in object
  #change to user input if supplied
  if(is.null(mod_in$call$formula)){
    xlabs <- colnames(eval(mod_in$call$x))
    ylabs <- colnames(eval(mod_in$call$y))
  }
  else{
    forms <- eval(mod_in$call$formula)
    xlabs <- mod_in$coefnames
    facts <- attr(terms(mod_in), 'factors')
    y_check <- mod_in$fitted
    if(ncol(y_check)>1) ylabs <- colnames(y_check)
    else ylabs <- as.character(forms)[2]
  } 
  if(is.null(x_names)) x_names <- xlabs
  if(is.null(y_names)) y_names <- ylabs

  plotnet.default(mod_in, x_names = x_names, y_names = y_names, skip = skip, ...)

}

#' @rdname plotnet
#'
#' @param struct  numeric vector equal in length to the number of layers in the network.  Each number indicates the number of nodes in each layer starting with the input and ending with the output.  An arbitrary number of hidden layers can be included.
#' 
#' @export
#' 
#' @method plotnet numeric
plotnet.numeric <- function(mod_in, struct, x_names = NULL, y_names = NULL, ...){

  #get variable names from mod_in object
  #change to user input if supplied
  if(is.null(x_names))
    x_names <- paste0(rep('X', struct[1]), seq(1:struct[1]))
  if(is.null(y_names))
    y_names <- paste0(rep('Y', struct[length(struct)]), seq(1:struct[length(struct)]))

  plotnet.default(mod_in, struct = struct, x_names = x_names, y_names = y_names, skip = FALSE, ...)
 
}

#' @rdname plotnet
#' 
#' @export
#' 
#' @method plotnet mlp
plotnet.mlp <- function(mod_in, x_names = NULL, y_names = NULL, prune_col = NULL, prune_lty = 'dashed', ...){

  #get variable names from mod_in object
  all_names <- mod_in$snnsObject$getUnitDefinitions()
  if(is.null(x_names))
    x_names <- all_names[grep('Input', all_names$unitName), 'unitName']
  if(is.null(y_names))
    y_names <- all_names[grep('Output', all_names$unitName), 'unitName']
  
  bias <- FALSE
  
  plotnet.default(mod_in, x_names = x_names, y_names = y_names, bias = bias, prune_col = prune_col, 
    prune_lty = prune_lty, skip = FALSE, ...)
  
}

#' @rdname plotnet
#' 
#' @export
#' 
#' @method plotnet nn
plotnet.nn <- function(mod_in, x_names = NULL, y_names = NULL, ...){
  
  #get variable names from mod_in object
  if(is.null(x_names))
    x_names <- mod_in$model.list$variables
  if(is.null(y_names))
    y_names <- mod_in$model.list$respons

  plotnet.default(mod_in, x_names = x_names, y_names = y_names, skip = FALSE, ...)

}

#' @rdname plotnet
#' 
#' @export
#' 
#' @method plotnet train
plotnet.train <- function(mod_in, x_names = NULL, y_names = NULL, skip = FALSE, ...){
  
  if(is.null(y_names))
    y_names <- strsplit(as.character(mod_in$terms[[2]]), ' + ', fixed = TRUE)[[1]]
  mod_in <- mod_in$finalModel
  if(is.null(x_names))
    x_names <- mod_in$xNames

  # check for skip layers
  chk <- grepl('skip-layer', capture.output(mod_in))
  if(!any(chk)) skip <- FALSE

  plotnet.default(mod_in, x_names = x_names, y_names = y_names, skip = skip, ...)
  
}