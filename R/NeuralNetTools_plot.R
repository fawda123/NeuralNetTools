#' Plot a neural network model
#'
#' @param mod_in 
#' @param nid 
#' @param all_out 
#' @param all_in
#' @param bias
#' @param wts_only
#' @param rel_rsc
#' @param circle_cex
#' @param node_labs
#' @param var_labs
#' @param x_lab
#' @param y_lab
#' @param line_stag
#' @param struct
#' @param cex_val
#' @param alpha_val
#' @param circle_col
#' @param pos_col
#' @param neg_col
#' @param bor_col
#' @param max_sp
#' @param ...
#' 
#' @export alpha
#' 
#' @import scales
#' 
#' @references
#' Odezmi
#' 
#' @return A graphics object unless \code{wts_only = T}, then neural network weights from \code{\link{neuralweights}}.
#' 
#' @examples 
#' ## using numeric input
#' 
#' wts_in <- c(13.12, 1.49, 0.16, -0.11, -0.19, -0.16, 0.56, -0.52, 0.81)
#' struct <- c(2, 2, 1) #two inputs, two hidden, one output 
#' 
#' plot.nnet(wts_in, struct = struct)
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
#' plot(mod)  
#' 
#' ## using RSNNS, no bias layers
#' 
#' library(RSNNS)
#' 
#' x <- neuraldat[, c('X1', 'X2', 'X3')]
#' y <- neuraldat[, 'Y1']
#' mod <- mlp(x, y, size = 5)
#' 
#' garson(mod, 'Y1')
#' 
#' ## using neuralnet
#' 
#' library(neuralnet)
#' 
#' mod <- neuralnet(Y1 ~ X1 + X2 + X3, data = neuraldat, hidden = 5)
#' 
#' garson(mod, 'Y1')
plot.nnet <- function(mod_in, nid = T, all_out = T, all_in = T, bias = T, wts_only = F, rel_rsc = 5, circle_cex = 5, node_labs = T, var_labs = T, x_lab = NULL, y_lab = NULL, line_stag = NULL, struct = NULL, cex_val = 1, alpha_val = 1, circle_col = 'lightblue', pos_col = 'black', neg_col = 'grey', bord_col = 'lightblue', max_sp = F, ...){
  
  #sanity checks
  if('mlp' %in% class(mod_in)) warning('Bias layer not applicable for rsnns object')
  if('numeric' %in% class(mod_in)){
    if(is.null(struct)) stop('Three-element vector required for struct')
    if(length(mod_in) != ((struct[1] * struct[2] + struct[2] * struct[3]) + (struct[3] + struct[2])))
      stop('Incorrect length of weight matrix for given network structure')
  }
  if('train' %in% class(mod_in)){
    if('nnet' %in% class(mod_in$finalModel)){
      mod_in <- mod_in$finalModel
      warning('Using best nnet model from train output')
    }
    else stop('Only nnet method can be used with train object')
  }
  
  wts <- neuralweights(mod_in, struct = struct)
  
  if(wts_only) return(wts)
  
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
  
  #get variable names from mod_in object
  #change to user input if supplied
  if('numeric' %in% class(mod_in)){
    x_names <- paste0(rep('X', struct[1]), seq(1:struct[1]))
    y_names <- paste0(rep('Y', struct[3]), seq(1:struct[3]))
  }
  if('mlp' %in% class(mod_in)){
    all_names <- mod_in$snnsObject$getUnitDefinitions()
    x_names <- all_names[grep('Input', all_names$unitName), 'unitName']
    y_names <- all_names[grep('Output', all_names$unitName), 'unitName']
  }
  if('nn' %in% class(mod_in)){
    x_names <- mod_in$model.list$variables
    y_names <- mod_in$model.list$respons
  }
  if('xNames' %in% names(mod_in)){
    x_names <- mod_in$xNames
    y_names <- attr(terms(mod_in), 'factor')
    y_names <- row.names(y_names)[!row.names(y_names) %in% x_names]
  }
  if(!'xNames' %in% names(mod_in) & 'nnet' %in% class(mod_in)){
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
  }
  #change variables names to user sub 
  if(!is.null(x_lab)){
    if(length(x_names) != length(x_lab)) stop('x_lab length not equal to number of input variables')
    else x_names <- x_lab
  }
  if(!is.null(y_lab)){
    if(length(y_names) != length(y_lab)) stop('y_lab length not equal to number of output variables')
    else y_names <- y_lab
  }
  
  #initiate plot
  plot(x_range, y_range, type = 'n', axes = F, ylab = '', xlab = '', ...)
  
  #function for getting y locations for input, hidden, output layers
  #input is integer value from 'struct'
  get_ys <- function(lyr, max_space = max_sp){
    if(max_space){ 
      spacing <- diff(c(0 * diff(y_range), 0.9 * diff(y_range)))/lyr
    } else {
      spacing <- diff(c(0 * diff(y_range), 0.9 * diff(y_range)))/max(struct)
    }
    
    seq(0.5 * (diff(y_range) + spacing * (lyr - 1)), 0.5 * (diff(y_range) - spacing * (lyr - 1)), 
        length = lyr)
  }
  
  #function for plotting nodes
  #'layer' specifies which layer, integer from 'struct'
  #'x_loc' indicates x location for layer, integer from 'layer_x'
  #'layer_name' is string indicating text to put in node
  layer_points <- function(layer, x_loc, layer_name, cex = cex_val){
    x <- rep(x_loc * diff(x_range), layer)
    y <- get_ys(layer)
    points(x, y, pch = 21, cex = circle_cex, col = bord_col, bg = in_col)
    if(node_labs) text(x, y, paste(layer_name, 1:layer, sep = ''), cex = cex_val)
    if(layer_name == 'I' & var_labs) text(x - line_stag * diff(x_range), y, x_names, pos = 2, cex = cex_val)      
    if(layer_name == 'O' & var_labs) text(x + line_stag * diff(x_range), y, y_names, pos = 4, cex = cex_val)
  }
  
  #function for plotting bias points
  #'bias_x' is vector of values for x locations
  #'bias_y' is vector for y location
  #'layer_name' is  string indicating text to put in node
  bias_points <- function(bias_x, bias_y, layer_name, cex, ...){
    for(val in 1:length(bias_x)){
      points(
        diff(x_range) * bias_x[val], 
        bias_y * diff(y_range), 
        pch = 21, col = bord_col, bg = in_col, cex = circle_cex
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
  
  #function creates lines colored by direction and width as proportion of magnitude
  #use 'all_in' argument if you want to plot connection lines for only a single input node
  layer_lines <- function(mod_in, h_layer, layer1 = 1, layer2 = 2, out_layer = F, nid, rel_rsc, all_in, pos_col, neg_col, ...){
    
    x0 <- rep(layer_x[layer1] * diff(x_range) + line_stag * diff(x_range), struct[layer1])
    x1 <- rep(layer_x[layer2] * diff(x_range) - line_stag * diff(x_range), struct[layer1])
    
    if(out_layer == T){
      
      y0 <- get_ys(struct[layer1])
      y1 <- rep(get_ys(struct[layer2])[h_layer], struct[layer1])
      src_str <- paste('out', h_layer)
      
      wts <- neuralweights(mod_in, rel_rsc, struct = struct)
      wts <- wts[grep(src_str, names(wts))][[1]][-1]
      wts_rs <- neuralweights(mod_in, rel_rsc, struct = struct)
      wts_rs <- wts_rs[grep(src_str, names(wts_rs))][[1]][-1]
      
      cols <- rep(pos_col, struct[layer1])
      cols[wts<0] <- neg_col
      
      if(nid) segments(x0, y0, x1, y1, col = cols, lwd = wts_rs)
      else segments(x0, y0, x1, y1)
      
    }
    
    else{
      
      if(is.logical(all_in)) all_in <- h_layer
      else all_in <- which(x_names == all_in)
      
      y0 <- rep(get_ys(struct[layer1])[all_in], struct[2])
      y1 <- get_ys(struct[layer2])
      src_str <- paste('hidden', layer1)
      
      wts <- neuralweights(mod_in, rel_rsc, struct = struct)
      wts <- unlist(lapply(wts[grep(src_str, names(wts))], function(x) x[all_in + 1]))
      wts_rs <- neuralweights(mod_in, rel_rsc, struct = struct)
      wts_rs <- unlist(lapply(wts_rs[grep(src_str, names(wts_rs))], function(x) x[all_in + 1]))
      
      cols <- rep(pos_col, struct[layer2])
      cols[wts<0] <- neg_col
      
      if(nid) segments(x0, y0, x1, y1, col = cols, lwd = wts_rs)
      else segments(x0, y0, x1, y1)
      
    }
    
  }
  
  bias_lines <- function(bias_x, mod_in, nid, rel_rsc, all_out, pos_col, neg_col, ...){
    
    if(is.logical(all_out)) all_out <- 1:struct[length(struct)]
    else all_out <- which(y_names == all_out)
    
    for(val in 1:length(bias_x)){
      
      wts <- neuralweights(mod_in, rel_rsc, struct = struct)
      wts_rs <- neuralweights(mod_in, rel_rsc, struct = struct)
      
      if(val != length(bias_x)){
        wts <- wts[grep('out', names(wts), invert = T)]
        wts_rs <- wts_rs[grep('out', names(wts_rs), invert = T)]
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
      
      if(nid == F){
        wts_rs <- rep(1, struct[val + 1])
        cols <- rep('black', struct[val + 1])
      }
      
      if(val != length(bias_x)){
        segments(
          rep(diff(x_range) * bias_x[val] + diff(x_range) * line_stag, struct[val + 1]), 
          rep(bias_y * diff(y_range), struct[val + 1]), 
          rep(diff(x_range) * layer_x[val + 1] - diff(x_range) * line_stag, struct[val + 1]), 
          get_ys(struct[val + 1]), 
          lwd = wts_rs, 
          col = cols
        )
      }
      
      else{
        segments(
          rep(diff(x_range) * bias_x[val] + diff(x_range) * line_stag, struct[val + 1]), 
          rep(bias_y * diff(y_range), struct[val + 1]), 
          rep(diff(x_range) * layer_x[val + 1] - diff(x_range) * line_stag, struct[val + 1]), 
          get_ys(struct[val + 1])[all_out], 
          lwd = wts_rs[all_out], 
          col = cols[all_out]
        )
      }
      
    }
  }
  
  #use functions to plot connections between layers
  #bias lines
  if(bias) bias_lines(bias_x, mod_in, nid = nid, rel_rsc = rel_rsc, all_out = all_out, pos_col = scales::alpha(pos_col, alpha_val), neg_col = scales::alpha(neg_col, alpha_val))
  
  #layer lines,  makes use of arguments to plot all or for individual layers
  #starts with input - hidden
  #uses 'all_in' argument to plot connection lines for all input nodes or a single node
  if(is.logical(all_in)){  
    mapply(
      function(x) layer_lines(mod_in, x, layer1 = 1, layer2 = 2, nid = nid, rel_rsc = rel_rsc, all_in = all_in, pos_col = scales::alpha(pos_col, alpha_val), neg_col = scales::alpha(neg_col, alpha_val)), 
      1:struct[1]
    )
  }
  else{
    node_in <- which(x_names == all_in)
    layer_lines(mod_in, node_in, layer1 = 1, layer2 = 2, nid = nid, rel_rsc = rel_rsc, all_in = all_in, 
                pos_col = scales::alpha(pos_col, alpha_val), neg_col = scales::alpha(neg_col, alpha_val))
  }
  #connections between hidden layers
  lays <- split(c(1, rep(2:(length(struct) - 1), each = 2), length(struct)), 
              f = rep(1:(length(struct) - 1), each = 2))
  lays <- lays[-c(1, (length(struct) - 1))]
  for(lay in lays){
    for(node in 1:struct[lay[1]]){
      layer_lines(mod_in, node, layer1 = lay[1], layer2 = lay[2], nid = nid, rel_rsc = rel_rsc, all_in = T, 
                  pos_col = scales::alpha(pos_col, alpha_val), neg_col = scales::alpha(neg_col, alpha_val))
    }
  }
  #lines for hidden - output
  #uses 'all_out' argument to plot connection lines for all output nodes or a single node
  if(is.logical(all_out))
    mapply(
      function(x) layer_lines(mod_in, x, layer1 = length(struct) - 1, layer2 = length(struct), out_layer = T, nid = nid, rel_rsc = rel_rsc, all_in = all_in, pos_col = scales::alpha(pos_col, alpha_val), neg_col = scales::alpha(neg_col, alpha_val)), 
      1:struct[length(struct)]
    )
  else{
    node_in <- which(y_names == all_out)
    layer_lines(mod_in, node_in, layer1 = length(struct) - 1, layer2 = length(struct), out_layer = T, nid = nid, rel_rsc = rel_rsc, 
                pos_col = pos_col, neg_col = neg_col, all_out = all_out)
  }
  
  #use functions to plot nodes
  for(i in 1:length(struct)){
    in_col <- circle_col
    layer_name <- 'H'
    if(i == 1) { layer_name <- 'I'; in_col <- circle_col_inp}
    if(i == length(struct)) layer_name <- 'O'
    layer_points(struct[i], layer_x[i], layer_name)
  }
  
  if(bias) bias_points(bias_x, bias_y, 'B')
  
}
