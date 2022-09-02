#' CART with Linear Model
#'
#' @param formula model formula
#' @param data data
#' @param max_depth max depth of tree
#' @param min_samples_leaf minimal samples in leaf
#' @param min_samples_split minimal samples in node to split
#' @param min_ig minimal information gain for split node
#' @param metric_func default for regression is variance dan for classification is gini index / gini impurity. other metric for classification is entrophy
#'
#' @return model, feature importance
#' @export
#'
#' @examples
#' cartLM(len ~ ., data = ToothGrowth)
cartLM <- function(
    formula, data, max_depth = 3, min_samples_leaf = 20,
    min_samples_split = min_samples_leaf * 2, min_ig = 1e-3,
    metric_func = NULL
) {
  
  dat <- model.frame(formula, data)
  y <- dat[, 1]
  is_reg <- !(is.factor(y) | is.logical(y) | is.character(y))
  if(is_reg && is.null(metric_func)){
    metric_func <- "var"
  }else if(!is_reg && is.null(metric_func)){
    metric_func <- "gini"
  }
  metric_func <- match.arg(metric_func, c('gini', 'variance', 'entropy'))
  
  if (metric_func == 'gini' & !is_reg) {
    result <- .Call(
      `_cartlm_cartReg`,
      y = y,
      x = dat[,-1],
      max_depth = max_depth,
      min_samples_leaf = min_samples_leaf,
      min_samples_split = min_samples_split,
      min_ig = min_ig
    )
    
  } else if (metric_func == "variance" & is_reg) {
    result <- .Call(
      `_cartlm_cartReg`,
      y = y,
      x = dat[,-1],
      max_depth = max_depth,
      min_samples_leaf = min_samples_leaf,
      min_samples_split = min_samples_split,
      min_ig = min_ig
    )
    
  } else if (metric_func == "entropy" & !is_reg){
    result <- .Call(
      `_cartlm_cartReg`,
      y = y,
      x = dat[,-1],
      max_depth = max_depth,
      min_samples_leaf = min_samples_leaf,
      min_samples_split = min_samples_split,
      min_ig = min_ig
    )
    
  }else {
    stop('metric function tidak sesuai')
  }
  
  class(result) <- "DecisionTree"
  return(invisible(result))
}
