#' Predict Method for Decision Tree model fits
#'
#' @param model DecisionTree Model from package cartlm
#' @param newdata data.frame
#' @param ... 
#'
#' @return numeric vector of y prediction
#' @export
#'
#' @examples
#' mod <- cartLM(len ~ ., data = ToothGrowth)
#' predict(mod, ToothGrowth, method = "mean")
predict.DecisionTree <- function(model, newdata, ...){
  x <- newdata[names(model$is_numeric)]
  predictCart(model$tree, x, ...)
}