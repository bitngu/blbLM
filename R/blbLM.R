
#' @title Bag of Little Bootstrap Confidence Interval
#' @description blb.ceof.ci is used to calculate the bag of little bootstrap confidence interval for the linear model
#' @param data a dataframe or tibble object
#' @param y the name of the response variable
#' @param x the name of the indepedent variables
#' @param m the number of group to split
#' @param r the number of iteration for each boot
#' @param alpha the significance level
#'
#' @return returns a matrix of the bootstrap ci
#' @export
#'
#' @examples
#' blb.coef.ci(iris, "Sepal.Length",  "Petal.Width", 10, 15, 0.05)
blb.coef.ci <- function(data, y, x, m, r, alpha){
  ci = blb.coef.ci.list(data, y, x, m, r, alpha)
  purrr::reduce(ci, `+`)/base::length(ci)
}







