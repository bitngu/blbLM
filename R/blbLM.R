
#' @title Bag of Little Bootstrap Confidence Interval for Linear Regression Estimators
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
#' blb.coef.ci(iris, "Sepal.Length", c("Sepal.Width", "Petal.Width"), 10, 15, 0.05)
blb.coef.ci <- function(data, y, x, m, r, alpha){
  ci <- blb.coef.ci.list(data, y, x, m, r, alpha)
  avg_ci(ci)
}

#' Bag of Little Bootstrap Confidence Interval for Sigma (Linear Model)
#'
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
#' blb.sigma.ci(iris, "Sepal.Length",  "Petal.Width", 10, 15, 0.05)
#' blb.sigma.ci(iris, "Sepal.Length", c("Sepal.Width", "Petal.Width"), 10, 15, 0.05)
blb.sigma.ci <- function(data, y, x, m, r, alpha){
  ci <- blb.sigma.ci.list(data, y, x, m, r, alpha)
  avg_ci(ci)
}


#' Bag of Little Bootstrap Confidence Interval for Prediction (Linear Model)
#'
#' @param data a dataframe or tibble object
#' @param pred_df a dataframe or tibble object with corresponding names for indepedent variables
#' @param y the name of the response variable
#' @param x the name(s) of the indepedent variables
#' @param m the number of group to split
#' @param r the number of iteration for each boot
#' @param alpha the significance level
#'
#' @return returns a matrix of the bootstrap ci
#' @export
#'
#' @examples
#'pred_df <- tibble(Sepal.Length = c(1,2,3,4,5), Sepal.Width = c(1,2,3,4,5))
#'blb.pred.ci(iris, df, "Petal.Width", c("Sepal.Length", "Sepal.Width"), 10, 15, 0.05)

blb.pred.ci<- function(data, pred_df, y, x, m, r, alpha){
  ci <- blb.pred.ci.list(data, pred_df, y, x, m, r, alpha)
  avg_ci(ci)
}





