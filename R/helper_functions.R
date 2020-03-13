#groupSplit is a helper function that seperates a dataframe or tibble like format into m groups
#data -> dataframe or tibble
#m -> number of groups the user want to spot
#return a list of dataframes

#'@keywords internal
groupSplit<- function(data, m){
  1:m %>% purrr::map(~dplyr::sample_n(data, size = nrow(data)/m))
}


#multFreq is a helper function that calculates the multinomial distribution based
#on the original size of data, n, and the size of the sub sample
#n -> an integer that represent the original data size
#subSample-> dataframe or tibble of the partitioned of the original data
#returns a matrix of [1:n,1] frequency

#'@keywords internal
each_boot <- function(subSample, n, r, y, x){
  freq = stats::rmultinom(1, n, rep(1, nrow(subSample)))
  lm(stats::formula(subSample[c(y,x)]), data = subSample, weights = freq)
}


#Helper function that calculates the bootstrap percentile for sigma, coefficients, etc
#'@keywords internal
perc.conf.int <- function (list_obj, alpha){
    apply(list_obj, MARGIN = 2, stats::quantile, probs = c(alpha/2, 1- alpha/2))
}

#Helper function that calculates the boostrap percentile for each group
#returns a list of the percentile confidence interval
#'@keywords internal
blb.coef.ci.list <- function(data, y, x, m, r, alpha){
  subSamples = groupSplit(data, m)
  subSamples %>% purrr::map(function(df){
    #I need to do iteration
    1:r %>% purrr::map(~each_boot(df, base::nrow(df), ., y, x))} %>%
      purrr::map(~.$coefficient) %>%
      purrr::reduce(base::rbind) %>%
      perc.conf.int(., alpha)
    )
}

