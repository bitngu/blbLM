#groupSplit is a helper function that seperates a dataframe or tibble like format into m groups
#data -> dataframe or tibble
#m -> number of groups the user want to spot
#return a list of dataframes
#'@keywords internal
groupSplit<- function(data, m){
  1:m %>% purrr::map(~dplyr::sample_n(data, size <- nrow(data)/m))
}


#each_boot helper function that calculates linear regression based on weights using mutlinomial distribution
#subSample -> represent the subsample dataframe from the split
#n -> represent the size of the original dataset
#r -> represent the number of iteration
#y -> represent the response variable
#x -> represent the indepedent variables
#returns an lm object
#'@keywords internal
each_boot <- function(subSample, n, r, y, x){
  subSample <- subSample[stats::complete.cases(subSample), ]
  freq <- stats::rmultinom(1, n, base::rep(1, base::nrow(subSample)))
  stats::lm(stats::formula(subSample[c(y,x)]), data <- subSample, weights = freq)
}

#each_boot_pred is a helper function that calculates prediction of a linear model based
#on weights from the mutlinomial distribution
#subSample -> represent the subsample dataframe from the split
#n -> represent the size of the original dataset
#r -> represent the number of iteration
#y -> represent the response variable
#x -> represent the indepedent variables
#pred_df -> represent the dataframe with the values to predict
#returns the prediction values
#'@keywords internal
each_boot_pred <- function(subSample, n, r, y, x, pred_df){
  subSample <- subSample[stats::complete.cases(subSample), ]
  freq <- stats::rmultinom(1, n, base::rep(1, base::nrow(subSample)))
  fit <- stats::lm(stats::formula(subSample[c(y,x)]), data = subSample, weights = freq)
  stats::predict(fit, pred_df)
}


#perc.conf.int is a helper function that calculates the bootstrap percentile for sigma, coefficients, etc
#returns list of list corresponding to the confidence interval based on alpha
#'@keywords internal
perc.conf.int <- function (list_obj, alpha){
    apply(list_obj, MARGIN = 2, stats::quantile, probs <- c(alpha/2, 1- alpha/2))
}

#blb.coef.ci.list is a helper function that calculates the boostrap percentile for the regression estimator
#returns a list of the percentile confidence interval
#'@keywords internal
blb.coef.ci.list <- function(data, y, x, m, r = 10, alpha = 0.05, parallel = FALSE, n_cores = 4){
  subSamples <- groupSplit(data, m)
  if (parallel){
    future::plan(future::multiprocess, workers = n_cores)
    subSamples %>% furrr::future_map(function(df){
      1:r %>% purrr::map(~each_boot(df, base::nrow(data), ., y, x))} %>%
        purrr::map(~.$coefficient) %>%
        purrr::reduce(base::rbind) %>%
        perc.conf.int(., alpha)
    )
  }
  subSamples %>% purrr::map(function(df){
    1:r %>% purrr::map(~each_boot(df, base::nrow(data), ., y, x))} %>%
      purrr::map(~.$coefficient) %>%
      purrr::reduce(base::rbind) %>%
      perc.conf.int(., alpha)
    )
}

#blb.coef.ci.list is a helper function that calculates the bootstrap percentile for sigma
#returns a list of the percentile confidence interval
blb.sigma.ci.list <- function(data, y, x, m, r = 10, alpha = 0.05,  parallel = FALSE, n_cores){
  subSamples <- groupSplit(data, m)
  if (parallel) {
    future::plan(future::multiprocess, workers = n_cores)
    subSamples %>% furrr::future_map(function(df){
      1:r %>% purrr::map(~each_boot(df, nrow(data),., y,x))} %>%
        purrr::map(base::summary) %>% purrr::map(~.$sigma)) %>%
      purrr::map(base::unlist) %>%
      purrr::map(stats::quantile, probs <- c(alpha/2, 1- alpha/2))
  }
  else{
  subSamples %>% purrr::map(function(df){
    1:r %>% purrr::map(~each_boot(df, nrow(data),., y,x))} %>%
      purrr::map(base::summary) %>% purrr::map(~.$sigma)) %>%
    purrr::map(base::unlist) %>%
    purrr::map(stats::quantile, probs <- c(alpha/2, 1- alpha/2))
  }
}

#blb.pred.ci.list is a helper function that calculates the bootstrap percentile for the prediction values
#returns a list of the percentile confidence interval
blb.pred.ci.list <- function(data, pred_df, y, x, m, r, alpha = 0.05, parallel = FALSE, n_core = 4){
  subSamples <- groupSplit(data, m)
  if (parallel){
    future::plan(future::multiprocess, n_core)
    subSamples %>% furrr::future_map(function(df){
      1:r %>% purrr::map(~each_boot_pred(df, nrow(data), ., y, x, pred_df))
    }) %>% purrr::map(purrr::reduce, rbind) %>%
      purrr::map(perc.conf.int, alpha)
  }
  else{
  subSamples %>% purrr::map(function(df){
    1:r %>% purrr::map(~each_boot_pred(df, nrow(data), ., y, x, pred_df))
  }) %>% purrr::map(purrr::reduce, rbind) %>%
    purrr::map(perc.conf.int, alpha)
  }
}



#avg_ci is a helper function that calculates the average percentile of the bootstrap methods
avg_ci <- function(ci){
  purrr::reduce(ci, `+`)/base::length(ci)
}

