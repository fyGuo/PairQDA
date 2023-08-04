#' Mannual QDA
#' @import mvtnorm

#' @param x A vector of predictors
#' @param prior prior weights of subtypes
#' @param mu_list A list of mean parameters
#' @param var_list A list of variance-covariance matrices

#' @export
QDA_function <- function(x, prior = prior,
                         mu_list = mu_list,
                         var_list = var_list) {
  post_p <- numeric(4)
  for (i in 1:4) {
    post_p[i] <- dmvnorm(x, mu_list[[i]], sigma = var_list[[i]], log = T) +
      as.numeric(log(prior[i]))

  }
  # here we convert p into posterior probabilities so sum(p[i]) = 1
  post_p <- exp(post_p)/sum(exp(post_p))

  type <- which(post_p == max(post_p))
  list(type,
       post_p) %>%
    return()
}
