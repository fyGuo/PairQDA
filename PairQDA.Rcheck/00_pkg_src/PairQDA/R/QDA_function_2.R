#' QDA-2 function for conditional on two phenotypes
#' @import mvtnorm

#' @param x A vector of predictors
#' @param prior prior weights of subtypes
#' @param mu_list_1 A list of mean parameters for phenotype 1
#' @param mu_list_2 A list of mean parameters for phenotype 2
#' @param var_list_1 A list of variance-covariance matrices for phenotype 1
#' @param var_list_2 A list of variance-covariance matrices for phenotype 2

QDA_function_2 <- function(x, prior = prior,
                           mu_list_1 = mu_list_1,
                           mu_list_2 = mu_list_2,
                           var_list_1 = var_list_1,
                           var_list_2 = var_list_2) {
  post_p <- numeric(4)
  for (i in 1:4) {
    post_p[i] <- dmvnorm(x, mu_list_1[[i]], sigma = var_list_1[[i]], log = T) +
      dmvnorm(x, mu_list_2[[i]], sigma = var_list_2[[i]], log = T) +
      as.numeric(log(prior[i]))
  }
  type <- which(post_p == max(post_p))
  list(type,
       post_p) %>%
    return()
}
