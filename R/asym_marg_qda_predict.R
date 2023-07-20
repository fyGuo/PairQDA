#' Asymmetric Marginal QDA (predicting new phenotypes)
#' @import dplyr
#' @import mvtnorm
#'
#' @param qda_model A list output from asym_marg_qda_fit
#' @param test_data_X A vector of predictors from the test data
#' @param Ear1_mark A character shows the last several letters to denote ear 1 label.
#' @param Ear2_mark A character shows the last several letters to denote ear 2 label.
#' @param number_features Number of features (covariates) to fit the model for each ear.
#' @returns Predicted individual level phenotype
#'
#' @export

#' @examples
#' library(dplyr)
#' library(mvtnorm)
#' data(HearingLoss_simu)
#' fit <- asym_marg_qda_fit(HearingLoss_simu,
#'                   id = "id")
#' test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
#' asym_marg_qda_predict(fit, test_data_X)


asym_marg_qda_predict<- function(qda_model = NA,
                                   test_data_X = NA,
                                   Ear1_mark = "_1",
                                   Ear2_mark = "_2",
                                   number_features = 7) {

  test_data_ear1_X <- test_data_X %>%
    dplyr::select(ends_with(Ear1_mark))

  colnames(test_data_ear1_X) <- c(paste("S", 1:7, sep =""))

  test_data_ear2_X <-test_data_X %>%
    dplyr::select(ends_with(Ear2_mark))

  colnames(test_data_ear2_X) <- c(paste("S", 1:7, sep =""))


  predict_ear_1<- QDA_function_2(test_data_ear1_X,
                                 prior = qda_model$prior_ear1,
                                 mu_list_1 = qda_model$mu_list_ear1,
                                 mu_list_2 = qda_model$mu_list_ear1,
                                 var_list_1 = qda_model$var_list_ear1,
                                 var_list_2 = qda_model$var_list_ear1)[[1]]

  predict_ear_2 <- QDA_function(test_data_ear2_X,
                                prior = qda_model$prior_ear2_conditional_ear1[predict_ear_1,],
                                mu_list = qda_model$mu_list_ear2,
                                var_list = qda_model$var_list_ear2)[[1]]


  predicted_valid <- paste(predict_ear_1, predict_ear_2, sep = "_")

  predicted_valid%>% return()
}
