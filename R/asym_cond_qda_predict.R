#' Asymmetric Conditional QDA (predicting new phenotypes)
#' @import dplyr
#' @import mvtnorm
#'
#' @param qda_model A list output from sym_joint_qda_fit
#' @param test_data_X A vector of predictors from the test data
#' @param X1 A string of characters for the column names of predictor set 1.
#' @param X2 A string of characters for the column names of predictor set 2.
#' @param iter Number of interations set by users. The default is 1000 time.
#'
#' @returns Predicted individual level phenotype
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(mvtnorm)
#' data(HearingLoss_simu)
#' fit <- asym_cond_qda_fit(HearingLoss_simu,
#'                   id = "id",
#'                   method = "pooled")
#' test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
#' asym_cond_qda_predict(fit, test_data_X)

asym_cond_qda_predict <- function(qda_model = NA,
                                  test_data_X = NA,
                                  X1 = c("T500_1", "T1K_1", "T2K_1", "T3K_1",
                                         "T4K_1", "T6K_1", "T8K_1"),
                                  X2 = c("T500_2", "T1K_2", "T2K_2", "T3K_2",
                                         "T4K_2", "T6K_2", "T8K_2"),
                                   iter = 1000) {

  test_data_ear1_X <- test_data_X[,X1]

  colnames(test_data_ear1_X) <- c(paste("S", 1:length(X1), sep =""))

  test_data_ear2_X <-test_data_X[,X2]

  colnames(test_data_ear2_X) <- c(paste("S", 1:length(X2), sep =""))
  ## Conduct iteration
  predict_ear_1 <- numeric(iter)
  predict_ear_2 <- numeric(iter)

  for (i in 1:iter) {
    j <- max(i-1, 1)
    if (i == 1) {
      predict_ear_1[i]<- QDA_function_2(test_data_ear1_X,
                                        prior = qda_model$prior_ear1,
                                        mu_list_1 = qda_model$mu_list_ear1,
                                        mu_list_2 = qda_model$mu_list_ear1,
                                        var_list_1 = qda_model$var_list_ear1,
                                        var_list_2 = qda_model$var_list_ear1)[[1]]
    } else {
      predict_ear_1[i]<- QDA_function_2(test_data_ear1_X,
                                        prior = qda_model$prior_ear1,
                                        mu_list_1 = qda_model$mu_list_ear1,
                                        mu_list_2 = qda_model$mu_list_ear1_Cond21[[predict_ear_2[j]]],
                                        var_list_1 = qda_model$var_list_ear1,
                                        var_list_2 = qda_model$var_list_ear1_Cond21[[predict_ear_2[j]]])[[1]]
    }
    predict_ear_2[i] <- QDA_function(test_data_ear2_X,
                                     prior = qda_model$prior_ear2_conditional_ear1[predict_ear_1[i],],
                                     mu_list = qda_model$mu_list_ear2_Cond12[[predict_ear_1[i]]],
                                     var_list = qda_model$var_list_ear2_Cond12[[predict_ear_1[[i]]]]
    )[[1]]
    if(i> 1 & predict_ear_1[[j]] == predict_ear_1[[i]] &
       predict_ear_2[[j]] == predict_ear_2[[i]]) {break}
  }
  if (i < iter) {
    predicted_valid <- paste(predict_ear_1[[i]], predict_ear_2[[i]], sep = "_")
  } else {
    vector_prediction <- paste(predict_ear_1, predict_ear_2, sep = "_")
    vector_prediction %>% table %>% sort(decreasing = T)
    predicted_valid <- vector_prediction[1]
  }
  phenotypes <-predicted_valid

  phenotypes%>% return()
}
