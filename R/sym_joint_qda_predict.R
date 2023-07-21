#' Symmetric Joint QDA (predicting new phenotypes)
#' @import dplyr
#' @import mvtnorm
#'
#' @param qda_model A list output from sym_joint_qda_fit
#' @param test_data_X A vector of predictors from the test data
#' @param X1 A string of characters for the column names of predictor set 1.
#' @param X2 A string of characters for the column names of predictor set 2.
#'
#' @returns Predicted individual level phenotype
#'
#' @export

#' @examples
#' library(dplyr)
#' library(mvtnorm)
#' data(HearingLoss_simu)
#' fit <- sym_joint_qda_fit(HearingLoss_simu,
#'                   id = "id",
#'                   method = "separate")
#' test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
#' sym_joint_qda_predict(fit, test_data_X)

sym_joint_qda_predict <- function(qda_model = NA,
                              test_data_X = NA,
                              X1 = c("T500_1", "T1K_1", "T2K_1", "T3K_1",
                                     "T4K_1", "T6K_1", "T8K_1"),
                              X2 = c("T500_2", "T1K_2", "T2K_2", "T3K_2",
                                     "T4K_2", "T6K_2", "T8K_2")) {

  # arrange the validation dataset

  test_data_ear1_X <- test_data_X[,X1]

  colnames(test_data_ear1_X) <- c(paste("S", 1:length(X1), sep =""))

  test_data_ear2_X <-test_data_X[,X2]

  colnames(test_data_ear2_X) <- c(paste("S", 1:length(X2), sep =""))

  probability_log <- matrix(NA, nrow = qda_model$k, ncol = qda_model$k)
  for (ear1 in 1:qda_model$k) {
    for (ear2 in 1:qda_model$k) {
      probability_log[ear1, ear2] <-
        dmvnorm(test_data_ear1_X, qda_model$mu_list_ear1[[ear1]], sigma = qda_model$var_list_ear1[[ear1]], log = T) +
        dmvnorm(test_data_ear2_X, qda_model$mu_list_ear2[[ear2]], sigma = qda_model$var_list_ear2[[ear2]], log = T) +
        log(qda_model$prior_weight[ear1,ear2])
    }
  }
  index <- which.max(probability_log)
  predict_ear_1 <- index %% qda_model$k
  predict_ear_2 <- ceiling(index / qda_model$k)
  if (predict_ear_1 == 0) predict_ear_1 <- qda_model$k
  predicted_valid <- paste(predict_ear_1, predict_ear_2, sep = "_")
  predicted_valid%>% return()
}
