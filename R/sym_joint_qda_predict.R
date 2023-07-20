#' Symmetric Joint QDA (predicting new phenotypes)
#' @import dplyr
#' @import mvtnorm
#'
#' @param qda_model A list output from sym_joint_qda_fit
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
#' fit <- sym_joint_qda_fit(HearingLoss_simu,
#'                   id = "id")
#' test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
#' sym_joint_qda_predict(fit, test_data_X)

sym_joint_qda_predict <- function(qda_model = NA,
                              test_data_X = NA,
                              Ear1_mark = "_1",
                              Ear2_mark = "_2",
                              number_features = 7) {

  # arrange the validation dataset

  test_data_ear1_X <- test_data_X %>%
    dplyr::select(ends_with(Ear1_mark))

  colnames(test_data_ear1_X) <- c(paste("S", 1:7, sep =""))

  test_data_ear2_X <-test_data_X %>%
    dplyr::select(ends_with(Ear2_mark))

  colnames(test_data_ear2_X) <- c(paste("S", 1:7, sep =""))

  probability_log <- matrix(NA, nrow = 4, ncol = 4)
  for (ear1 in 1:4) {
    for (ear2 in 1:4) {
      probability_log[ear1, ear2] <-
        dmvnorm(test_data_ear1_X, qda_model$mu_list_ear1[[ear1]], sigma = qda_model$var_list_ear1[[ear1]], log = T) +
        dmvnorm(test_data_ear2_X, qda_model$mu_list_ear2[[ear2]], sigma = qda_model$var_list_ear2[[ear2]], log = T) +
        log(qda_model$prior_weight[ear1,ear2])
    }
  }
  index <- which.max(probability_log)
  predict_ear_1 <- index %% 4
  predict_ear_2 <- ceiling(index / 4)
  if (predict_ear_1 == 0) predict_ear_1 <- 4
  predicted_valid <- paste(predict_ear_1, predict_ear_2, sep = "_")
  predicted_valid%>% return()
}
