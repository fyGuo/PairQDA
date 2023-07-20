#' Conventional QDA (predicting new phenotypes)
#' @import dplyr
#' @importFrom utils getFromNamespace
#' @importFrom MASS qda
#'
#' @param qda_model A list output from conv_qda_predict
#' @param test_data_X A vector of predictors from the test data
#' @param X1 A string of characters for the column names of predictor set 1.
#' @param X2 A string of characters for the column names of predictor set 2.
#' @returns Predicted individual level phenotype
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(mvtnorm)
#' data(HearingLoss_simu)
#' fit <- conv_qda_fit(HearingLoss_simu,
#'                   id = "id")
#' test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
#' conv_qda_predict(fit, test_data_X)

conv_qda_predict <- function(qda_model = NA,
                             test_data_X = NA,
                             X1 = c("T500_1", "T1K_1", "T2K_1", "T3K_1",
                                                     "T4K_1", "T6K_1", "T8K_1"),
                             X2 = c("T500_2", "T1K_2", "T2K_2", "T3K_2",
                                    "T4K_2", "T6K_2", "T8K_2")) {

  predict.qda <- getFromNamespace("predict.qda","MASS")

  pred_ear1 <- predict.qda(qda_model$qda1, test_data_X[,X1])$class

  pred_ear2 <- predict.qda(qda_model$qda2, test_data_X[,X2])$class

  prediction <- paste(pred_ear1, pred_ear2, sep = "_")
  return(prediction)
}
