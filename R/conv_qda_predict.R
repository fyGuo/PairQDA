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
#'                   id = "id",
#'                   method = "separate",
#'                   k = 4)
#' test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
#' conv_qda_predict(fit, test_data_X)

conv_qda_predict <- function(qda_model = NA,
                             test_data_X = NA,
                             X1 = c("T500_1", "T1K_1", "T2K_1", "T3K_1",
                                                     "T4K_1", "T6K_1", "T8K_1"),
                             X2 = c("T500_2", "T1K_2", "T2K_2", "T3K_2",
                                    "T4K_2", "T6K_2", "T8K_2")) {

  test_data_ear1_X <- test_data_X[,X1]

  colnames(test_data_ear1_X) <- c(paste("S", 1:length(X1), sep =""))

  test_data_ear2_X <-test_data_X[,X2]

  colnames(test_data_ear2_X) <- c(paste("S", 1:length(X2), sep =""))


  predict_ear_1<- QDA_function(test_data_ear1_X,
                                 prior = qda_model$prior_ear1,
                                 mu_list = qda_model$mu_list_ear1,
                                 var_list = qda_model$var_list_ear1)[[1]]

  predict_ear_2 <- QDA_function(test_data_ear2_X,
                                prior = qda_model$prior_ear2,
                                mu_list = qda_model$mu_list_ear2,
                                var_list = qda_model$var_list_ear2)[[1]]


  predicted_valid <- paste(predict_ear_1, predict_ear_2, sep = "_")

  predicted_valid%>% return()}
