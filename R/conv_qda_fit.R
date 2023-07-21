#' Conventional QDA (fitting model)
#' @import dplyr
#' @importFrom MASS qda
#'
#' @param train_data A data.frame for the training dataset
#' @param id A character for the column name which stores id.
#' @param Y1 A character shows the column name of label 1.
#' @param Y2 A character shows the column name of label 2.
#' @param X1 A string of characters for the column names of predictor set 1.
#' @param X2 A string of characters for the column names of predictor set 2.
#' @param method The method user set to estimate mean and covariance parameters.
#' @returns A list to store model parameters.
#'
#' @export

#' @examples
#' library(dplyr)
#' library(mvtnorm)
#' data(HearingLoss_simu)
#' fit <- conv_qda_fit(HearingLoss_simu,
#'                   id = "id",
#'                   method = "separate")
#' test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
#' conv_qda_predict(fit, test_data_X)

conv_qda_fit <- function(train_data,
                        id = "SID",
                        Y1 = "Label_1",
                        Y2 = "Label_2",
                        X1 = c("T500_1", "T1K_1", "T2K_1", "T3K_1",
                               "T4K_1", "T6K_1", "T8K_1"),
                        X2 = c("T500_2", "T1K_2", "T2K_2", "T3K_2",
                                 "T4K_2", "T6K_2", "T8K_2"),
                        method = c("pooled", "separate")) {

  train_data_ear1 <- train_data %>% dplyr::select(all_of(c(X1, Y1)))

  train_data_ear1 <- train_data_ear1[stats::complete.cases(train_data_ear1),]

  colnames(train_data_ear1) <- c(paste0("S", 1:length(X1)), "Y")

  train_data_ear2 <- train_data  %>% dplyr::select(all_of(c(X2, Y2)))

  train_data_ear2 <- train_data_ear2[stats::complete.cases(train_data_ear2),]

  colnames(train_data_ear2) <- c(paste0("S", 1:length(X1)), "Y")


  if (method == "pooled") {
    # if the user set "pooled" method to estimate mean and covariance parameters
    # then two ears will be combined together

    train_data_pooled <- rbind(train_data_ear1, train_data_ear2)

    qda1 <- qda(x = train_data_pooled[,paste0("S", 1:7)],
                grouping = train_data_pooled$Y)


    qda2 <- qda(x = train_data_pooled[,paste0("S", 1:7)],
                grouping = train_data_pooled$Y)

  } else if (method == "separate") {

    # qda model on ear1
    qda1 <- qda(x = train_data_ear1[,paste0("S", 1:7)],
                grouping = train_data_ear1$Y)


    qda2 <- qda(x = train_data_ear2[,paste0("S", 1:7)],
                grouping = train_data_ear2$Y)

  }

  parameters <- list(qda1 = qda1,
                     qda2 = qda2)
  return(parameters)
}
