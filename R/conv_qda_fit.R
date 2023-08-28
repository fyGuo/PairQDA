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
#' @param k The number of classifications.
#' @returns A list to store model parameters.
#'
#' @export

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

conv_qda_fit <- function(train_data,
                        id = "SID",
                        Y1 = "Label_1",
                        Y2 = "Label_2",
                        X1 = c("T500_1", "T1K_1", "T2K_1", "T3K_1",
                               "T4K_1", "T6K_1", "T8K_1"),
                        X2 = c("T500_2", "T1K_2", "T2K_2", "T3K_2",
                                 "T4K_2", "T6K_2", "T8K_2"),
                        method = c("pooled", "separate"),
                        k) {

  # arrange the training dataset
  train_data_ear1 <- train_data[,c(id, Y1, X1)] %>% dplyr::mutate(ear = 1)
  colnames(train_data_ear1) <- c("id", "Y", paste("S", 1:length(X1), sep =""), "ear")


  train_data_ear2 <- train_data[,c(id, Y2, X2)] %>% dplyr::mutate(ear = 2)
  colnames(train_data_ear2) <- c("id", "Y", paste("S", 1:length(X2), sep =""), "ear")

  # excluding rows with missing values
  train_data_ear1 <- train_data_ear1[stats::complete.cases(train_data_ear1),]
  train_data_ear2 <- train_data_ear2[stats::complete.cases(train_data_ear2),]

  # create a pooled dataset

  train_data_pooled <- rbind(train_data_ear1, train_data_ear2)

  if (method == "pooled") {
    mu_list_ear1 <- mu_list_ear2 <- vector(mode = "list", length = k)

    for (h in 1:k) {
      mu_list_ear1[[h]] <- mu_list_ear2[[h]] <-
        train_data_pooled %>% dplyr::filter(.data[["Y"]] == h) %>%
        dplyr::select(paste("S", 1:length(X1), sep ="")) %>%
        colMeans(na.rm = T)
    }

    var_list_ear1 <- var_list_ear2 <- vector(mode = "list", length = k)
    for (h in 1:k) {
      var_list_ear1[[h]] <- var_list_ear2[[h]] <-
        train_data_pooled %>% dplyr::filter(.data[["Y"]] == h) %>%
        dplyr::select(paste("S", 1:length(X1), sep ="")) %>%
        stats::var(na.rm = T)
    }

  } else if(method == "separate") {
    mu_list_ear1 <- vector(mode = "list", length = k)
    for (h in 1:k) {
      mu_list_ear1[[h]] <- train_data_ear1 %>% dplyr::filter(.data[["Y"]] == h)  %>%
        dplyr::select(paste("S", 1:length(X1), sep ="")) %>%
        colMeans(na.rm = T)
    }

    var_list_ear1 <- vector(mode = "list", length = k)
    for (h in 1:k) {
      var_list_ear1[[h]] <- train_data_ear1 %>% dplyr::filter(.data[["Y"]] == h)  %>%
        dplyr::select(paste("S", 1:length(X1), sep ="")) %>%
        stats::var(na.rm = T)
    }

    mu_list_ear2 <- vector(mode = "list", length = k)
    for (h in 1:k) {
      mu_list_ear2[[h]] <- train_data_ear2 %>% dplyr::filter(.data[["Y"]] == h)  %>%
        dplyr::select(paste("S", 1:length(X2), sep ="")) %>%
        colMeans(na.rm = T)
    }

    var_list_ear2 <- vector(mode = "list", length = k)
    for (h in 1:k) {
      var_list_ear2[[h]] <- train_data_ear2 %>% dplyr::filter(.data[["Y"]] == h)  %>%
        dplyr::select(paste("S", 1:length(X2), sep ="")) %>%
        stats::var(na.rm = T)
    }
  }


  # estimate the prior weights
  prior_ear1 <- train_data_ear1 %>%
    summarise(prop1 = mean(.data[["Y"]] == 1, na.rm = T),
              prop2 = mean(.data[["Y"]] == 2, na.rm = T),
              prop3 = mean(.data[["Y"]] == 3, na.rm = T),
              prop4 = mean(.data[["Y"]] == 4, na.rm = T)) %>%
    as.numeric()

  prior_ear2 <- train_data_ear2 %>%
    summarise(prop1 = mean(.data[["Y"]] == 1, na.rm = T),
              prop2 = mean(.data[["Y"]] == 2, na.rm = T),
              prop3 = mean(.data[["Y"]] == 3, na.rm = T),
              prop4 = mean(.data[["Y"]] == 4, na.rm = T)) %>%
    as.numeric()



  parameters <- list(mu_list_ear1 = mu_list_ear1,
                     var_list_ear1 = var_list_ear1,
                     mu_list_ear2 = mu_list_ear2,
                     var_list_ear2 = var_list_ear2,
                     prior_ear1 = prior_ear1,
                     prior_ear2 = prior_ear2)
  return(parameters)
}
