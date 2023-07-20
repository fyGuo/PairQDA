#' Symmetric Bayesian QDA (fitting model)
#' @import dplyr
#' @importFrom MASS qda

#' @param train_data A data.frame for the training dataset
#' @param id A character for the column name which stores id.
#' @param Ear1_mark A character shows the last several letters to denote ear 1 label.
#' @param Ear2_mark A character shows the last several letters to denote ear 2 label.
#' @param Y A character denotes the first several letters of label.
#' @param number_features Number of features (covariates) to fit the model for each ear.
#' @returns A list to store model parameters.
#'
#' @export

#' @examples
#' library(dplyr)
#' library(mvtnorm)
#' library(MASS)
#' data(HearingLoss_simu)
#' fit <- sym_bayes_qda_fit(HearingLoss_simu,
#'                   id = "id")
#' test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
#' sym_bayes_qda_predict(fit, test_data_X)

sym_bayes_qda_fit <- function(train_data,
                        id = "SID",
                        Ear1_mark = "_1",
                        Ear2_mark = "_2",
                        Y = "Label",
                        number_features = 7) {

  # arrange the training dataset
  train_data_ear1 <- train_data %>% dplyr::select(
    all_of(c(id, paste0(Y,Ear1_mark))), ends_with(Ear1_mark)
  ) %>% mutate(ear = 1)
  colnames(train_data_ear1) <- c("SID", "Y", paste("S", 1:7, sep =""), "ear")


  train_data_ear2 <- train_data %>% dplyr::select(
    all_of(c(id, paste0(Y,Ear2_mark))), ends_with(Ear2_mark)
  ) %>% mutate(ear = 2)
  colnames(train_data_ear2) <- c("SID", "Y", paste("S", 1:7, sep =""), "ear")

  # prior weights
  prior_ear1 <- train_data_ear1 %>%
    summarise(prop1 = mean(Y == 1, na.rm = T),
              prop2 = mean(Y == 2, na.rm = T),
              prop3 = mean(Y == 3, na.rm = T),
              prop4 = mean(Y == 4, na.rm = T))

  prior_ear1_conditional_ear2 <-
    table(train_data_ear1$Y, train_data_ear2$Y, dnn = c("Ear1", "Ear2")) %>%
    prop.table(margin = 2) %>%
    as.matrix()


  prior_ear2_conditional_ear1 <-
    table(train_data_ear1$Y, train_data_ear2$Y, dnn = c("Ear1", "Ear2")) %>%
    prop.table(margin = 1) %>%
    as.matrix()

  # two qdas to estimate mean and sigma automatically
  train_data_ear1 <- train_data_ear1[stats::complete.cases(train_data_ear1),]
  qda_ear1 <- qda(x = train_data_ear1[,paste("S", 1:7, sep ="")],
                  grouping = train_data_ear1$Y)

  # omit Na in train_data_ear2
  train_data_ear2 <- train_data_ear2[stats::complete.cases(train_data_ear2),]
  qda_ear2 <- qda(x = train_data_ear2[,paste("S", 1:7, sep ="")],
                  grouping = train_data_ear2$Y)

  list(qda_model_ear1 = qda_ear1,
       qda_model_ear2 = qda_ear2,
       prior_ear1 = prior_ear1,
       prior_ear1_conditional_ear2 = prior_ear1_conditional_ear2,
       prior_ear2_conditional_ear1 = prior_ear2_conditional_ear1) %>% return()
}
