#' Fitting Symmetric Joint QDA model (No shuffling version)
#' @import dplyr
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


sym_joint_qda_fit_NoShuffling <- function(train_data,
                          id = "SID",
                          Y1 = "Label_1",
                          Y2 = "Label_2",
                          X1 = c("T500_1", "T1K_1", "T2K_1", "T3K_1",
                                 "T4K_1", "T6K_1", "T8K_1"),
                          X2 = c("T500_2", "T1K_2", "T2K_2", "T3K_2",
                                 "T4K_2", "T6K_2", "T8K_2"),
                          method = c("pooled", "separate"),
                          k = 4) {

  # arrange the training dataset
  train_data_ear1 <- train_data[,c(id, Y1, X1)] %>% dplyr::mutate(ear = 1)
  colnames(train_data_ear1) <- c("id", "Y", paste("S", 1:length(X1), sep =""), "ear")


  train_data_ear2 <- train_data[,c(id, Y2, X2)] %>% dplyr::mutate(ear = 2)
  colnames(train_data_ear2) <- c("id", "Y", paste("S", 1:length(X2), sep =""), "ear")

  # prior weights
  prior_weight <-
    table(train_data_ear1$Y, train_data_ear2$Y, dnn = c("Ear1", "Ear2"))  %>%
    prop.table() %>%
    as.matrix()

  # excluding rows with missing values
  train_data_ear1 <- train_data_ear1[stats::complete.cases(train_data_ear1),]
  train_data_ear2 <- train_data_ear2[stats::complete.cases(train_data_ear2),]

  # create a pooled dataset

  train_data_pooled <- rbind(train_data_ear1, train_data_ear2)

  ## compute the mu_list for E[Y1|K1] and var[Y1|K1]

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

  list(mu_list_ear1 = mu_list_ear1,
       var_list_ear1 = var_list_ear1,
       mu_list_ear2 = mu_list_ear2,
       var_list_ear2 = var_list_ear2,
       prior_weight = prior_weight,
       k = k) %>% return()
}




#' Fitting Symmetric Joint QDA model (shuffling version)
#' @import dplyr
#'
#' @param train_data A data.frame for the training dataset
#' @param id A character for the column name which stores id.
#' @param Y1 A character shows the column name of label 1.
#' @param Y2 A character shows the column name of label 2.
#' @param X1 A string of characters for the column names of predictor set 1.
#' @param X2 A string of characters for the column names of predictor set 2.
#' @param k The number of classifications.
#' @param S The number of shuffles.
#' @param seed The random seed.
#' @returns A list to store model parameters.
#'
#' @export


sym_joint_qda_fit_Shuffling  <- function(train_data,
                              id = "SID",
                              Y1 = "Label_1",
                              Y2 = "Label_2",
                              X1 = c("T500_1", "T1K_1", "T2K_1", "T3K_1",
                                     "T4K_1", "T6K_1", "T8K_1"),
                              X2 = c("T500_2", "T1K_2", "T2K_2", "T3K_2",
                                     "T4K_2", "T6K_2", "T8K_2"),
                              k = 4,
                              S = 10,
                              seed) {

  set.seed(seed)

  prior_weight <- matrix(0, nrow = 4, ncol = 4)

  mu_list_ear1 <- mu_list_ear2 <- vector(mode = "list", length = k)
  var_list_ear1 <- var_list_ear2 <- vector(mode = "list", length = k)

  for (s in 1:S) {
    train_data_s <- train_data
   for (i in 1:dim(train_data_s)[1]) {
     pair <- sample(c(1,2), size = 2, replace = F)

     # if after the location did not change we do nothing
     # otherwise, we switch Y1 and Y2, X1 and X2
     if (pair[1] == 2 & pair[2] == 1) {
       train_data_s[i,X1] <- train_data[i,X2]
       train_data_s[i,X2] <- train_data[i,X1]

       train_data_s[i,Y1] <- train_data[i,Y2]
       train_data_s[i,Y2] <- train_data[i,Y1]

     }
   }

    # arrange the training dataset
    train_data_ear1 <- train_data_s[,c(id, Y1, X1)] %>% dplyr::mutate(ear = 1)
    colnames(train_data_ear1) <- c("id", "Y", paste("S", 1:length(X1), sep =""), "ear")


    train_data_ear2 <- train_data_s[,c(id, Y2, X2)] %>% dplyr::mutate(ear = 2)
    colnames(train_data_ear2) <- c("id", "Y", paste("S", 1:length(X2), sep =""), "ear")

    # prior weights
    weight_matrix <- +
      table(train_data_ear1$Y, train_data_ear2$Y, dnn = c("Ear1", "Ear2"))  %>%
      prop.table() %>%
      as.matrix()

    prior_weight <- prior_weight + weight_matrix/S


    # excluding rows with missing values
    train_data_ear1 <- train_data_ear1[stats::complete.cases(train_data_ear1),]
    train_data_ear2 <- train_data_ear2[stats::complete.cases(train_data_ear2),]

    # create a pooled dataset
    train_data_pooled <- rbind(train_data_ear1, train_data_ear2)


    # excluding rows with missing values
    train_data_ear1 <- train_data_ear1[stats::complete.cases(train_data_ear1),]
    train_data_ear2 <- train_data_ear2[stats::complete.cases(train_data_ear2),]

    # calculating the mean list
    for (h in 1:k) {
      mu <- train_data_pooled %>% dplyr::filter(.data[["Y"]] == h) %>%
        dplyr::select(paste("S", 1:length(X1), sep ="")) %>%
        colMeans(na.rm = T)

      if (s == 1) {mu_list_ear1[[h]] <- mu_list_ear2[[h]] <- mu/S}
      else {mu_list_ear1[[h]] <- mu_list_ear1[[h]] +  mu/S
            mu_list_ear2[[h]] <- mu_list_ear2[[h]] +  mu/S}
    }

    # calculating he variance list
    for (h in 1:k) {
      var <-
        train_data_pooled %>% dplyr::filter(.data[["Y"]] == h) %>%
        dplyr::select(paste("S", 1:length(X1), sep ="")) %>%
        stats::var(na.rm = T)
      if (s == 1) {var_list_ear1[[h]] <- var_list_ear2[[h]] <- var/S}
      else {var_list_ear1[[h]] <- var_list_ear1[[h]] + var/S
            var_list_ear2[[h]] <- var_list_ear2[[h]] + var/S}
    }

  }

  list(mu_list_ear1 = mu_list_ear1,
       var_list_ear1 = var_list_ear1,
       mu_list_ear2 = mu_list_ear2,
       var_list_ear2 = var_list_ear2,
       prior_weight = prior_weight,
       k = k) %>% return()
}


#' Fitting Symmetric Joint QDA model
#' @import dplyr
#'
#' @param train_data A data.frame for the training dataset
#' @param id A character for the column name which stores id.
#' @param Y1 A character shows the column name of label 1.
#' @param Y2 A character shows the column name of label 2.
#' @param X1 A string of characters for the column names of predictor set 1.
#' @param X2 A string of characters for the column names of predictor set 2.
#' @param k The number of classifications.
#' @param Shuffle Whether to use data shuffling before the QDA methods.
#' @param seed The random seed. Only applicable when using Shuffling.
#' @param S The number of shuffles. Only applicable when using Shuffling.
#' @param method The estimation method. Only applicable when not using Shuffling.
#' @returns A list to store model parameters.
#'
#' @export

#' @examples
#' library(dplyr)
#' library(mvtnorm)
#' data(HearingLoss_simu)
#' #Use the Shuffling method
#' fit <- sym_joint_qda_fit(HearingLoss_simu,
#'                   id = "id",
#'                   Shuffle = TRUE)
#'
#' #Use the Non-shuffling method
#' fit <- sym_joint_qda_fit(HearingLoss_simu,
#'                   id = "id",
#'                   Shuffle = FALSE,
#'                   method = "pooled")

sym_joint_qda_fit  <- function(train_data,
                               id = "SID",
                               Y1 = "Label_1",
                               Y2 = "Label_2",
                               X1 = c("T500_1", "T1K_1", "T2K_1", "T3K_1",
                                      "T4K_1", "T6K_1", "T8K_1"),
                               X2 = c("T500_2", "T1K_2", "T2K_2", "T3K_2",
                                      "T4K_2", "T6K_2", "T8K_2"),
                              k = 4,
                              Shuffle = TRUE,
                              seed = 123,
                              S = 10,
                              method = c("pooled", "separate")) {

  if (Shuffle) {sym_joint_qda_fit_Shuffling(
    train_data = train_data, id = id, Y1 = Y1, Y2 = Y2, X1 = X1,
    X2 = X2, k = k, seed = seed, S = S
  )}

  else {sym_joint_qda_fit_NoShuffling(
    train_data = train_data, id = id, Y1 = Y1, Y2 = Y2, X1 = X1,
    X2 = X2, k = k, method = method)}
}




