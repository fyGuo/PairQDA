#' Symmetric Bayesian QDA (predicting new phenotypes)
#' @import dplyr
#' @import mvtnorm
#' @importFrom stringr str_sub
#' @param qda_model A list output from sym_joint_qda_fit
#' @param test_data_X A vector of predictors from the test data
#' @param X1 A string of characters for the column names of predictor set 1.
#' @param X2 A string of characters for the column names of predictor set 2.
#' @param Shuffle Whether to use data shuffling before the QDA methods.
#' @param iter Number of interations set by users. The default is 1000 times
#' @returns Predicted individual level phenotype
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(stringr)
#' library(mvtnorm)
#' data(HearingLoss_simu)
#' #Use the Shuffling method
#' fit <- sym_bayes_qda_fit(HearingLoss_simu,
#'                   id = "id",
#'                   Shuffle = TRUE)
#' test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
#' sym_bayes_qda_predict(fit, test_data_X, Shuffle = TRUE)
#'
#' # Use the Non-Shuffling method
#' fit <- sym_bayes_qda_fit(HearingLoss_simu,
#'                   id = "id",
#'                   Shuffle = FALSE,
#'                   method = "pooled")
#' test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
#' sym_bayes_qda_predict(fit, test_data_X, Shuffle = FALSE)


sym_bayes_qda_predict <- function(qda_model = NA,
                            test_data_X = NA,
                            X1 = c("T500_1", "T1K_1", "T2K_1", "T3K_1",
                                   "T4K_1", "T6K_1", "T8K_1"),
                            X2 = c("T500_2", "T1K_2", "T2K_2", "T3K_2",
                                   "T4K_2", "T6K_2", "T8K_2"),
                            Shuffle = TRUE,
                            iter = 1000) {

  # arrange the validation dataset
  test_data_ear1_X <- test_data_X[,X1]
  colnames(test_data_ear1_X) <- c(paste("S", 1:length(X1), sep =""))
  test_data_ear2_X <-test_data_X[,X2]
  colnames(test_data_ear2_X) <- c(paste("S", 1:length(X2), sep =""))

  ## Conduct iteration
  predict_ear_1 <- numeric(iter)
  predict_ear_2 <- numeric(iter)


  for (i in 1:iter) {
    j <- max(i - 1, 1)
    if (i == 1) {
      predict_ear_1_prob <- QDA_function(test_data_ear1_X,
                                         prior = qda_model$prior_ear1,
                                         mu_list = qda_model$mu_list_ear1,
                                         var_list = qda_model$var_list_ear1)[[2]]
      predict_ear_1[[i]] <- which.max(predict_ear_1_prob)
    } else {
      predict_ear_1_prob <-QDA_function(test_data_ear1_X ,
                                    prior = as.numeric(qda_model$prior_ear1_conditional_ear2[,predict_ear_2[[i-1]]]),
                                    mu_list = qda_model$mu_list_ear1,
                                    var_list = qda_model$var_list_ear1)[[2]]

      predict_ear_1[[i]] <- which.max(predict_ear_1_prob)
    }


    predict_ear_2_prob <- QDA_function( test_data_ear2_X,
                                  prior = as.numeric(qda_model$prior_ear2_conditional_ear1[predict_ear_1[[i]],]),
                                  mu_list = qda_model$mu_list_ear2,
                                  var_list = qda_model$var_list_ear2)[[2]]

    predict_ear_2[[i]] <- which.max(predict_ear_2_prob)


    if(i> 1 & predict_ear_1[[j]] == predict_ear_1[[i]] &
       predict_ear_2[[j]] == predict_ear_2[[i]]) {break}
  }
  if (i < iter) {
    predicted_valid_1 <- paste(predict_ear_1[[i]], predict_ear_2[[i]], sep = "_")
  } else {
    vector_prediction <- paste(predict_ear_1, predict_ear_2, sep = "_")
    vector_prediction %>% table %>% sort(decreasing = T)
    predicted_valid_1 <- vector_prediction[1]
  }

  predicted_ear1 <- stringr::str_sub(predicted_valid_1, 1, 1) %>% as.numeric()
  predicted_ear2 <- stringr::str_sub(predicted_valid_1, 3, 3) %>% as.numeric()

  P1 <- QDA_function(test_data_ear1_X ,
                     prior = as.numeric(qda_model$prior_ear1_conditional_ear2[,predict_ear_2[[i-1]]]),
                     mu_list = qda_model$mu_list_ear1,
                     var_list = qda_model$var_list_ear1)[[2]][predicted_ear1] +
    QDA_function( test_data_ear2_X,
                  prior = as.numeric(qda_model$prior_ear2_conditional_ear1[predict_ear_1[[i]],]),
                  mu_list = qda_model$mu_list_ear2,
                  var_list = qda_model$var_list_ear2)[[2]][predicted_ear2]




  if (Shuffle == FALSE) {return (predicted_valid_1)}

  # repead the above process if shuffle = TRUE
  else {
    test_data_ear1_X <- test_data_X[,X2]

    colnames(test_data_ear1_X) <- c(paste("S", 1:length(X2), sep =""))

    test_data_ear2_X <-test_data_X[,X1]

    colnames(test_data_ear2_X) <- c(paste("S", 1:length(X1), sep =""))

    ## Conduct iteration
    predict_ear_1 <- numeric(iter)
    predict_ear_2 <- numeric(iter)


    for (i in 1:iter) {
      j <- max(i - 1, 1)
      if (i == 1) {
        predict_ear_1_prob <- QDA_function(test_data_ear1_X[,paste("S", 1:7, sep ="")],
                                           prior = qda_model$prior_ear1,
                                           mu_list = qda_model$mu_list_ear1,
                                           var_list = qda_model$var_list_ear1)[[2]]
        predict_ear_1[[i]] <- which.max(predict_ear_1_prob)
      } else {
        predict_ear_1_prob <-QDA_function(test_data_ear1_X[,paste("S", 1:7, sep ="")],
                                          prior = as.numeric(qda_model$prior_ear1_conditional_ear2[,predict_ear_2[[i-1]]]),
                                          mu_list = qda_model$mu_list_ear1,
                                          var_list = qda_model$var_list_ear1)[[2]]

        predict_ear_1[[i]] <- which.max(predict_ear_1_prob)
      }


      predict_ear_2_prob <- QDA_function( test_data_ear2_X[,paste("S", 1:7, sep ="")],
                                          prior = as.numeric(qda_model$prior_ear2_conditional_ear1[predict_ear_1[[i]],]),
                                          mu_list = qda_model$mu_list_ear2,
                                          var_list = qda_model$var_list_ear2)[[2]]

      predict_ear_2[[i]] <- which.max(predict_ear_2_prob)


      if(i> 1 & predict_ear_1[[j]] == predict_ear_1[[i]] &
         predict_ear_2[[j]] == predict_ear_2[[i]]) {break}
    }
    if (i < iter) {
      predicted_valid_2 <- paste(predict_ear_1[[i]], predict_ear_2[[i]], sep = "_")
    } else {
      vector_prediction <- paste(predict_ear_1, predict_ear_2, sep = "_")
      vector_prediction %>% table %>% sort(decreasing = T)
      predicted_valid_2 <- vector_prediction[1]
    }

    predicted_ear1 <- stringr::str_sub(predicted_valid_2, 1, 1) %>% as.numeric()
    predicted_ear2 <- stringr::str_sub(predicted_valid_2, 3, 3) %>% as.numeric()

    P2 <- QDA_function(test_data_ear1_X ,
                       prior = as.numeric(qda_model$prior_ear1_conditional_ear2[,predict_ear_2[[i-1]]]),
                       mu_list = qda_model$mu_list_ear1,
                       var_list = qda_model$var_list_ear1)[[2]][predicted_ear1] +
      QDA_function(test_data_ear2_X,
                    prior = as.numeric(qda_model$prior_ear2_conditional_ear1[predict_ear_1[[i]],]),
                    mu_list = qda_model$mu_list_ear2,
                    var_list = qda_model$var_list_ear2)[[2]][predicted_ear2]

    if (P1 >= P2) return(predicted_valid_1)
    else {paste(predict_ear_2, predict_ear_1, sep = "_")}
  }
}
