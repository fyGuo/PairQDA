#' Symmetric Bayesian QDA (predicting new phenotypes)
#' @import dplyr
#' @import mvtnorm
#' @importFrom utils getFromNamespace
#' @importFrom MASS qda

#' @param qda_model A list output from sym_bayes_qda_fit
#' @param test_data_X A vector of predictors from the test data
#' @param Ear1_mark A character shows the last several letters to denote ear 1 label.
#' @param Ear2_mark A character shows the last several letters to denote ear 2 label.
#' @param number_features Number of features (covariates) to fit the model for each ear.
#' @param iter Number of interations set by users. The default is 1000 times
#' @returns Predicted individual level phenotype
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(mvtnorm)
#' library(MASS)
#' data(HearingLoss_simu)
#' fit <- sym_bayes_qda_fit(HearingLoss_simu,
#'                   id = "id")
#' test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
#' sym_bayes_qda_predict(fit, test_data_X)


sym_bayes_qda_predict <- function(qda_model = NA,
                            test_data_X = NA,
                            Ear1_mark = "_1",
                            Ear2_mark = "_2",
                            number_features = 7,
                            iter = 1000) {
predict.qda <- getFromNamespace("predict.qda","MASS")

  # arrange the validation dataset

  test_data_ear1_X <- test_data_X %>%
    dplyr::select(ends_with(Ear1_mark))

  colnames(test_data_ear1_X) <- c(paste("S", 1:7, sep =""))

  test_data_ear2_X <-test_data_X %>%
    dplyr::select(ends_with(Ear2_mark))

  colnames(test_data_ear2_X) <- c(paste("S", 1:7, sep =""))

  ## Conduct iteration
  predict_ear_1 <- numeric(iter)
  predict_ear_2 <- numeric(iter)


  for (i in 1:iter) {
    j <- max(i - 1, 1)
    if (i == 1) {
      predict_ear_1_prob <- predict.qda(qda_model$qda_model_ear1,
                                    test_data_ear1_X[,paste("S", 1:7, sep ="")],
                                    prior = qda_model$prior_ear1 %>% as.numeric())$posterior
      predict_ear_1[[i]] <- which.max(predict_ear_1_prob)
    } else {
      predict_ear_1_prob <- predict.qda(qda_model$qda_model_ear1,
                                    test_data_ear1_X[,paste("S", 1:7, sep ="")],
                                    prior = as.numeric(qda_model$prior_ear1_conditional_ear2[,predict_ear_2[[i-1]]]))$posterior

      predict_ear_1[[i]] <- which.max(predict_ear_1_prob)
    }


    predict_ear_2_prob <- predict.qda(qda_model$qda_model_ear2,
                                  test_data_ear2_X[,paste("S", 1:7, sep ="")],
                                  prior = as.numeric(qda_model$prior_ear2_conditional_ear1[predict_ear_1[[i]],]))$posterior

    predict_ear_2[[i]] <- which.max(predict_ear_2_prob)


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
  predicted_valid %>% return()
}
