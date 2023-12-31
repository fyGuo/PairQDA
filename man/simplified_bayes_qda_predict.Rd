% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simplified_bayes_qda_predict.R
\name{simplified_bayes_qda_predict}
\alias{simplified_bayes_qda_predict}
\title{Simplified Bayesian QDA (predicting new phenotypes)}
\usage{
simplified_bayes_qda_predict(
  qda_model = NA,
  test_data_X = NA,
  X1 = c("T500_1", "T1K_1", "T2K_1", "T3K_1", "T4K_1", "T6K_1", "T8K_1"),
  X2 = c("T500_2", "T1K_2", "T2K_2", "T3K_2", "T4K_2", "T6K_2", "T8K_2"),
  Shuffle = TRUE,
  iter = 1000
)
}
\arguments{
\item{qda_model}{A list output from sym_joint_qda_fit}

\item{test_data_X}{A vector of predictors from the test data}

\item{X1}{A string of characters for the column names of predictor set 1.}

\item{X2}{A string of characters for the column names of predictor set 2.}

\item{Shuffle}{Whether to use data shuffling before the QDA methods.}

\item{iter}{Number of interations set by users. The default is 1000 times}
}
\value{
Predicted individual level phenotype
}
\description{
Simplified Bayesian QDA (predicting new phenotypes)
}
\examples{
library(dplyr)
library(mvtnorm)
data(HearingLoss_simu)
#Use the Shuffling method
fit <- simplified_bayes_qda_fit(HearingLoss_simu,
                  id = "id",
                  Shuffle = TRUE)
test_data_X <- HearingLoss_simu[1,] \%>\% dplyr::select(-"Label_1", -"Label_2",-"id")
simplified_bayes_qda_predict(fit, test_data_X, Shuffle = TRUE)

# Use the Non-Shuffling method
fit <- simplified_bayes_qda_fit(HearingLoss_simu,
                  id = "id",
                  Shuffle = FALSE,
                  method = "pooled")
test_data_X <- HearingLoss_simu[1,] \%>\% dplyr::select(-"Label_1", -"Label_2",-"id")
simplified_bayes_qda_predict(fit, test_data_X, Shuffle = FALSE)
}
