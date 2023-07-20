pkgname <- "PairQDA"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('PairQDA')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("asym_cond_qda_fit")
### * asym_cond_qda_fit

flush(stderr()); flush(stdout())

### Name: asym_cond_qda_fit
### Title: Asymmetric Conditional QDA (fitting the model)
### Aliases: asym_cond_qda_fit

### ** Examples

library(dplyr)
library(mvtnorm)
data(HearingLoss_simu)
fit <- asym_cond_qda_fit(HearingLoss_simu,
                  id = "id")
test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
asym_cond_qda_predict(fit, test_data_X)



cleanEx()
nameEx("asym_cond_qda_predict")
### * asym_cond_qda_predict

flush(stderr()); flush(stdout())

### Name: asym_cond_qda_predict
### Title: Asymmetric Conditional QDA (predicting new phenotypes)
### Aliases: asym_cond_qda_predict

### ** Examples

library(dplyr)
library(mvtnorm)
data(HearingLoss_simu)
fit <- asym_cond_qda_fit(HearingLoss_simu,
                  id = "id")
test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
asym_cond_qda_predict(fit, test_data_X)



cleanEx()
nameEx("asym_marg_qda_fit")
### * asym_marg_qda_fit

flush(stderr()); flush(stdout())

### Name: asym_marg_qda_fit
### Title: Asymmetric Marginal QDA (fitting model)
### Aliases: asym_marg_qda_fit

### ** Examples

library(dplyr)
library(mvtnorm)
data(HearingLoss_simu)
fit <- asym_marg_qda_fit(HearingLoss_simu,
                  id = "id")
test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
asym_marg_qda_predict(fit, test_data_X)



cleanEx()
nameEx("asym_marg_qda_predict")
### * asym_marg_qda_predict

flush(stderr()); flush(stdout())

### Name: asym_marg_qda_predict
### Title: Asymmetric Marginal QDA (predicting new phenotypes)
### Aliases: asym_marg_qda_predict

### ** Examples

library(dplyr)
library(mvtnorm)
data(HearingLoss_simu)
fit <- asym_marg_qda_fit(HearingLoss_simu,
                  id = "id")
test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
asym_marg_qda_predict(fit, test_data_X)



cleanEx()
nameEx("conv_qda_fit")
### * conv_qda_fit

flush(stderr()); flush(stdout())

### Name: conv_qda_fit
### Title: Conventional QDA (fitting model)
### Aliases: conv_qda_fit

### ** Examples

library(dplyr)
library(mvtnorm)
data(HearingLoss_simu)
fit <- conv_qda_fit(HearingLoss_simu,
                  id = "id")
test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
conv_qda_predict(fit, test_data_X)



cleanEx()
nameEx("conv_qda_predict")
### * conv_qda_predict

flush(stderr()); flush(stdout())

### Name: conv_qda_predict
### Title: Conventional QDA (predicting new phenotypes)
### Aliases: conv_qda_predict

### ** Examples

library(dplyr)
library(mvtnorm)
data(HearingLoss_simu)
fit <- conv_qda_fit(HearingLoss_simu,
                  id = "id")
test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
conv_qda_predict(fit, test_data_X)



cleanEx()
nameEx("sym_bayes_qda_fit")
### * sym_bayes_qda_fit

flush(stderr()); flush(stdout())

### Name: sym_bayes_qda_fit
### Title: Symmetric Bayesian QDA (fitting model)
### Aliases: sym_bayes_qda_fit

### ** Examples

library(dplyr)
library(mvtnorm)
library(MASS)
data(HearingLoss_simu)
fit <- sym_bayes_qda_fit(HearingLoss_simu,
                  id = "id")
test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
sym_bayes_qda_predict(fit, test_data_X)



cleanEx()
nameEx("sym_bayes_qda_predict")
### * sym_bayes_qda_predict

flush(stderr()); flush(stdout())

### Name: sym_bayes_qda_predict
### Title: Symmetric Bayesian QDA (predicting new phenotypes)
### Aliases: sym_bayes_qda_predict

### ** Examples

library(dplyr)
library(mvtnorm)
library(MASS)
data(HearingLoss_simu)
fit <- sym_bayes_qda_fit(HearingLoss_simu,
                  id = "id")
test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
sym_bayes_qda_predict(fit, test_data_X)



cleanEx()
nameEx("sym_joint_qda_fit")
### * sym_joint_qda_fit

flush(stderr()); flush(stdout())

### Name: sym_joint_qda_fit
### Title: Symmetric Joint QDA (fitting model)
### Aliases: sym_joint_qda_fit

### ** Examples

library(dplyr)
data(HearingLoss_simu)
fit <- sym_joint_qda_fit(HearingLoss_simu,
                  id = "id")



cleanEx()
nameEx("sym_joint_qda_predict")
### * sym_joint_qda_predict

flush(stderr()); flush(stdout())

### Name: sym_joint_qda_predict
### Title: Symmetric Joint QDA (predicting new phenotypes)
### Aliases: sym_joint_qda_predict

### ** Examples

library(dplyr)
library(mvtnorm)
data(HearingLoss_simu)
fit <- sym_joint_qda_fit(HearingLoss_simu,
                  id = "id")
test_data_X <- HearingLoss_simu[1,] %>% dplyr::select(-"Label_1", -"Label_2",-"id")
sym_joint_qda_predict(fit, test_data_X)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
