libs <- c("foreign", "plyr" , "arm", "dplyr", "ggplot2", "car", "boot")
sapply(libs, require, character.only = T)

######### Diagnostics ##############
correct.predictions <- function(out, select_training, threshold, results){
  
  ##PREDICTING IN SAMPLE
  cat('In Sample Predictions \n')
  pred_female <- ifelse(predict(out) >= threshold,1,0) # choosing .5 as cutoff - should be evaluated
  actual_female <- out$model[,1]
  
  tt <- table(pred_female,out$model[,1])
  
  colnames(tt) <- c('M','F')
  rownames(tt) <- c('M','F')
  print(tt)
  
  cat('Percent Correctly Predicted \n')
  in_sample <- round(sum(diag(tt))/sum(tt),3)
  cat(in_sample,'\n \n \n')
  
  
  ##PREDICTING OUT OF SAMPLE
  cat('Out of Sample Predictions \n')
  keepers <- dta[-select_training,]
  pv1 <-predict(out,keepers)
  actual_female <- keepers$is_female
  pred_female <- ifelse(pv1 >= threshold,1,0)
  tt <- table(pred_female,actual_female)
  colnames(tt) <- c('M','F')
  rownames(tt) <- c('M','F')
  print(tt)
  sum(diag(tt))/sum(tt)
  cat('Percent Correctly Predicted \n')
  out_of_sample <- round(sum(diag(tt))/sum(tt),3)
  cat(out_of_sample,'\n')
  
  return (rbind(results, data.frame(threshold=threshold, in_sample = in_sample, out_of_sample = out_of_sample)))
}

#########################################################


# Read Data
setwd("/Users/kwilson/Documents/WiDS kaggle/") 

train_dta <- read.csv("./Data/train.csv")

### Variables
# DG3 marital status cor = -.0086
# DG3A religion cor = -.043
# DG4 education cor = -.09995
# DG6 relation to hh head (if answer is myself (1), much more likely to be male) cor = -.0149
# DL0 main income earner cor = 0.62568
# MT1A who decides who should have a phone -.0409
# FF1 personally have bank account in your name cor .083
# FF2 you make transactions or someone else (lots of missingness but for complete obs, cor = .13)
# GN1 who decides how money you earn will be used cor .05

dta <- train_dta %>% select(train_id, is_female, DG3, DG3A, DG4, DG6, DL0, MT1A, FF1, FF2, GN1,DL1,MT6)
for (col in colnames(dta)[3:length(colnames(dta))]) {
  dta[,col] <- ifelse(dta[,col] >= 96, NA, dta[,col])
  ## Simple imputation of missing as mode
  dta[,col] <- ifelse(is.na(dta[,col]), names(table(dta[,col]))[table(dta[,col])==max(table(dta[,col]))], dta[,col])
  dta[,col] <- as.factor(dta[,col])
}

set.seed(10) # setting seed so sample can be reproduced
select_training <- sample.int(n=nrow(dta), size =floor(.1*nrow(dta)), replace=F) # exclude these obs from our model so we can test out of sample predicting

model <- glm(is_female ~ DG3 + DG4 + DG6 + DL0 + MT1A + FF1 + GN1 + FF2 + DL1 + MT6, family=binomial(link='probit'), data = dta[select_training,])
# giving an error for FF2 about can only use factors with 2 or more levels???
#summary(model)

oos <- dta[-select_training,]
oos$predict_prob <- predict(model, oos, type='response')

results <- data.frame()
for (threshold in c(.3, .31, .32, .33, .34, .35, .36, .37, .38, .39, .4, .41, .42, .43, .44, .45, .46, .47, .48, .49, .5, .55, .6, .65, .7, .75)) {
  results <- correct.predictions(model, select_training, threshold, results)
}

### TEST DATA #####
test_dta <- read.csv("./Data/test.csv")
model <- glm(is_female ~ DG3 + DG4 + DG6 + DL0 + MT1A + FF1 + GN1 + FF2, family=binomial(link='probit'), data = dta)
vars <- c("train_id", "is_female", "DG3", "DG3A", "DG4", "DG6", "DL0", "MT1A", "FF1", "FF2", "GN1", "DL1", "MT6")
for (col in vars[3:length(vars)]) {
  test_dta[,col] <- ifelse(test_dta[,col] >= 96, NA, test_dta[,col])
  ## Simple imputation of missing as mode
  test_dta[,col] <- ifelse(is.na(test_dta[,col]), names(table(test_dta[,col]))[table(test_dta[,col])==max(table(test_dta[,col]))], test_dta[,col])
  test_dta[,col] <- as.factor(test_dta[,col])
}
test_dta$is_female <- ifelse(predict(model, test_dta) >= .31, 1, 0)

write.csv(select(test_dta, "test_id", "is_female"), "./Data/submission_02.26.18.csv", row.names=F)

