libs <- c("foreign", "plyr" , "arm", "dplyr", "ggplot2", "car", "boot")
sapply(libs, require, character.only = T)

######### Diagnostics ##############
correct.predictions <- function(out, oos_ids, threshold, results){
  
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
  keepers <- subset(dta, train_id %in% oos_ids)
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
# MT1 who decides who should have a phone -.0409
# FF1 personally have bank account in your name cor .083
# FF2 you make transactions or someone else (lots of missingness but for complete obs, cor = .13)
# GN1 who decides how money you earn will be used cor .05

dta <- train_dta %>% select(train_id, is_female, DG3, DG3A, DG4, DG6, DL0, MT1, FF1, FF2, GN1)
for (col in colnames(dta)[3:length(colnames(dta))]) {
  dta[,col] <- as.factor(ifelse(dta[,col] >= 96, NA, dta[,col]))
}
oos_ids <- c(1:1000) # exclude these obs from our model so we can test out of sample predicting

model <- glm(is_female ~ DG3 + DG3A + DG4 + DG6 + DL0 + MT1 + FF1 + FF2 + GN1, family=binomial(link='probit'), data = subset(dta, !(train_id %in% exclude_ids)))
#model <- probit(is_female ~ DG3 + DG3A + DG4 + DG6 + DL0 + MT1 + FF1,data = subset(dta, !(train_id %in% oos_ids)))
# FF2 & GN1 have missingness - have to decide how to handle
#summary(model)

oos <- subset(dta, train_id %in% oos_ids)
oos$predict_prob <- predict(model, oos, type='response')

results <- data.frame()
for (threshold in c(.4, .41, .42, .43, .44, .45, .46, .47, .48, .49, .5, .55, .6, .65, .7, .75)) {
  results <- correct.predictions(model, oos_ids, threshold, results)
}
