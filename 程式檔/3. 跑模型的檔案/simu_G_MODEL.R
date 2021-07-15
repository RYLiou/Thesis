library('nnet')
library('ordinal')
library('e1071')
library('caret')
library('plyr')
library('VGAM')
library('ramify')
### path
setwd("D:\\desktop\\論文\\data")

### performance index 
check <- function(data1, data2){
  return(sum(data1==data2)/length(data1))
}
mse <- function(data1, data2){
  return(sum((data1 - data2)^2)/length(data1))
}
macro_F1 <- function(data1, data2){
  confu_matrix = confusionMatrix(data1, data2)
  F1 = 0
  for (i in 1:nrow(confu_matrix$byClass)){
    if (is.na(confu_matrix$byClass[i,1])==TRUE){
      recall = 0.0
    }
    else {recall = confu_matrix$byClass[i,1]}
    
    if (is.na(confu_matrix$byClass[i,3])==TRUE){
      precision = 0.0
    }
    else{precision = confu_matrix$byClass[i,3]}
    
    if (recall == 0 & precision ==0){
      denominator = recall + precision + 1
    }
    else{denominator = recall + precision}
    F1_tmp = 2 * recall * precision / denominator
    F1 = (F1 + F1_tmp) 
  }
  
  return(F1/nrow(confu_matrix$byClass))
}
macro_F2 <- function(data1, data2){
  confu_matrix = confusionMatrix(data1, data2)
  F1 = c()
  for (i in 1:nrow(confu_matrix$byClass)){
    if (is.na(confu_matrix$byClass[i,1])==TRUE){
      recall = NA
    }
    else {recall = confu_matrix$byClass[i,1]}
    
    if (is.na(confu_matrix$byClass[i,3])==TRUE){
      precision = NA
    }
    else{precision = confu_matrix$byClass[i,3]}
    
    F1_tmp = 2 * recall * precision / (recall + precision)
    F1 = c(F1, F1_tmp) 
  }
  
  return(mean(F1,na.rm = T))
}

######################################################################## simulation1 data

#############################################################
### initial values
training_split = 0.7
N = 1
perform_ALL32 <- data.frame(name = c("MSE", "Accuracy", "Macro-F1"))
data_num <- data.frame(name = c("train", "test", "total"))
count_Y <- NULL

data_list = list(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10)

### run GModel
for (j in 1:10){
  
  set.seed(2020)
  
  ### read data
  data <- data_list[j]
  data_a = data[[1]]
  
  ### General model
  
  ## define empty
  MSE_or = NULL ; MSE_na = NULL ; MSE_lo = NULL ; MSE_cr = NULL ; MSE_ac = NULL
  ACC_or = NULL ; ACC_na = NULL ; ACC_lo = NULL ; ACC_cr = NULL ; ACC_ac = NULL
  F1_or  = NULL ; F1_na  = NULL ; F1_lo  = NULL ; F1_cr = NULL  ; F1_ac = NULL
  
  ## define formula
  formula <- rating~.
  
  ## run N times
  for (i in 1:N){
    
    ## astype Y to ordered
    data_a[,1] <- as.ordered(data_a[,1])
    
    ## split traininig and testing data
    training_data = NULL
    testing_data = NULL
    
    for (h in 1:length(levels(data_a[,1]))){
      
      data_n = data_a[data_a[,1] == h,]
      
      index = sample(nrow(data_n), nrow(data_n) * training_split , replace = F)
      split_data = data_n[index,]
      left_data = data_n[-index,]
      
      training_data = rbind(training_data, split_data)
      testing_data = rbind(testing_data, left_data)
    }
    
    train = training_data
    test = testing_data
    
    ## ordinal reg.
    ol <- clm(formula, data = train)
    ol_output <-  predict(object = ol, newdata = test, type="class")
    
    ## cratio
    fit.cratio <- vglm(rating~., family=cratio(reverse=FALSE, parallel=TRUE,link = "logitlink"), data=train)
    cratio_output <- predict(fit.cratio, newdata=test, type="response")
    cratio_output2 = argmax(cratio_output, rows = TRUE)
    
    ## acat
    fit.acat <- vglm(rating~., family=acat(reverse=TRUE, parallel=TRUE), data=train)
    acat_output <- predict(fit.acat, newdata=test, type="response")
    acat_output2 = argmax(acat_output, rows = TRUE)
    
    #fit.acat2 <- vglm(V1~., family=acat(reverse=TRUE, parallel=TRUE), data=train)
    #predict(fit.acat, newdata=test,type="response")[100,]
    #predict(logistic_model, test,type="probs")[100,]
    
    ## astype Y to nominal
    data_a[,1] <- as.character(data_a[,1])
    data_a[,1] <- as.factor(data_a[,1])
    train[,1] <- as.character(train[,1])
    train[,1] <- as.factor(train[,1])
    test[,1] <- as.character(test[,1])
    test[,1] <- as.factor(test[,1])
    
    ## naive
    naiveBayesModel <- naiveBayes(formula, data = train)
    naive_output <- predict(naiveBayesModel, test)
    
    ## logistic
    logistic_model = multinom(formula, data = train)
    logistic_pred <- predict(logistic_model, test)
    
    ## astype Y to numeric to performance
    truth <- as.numeric(test[,1])
    
    cratio_output3 <- as.numeric(cratio_output2)
    acat_output3 <- as.numeric(acat_output2)
    ol_output2 <- as.numeric(ol_output$fit)
    naive_output <- as.numeric(naive_output)
    logistic_pred <- as.numeric(logistic_pred)
    
    ## ACC
    ACC_or = c(ACC_or, check(truth, ol_output2))
    ACC_na = c(ACC_na, check(truth, naive_output))
    ACC_lo = c(ACC_lo, check(truth, logistic_pred))
    ACC_cr = c(ACC_cr, check(truth, cratio_output3))
    ACC_ac = c(ACC_ac, check(truth, acat_output3))
    
    ## MSE
    MSE_or = c(MSE_or, mse(truth, ol_output2))
    MSE_na = c(MSE_na, mse(truth, naive_output))
    MSE_lo = c(MSE_lo, mse(truth, logistic_pred))
    MSE_cr = c(MSE_cr, mse(truth, cratio_output3))
    MSE_ac = c(MSE_ac, mse(truth, acat_output3))
    
    ## macro F1
    truth2 <- factor(test[,1],
                     levels = c(1:length(levels(data_a[,1]))))
    pred1 <- factor(ol_output$fit,
                    levels = c(1:length(levels(data_a[,1]))))
    pred2 <- factor(naive_output,
                    levels = c(1:length(levels(data_a[,1]))))
    pred3 <- factor(logistic_pred,
                    levels = c(1:length(levels(data_a[,1]))))
    pred4 <- factor(cratio_output3,
                    levels = c(1:length(levels(data_a[,1]))))
    pred5 <- factor(acat_output3,
                    levels = c(1:length(levels(data_a[,1]))))
    
    F1_or = c(F1_or, macro_F2(pred1, truth2))
    F1_na = c(F1_na, macro_F2(pred2, truth2))
    F1_lo = c(F1_lo, macro_F2(pred3, truth2))
    F1_cr = c(F1_cr, macro_F2(pred4, truth2))
    F1_ac = c(F1_ac, macro_F2(pred5, truth2))
  }
  
  ## output
  perform_DF <- data.frame( Clm. = c( paste(as.character(round(mean(MSE_or),3)), '(', as.character(round(sd(MSE_or),3)),')',sep=''),
                                      paste(as.character(round(mean(ACC_or),3)), '(', as.character(round(sd(ACC_or),3)),')',sep=''),
                                      paste(as.character(round(mean(F1_or),3)), '(', as.character(round(sd(F1_or),3)),')',sep='')),
                            Naive = c(paste(as.character(round(mean(MSE_na),3)), '(', as.character(round(sd(MSE_na),3)),')',sep=''),
                                      paste(as.character(round(mean(ACC_na),3)), '(', as.character(round(sd(ACC_na),3)),')',sep=''),
                                      paste(as.character(round(mean(F1_na),3)), '(', as.character(round(sd(F1_na),3)),')',sep='')),
                            Logis = c(paste(as.character(round(mean(MSE_lo),3)), '(', as.character(round(sd(MSE_lo),3)),')',sep=''),
                                      paste(as.character(round(mean(ACC_lo),3)), '(', as.character(round(sd(ACC_lo),3)),')',sep=''),
                                      paste(as.character(round(mean(F1_lo),3)), '(', as.character(round(sd(F1_lo),3)),')',sep='')),
                            cratio = c(paste(as.character(round(mean(MSE_cr),3)), '(', as.character(round(sd(MSE_cr),3)),')',sep=''),
                                       paste(as.character(round(mean(ACC_cr),3)), '(', as.character(round(sd(ACC_cr),3)),')',sep=''),
                                       paste(as.character(round(mean(F1_cr),3)), '(', as.character(round(sd(F1_cr),3)),')',sep='')),
                            acat =   c(paste(as.character(round(mean(MSE_ac),3)), '(', as.character(round(sd(MSE_ac),3)),')',sep=''),
                                       paste(as.character(round(mean(ACC_ac),3)), '(', as.character(round(sd(ACC_ac),3)),')',sep=''),
                                       paste(as.character(round(mean(F1_ac),3)), '(', as.character(round(sd(F1_ac),3)),')',sep=''))
  )
  
  data_all = rbind(nrow(train), nrow(test), nrow(data_a))
  data_num = cbind(data_num, 'Data' = data_all)
  
  count_Y2 = rbind(table(train$rating), table(test$rating), table(data_a$rating))
  count_Y = rbind.fill(count_Y, data.frame(count_Y2))
  
  perform_ALL32 = cbind(perform_ALL32, perform_DF)
  
}

perform_ALL32
data_num
count_Y
write.table(perform_ALL32, file="GModel_simulation.csv", sep=",", row.names=F, col.names = T, na = "NA")


