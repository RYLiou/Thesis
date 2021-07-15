library(tidyr)

###################################################################
## combind data

yahoo <- NULL ; trip <- NULL

perform_ALL32a <- perform_ALL32[,-1] ; perform_ALL33a <- perform_ALL33[,-1]

yahoo <- cbind(perform_ALL32[,1],
               perform_ALL32a[c(1:15)], perform_ALL33a[c(1:5)], perform_ALL32a[c(16:30)], perform_ALL33a[c(6:10)], 
               perform_ALL32a[c(31:45)], perform_ALL33a[c(11:15)], perform_ALL32a[c(46:60)], perform_ALL33a[c(16:20)], 
               perform_ALL32a[c(61:75)], perform_ALL33a[c(21:25)], perform_ALL33a[c(26:30)])
yahoo2 <- yahoo
yahoo <- yahoo2

perform_ALL34a <- perform_ALL34[,-1] ; perform_ALL35a <- perform_ALL35[,-1]

trip <- cbind(perform_ALL34[,1],
              perform_ALL34a[c(1:15)], perform_ALL35a[c(1:5)], perform_ALL34a[c(16:30)], perform_ALL35a[c(6:10)], 
              perform_ALL34a[c(31:45)], perform_ALL35a[c(11:15)], perform_ALL34a[c(46:60)], perform_ALL35a[c(16:20)], 
              perform_ALL34a[c(61:75)], perform_ALL35a[c(21:25)], perform_ALL34a[c(26:30)])
trip2 <- trip
trip <- trip2

yahoo_matrix <- as.data.frame(matrix(rep(0,318),nrow = 3))
trip_matrix <- as.data.frame(matrix(rep(0,318),nrow = 3))

# yahoo
for (i in 2:ncol(yahoo)){
  for (j in 1:3){
    
    a <- (strsplit(as.character(yahoo[j,i]), split='\\(')[[1]][1])
    yahoo_matrix[j,i] <- a
    
  }
}

yahoo_matrix[1,1] <- "MSE" ; yahoo_matrix[2,1] <- "Accuracy" ; yahoo_matrix[3,1] <- "Macro-F1"

new_yahoo <- as.data.frame(matrix(rep(0,40),nrow = 5))

yahoo_matrix2 <- yahoo_matrix[,-1]
yahoo_matrix2


# trip
for (i in 2:ncol(trip)){
  for (j in 1:3){
    
    a <- (strsplit(as.character(trip[j,i]), split='\\(')[[1]][1])
    trip_matrix[j,i] <- a
    
  }
}

trip_matrix[1,1] <- "MSE" ; trip_matrix[2,1] <- "Accuracy" ; trip_matrix[3,1] <- "Macro-F1"

new_trip <- as.data.frame(matrix(rep(0,40),nrow = 5))

trip_matrix2 <- trip_matrix[,-1]
trip_matrix2

###################################################################################
## yahoo plot
## w=1:MSE  ,  2:ACC  ,  3:F1
w = 3
a2 = c()
for (z in 0:4){
  a1 <- c(yahoo_matrix2[w,1+z],yahoo_matrix2[w,6+z],yahoo_matrix2[w,11+z],yahoo_matrix2[w,16+z],
          yahoo_matrix2[w,21+z],yahoo_matrix2[w,26+z],yahoo_matrix2[w,31+z],yahoo_matrix2[w,36+z],
          yahoo_matrix2[w,41+z],yahoo_matrix2[w,46+z],yahoo_matrix2[w,51+z],yahoo_matrix2[w,56+z],
          yahoo_matrix2[w,61+z],yahoo_matrix2[w,66+z],yahoo_matrix2[w,71+z],yahoo_matrix2[w,76+z],
          yahoo_matrix2[w,81+z],yahoo_matrix2[w,86+z],yahoo_matrix2[w,91+z],yahoo_matrix2[w,96+z],
          yahoo_matrix2[w,101+z])
  
  a2 = c(a2,a1)
}
a3 <- as.data.frame(a2)

name <- rep(c("50","100","150","200",
              "50","100","150","200",
              "50","100","150","200",
              "50","100","150","200",
              "50","100","150","200",
              "300"),5)


#model <- c(rep("clm",21),rep("naive",21),rep("logistic",21),rep("cratio",21),rep("acat",21))
model <- c(rep("clm_w=3",4),rep("clm_w=5",4),rep("clm_w=3",4),rep("clm_w=5",4),rep("clm",5),
           rep("naive_w=3",4),rep("naive_w=5",4),rep("naive_w=3",4),rep("naive_w=5",4),rep("naive",5),
           rep("logistic_w=3",4),rep("logistic_w=5",4),rep("logistic_w=3",4),rep("logistic_w=5",4),rep("logistic",5),
           rep("cratio_w=3",4),rep("cratio_w=5",4),rep("cratio_w=3",4),rep("cratio_w=5",4),rep("cratio",5),
           rep("acat_w=3",4),rep("acat_w=5",4),rep("acat_w=3",4),rep("acat_w=5",4),rep("acat",5))


yahoo_test <- cbind(a3,name,model)



## plot
as.numeric(as.character(yahoo_test$a2))
yahoo_test$a2 <- as.numeric(as.character(yahoo_test$a2))

### MSE
# CBOW 
yahoo_test2 <- yahoo_test[c(1:8,22:29,43:45,47:49,64:71,85:92),]
yahoo_test2$name <- factor(yahoo_test2$name, levels = c("50","100","150","200")) 

yahoo_test2 %>% 
  ggplot(aes(x=yahoo_test2$name,y=as.numeric(yahoo_test2$a2),group=yahoo_test2$model,color=yahoo_test2$model)) +
  geom_line(size = 1) + 
  geom_point(size = 3, pch=18) + 
  labs(x = "詞嵌入維度 (D)" , y = "MSE",title = "CBOW法 w為3和5 五種模型於各維度MSE表現", color = "Models")+
  #scale_colour_manual(values = c("#006BA6", "#FFBC42", "#D81159","#8F2D56","gray","darkgreen"))+#改變顏色
  # coord_cartesian(ylim = c(min(yahoo_test$a2), max(yahoo_test$a2)))+
  theme(plot.title = element_text(hjust = 0.5))

# SKIP
yahoo_test2 <- yahoo_test[c(9:16,30:37,51:53,55:57,72:79,93:100),]
yahoo_test2$name <- factor(yahoo_test2$name, levels = c("50","100","150","200")) 

yahoo_test2 %>% 
  ggplot(aes(x=yahoo_test2$name,y=as.numeric(yahoo_test2$a2),group=yahoo_test2$model,color=yahoo_test2$model)) +
  geom_line(size = 1) + 
  geom_point(size = 3, pch=18) + 
  labs(x = "詞嵌入維度 (D)" , y = "MSE",title = "Skip-gram法 w為3和5 五種模型於各維度MSE表現", color = "Models")+
  #scale_colour_manual(values = c("#006BA6", "#FFBC42", "#D81159","#8F2D56","gray","darkgreen"))+#改變顏色
  # coord_cartesian(ylim = c(min(yahoo_test$a2), max(yahoo_test$a2)))+
  theme(plot.title = element_text(hjust = 0.5))

# TFIDF
yahoo_test2 <- yahoo_test[c(17:20,38:41,59:61,80:83,101:104),]
yahoo_test2$name <- factor(yahoo_test2$name, levels = c("50","100","150","200")) 

yahoo_test2 %>% 
  ggplot(aes(x=yahoo_test2$name,y=as.numeric(yahoo_test2$a2),group=yahoo_test2$model,color=yahoo_test2$model)) +
  geom_line(size = 1) + 
  geom_point(size = 3, pch=18) + 
  labs(x = "詞嵌入維度 (D)" , y = "MSE",title = "TFIDF法 五種模型於各維度MSE表現", color = "Models")+
  #scale_colour_manual(values = c("#006BA6", "#FFBC42", "#D81159","#8F2D56","gray","darkgreen"))+#改變顏色
  # coord_cartesian(ylim = c(min(yahoo_test$a2), max(yahoo_test$a2)))+
  theme(plot.title = element_text(hjust = 0.5))


### ACC
# CBOW 
yahoo_test2 <- yahoo_test[c(1:8,22:29,43:45,47:49,64:71,85:92),]
yahoo_test2$name <- factor(yahoo_test2$name, levels = c("50","100","150","200")) 

yahoo_test2 %>% 
  ggplot(aes(x=yahoo_test2$name,y=as.numeric(yahoo_test2$a2),group=yahoo_test2$model,color=yahoo_test2$model)) +
  geom_line(size = 1) + 
  geom_point(size = 3, pch=18) + 
  labs(x = "詞嵌入維度 (D)" , y = "Accuracy(%)",title = "CBOW法 w為3和5 五種模型於各維度Accuracy表現", color = "Models")+
  #scale_colour_manual(values = c("#006BA6", "#FFBC42", "#D81159","#8F2D56","gray","darkgreen"))+#改變顏色
  # coord_cartesian(ylim = c(min(yahoo_test$a2), max(yahoo_test$a2)))+
  theme(plot.title = element_text(hjust = 0.5))

# SKIP
yahoo_test2 <- yahoo_test[c(9:16,30:37,51:53,55:57,72:79,93:100),]
yahoo_test2$name <- factor(yahoo_test2$name, levels = c("50","100","150","200")) 

yahoo_test2 %>% 
  ggplot(aes(x=yahoo_test2$name,y=as.numeric(yahoo_test2$a2),group=yahoo_test2$model,color=yahoo_test2$model)) +
  geom_line(size = 1) + 
  geom_point(size = 3, pch=18) + 
  labs(x = "詞嵌入維度 (D)" , y = "Accuracy(%)",title = "Skip-gram法 w為3和5 五種模型於各維度Accuracy表現", color = "Models")+
  #scale_colour_manual(values = c("#006BA6", "#FFBC42", "#D81159","#8F2D56","gray","darkgreen"))+#改變顏色
  # coord_cartesian(ylim = c(min(yahoo_test$a2), max(yahoo_test$a2)))+
  theme(plot.title = element_text(hjust = 0.5))

# TFIDF
yahoo_test2 <- yahoo_test[c(17:20,38:41,59:61,80:83,101:104),]
yahoo_test2$name <- factor(yahoo_test2$name, levels = c("50","100","150","200")) 

yahoo_test2 %>% 
  ggplot(aes(x=yahoo_test2$name,y=as.numeric(yahoo_test2$a2),group=yahoo_test2$model,color=yahoo_test2$model)) +
  geom_line(size = 1) + 
  geom_point(size = 3, pch=18) + 
  labs(x = "詞嵌入維度 (D)" , y = "Accuracy(%)",title = "TFIDF法 五種模型於各維度Accuracy表現", color = "Models")+
  #scale_colour_manual(values = c("#006BA6", "#FFBC42", "#D81159","#8F2D56","gray","darkgreen"))+#改變顏色
  # coord_cartesian(ylim = c(min(yahoo_test$a2), max(yahoo_test$a2)))+
  theme(plot.title = element_text(hjust = 0.5))


### F1
# CBOW 
yahoo_test2 <- yahoo_test[c(1:8,22:29,43:45,47:49,64:71,85:92),]
yahoo_test2$name <- factor(yahoo_test2$name, levels = c("50","100","150","200")) 

yahoo_test2 %>% 
  ggplot(aes(x=yahoo_test2$name,y=as.numeric(yahoo_test2$a2),group=yahoo_test2$model,color=yahoo_test2$model)) +
  geom_line(size = 1) + 
  geom_point(size = 3, pch=18) + 
  labs(x = "詞嵌入維度 (D)" , y = "Macro-F1",title = "CBOW法 w為3和5 五種模型於各維度Macro-F1表現", color = "Models")+
  #scale_colour_manual(values = c("#006BA6", "#FFBC42", "#D81159","#8F2D56","gray","darkgreen"))+#改變顏色
  # coord_cartesian(ylim = c(min(yahoo_test$a2), max(yahoo_test$a2)))+
  theme(plot.title = element_text(hjust = 0.5))

# SKIP
yahoo_test2 <- yahoo_test[c(9:16,30:37,51:53,55:57,72:79,93:100),]
yahoo_test2$name <- factor(yahoo_test2$name, levels = c("50","100","150","200")) 

yahoo_test2 %>% 
  ggplot(aes(x=yahoo_test2$name,y=as.numeric(yahoo_test2$a2),group=yahoo_test2$model,color=yahoo_test2$model)) +
  geom_line(size = 1) + 
  geom_point(size = 3, pch=18) + 
  labs(x = "詞嵌入維度 (D)" , y = "Macro-F1",title = "Skip-gram法 w為3和5 五種模型於各維度Macro-F1表現", color = "Models")+
  #scale_colour_manual(values = c("#006BA6", "#FFBC42", "#D81159","#8F2D56","gray","darkgreen"))+#改變顏色
  # coord_cartesian(ylim = c(min(yahoo_test$a2), max(yahoo_test$a2)))+
  theme(plot.title = element_text(hjust = 0.5))

# TFIDF
yahoo_test2 <- yahoo_test[c(17:20,38:41,59:61,80:83,101:104),]
yahoo_test2$name <- factor(yahoo_test2$name, levels = c("50","100","150","200")) 

yahoo_test2 %>% 
  ggplot(aes(x=yahoo_test2$name,y=as.numeric(yahoo_test2$a2),group=yahoo_test2$model,color=yahoo_test2$model)) +
  geom_line(size = 1) + 
  geom_point(size = 3, pch=18) + 
  labs(x = "詞嵌入維度 (D)" , y = "Macro-F1",title = "TFIDF法 五種模型於各維度Macro-F1表現", color = "Models")+
  #scale_colour_manual(values = c("#006BA6", "#FFBC42", "#D81159","#8F2D56","gray","darkgreen"))+#改變顏色
  # coord_cartesian(ylim = c(min(yahoo_test$a2), max(yahoo_test$a2)))+
  theme(plot.title = element_text(hjust = 0.5))



###################################################################################
## trip
## w=1:MSE  ,  2:ACC  ,  3:F1
w = 2
a2 = c()
for (z in 0:4){
  a1 <- c(trip_matrix2[w,1+z],trip_matrix2[w,6+z],trip_matrix2[w,11+z],trip_matrix2[w,16+z],
          trip_matrix2[w,21+z],trip_matrix2[w,26+z],trip_matrix2[w,31+z],trip_matrix2[w,36+z],
          trip_matrix2[w,41+z],trip_matrix2[w,46+z],trip_matrix2[w,51+z],trip_matrix2[w,56+z],
          trip_matrix2[w,61+z],trip_matrix2[w,66+z],trip_matrix2[w,71+z],trip_matrix2[w,76+z],
          trip_matrix2[w,81+z],trip_matrix2[w,86+z],trip_matrix2[w,91+z],trip_matrix2[w,96+z],
          trip_matrix2[w,101+z])
  
  a2 = c(a2,a1)
}
a3 <- as.data.frame(a2)

name <- rep(c("50","100","150","200",
              "50","100","150","200",
              "50","100","150","200",
              "50","100","150","200",
              "50","100","150","200",
              "300"),5)

model <- c(rep("clm_w=3",4),rep("clm_w=5",4),rep("clm_w=3",4),rep("clm_w=5",4),rep("clm",5),
           rep("naive_w=3",4),rep("naive_w=5",4),rep("naive_w=3",4),rep("naive_w=5",4),rep("naive",5),
           rep("logistic_w=3",4),rep("logistic_w=5",4),rep("logistic_w=3",4),rep("logistic_w=5",4),rep("logistic",5),
           rep("cratio_w=3",4),rep("cratio_w=5",4),rep("cratio_w=3",4),rep("cratio_w=5",4),rep("cratio",5),
           rep("acat_w=3",4),rep("acat_w=5",4),rep("acat_w=3",4),rep("acat_w=5",4),rep("acat",5))


trip_test <- cbind(a3,name,model)



## plot
as.numeric(as.character(trip_test$a2))
trip_test$a2 <- as.numeric(as.character(trip_test$a2))

### MSE
# CBOW 
trip_test2 <- trip_test[c(1:8,22:29,43:45,47:49,64:71,85:92),]
trip_test2$name <- factor(trip_test2$name, levels = c("50","100","150","200")) 

trip_test2 %>% 
  ggplot(aes(x=trip_test2$name,y=as.numeric(trip_test2$a2),group=trip_test2$model,color=trip_test2$model)) +
  geom_line(size = 1) + 
  geom_point(size = 3, pch=18) + 
  labs(x = "詞嵌入維度 (D)" , y = "MSE",title = "CBOW法 w為3和5 五種模型於各維度MSE表現", color = "Models")+
  theme(plot.title = element_text(hjust = 0.5))

# SKIP
trip_test2 <- trip_test[c(9:16,30:37,51:53,55:57,72:79,93:100),]
trip_test2$name <- factor(trip_test2$name, levels = c("50","100","150","200")) 

trip_test2 %>% 
  ggplot(aes(x=trip_test2$name,y=as.numeric(trip_test2$a2),group=trip_test2$model,color=trip_test2$model)) +
  geom_line(size = 1) + 
  geom_point(size = 3, pch=18) + 
  labs(x = "詞嵌入維度 (D)" , y = "MSE",title = "Skip-gram法 w為3和5 五種模型於各維度MSE表現", color = "Models")+
  theme(plot.title = element_text(hjust = 0.5))

# TFIDF
trip_test2 <- trip_test[c(17:20,38:41,59:61,80:83,101:104),]
trip_test2$name <- factor(trip_test2$name, levels = c("50","100","150","200")) 

trip_test2 %>% 
  ggplot(aes(x=trip_test2$name,y=as.numeric(trip_test2$a2),group=trip_test2$model,color=trip_test2$model)) +
  geom_line(size = 1) + 
  geom_point(size = 3, pch=18) + 
  labs(x = "詞嵌入維度 (D)" , y = "MSE",title = "TFIDF法 五種模型於各維度MSE表現", color = "Models")+
  theme(plot.title = element_text(hjust = 0.5))


### ACC
# CBOW 
trip_test2 <- trip_test[c(1:8,22:29,43:45,47:49,64:71,85:92),]
trip_test2$name <- factor(trip_test2$name, levels = c("50","100","150","200")) 

trip_test2 %>% 
  ggplot(aes(x=trip_test2$name,y=as.numeric(trip_test2$a2),group=trip_test2$model,color=trip_test2$model)) +
  geom_line(size = 1) + 
  geom_point(size = 3, pch=18) + 
  labs(x = "詞嵌入維度 (D)" , y = "Accuracy(%)",title = "CBOW法 w為3和5 五種模型於各維度Accuracy表現", color = "Models")+
  theme(plot.title = element_text(hjust = 0.5))

# SKIP
trip_test2 <- trip_test[c(9:16,30:37,51:53,55:57,72:79,93:100),]
trip_test2$name <- factor(trip_test2$name, levels = c("50","100","150","200")) 

trip_test2 %>% 
  ggplot(aes(x=trip_test2$name,y=as.numeric(trip_test2$a2),group=trip_test2$model,color=trip_test2$model)) +
  geom_line(size = 1) + 
  geom_point(size = 3, pch=18) + 
  labs(x = "詞嵌入維度 (D)" , y = "Accuracy(%)",title = "Skip-gram法 w為3和5 五種模型於各維度Accuracy表現", color = "Models")+
  theme(plot.title = element_text(hjust = 0.5))

# TFIDF
trip_test2 <- trip_test[c(17:20,38:41,59:61,80:83,101:104),]
trip_test2$name <- factor(trip_test2$name, levels = c("50","100","150","200")) 

trip_test2 %>% 
  ggplot(aes(x=trip_test2$name,y=as.numeric(trip_test2$a2),group=trip_test2$model,color=trip_test2$model)) +
  geom_line(size = 1) + 
  geom_point(size = 3, pch=18) + 
  labs(x = "詞嵌入維度 (D)" , y = "Accuracy(%)",title = "TFIDF法 五種模型於各維度Accuracy表現", color = "Models")+
  theme(plot.title = element_text(hjust = 0.5))


### F1
# CBOW 
trip_test2 <- trip_test[c(1:8,22:29,43:45,47:49,64:71,85:92),]
trip_test2$name <- factor(trip_test2$name, levels = c("50","100","150","200")) 

trip_test2 %>% 
  ggplot(aes(x=trip_test2$name,y=as.numeric(trip_test2$a2),group=trip_test2$model,color=trip_test2$model)) +
  geom_line(size = 1) + 
  geom_point(size = 3, pch=18) + 
  labs(x = "詞嵌入維度 (D)" , y = "Macro-F1",title = "CBOW法 w為3和5 五種模型於各維度Macro-F1表現", color = "Models")+
  theme(plot.title = element_text(hjust = 0.5))

# SKIP
trip_test2 <- trip_test[c(9:16,30:37,51:53,55:57,72:79,93:100),]
trip_test2$name <- factor(trip_test2$name, levels = c("50","100","150","200")) 

trip_test2 %>% 
  ggplot(aes(x=trip_test2$name,y=as.numeric(trip_test2$a2),group=trip_test2$model,color=trip_test2$model)) +
  geom_line(size = 1) + 
  geom_point(size = 3, pch=18) + 
  labs(x = "詞嵌入維度 (D)" , y = "Macro-F1",title = "Skip-gram法 w為3和5 五種模型於各維度Macro-F1表現", color = "Models")+
  theme(plot.title = element_text(hjust = 0.5))

# TFIDF
trip_test2 <- trip_test[c(17:20,38:41,59:61,80:83,101:104),]
trip_test2$name <- factor(trip_test2$name, levels = c("50","100","150","200")) 

trip_test2 %>% 
  ggplot(aes(x=trip_test2$name,y=as.numeric(trip_test2$a2),group=trip_test2$model,color=trip_test2$model)) +
  geom_line(size = 1,) + 
  geom_point(size = 3, pch=18) + 
  labs(x = "詞嵌入維度 (D)" , y = "Macro-F1",title = "TFIDF法 五種模型於各維度Macro-F1表現", color = "Models")+
  theme(plot.title = element_text(hjust = 0.5))


