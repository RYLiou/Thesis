######################################################################
### soup data
#####

data = soup
data <- data[,c(4,3,5,7,8,10,11)]
write.table(data, file="soup.csv", sep=",", row.names=F, col.names = F, na = "NA")
######################################################################
### diamonds data
#####

data = diamonds
class(data$clarity)
data$clarity <- as.character(data$clarity)
data[which(data$clarity == 'I1'),4] = '1' 
data[which(data$clarity == 'SI2'),4] = '2' 
data[which(data$clarity == 'SI1'),4] = '3' 
data[which(data$clarity == 'VS2'),4] = '4'
data[which(data$clarity == 'VS1'),4] = '5' 
data[which(data$clarity == 'VVS2'),4] = '6' 
data[which(data$clarity == 'VVS1'),4] = '7' 
data[which(data$clarity == 'IF'),4] = '8' 
data$clarity <- as.ordered(data$clarity)
data = as.data.frame(data)
data$price <- data$price/1000
#I1 < SI2 < SI1 < VS2 < VS1 < VVS2 < VVS1 < IF
data <- data[,c(4,1,2,3,6,7,8,9,10)]
write.table(data, file="diamonds.csv", sep=",", row.names=F, col.names = F, na = "NA")

######################################################################
### car data
#####
data <- read.table('D:\\desktop\\論文\\data\\原始資料\\cardata.txt',sep=',')

data$V7 <- as.character(data$V7)
data[which(data$V7 == 'unacc'),7] = '1' 
data[which(data$V7 == 'acc'),7] = '2' 
data[which(data$V7 == 'good'),7] = '3' 
data[which(data$V7 == 'vgood'),7] = '4' 

data <- data[,c(7,1,2,3,4,5,6)]
write.table(data, file="car.csv", sep=",", row.names=F, col.names = F, na = "NA")

######################################################################
### eshop data
#####
library(classInt)

data <- read.table('D:\\desktop\\論文\\data\\原始資料\\e-shop clothing 2008.csv', sep=';', header = T)
classIntervals(data$order, 5, style = 'quantile')

new = NULL
for (i in 1:nrow(data)){
  if (1 <= data$order[i] & data$order[i] < 2){
    new[i] = '1'
  } else if (2 <= data$order[i] & data$order[i] < 4){ new[i] = '2'
  } else if (4 <= data$order[i] & data$order[i] < 7){ new[i] = '3'
  } else if (7 <= data$order[i] & data$order[i] < 14){ new[i] = '4'
  } else if (14 <= data$order[i] & data$order[i] <= 195){ new[i] = '5'}
}

data <- cbind(data,new)
data$new <- as.character(data$new)

data <- data[,c(15,3,5,6,7,9,10,11,12,13,14)]
write.table(data, file="eshop.csv", sep=",", row.names=F, col.names = F, na = "NA")

######################################################################
### Estate_data
#####
library(classInt)

data <- read.table('D:\\desktop\\論文\\data\\原始資料\\Estate_data.csv', sep=',', header = T)
classIntervals(data$CRIM, 5, style = 'quantile')

new = NULL
for (i in 1:nrow(data)){
  if (0 <= data$CRIM[i] & data$CRIM[i] < 0.06466){
    new[i] = '1'
  } else if (0.06466 <= data$CRIM[i] & data$CRIM[i] < 0.15098){ new[i] = '2'
  } else if (0.15098 <= data$CRIM[i] & data$CRIM[i] < 0.55778){ new[i] = '3'
  } else if (0.55778 <= data$CRIM[i] & data$CRIM[i] < 5.44114){ new[i] = '4'
  } else if (5.44114 <= data$CRIM[i] & data$CRIM[i] <= 88.9762){ new[i] = '5'}
}

data <- cbind(data,new)
data$new <- as.character(data$new)
data <- data[complete.cases(data),]

data <- data[,c(15,2,3,4,5,6,7,8,9,10,11,12,13,14)]

write.table(data, file="estate.csv", sep=",", row.names=F, col.names = F, na = "NA")

######################################################################
### StudentsPerformance
#####
library(classInt)

data <- read.table('D:\\desktop\\論文\\data\\原始資料\\StudentsPerformance.csv', sep=',', header = T)

cal_mean <- data[,c(6,7,8)]
mean_score <- apply(cal_mean, mean, MARGIN = 1)
data <- cbind(data[,c(1,2,3,4)],mean_score)

classIntervals(data$mean_score, 5, style = 'quantile')

new = NULL
for (i in 1:nrow(data)){
  if (0 <= data$mean_score[i] & data$mean_score[i] < 55.66667){
    new[i] = '1'
  } else if (55.66667 <= data$mean_score[i] & data$mean_score[i] < 65){ new[i] = '2'
  } else if (65 <= data$mean_score[i] & data$mean_score[i] < 72){ new[i] = '3'
  } else if (72 <= data$mean_score[i] & data$mean_score[i] < 79.66667){ new[i] = '4'
  } else if (79.66667 <= data$mean_score[i] & data$mean_score[i] <= 100){ new[i] = '5'}
}

data <- cbind(data,new)
data$new <- as.character(data$new)
data <- data[complete.cases(data),]

data <- data[,c(6,1,2,3,4)]

write.table(data, file="StudentsPerformance.csv", sep=",", row.names=F, col.names = F, na = "NA")

######################################################################
### AirQualityUCI
#####
library(classInt)

data <- read.table('D:\\desktop\\論文\\data\\原始資料\\AirQualityUCI.csv', sep=',', header = T)

data <- data[,-c(16,17)]
data <- data[complete.cases(data),]

classIntervals(data$RH, 5, style = 'quantile')

new = NULL
for (i in 1:nrow(data)){
  if (-9999 <= data$RH[i] & data$RH[i] < 30.5){
    new[i] = '1'
  } else if (30.5 <= data$RH[i] & data$RH[i] < 43.3){ new[i] = '2'
  } else if (43.3 <= data$RH[i] & data$RH[i] < 53.6){ new[i] = '3'
  } else if (53.6 <= data$RH[i] & data$RH[i] < 64.8){ new[i] = '4'
  } else if (64.8 <= data$RH[i] & data$RH[i] <= 88.7){ new[i] = '5'}
}

data <- cbind(data,new)
data$new <- as.character(data$new)
data <- data[complete.cases(data),]

data <- data[,c(16,3,4,5,6,7,8,9,10,11,12,13)]

write.table(data, file="AirQualityUCI.csv", sep=",", row.names=F, col.names = F, na = "NA")


######################################################################
### ENB2012_data   1
#####
library(classInt)

data <- read.table('D:\\desktop\\論文\\data\\原始資料\\ENB2012_data.csv', sep=',', header = T)

data <- data[,-c(11,12)]
data <- data[complete.cases(data),]

classIntervals(data$Y1, 5, style = 'quantile')

new = NULL
for (i in 1:nrow(data)){
  if (-9999 <= data$Y1[i] & data$Y1[i] < 12.63){
    new[i] = '1'
  } else if (12.63 <= data$Y1[i] & data$Y1[i] < 15.23){ new[i] = '2'
  } else if (15.23 <= data$Y1[i] & data$Y1[i] < 25.668){ new[i] = '3'
  } else if (25.668 <= data$Y1[i] & data$Y1[i] < 32.508){ new[i] = '4'
  } else if (32.508 <= data$Y1[i] & data$Y1[i] <= 43.1){ new[i] = '5'}
}

data <- cbind(data,new)
data$new <- as.character(data$new)
data <- data[complete.cases(data),]

data <- data[,c(11,1,2,3,4,5,6,7,8)]

write.table(data, file="ENB2012_Y1.csv", sep=",", row.names=F, col.names = F, na = "NA")

######################################################################
### ENB2012_data   2
#####
library(classInt)

data <- read.table('D:\\desktop\\論文\\data\\原始資料\\ENB2012_data.csv', sep=',', header = T)

data <- data[,-c(11,12)]
data <- data[complete.cases(data),]

classIntervals(data$Y2, 5, style = 'quantile')

new = NULL
for (i in 1:nrow(data)){
  if (10.9 <= data$Y2[i] & data$Y2[i] < 15.116){
    new[i] = '1'
  } else if (15.116 <= data$Y2[i] & data$Y2[i] < 17.72){ new[i] = '2'
  } else if (17.72 <= data$Y2[i] & data$Y2[i] < 28.286){ new[i] = '3'
  } else if (28.286 <= data$Y2[i] & data$Y2[i] < 34.062){ new[i] = '4'
  } else if (34.062 <= data$Y2[i] & data$Y2[i] <= 48.03){ new[i] = '5'}
}

data <- cbind(data,new)
data$new <- as.character(data$new)
data <- data[complete.cases(data),]

data <- data[,c(11,1,2,3,4,5,6,7,8)]

write.table(data, file="ENB2012_Y2.csv", sep=",", row.names=F, col.names = F, na = "NA")


######################################################################
### qsar_fish_toxicity
#####
library(classInt)

data <- read.table('D:\\desktop\\論文\\data\\原始資料\\qsar_fish_toxicity.csv', sep=';', header = F)

data <- data[complete.cases(data),]

classIntervals(data$V7, 5, style = 'quantile')

new = NULL
for (i in 1:nrow(data)){
  if (-9999 <= data$V7[i] & data$V7[i] < 2.9562){
    new[i] = '1'
  } else if (2.9562 <= data$V7[i] & data$V7[i] < 3.6954){ new[i] = '2'
  } else if (3.6954 <= data$V7[i] & data$V7[i] < 4.38){ new[i] = '3'
  } else if (4.38 <= data$V7[i] & data$V7[i] < 5.187){ new[i] = '4'
  } else if (5.187 <= data$V7[i] & data$V7[i] <= 9.612){ new[i] = '5'}
}

data <- cbind(data,new)
data$new <- as.character(data$new)
data <- data[complete.cases(data),]

data <- data[,c(8,1,2,3,4,5,6)]

write.table(data, file="fish_toxicity.csv", sep=",", row.names=F, col.names = F, na = "NA")

######################################################################
### housing
#####
library(classInt)

data <- read.table('D:\\desktop\\論文\\data\\原始資料\\housing.csv', sep=',', header = T)

data <- data[complete.cases(data),]

classIntervals(data$median_house_value, 5, style = 'quantile')

new = NULL
for (i in 1:nrow(data)){
  if (-9999 <= data$median_house_value[i] & data$median_house_value[i] < 107300){
    new[i] = '1'
  } else if (107300 <= data$median_house_value[i] & data$median_house_value[i] < 157300){ new[i] = '2'
  } else if (157300 <= data$median_house_value[i] & data$median_house_value[i] < 209400){ new[i] = '3'
  } else if (209400 <= data$median_house_value[i] & data$median_house_value[i] < 290000){ new[i] = '4'
  } else if (290000 <= data$median_house_value[i] & data$median_house_value[i] <= 500001){ new[i] = '5'}
}

data <- cbind(data,new)
data$new <- as.character(data$new)
data <- data[complete.cases(data),]

data <- data[,c(11,1,2,3,4,5,6,7,8,10)]

write.table(data, file="housings.csv", sep=",", row.names=F, col.names = F, na = "NA")


######################################################################
### affair
#####
library(classInt)

data <- read.table('D:\\desktop\\論文\\data\\原始資料\\Affairs.csv', sep=',', header = T)

data <- data[complete.cases(data),]

new = NULL
for (i in 1:nrow(data)){
  if (-9999 <= data$median_house_value[i] & data$median_house_value[i] < 107300){
    new[i] = '1'
  } else if (107300 <= data$median_house_value[i] & data$median_house_value[i] < 157300){ new[i] = '2'
  } else if (157300 <= data$median_house_value[i] & data$median_house_value[i] < 209400){ new[i] = '3'
  } else if (209400 <= data$median_house_value[i] & data$median_house_value[i] < 290000){ new[i] = '4'
  } else if (290000 <= data$median_house_value[i] & data$median_house_value[i] <= 500001){ new[i] = '5'}
}

data <- cbind(data,new)
data$new <- as.character(data$new)
data <- data[complete.cases(data),]

data <- data[,c(10,1,2,3,4,5,6,7,8,9)]

write.table(data, file="affair.csv", sep=",", row.names=F, col.names = F, na = "NA")



######################################################################
### abalone
#####
library(classInt)

data <- read.table('D:\\desktop\\論文\\data\\原始資料\\abalone_banchmark.txt', sep=',', header = T)
table(data$response)

data <- data[complete.cases(data),]

new = NULL
for (i in 1:nrow(data)){
  if (-9999 <= data$median_house_value[i] & data$median_house_value[i] < 107300){
    new[i] = '1'
  } else if (107300 <= data$median_house_value[i] & data$median_house_value[i] < 157300){ new[i] = '2'
  } else if (157300 <= data$median_house_value[i] & data$median_house_value[i] < 209400){ new[i] = '3'
  } else if (209400 <= data$median_house_value[i] & data$median_house_value[i] < 290000){ new[i] = '4'
  } else if (290000 <= data$median_house_value[i] & data$median_house_value[i] <= 500001){ new[i] = '5'}
}

data <- cbind(data,new)
data$new <- as.character(data$new)
data <- data[complete.cases(data),]

data <- data[,c(1,2,3,4,5,6,7,8)]

write.table(data, file="abalone.csv", sep=",", row.names=F, col.names = F, na = "NA")



######################################################################
### wine
#####
library(classInt)

data <- read.table('D:\\desktop\\論文\\data\\原始資料\\wine_white_benchmark.txt', sep=',', header = T)
table(data$response)
data <- data[complete.cases(data),]

new = NULL
for (i in 1:nrow(data)){
  if (-9999 <= data$median_house_value[i] & data$median_house_value[i] < 107300){
    new[i] = '1'
  } else if (107300 <= data$median_house_value[i] & data$median_house_value[i] < 157300){ new[i] = '2'
  } else if (157300 <= data$median_house_value[i] & data$median_house_value[i] < 209400){ new[i] = '3'
  } else if (209400 <= data$median_house_value[i] & data$median_house_value[i] < 290000){ new[i] = '4'
  } else if (290000 <= data$median_house_value[i] & data$median_house_value[i] <= 500001){ new[i] = '5'}
}

data <- cbind(data,new)
data$new <- as.character(data$new)
data <- data[complete.cases(data),]

data <- data[,c(1,2,3,4,5,6,7,8,9)]

write.table(data, file="wine_white.csv", sep=",", row.names=F, col.names = F, na = "NA")



######################################################################
### stock
#####
library(classInt)

data <- read.table('D:\\desktop\\論文\\data\\原始資料\\stock.txt', sep=',', header = T)
table(data$response)
data <- data[complete.cases(data),]

new = NULL
for (i in 1:nrow(data)){
  if (-9999 <= data$median_house_value[i] & data$median_house_value[i] < 107300){
    new[i] = '1'
  } else if (107300 <= data$median_house_value[i] & data$median_house_value[i] < 157300){ new[i] = '2'
  } else if (157300 <= data$median_house_value[i] & data$median_house_value[i] < 209400){ new[i] = '3'
  } else if (209400 <= data$median_house_value[i] & data$median_house_value[i] < 290000){ new[i] = '4'
  } else if (290000 <= data$median_house_value[i] & data$median_house_value[i] <= 500001){ new[i] = '5'}
}

data <- cbind(data,new)
data$new <- as.character(data$new)
data <- data[complete.cases(data),]

data <- data[,c(1,2,3,4,5,6,7,8,9,10)]

write.table(data, file="stock.csv", sep=",", row.names=F, col.names = F, na = "NA")


