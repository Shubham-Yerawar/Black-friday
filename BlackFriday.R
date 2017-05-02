
setwd("I:/black friday/codes n data/")
black <- read.csv(file = "train.csv",header=TRUE,as.is=T)
test_black <- read.csv(file = "test.csv", header = TRUE, as.is = T)

black[black==""]<- NA
black[is.na(black)] <- 0
#
test_black[test_black==""]<-NA
test_black[is.na(test_black)]<-0


col_numeric <- c(1,9:12)
for(i in col_numeric){
  black[,i] <- as.numeric(black[,i])
}

col_factor <- c(3:8)
for(i in col_factor){
  black[,i] <- as.factor(black[,i])
}


col_numeric_test <- c(1,9:11)
for(i in col_numeric_test){
  test_black[,i] <- as.numeric(test_black[,i])
}

col_factor_test <- c(3:8)
for(i in col_factor_test){
  test_black[,i] <- as.factor(test_black[,i])
}

#summary(black)
set.seed(999)
smp_size1 <- 100          # change to 150000 or 200000
smp_ind1 <- sample(seq_len(nrow(black)),size = smp_size1)
train_data_smp1 <- black[smp_ind1,]

set.seed(123)
smp_size2 <- 2000
smp_ind2 <- sample(seq_len(nrow(black)),size = smp_size2)
test_data_smp11 <- black[smp_ind2,]

#install.packages("Boruta")
#library(Boruta)
#boruta.train <- Boruta(Purchase ~ . -c(User_ID,Product_ID) , data= train_data_smp1, pValue=0.01 , doTrace=2)
#print(boruta.train)

#final.boruta <- TentativeRoughFix(boruta.train)
#print(final.boruta)

#getSelectedAttributes(final.boruta, withTentative = F)

############################################################
black1 <- train_data_smp1[c(1,2,6,9,10,11,12)] 
test_data_smp1 <- test_data_smp11[c(1,2,6,9,10,11)]
test_black_new <- test_black[c(1,2,6,9,10,11)] 


######################################################
#svr
rmse <- function(error)
{
  sqrt(mean(error^2))
}

install.packages("e1071")
library(e1071)
model_svr <- svm(Purchase ~ City_Category + Product_Category_1 + Product_Category_2 + Product_Category_3, data = black1)

########## tuning ######################



predicted <- predict(model_svr,test_data_smp1 )
#points(test_data_smp11$Purchase, predicted, col = "red", pch=4)
#plot(points(test_data_smp11$Purchase, predicted, col = "red", pch=4))
error <- test_data_smp11$Purchase - predicted
svrPredictionRMSE <- rmse(error)
svrPredictionRMSE

ts1 <- test_black_new[1:50000,]
ts2 <- test_black_new[50001:100000,]
ts3 <- test_black_new[100001:150000,]
ts4<- test_black_new[150001:200000,]
ts5 <- test_black_new[200001:233599,]

#sample 1
predicted1 <- predict(model_svr,ts1 )
write.csv(predicted1,"ts1.csv",row.names = F)


#sample 2
predicted2 <- predict(model_svr,ts2 )
write.csv(predicted2,"ts2.csv",row.names = F)


#sample 3
predicted3 <- predict(model_svr,ts3 )
write.csv(predicted3,"ts3.csv",row.names = F)


#sample 4
predicted4 <- predict(model_svr,ts4 )
write.csv(predicted4,"ts4.csv",row.names = F)


#sample 5
predicted5 <- predict(model_svr,ts5 )
write.csv(predicted5,"ts5.csv",row.names = F)