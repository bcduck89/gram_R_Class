rm(list=ls())

data_cust3 <- read.csv("Working/data_cust_2-2.csv", header=TRUE)

#####################################################
#데이터를 사기여부가 판별된 데이터와 그렇지 않은 데이터 분리
#####################################################
cust_train <- subset(data_cust3, subset=!(data_cust3$SIU_CUST_YN=="")) #Training set
cust_train <- cust_train[order(cust_train$CUST_ID),] #트레이닝 셋 정렬
nrow(cust_train) #20607

cust_test <- subset(data_cust3, subset=(is.na(data_cust3$SIU_CUST_YN))) #Test set
cust_test <- cust_test[order(cust_test$CUST_ID),] #테스트 셋 정렬
nrow(cust_test) #1793

write.csv(cust_train, "Working/model_train_set.csv", row.names = FALSE)
write.csv(cust_test, "Working/model_test_set.csv", row.names = FALSE)
