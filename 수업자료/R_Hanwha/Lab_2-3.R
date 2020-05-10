rm(list=ls())
data_cust2 <- read.csv("Working/data_cust_2-2.csv", header=TRUE)

data_cust_known <- subset(data_cust2, subset=DIVIDED_SET==1) 	#사기자 여부가 판별된 데이터를 이용해 학습시킴
data_cust_known <- subset(data_cust_known, select=-DIVIDED_SET)	#DIVIDED_SET 열 제거

idx <- sample(1:nrow(data_cust_known), nrow(data_cust_known), replace=FALSE)
data_cust_known <- data_cust_known[idx, ]

cust_train <- data_cust_known[1:floor(nrow(data_cust_known)*0.7), ]
cust_train <- cust_train[order(cust_train$CUST_ID),]

cust_test <- data_cust_known[(floor(nrow(data_cust_known)*0.7)+1):nrow(data_cust_known), ]
cust_test <- cust_test[order(cust_test$CUST_ID),]

write.csv(cust_train, "Working/data_cust_2-3_train.csv", row.names = FALSE)
write.csv(cust_test, "Working/data_cust_2-3_test.csv", row.names = FALSE)
