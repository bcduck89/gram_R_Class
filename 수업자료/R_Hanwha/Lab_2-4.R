rm(list=ls())
cust_train <- read.csv("Working/data_cust_2-3_train.csv", header=TRUE)
cust_test <- read.csv("Working/data_cust_2-3_test.csv", header=TRUE)

#####################################################
install.packages("xgboost")
library(xgboost)

cust_train <- cbind(subset(cust_train, select=-SIU_CUST_YN), subset(cust_train, select=SIU_CUST_YN))

cust_test_yn <- subset(cust_test, select=c(CUST_ID, SIU_CUST_YN))   #예측 한 데이터와 비교하기 위해 고객별 사기자 여부를 임시 저장함
cust_test <- subset(cust_test, select=-SIU_CUST_YN) #사기자 여부 라벨 제거

cust_train <- subset(cust_train, select=-CUST_ID) #트레이닝 셋과 테스트 셋에서 고객 아이디 열 제거
cust_test <- subset(cust_test, select=-CUST_ID)

y <- cust_train[,ncol(cust_train)]  #트레이닝 셋의 사기자 여부에 해당하는 열(종속변수)
x <- subset(cust_train, select=-SIU_CUST_YN) #x는 분석하기 위한 데이터(독립변수)

x <- as.matrix(x)
x <- matrix(suppressWarnings(as.numeric(x)), nrow(x), ncol(x))

#여러개로 분류 가능함 넘 클래스 숫자가 분류 숫자임
param <- list("objective"="multi:softprob",
              "eval_metric"="mlogloss",
              "num_class"=2,
              "nthread"=8)

# Run Cross Validation
bst.cv <- xgb.cv(param=param, 
                 data=x[1:length(y), ], 
                 nrounds=50, nfold = 3, 
                 label=y, missing=NaN)
rm(bst.cv)

bst_model <- xgboost(param=param, 
                     data=x[1:length(y), ], 
                     nrounds=60, 
                     label=y, 
                     missing=NaN) # Train the model(모델 생성)

x_test <- as.matrix(cust_test)
x_test <- matrix(suppressWarnings(as.numeric(x_test)), nrow(x_test), ncol(x_test))
predicted_data <- predict(bst_model, x_test, missing=NaN)

predicted_data <- matrix(predicted_data, ncol=2, nrow=length(predicted_data)/2, byrow=TRUE) #2열짜리 매트릭스로 변환
predicted_yn <- max.col(predicted_data)  #출력으로 나온 클래스 라벨(1또는 2)이 결과 값임

#예측한 데이터가 정상인은 1, 사기자는 2이므로 이것을 정상인은 0, 사기자는 1로 바꿈
#제출해야 할 데이터가 정상인은 0, 사기자는 1 형식 또는 정상인은 N, 사기자는 Y 형식임
yn12_to_01 <- function(row) {
  if(row == 1) row = 0
  else if(row == 2) row = 1
}
predicted_yn <- sapply(predicted_yn, yn12_to_01)
rm(yn12_to_01)

result_xgboost <- cbind(cust_test_yn, predicted_yn) 
write.csv(result_xgboost, "Working/result_xgboost.csv", row.names = FALSE)


#####################################################
rm(list=ls())
cust_train <- read.csv("Working/data_cust_2-3_train.csv", header=TRUE)
cust_test <- read.csv("Working/data_cust_2-3_test.csv", header=TRUE)
cust_test_yn <- subset(cust_test, select=c(CUST_ID, SIU_CUST_YN))

install.packages("nnet")
library(nnet)

x = subset(cust_train, select=-c(CUST_ID, SIU_CUST_YN)) #트레이닝 셋의 독립변수
y = subset(cust_train, select=SIU_CUST_YN) #트레이닝 셋의 라벨(사기자여부), 종속변수

nnet_model <- nnet(x, y,     
                   size=30, na.action=na.omit, rang=0.1,
                   abstol=0.0001, maxit=500, MaxNWts=30000, decay=0.05)

cust_test <- subset(cust_test, select=-c(CUST_ID, SIU_CUST_YN))
nnet_predicted_data <- predict(nnet_model, cust_test)
nnet_predicted_yn <- round(nnet_predicted_data)

result_nnet <- cbind(cust_test_yn, nnet_predicted_yn) 
write.csv(result_nnet, "Working/result_nnet.csv", row.names = FALSE)


#####################################################
rm(list=ls())
cust_train <- read.csv("Working/data_cust_2-3_train.csv", header=TRUE)
cust_test <- read.csv("Working/data_cust_2-3_test.csv", header=TRUE)
cust_test_yn <- subset(cust_test, select=c(CUST_ID, SIU_CUST_YN))

install.packages("e1071")
library(e1071)

x = subset(cust_train, select=-c(CUST_ID, SIU_CUST_YN))
y = subset(cust_train, select=SIU_CUST_YN)

x <- x[, sapply(x, function(v) var(v, na.rm=TRUE)!=0)] #분산이 0인 열 삭제

Sys.time() #약 10분 정도 소요됨
svm_model <- svm(x, y, scale=TRUE, cross=10, type="C-classification", kernel="radial")
Sys.time()

cust_test <- subset(cust_test, select=-c(CUST_ID, SIU_CUST_YN))
cust_test <- cust_test[, sapply(cust_test, function(v) var(v, na.rm=TRUE)!=0)] #분산이 0인 열 삭제

svm_predicted_data <- predict(svm_model, cust_test)

result_svm <- cbind(cust_test_yn, svm_predicted_data) 
write.csv(result_svm, "Working/result_svm.csv", row.names = FALSE)




