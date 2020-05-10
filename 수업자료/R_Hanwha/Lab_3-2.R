rm(list=ls())

cust_train <- read.csv("Working/model_train_set.csv", header=TRUE)
cust_test <- read.csv("Working/model_test_set.csv", header=TRUE)

#####################################################
#XGBoost를 이용한 분석(기본 정보를 이용한 분석)
#####################################################
install.packages('xgboost')
library(xgboost)

#트레이닝에는 라벨이 있어야 함(종속 변수를 가장 오른쪽에)
cust_train <- cbind(subset(cust_train, select=-SIU_CUST_YN), subset(cust_train, select=SIU_CUST_YN))

#테스트에는 라벨이 없어야 함, #cust_test_yn 변수는 예측 후 원본과 비교하기 위해서 임시 저장함
cust_test_yn <- subset(cust_test, select=c(CUST_ID, SIU_CUST_YN)) 
cust_test <- subset(cust_test, select=-SIU_CUST_YN) #사기자 여부 라벨 제거

#트레이닝 셋과 테스트 셋에서 고객 아이디 열 제거
cust_train <- subset(cust_train, select=-CUST_ID)
cust_test <- subset(cust_test, select=-CUST_ID)

y <- cust_train[,ncol(cust_train)] #트레이닝 셋의 사기자 여부에 해당하는 열
table(y) #트레이닝 셋의 일반인가 사기자 수 18801  1806 

x <- subset(cust_train, select=-SIU_CUST_YN) #x는 분석하기 위한 데이터
nrow(x) #20607

x <- as.matrix(x)
x <- matrix(suppressWarnings(as.numeric(x)), nrow(x), ncol(x))

#여러개로 분류 가능함 넘 클래스 숫자가 분류 숫자임
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 2, #2개로만 분류
              "nthread" = 8)

# Train the model(모델 생성)
bst_trained_model <- xgboost(param=param, 
                             data=x[1:length(y),],
                             nrounds=60, 
                             label=y, missing=NaN)

#테스트 셋을 이용하여 예측
x_test <- as.matrix(cust_test)
x_test <- matrix(suppressWarnings(as.numeric(x_test)), nrow(x_test), ncol(x_test))
predicted_data <- predict(bst_trained_model, x_test, missing=NaN)

#2열짜리 매트릭스로 변환
predicted_data <- matrix(predicted_data, ncol=2, nrow=length(predicted_data)/2, byrow=TRUE)

#두 열 중에 큰 값의 열을 반환, #두 범주로 나누어 예측했기 때문에 사기가 아니다와 사기이다 두 값이  각각 V1열과 V2열에 저장되어 있음
predicted_yn <- max.col(predicted_data)  #신경망 출력으로 나온 클래스 라벨

yn12_to_01 <- function(row) {
  if(row == 1) 
    row = 0
  else if(row == 2)
    row = 1
}
predicted_yn <- sapply(predicted_yn, yn12_to_01)
rm(yn12_to_01)

#테스트셋에서 뽑아낸 원래의 값과 예측한 값을 합침
result <- cbind(cust_test_yn, predicted_yn) 

#정답 불러오기.
answer <- read.csv("answer.csv", header=TRUE)
result$SIU_CUST_YN <- answer$SIU_CUST_YN

#결과 저장
write.csv(result, "Working/model_result.csv", row.names = FALSE)

