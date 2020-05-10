install.packages("e1071")
library(e1071)

rm(list=ls())
data_cust2 <- read.csv("Working/data_cust_2-1.csv", header=TRUE)

svm.data <- subset(data_cust2, select=-c(CUST_ID,DIVIDED_SET))
svm.data <- cbind(subset(svm.data, select=-SIU_CUST_YN), subset(svm.data, select=SIU_CUST_YN)) #종속변수를 가장 마지막 열로 옮김

svm.data <- svm.data[, sapply(svm.data, function(v) var(v, na.rm=TRUE)!=0)] #분산이 0인 열 삭제

pca <- function(svm.data) {
  x = svm.data[, -ncol(svm.data)]
  y = svm.data[, ncol(svm.data)]
  pc = prcomp(x, scale=TRUE) # scale=TRUE이면 주성분 분석을 실행하기 전에 독립변수의 정규화를 진행하라
  selected_data = cbind(pc[[5]], y)  
  return (selected_data)
}
selected_data <- pca(svm.data)
selected_data <- cbind(subset(data_cust2, select=c(CUST_ID, DIVIDED_SET)), selected_data)
names(selected_data)[ncol(selected_data)] <- c("SIU_CUST_YN")


#manually
data_cust2 <- subset(data_cust2, select=-c(RESI_COST, RESI_TYPE_CODE, TOTALPREM, MINCRDT, MAXCRDT))
#데이터 임시 저장
write.csv(data_cust2, "Working/data_cust_2-2.csv", row.names = FALSE)

library(e1071)

svmrfe = function(svm.data, numOfFeatures){
  x = svm.data[, -ncol(svm.data)]
  y = svm.data[, ncol(svm.data)]
  n = ncol(x)
  survivingFeaturesIndexes = seq(1:n)
  featureRankedList = vector(length=n)
  rankedFeatureIndex = n
  while(length(survivingFeaturesIndexes)>0){ 
    svmModel = svm(x[, survivingFeaturesIndexes], y, scale=TRUE, cross=10, type="C-classification", kernel="radial")
    w = t(svmModel$coefs)%*%svmModel$SV  # SVM의 가중치 벡터 계산
    rankingCriteria = w * w  # 가중치 벡터를 제곱하여 순서를 정하는데 사용
    ranking = sort(rankingCriteria, index.return = TRUE)$ix  # 변수들의 순서를 정함
    # featureRankedList를 업데이트 (가장 영향력이 부족한 변수를 낮은 순위에 저장)
    featureRankedList[rankedFeatureIndex] = survivingFeaturesIndexes[ranking[1]]
    rankedFeatureIndex = rankedFeatureIndex - 1
    (survivingFeaturesIndexes = survivingFeaturesIndexes[-ranking[1]]) # 가장 영향력이 부족한 변수를 제거
    length(survivingFeaturesIndexes)
  }
  index = sort(featureRankedList[1:numOfFeatures])
  selectedData = subset(x, select = index)
  selectedData = cbind(selectedData, y)
  return(selectedData)
}

#selected_data = svmrfe(svm.data, 25) #25개 독립변수만 선택함, 시간이 많이 소요

