#####################################################
#테스트셋의 원본 데이터와 예측한 데이터를 이용한 검정 통계량 출력
#####################################################
result <- read.csv("Working/result_xgboost.csv", header=TRUE)

temp <- table(result$SIU_CUST_YN, result$predicted_yn)
temp
temp[1,1] #True Negative
temp[1,2] #False Positive
temp[2,1] #False Negative
temp[2,2] #True Positive
fun_fmeasure <- function(table){
  precision <- table[2,2]/(table[1,2]+table[2,2])   # TP/(FP+TP)
  recall <- table[2,2]/(table[2,1]+table[2,2])      # TP/(FN+TP)
  return(2*precision*recall/(precision+recall))
}

#F-measure : Precision 과 Recall의 조화 평균
fun_fmeasure(temp)    #0.5365322



#####################################################
result <- read.csv("Working/result_nnet.csv", header=TRUE)

temp <- table(result$SIU_CUST_YN, result$SIU_CUST_YN.1)
temp

#F-measure : Precision 과 Recall의 조화 평균
fun_fmeasure(temp)    #0.5266594


#####################################################
result <- read.csv("Working/result_svm.csv", header=TRUE)

temp <- table(result$SIU_CUST_YN, result$svm_predicted_data)
temp

#F-measure : Precision 과 Recall의 조화 평균
fun_fmeasure(temp)     #0.05110733
