#####################################################
#테스트셋의 원본 데이터와 예측한 데이터를 이용한 검정 통계량 출력
#####################################################
result <- read.csv("Working/model_result.csv", header=TRUE)

temp <- table(result$SIU_CUST_YN, result$predicted_yn)
temp
fun_fmeasure <- function(table){
  precision <- table[2,2]/(table[1,2]+table[2,2])   # TP/(FP+TP)
  recall <- table[2,2]/(table[2,1]+table[2,2])      # TP/(FN+TP)
  return(2*precision*recall/(precision+recall))
}

fun_fmeasure(temp)    #0.5703125

#####################################################
#의사결정나무 출력
#####################################################
install.packages("rpart")
library(rpart)

decision_tree_data <- rpart(predicted_yn ~ ., data=cust_test)
decision_tree_data
plot(decision_tree_data, compress=TRUE, margin=.2)
text(decision_tree_data, cex=0.8)
