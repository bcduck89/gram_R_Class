#####################################################
#데이터 탐색을 통한 변수 선택 후 분석에서 제외할 변수 제거
#####################################################
rm(list=ls())
data_cust <- read.csv("Working/data_cust_1-1.csv", header=TRUE)

#drop OCCP_GRP_2 & MATE_OCCP_GRP_2 variable
data_cust <- subset(data_cust, select=-c(OCCP_GRP_2, MATE_OCCP_GRP_2))
data_cust <- subset(data_cust, select=-c(MAX_PAYM_YM, MAX_PRM))
data_cust <- subset(data_cust, select=-CUST_RGST)
data_cust <- subset(data_cust, select=-c(CHLD_CNT, LTBN_CHLD_AGE))
data_cust <- subset(data_cust, select=-JPBASE_HSHD_INCM)

#데이터 임시 저장
write.csv(data_cust, "Working/data_cust_1-3.csv", row.names = FALSE)
