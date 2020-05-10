rm(list=ls())
data_cust2 <- read.csv("Working/data_cust_1-3.csv", header=TRUE)

#소득이 0인 사람의 데이터를 직업별 평균 값으로 수정, 
(cust_incm_avg_by_occp <- tapply(data_cust2$CUST_INCM, data_cust2$OCCP_GRP_1, mean))
(cust_incm_avg_by_occp <- round(cust_incm_avg_by_occp))

zero_to_mean <- function(occp, incm) {
  if(incm == 0)
    return (cust_incm_avg_by_occp[occp+1])
  else
    return (incm)
}

#mapply(function, ...)
data_cust2$CUST_INCM <- mapply(zero_to_mean, data_cust2$OCCP_GRP_1, data_cust2$CUST_INCM)
rm(zero_to_mean)
rm(cust_incm_avg_by_occp)

#고객별 평균 입원일수는 BGCON_CLAIM_DATA 데이터파일 VLID_HOSP_OTDA의 고객별 평균
#aggregate(d[, 3:4], list(d$Name), mean) 함수를 이용
data_claim <- read.csv("Data/BGCON_CLAIM_DATA.csv", header=TRUE, sep=",", encoding="CP949", fileEncoding="UCS-2")
hosp_day_per_cust <- aggregate(data_claim$VLID_HOSP_OTDA, by=list(data_claim$CUST_ID), mean)
names(hosp_day_per_cust) <- c("CUST_ID", "HOSP_DAYS")
hosp_day_per_cust$HOSP_DAYS <- round(hosp_day_per_cust$HOSP_DAYS)

#CUST_ID를 기준으로 결합
data_cust2 <- merge(data_cust2, hosp_day_per_cust)


install.packages("reshape")
library(reshape) #cast() 함수를 사용하기 위함, cast 함수는 melt함수의 반대

table(data_claim$ACCI_DVSN, data_claim$DMND_RESN_CODE)

#고객아이디, 사고구분, 청구사유 조합
acci_dmnd_count <- table(data_claim$CUST_ID, data_claim$ACCI_DVSN, data_claim$DMND_RESN_CODE)
acci_dmnd_count <- as.data.frame(acci_dmnd_count)

#cast 하기 위해 마지막 열은 이름이 value 이어야 함
names(acci_dmnd_count) <- c("CUST_ID", "ACCI_DVSN", "DMND_RESN_CODE", "value") 

acci_dmnd_count <- cast(data=acci_dmnd_count, CUST_ID ~ ACCI_DVSN + DMND_RESN_CODE, fun=sum)
data_cust2 <- merge(data_cust2, acci_dmnd_count) #기존 데이터와 병합
data_cust2 <- data_cust2[, sapply(data_cust2, function(v) var(v, na.rm=TRUE)!=0)] #분산이 0인 열 삭제

#데이터 임시 저장
write.csv(data_cust2, "Working/data_cust_2-1.csv", row.names = FALSE)

