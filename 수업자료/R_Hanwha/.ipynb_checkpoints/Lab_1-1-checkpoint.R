#####################################################
#Lab 1. START
#####################################################
#Tools > Global Options > Code > Saving > Default text encoding: UTF-8
#####################################################
rm(list=ls())

#작업 디렉토리 설정
setwd("C:/Projects/R_Project/Hanwha/Workplace")

#####################################################
#원본 데이터 읽기
#####################################################
data_cust <- read.csv("Data/BGCON_CUST_DATA.csv", header=TRUE, sep=",", encoding="CP949", fileEncoding="UCS-2")

data_claim <- read.csv("Data/BGCON_CLAIM_DATA.csv", header=TRUE, sep=",", encoding="CP949", fileEncoding="UCS-2")

data_cntt <- read.csv("Data/BGCON_CNTT_DATA.csv", header=TRUE, sep=",", encoding="CP949", fileEncoding="UCS-2")

data_fmly <- read.csv("Data/BGCON_FMLY_DATA.csv", header=TRUE, sep=",", encoding="CP949", fileEncoding="UCS-2")

data_fpinfo <- read.csv("Data/BGCON_FPINFO_DATA.csv", header=TRUE, sep=",", encoding="CP949", fileEncoding="UCS-2")


#####################################################
#정상인과 사기자의수 확인
#####################################################
(count_siu <- table(data_cust$SIU_CUST_YN)) #사기자가 아닌 사람과 사기자인 사람의 수 18801, 1806
names(count_siu) <- c("분석대상", "정상인", "사기자")

pie(count_siu, 
    cex=0.8, #사기자 빨간색
    main="사기자 수",
    labels=paste(names(count_siu), "\n", 
                 count_siu, "명", "\n",
                 round(count_siu/sum(count_siu)*100), "%"))

rm(count_siu)


#####################################################
#데이터 전처리, NA, NULL 값 처리 등...
#####################################################
#나이를 연령대로 변환
age_to_gen <- function(row) {
  row = floor(row/10)
}
data_cust$AGE <- sapply(data_cust$AGE, age_to_gen)

#Y는 1으로 N은 0으로 변환
yn_to_10 <- function(row) {
  if(row=="Y") 
    row = 1
  else if(row=="N")
    row = 0
  else
    row = ""
}
data_cust$SIU_CUST_YN <- sapply(data_cust$SIU_CUST_YN, yn_to_10)
data_cust$FP_CAREER <- sapply(data_cust$FP_CAREER, yn_to_10)

#NA를 0으로
na_to_0 <- function(row) {
  if(is.na(row)) 
    row = 0
  else
    row = row
}
data_cust$RESI_TYPE_CODE <- sapply(data_cust$RESI_TYPE_CODE, na_to_0)
data_cust$TOTALPREM <- sapply(data_cust$TOTALPREM, na_to_0)


#####################################################
#지역을 코드로 변환
ctpr_to_code <- function(row) {
  if(row=="서울") 
    row = 1
  else if(row=="부산")
    row = 2
  else if(row=="대구")
    row = 3
  else if(row=="인천")
    row = 4
  else if(row=="광주")
    row = 5
  else if(row=="대전")
    row = 6
  else if(row=="울산")
    row = 7
  else if(row=="세종")
    row = 8
  else if(row=="경기")
    row = 9
  else if(row=="강원")
    row = 10
  else if(row=="충북")
    row = 11
  else if(row=="충남")
    row = 12
  else if(row=="전북")
    row = 13
  else if(row=="전남")
    row = 14
  else if(row=="경북")
    row = 15
  else if(row=="경남")
    row = 16
  else if(row=="제주")
    row = 17
  else
    row = 0
}

data_cust$CTPR <- sapply(data_cust$CTPR, ctpr_to_code)
data_cust$CTPR <- unlist(data_cust$CTPR)

#####################################################
#MINCRDT, MAXCRDT NA를 6으로 변환
na_to_6 <- function(row) {
  if(is.na(row))
    row = 6
  else
    row = row
}
data_cust$MINCRDT <- sapply(data_cust$MINCRDT, na_to_6)
data_cust$MAXCRDT <- sapply(data_cust$MAXCRDT, na_to_6)

data_cust$CUST_INCM <- sapply(data_cust$CUST_INCM, na_to_0)
data_cust$JPBASE_HSHD_INCM <- sapply(data_cust$JPBASE_HSHD_INCM, na_to_0)

#OCCP_GRP의 첫 문자만 빼냄
occp_grp_1_to_no <- function(row) {
  row = substr(row, 1, 1)
  if(row == "") 
    return (0)
  else
    return (as.integer(row))
}
data_cust$OCCP_GRP_1 <- sapply(data_cust$OCCP_GRP_1, occp_grp_1_to_no)
#결혼 유무 Y/N을 1/0으로
data_cust$MATE_OCCP_GRP_1 <- sapply(data_cust$MATE_OCCP_GRP_1, occp_grp_1_to_no)


#####################################################
#널스트링을 N으로, 결혼 유무에서 널스트링일 경우 N으로
nullstring_to_N <- function(row) {
  if(row == "N") 
    row = "N"
  else if(row == "Y")
    row = "Y"
  else 
    row = "N"
}
data_cust$WEDD_YN <- sapply(data_cust$WEDD_YN, nullstring_to_N)
data_cust$WEDD_YN <- sapply(data_cust$WEDD_YN, yn_to_10)

#임시 함수 제거
rm(age_to_gen)
rm(yn_to_10)
rm(na_to_0)
rm(ctpr_to_code)
rm(na_to_6)
rm(occp_grp_1_to_no)
rm(nullstring_to_N)

#데이터 임시 저장
write.csv(data_cust, "Working/data_cust_1-1.csv", row.names = FALSE)
