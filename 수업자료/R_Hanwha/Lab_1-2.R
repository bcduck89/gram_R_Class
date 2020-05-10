#####################################################
#Data Exploration
rm(list=ls())
data_cust <- read.csv("Working/data_cust_1-1.csv", header=TRUE)

#####################################################
#연령대 별 사기자 수
(count_by_age_gen <- table(subset(data_cust, select=AGE, subset=(data_cust$SIU_CUST_YN==1))))
names(count_by_age_gen) <- c("0대", "10대", "20대", "30대", "40대", "50대", "60대", "70대")
barplot(count_by_age_gen, main="연령대별 사기자 수")

#성별 별 사기자 수
(count_by_sex <- table(subset(data_cust, select=SEX, subset=(data_cust$SIU_CUST_YN==1))))
names(count_by_sex) <- c("남자", "여자")
pie(count_by_sex, 
    cex=0.8,
    main="성별별 사기자 수",
    labels=paste(names(count_by_sex), "\n", 
                 count_by_sex, "명", "\n",
                 round(count_by_sex/sum(count_by_sex)*100), "%"))


#결혼 여부 별 사기자 수
(count_by_wedd <- table(subset(data_cust, select=WEDD_YN, subset=(data_cust$SIU_CUST_YN==1))))
names(count_by_wedd) <- c("미혼", "결혼")
pie(count_by_wedd, 
#    col=c(4,2), cex=0.8,
    main="결혼 여부별 사기자 수",
    labels=paste(names(count_by_wedd), "\n", 
                 count_by_wedd, "명", "\n",
                 round(count_by_wedd/sum(count_by_wedd)*100), "%"))

#FP 경력 별 사기자 수
(count_by_fp_career <- table(subset(data_cust, select=FP_CAREER, subset=(data_cust$SIU_CUST_YN==1))))
names(count_by_fp_career) <- c("경력있음", "경력있음")
pie(count_by_fp_career, 
    #    col=c(4,2), cex=0.8,
    main="FP 경력별 사기자 수",
    labels=paste(names(count_by_fp_career), "\n", 
                 count_by_fp_career, "명", "\n",
                 round(count_by_fp_career/sum(count_by_fp_career)*100), "%"))


#거주지별 사기자 수
(count_by_ctpr <- table(subset(data_cust, select=CTPR, subset=(data_cust$SIU_CUST_YN==1))))
names(count_by_ctpr) <- c("--", "서울", "부산", "대구", "인천", "광주", "대전", "울산", "세종",
                          "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남")#, "제주")
barplot(count_by_ctpr, main="거주지별 사기자 수")

#본인 직업별 사기자 수
(count_by_grp1 <- table(subset(data_cust, select=OCCP_GRP_1, subset=(data_cust$SIU_CUST_YN==1))))
names(count_by_grp1) <- c("--", "주부", "자영업", "사무직", "전문직", "서비스", "제조업", "1차산업", "기타")
barplot(count_by_grp1, main="직업별 사기자 수")

#배우자 직업별 사기자 수
(count_by_mate_grp1 <- table(subset(data_cust, select=MATE_OCCP_GRP_1, subset=(data_cust$SIU_CUST_YN==1))))
names(count_by_mate_grp1) <- c("--", "주부", "자영업", "사무직", "전문직", "서비스", "제조업", "1차산업", "기타")
barplot(count_by_mate_grp1, main="배우자 직업별 사기자 수")

#Data Exploration용 변수 삭제
rm(count_by_age_gen)
rm(count_by_sex)
rm(count_by_wedd)
rm(count_by_fp_career)
rm(count_by_ctpr)
rm(count_by_grp1)
rm(count_by_mate_grp1)

