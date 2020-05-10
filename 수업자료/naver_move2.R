load("./movie/final_score.RData")
load("./movie/final_text.RData")
head(final_score)
head(final_text)

comments<-gsub("<.*?>","",final_text)
comments<-gsub("\t","",comments)
comments<-gsub("[][!#$%*,:;<=>@_`|‘~{}&★☆ㅋㅎ《》◈△▲▽▼○●◎◇◆□◁◀▷▶♤♠♡♥♧♣◉◈▣◐◑♨☏☎☜☞↖↘♭♩♪♬㈜]", " ",comments)
comments<-gsub("rdquo|gt|lt|nbsp|amp|quot|apos","",comments)
comments<-gsub("  "," ",comments)
comments<-gsub("\\^"," ",comments)
comments<-gsub("ㅠ|ㅜ|ㅡ"," ",comments)
comments<-gsub("\"|\n|+","",comments)
comments<-gsub("\\+","",comments)
comments<-gsub("/|!|\\*|\\+|\\@"," ",comments)
comments<-gsub("'","",comments)
comments<-gsub("\"","",comments)
comments<-gsub("\"","",comments)
comments<-gsub("=","",comments)
comments<-gsub("~|;|<|>","",comments)
comments<-gsub("\\?","",comments)
comments<-gsub("\\[.*?\\]","",comments)
comments<-gsub("\\(.*?\\)","",comments)
comments<-gsub("\\(|\\)"," ",comments)
comments<-gsub("\\]|\\[|\\(|\\)|:|-|\\,|\\."," ",comments)
comments<-gsub("\\!","",comments)
comments<-gsub("”|“|’|·","",comments)

head(comments)

# dtm 
library(stringr)

co<-str_split(comments," ")
head(co)
co2<-table(unlist(co))
length(co2)
wo<-names(sort(co2,decreasing = T)[2:301])
head(wo)
wo<-wo[!str_detect(wo,"\\d")]
head(wo)

#i<-1

mat<-matrix(0,ncol=length(wo),nrow=length(co))
head(mat)

for(i in 1:length(co)){
  mat[i,wo %in%  co[[i]]] <-1
  cat("\n",i)
}
colnames(mat)<-wo

mat<-data.frame(mat)
head(mat)

mat2<-mat[apply(mat,1,sum) > 0,]
head(mat2)

score2<-final_score[apply(mat,1,sum) > 0]

table(score2)
hist(score2)

score3<-ifelse(score2 > 5,1,0)

dim(mat2)
length(score3)

sam<-sample(1:nrow(mat2),nrow(mat2)*0.7)
head(sam)
train<-mat2[sam,]
valid<-mat2[-sam,]
tr_y<-score3[sam]
valid_y<-score3[-sam]

# lasso(numeric method) ->회귀계수 0 / 
# ridge(analytic) -> 회귀계수를 0에 가깝게만
# Shriankage regression /회귀 계수 축소법 
# labmda의 크기에 따라 회귀계수가 0으로 가는 비율이 결정됨. 
# glmnet
# alpha = 0 lasso / alpha = 1 ridge / 0 < alpha <1 elastic net 
#install.packages("glmnet")
library(glmnet)

pr1<-0
pr2<-0
acc_list<-NULL