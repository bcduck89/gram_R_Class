rm(list=ls())

########### movie review

library(stringr)

score_list<-list()
text_list<-list()
for(i in 1:2500){
  i<-1
  url<-paste0("https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=187940&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page=",i)
  url
  b<-readLines(url,encoding="UTF-8")
  head(b, 12)
  
  b2<-b[which(str_detect(b,"star_score"))+2]
  b2
  str_extract(b2,("(?<=<em>).*(</em>)"))
  str_sub(str_extract(b2,("(?<=<em>).*(</em>)")),end=-6)
  score<-as.numeric(str_sub(str_extract(b2,("(?<=<em>).*(</em>)")),end=-6))
  
  score_list[[i]]<-score
  
  which(str_detect(b,"<span id=\"_filtered_ment_"))+4
  text<-b[which(str_detect(b,"<span id=\"_filtered_ment_"))+4]
  head(text)
  
  text2<-gsub("\t","",text)
  head(text2)
  
  str_sub(text2[str_detect(text2,"javascript")],42,end=-36)
  text2[str_detect(text2,"javascript")]
  <-str_sub(text2[str_detect(text2,"javascript")],42,end=-36)
  head(text2)
  text_list[[i]]<-text2
  cat("\n",i)
  
  if(i %% 500 == 0){
    cat("\n save ",i)
    final_score<-unlist(score_list)
    final_text<-unlist(text_list)
    
    setwd("./")
    save(final_score,file="final_score.RData")
    save(final_text,file="final_text.RData")
  }
}

final_score<-unlist(score_list)
final_text<-unlist(text_list)

setwd("./")
save(final_score,file="./movie/final_score.RData")
save(final_text,file="./movie/final_text.RData")