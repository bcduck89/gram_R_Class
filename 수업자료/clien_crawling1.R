

########클리앙의 모두의 공원 게시글을 수집하는 코드 입니다.
########
########크롤링은 html소스를 보면서 하셔야 이해가 빠릅니다.
########긁고자 하는 사이트에서 오른쪽 마우스 클릭-> 소스 보기를 하시면 소스를 보실수 있고
########그 소스내에 내가 원하는 정보가 있는 위치를 찾고, 규칙을 찾아 원하는 정보만 추출합니다.
######## 예를 들어 게시판의 제목이 <h2>제목</h2> 이렇게 만 나온다고한다면
######## <h2>가 있는 line만 찾아서 뽑으면 될것입니다.

######## 글 목록, 조회수, 글 본문 링크 정보 수집
# install.packages('stringr')
library(stringr)

i <- 1
final_data <- NULL

for(i in 1:10) {
    url <- paste0("https://www.clien.net/service/board/park?&od=T31&po=", i-1)
    # url
    b <- readLines(url, encoding="UTF-8")
    # str(b)
    # length(b)
    #head(b)
    #tail(b)
    
    b2 <- b[str_detect(b, "subject_fixed")]
    # str(b2)
    title <- str_extract(b2, ("(?<=\">).*(?=</span>)")) ## page로 시작하고 </a> 끝나는 가운데걸 뽑습니다.
    # str(title)
    
    b3 <- b[str_detect(b, "<span class=\"hit\">")]
    # str(b3)
    hit <- str_extract(b3, ("(?<=\">).*(?=</span>)"))[-1]
    b4 <- str_split(b3, "hit\">")
    str(b4)
    hit <- str_sub(sapply(b4, function(x){x[2]}), end=-8)
    #hit
    
    # b[which(str_detect(b, "subject_fixed"))]
    # b[(str_detect(b, "subject_fixed"))]
    b5 <- b[which(str_detect(b, "subject_fixed"))-2]
    # str(b5)
    b6 <- str_sub(str_extract(b5, ("(?<=href=\").*(?=data-role)")), end=-4)
    # str(b6)
    url <- paste0("https://www.clien.net", b6)
    
    data <- cbind(title, hit, url)
    # data
    final_data <- rbind(final_data, data)
    # length(title)
    # length(hit)
    # length(url)
    cat("\n", i)
}

dim(final_data)
head(final_data)
tail(final_data)

setwd("./")
write.csv(final_data, "base_data.csv", row.names=F)


######## 글 목록 수집 데이터에서 글 본문 링크로 글 본문 수집
setwd("./")
data <- read.csv("base_data.csv")
head(data)

# save(data, file="base_data.RData")
# load("base_data.RData")

library(stringr)
url_list <- data[,3]

length(url_list)
content <- c()
for(i in 1:length(url_list)) {
    b <- readLines(as.character(url_list[1]), encoding="UTF-8")
    if(class(try(b <- readLines(as.character(url_list[i]), encoding="UTF-8"))) == "try-error")  {
        b6 <- ""
        content <- c(content, b6)
    }else{
        b2 <- b[which(str_detect(b, "post_content")):which(str_detect(b, "post_ccls"))]
        b3 <- paste(b2, collapse="")
        # str(b3)
        b4 <- gsub("<.*?>", "", b3)
        # str(b4)
        b5 <- gsub("\t|&nbsp;", "", b4)
        # str(b5)
        b6 <- str_trim(b5)
        content <- c(content, b6)
        cat("\n", i)
    }
}
head(content)

final_data <- cbind(data, content)

setwd("./")
write.csv(final_data, "final_data.csv", row.names=F)

######## 글 본문 데이터를 이용하여 형태소 분석 및 워드 클라우드
setwd("./")
final_data <- read.csv("./final_data.csv", encoding="EUC-KR", fileEncoding="EUC-KR")
#final_data <- read.csv("./Data/final_data.csv", encoding="cp949", fileEncoding="UTF-8")
#final_data <- read.csv("./final_data.csv")
head(data)
install.packages("KoNLP")
install.packages("wordcloud")

library(KoNLP) # 한글 형태소 분석기
library(dplyr) # 데이터ㅓ프레임 다루는 라이브러리
# useNIADic()  # 사전 데이터
library(stringr) # 텍스 처리 라이브러리
library(RColorBrewer) # 칼라 팔레트
library(wordcloud) # 워드클라우드 라이브러리
txt <- str_replace_all(final_data[,4], "\\W", " ")
str(txt)
nouns <- extractNoun(txt)
head(nouns)
wordcount <- table(unlist(nouns))
wordcount
df_word <- as.data.frame(wordcount, stringsAsFactors = F)
str(df_word)
df_word <- rename(df_word, word = Var1, freq = Freq)
str(df_word)
df_word <- filter(df_word, nchar(word) >= 2)
top_20 <- df_word %>% arrange(desc(freq)) %>% head(20)
top_20

?brewer.pal
pal <- brewer.pal(8,"Dark2")
set.seed(1234)

wordcloud(words = df_word$word, # 단어 
            freq = df_word$freq, # 빈도 
            min.freq = 2, # 최소 단어 빈도 
            max.words = 200, # 표현 단어 수 
            random.order = F, # 고빈도 단어 중앙 배치 
            rot.per = .1, # 회전 단어 비율 
            scale = c(4, 0.3), # 단어 크기 범위 
            colors = pal) # 색깔 목록
