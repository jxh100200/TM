if("rJava" %in% installed.packages("rJava") == FALSE)install.packages("rJava")
library(rJava)
if("memoise" %in% installed.packages("memoise") == FALSE)install.packages("memoise")
library(memoise)
if("KoNLP" %in% installed.packages("KoNLP") == FALSE)install.packages("KoNLP")
library(KoNLP)
if("tm" %in% installed.packages("tm") == FALSE)install.packages("tm")
library(tm)
if("wordcloud" %in% installed.packages("wordcloud") == FALSE)install.packages("wordcloud")
library(wordcloud)
if("dplyr" %in% installed.packages("dplyr") == FALSE)install.packages("dplyr")
library(dplyr)
if("stringr" %in% installed.packages("stringr") == FALSE)install.packages("stringr")
library(stringr)
KoNLP::useSejongDic()
getwd()

## step 1. 데이터를 로딩한다 
txt <- readLines("jeju.txt")
head(txt,10)

## step2: 특수문자 제거 
txt <- stringr::str_replace_all(txt,"\\W"," ")
head(txt,10)
txt <- stringr::str_replace_all(txt,"[^[:alpha:]]"," ")
head(txt,10)

## step3: 명사만 추출 

nouns <- sapply(txt,extractNoun,USE.NAMES = F)
class(nouns)
head(nouns,10)

cdata <- unlist(nouns)
cdata <- stringr::str_replace_all(txt,"[^[:alpha:]]"," ")
cdata <- gsub(" ","",cdata)

gsubtxt <- readLines("제주도여행코스gsub.txt")
cnt <- length(gsubtxt)
for(i in 1:cnt){
  cdata <- gsub(gsubtxt[i],"",cdata)
}
cdata

cdata <- Filter(function(x){nchar(x)>=2},cdata)
write(unlist(cdata),"jeju_2.txt")
nouns <- read.table("jeju_2.txt")
nrow(nouns)

wordcount <- table(nouns)
top10 <- head(sort(wordcount,decreasing = 10))




## step3.1 특정단어 삭제하기 
gsubtxt <- readLines("제주도여행코스gsub.txt")
gsubtxt
cnt <- length(gsubtxt)
print(paste("삭제하려는 단어의 수:",cnt))
for(i in 1:cnt){
  nouns <- gsub(gsubtxt[i],"",nouns)
}
nouns

## step4: 단어별 빈도표 작성 
wordcount <- table(unlist(nouns))

## step5: 데이터프레임으로 변경 
df_word <- as.data.frame(wordcount,stringsAsFactors = F)

## step6: 변수명 변경 
names(df_word)
df_word <- rename(df_word,word=Var1,freg=Freq)
df_word

## step7: 두글자 이상 단어 추출 
df_word <- dplyr::filter(df_word,nchar(word)>=2)

## step8: 빈도순 정렬 후 상위 20단어만 추출 
top_20 <- df_word %>% 
  arrange(desc(freg)) %>% 
  head(20)
top_20

install.packages("wordcloud")
library(wordcloud)
library(RColorBrewer)

wordcloud(words = df_word$word,#단어 
          freq = df_word$freg, #빈도 
          min.freq = 2,        # 최소 단어 빈도 
          max.words = 200,     # 표현 단어 수 
          random.order = F, # 고빈도 단어 중앙 배치 
          rot.per = .1,     # 회전 잔어 비율 
          scale = c(4,0.3), # 단어 크기 범위 
          colors = brewer.pal(8,"Dark2"))     # 색상 목록 






