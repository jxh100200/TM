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
txt <- readLines("hippop.txt")
head(txt)
## step2: 특수문자 제거 
txt <- stringr::str_replace_all(txt,"\\W"," ")
head(txt)
txt <- stringr::str_replace_all(txt,"[^[:alpha:]]"," ")


## step3: 명사만 추출 
nouns <- KoNLP::extractNoun(txt)
nouns <- sapply(txt,extractNoun,USE.NAMES = F)
class(nouns)

## step4: 단어별 빈도표 작성 
wordcount <- table(unlist(nouns))

## step5: 데이터프레임으로 변경 
df_word <- as.data.frame(wordcount,stringsAsFactors = F)
class(df_word)

## step6: 변수명 변경 
names(df_word)
df_word <- rename(df_word,word=Var1,freg=Freq)

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

## 색상 지정 
pal <- brewer.pal(8,"Dark2") ## Dark2 색상 목록에서 8개 색상 추출 

set.seed(1234)

wordcloud(words = df_word$word,#단어 
          freq = df_word$freg, #빈도 
          min.freq = 2,        # 최소 단어 빈도 
          max.words = 200,     # 표현 단어 수 
          random.order = F, # 고빈도 단어 중앙 배치 
          rot.per = .1,     # 회전 잔어 비율 
          scale = c(4,0.3), # 단어 크기 범위 
          colors = pal)     # 색상 목록 

pal <- brewer.pal(9,"Blues")[5:9]
set.seed(1234)
wordcloud(words = df_word$word,
          freq = df_word$freg,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(4,0.3),
          colors = pal)

twitter <- read.csv("twitter.csv",
                    header = T,
                    stringsAsFactors = F,
                    fileEncoding = "UTF-8")
twitter <- dplyr::rename(twitter,
                         no=번호,
                         id=계정이름,
                         date=작성일,
                         tw=내용)
twitter$tw <- str_replace_all(twitter$tw,"\\W"," ")
head(twitter$tw)

nouns <- extractNoun(twitter$tw)
nouns
wordcount <- table(unlist(nouns))
View(wordcount)
df_word <- as.data.frame(wordcount,stringsAsFactors = F)
str(df_word)
df_word <- rename(df_word,word=Var1,freq=Freq)

df_word <- dplyr::filter(df_word,nchar(word)>=2)
top20 <- df_word %>% 
  arrange(desc(freq)) %>% 
  head(20)
top20

library(ggplot2)
order <- arrange(top20,freq)$word  ## 빈도 순서 변수 생성 

ggplot(data = top20,aes(x=word,y=freq))+
  ylim(0,2500)+
  geom_col()+
  coord_flip()+
  scale_x_discrete(limit=order)+    ## 빈도순 막대 정렬 
  geom_text(aes(label=freq),hjust=-0.3) # 빈도 표시 
