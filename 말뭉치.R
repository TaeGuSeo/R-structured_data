library(RCurl)
library(XML)

t <- readLines("https://en.wikipedia.org/wiki/Data_Science")
d <- htmlParse(t, asText=T)
clean_doc <- xpathSApply(d,"//p",xmlValue)

library(tm)
library(SnowballC)

doc <- Corpus(VectorSource(clean_doc))#source로 쓰겠다.
inspect(doc)#말뭉치로 긁어오기

doc <- tm_map(doc,content_transformer(tolower))#소문자로 변환
doc <- tm_map(doc,removeNumbers)# 숫자 제거
doc <- tm_map(doc,removeWords,stopwords('english'))# 영어 불용어 제거
doc <- tm_map(doc, removePunctuation) #구두점 제거
doc <- tm_map(doc, stripWhitespace) #공백문자 제거

##DTM 말뭉치 구축

dtm <- DocumentTermMatrix(doc)
inspect(dtm) # 이걸 써야지만 안의 내용 확인 가능, 항 개수는 15*378

### 전체 예시
tt <- c("Data science is exciting and motivating.","I like literature class and science class",
        "What a data science?")

doc <- Corpus(VectorSource(tt))
doc <- tm_map(doc, content_transformer(tolower))
doc <- tm_map(doc,removeNubmers)
doc <- tm_map(doc, removeWords,stopwords("english"))
doc <- tm_map(doc, removePunctuation)
doc <- tm_map(doc, stripWhitespace)
inspect(doc)
dtm <- DocumentTermMatrix(doc)
inspect(dtm)
dim(dtm)#dtm의 디멘션을 알 수 있음(행열 갯수)

###워드 클라우드

m <- as.matrix(dtm)
v <- sort(colSums(m),decresing=T)
d <- data.frame(word=names(v),freq=v)
wordcloud(words=d$word,freq=d$freq,min.freq=3,max.words=100,random.order=F,rot.per=0.20)

#워드 클라우드 색 입히기
library(RColorBrewer)

pal <- brewer.pal(11,"Blues")
wordcloud(words=d$word,freq=d$freq,min.freq=3,max.words=100,random.order=F,
          rot.per=0.20,colors=pal)

#워드 클라우드 폰트 바꾸기
wordcloud(words=d$word,freq=d$freq,min.freq=3,max.words=100,random.order=F,
          rot.per=0.20,family='mono',font=3)

#다른 모양의 워드 클라우드
library(wordcloud2)
wordcloud2(d)
d1 = d[1:200, ] # 200개 단어만 표시
wordcloud2(d1, shape = 'star')
wordcloud2(d1, minRotation = pi/4, maxRotation = pi/4, rotateRatio = 1.0) 
# minRotation 인수: 최소 회전 각도
# maxRotation 인수: 최대 회전 각도
# rotateRatio 인수: 단어 회전 비율 (1로 놓으면 모두 회전)

findFreqTerms(dtm, lowfreq = 12)#빈도수 높은 단어 보기
findAssocs(dtm, terms = 'harvard', corlimit = 0.7)#연관있는 단어 보기





