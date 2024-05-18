## 단어 구름은 레이블링 안되어있기에 비지도학습.
## 분류 문제는 레이블을 주어야 하므로 지도학습.

install.packages("text2vec")
library(text2vec)

str(movie_review)

##문서를 분류할 훈련 집합과 모형 검증을 위한 테스트 집합으로 데이터 나누기

install.packages('caret')
library(caret)

train_list = createDataPartition(y=movie_review$sentiment,p=0.6,list=FALSE)
mtrain = movie_review[train_list,]
mtest = movie_review[-train_list,]

## 트레인 집합으로 DTM 구축
install.packages('tm')
library(tm)
## 데이터 전처리
m_doc <- Corpus(VectorSource(mtrain$review))
m_doc <- tm_map(m_doc, content_transformer(tolower))
m_doc <- tm_map(m_doc, removeNumbers)
m_doc <- tm_map(m_doc, removeWords, stopwords('english'))
m_doc <- tm_map(m_doc, removePunctuation)
m_doc <- tm_map(m_doc, stripWhitespace)

## dtm 구축
m_dtm <- DocumentTermMatrix(m_doc)
inspect(m_dtm)
# 총함수는 Non-/sparse entries 두 갯수 더하면됨, 전 숫자를 총함수로 나누면 정확도

## 빈도가 적은 단어 제거

m_dtm_small = removeSparseTerms(m_dtm, 0.90)
X = as.matrix(m_dtm_small)
dataTrain = as.data.frame(cbind(mtrain$sentiment, X)) #반응 변수를 덧붙여 훈련 데이터 구축
dataTrain$V1 = as.factor(dataTrain$V1)
colnames(dataTrain)[1] = 'y'

## <훈련 데이터로 결정 트리와 랜덤 포리스트 학습>
install.packages('rpart')
install.packages('randomForest')
library(rpart)
library(randomForest)

m_r = rpart(y~., data = dataTrain)# 결정트리 rpart
printcp(m_r)
par(mfrow = c(1, 1), xpd = NA)
plot(m_r)
text(m_r, use.n = TRUE)

m_f = randomForest(y~., data = dataTrain) #랜포 randomforest

install.packages('e1071')
library(e1071)

m_s <- svm(y~.,data=dataTrain)# SVM

##<예측과 성능 평가> mtest로 성능을 평가
m_docTest = Corpus(VectorSource(mtest$review))
m_docTest = tm_map(m_docTest, content_transformer(tolower))
m_docTest = tm_map(m_docTest, removeNumbers)
m_docTest = tm_map(m_docTest, removeWords, stopwords('english'))
m_docTest = tm_map(m_docTest, removePunctuation)
m_docTest = tm_map(m_docTest, stripWhitespace)

m_dtmTest = DocumentTermMatrix(m_docTest, control=list(dictionary=m_dtm_small$dimnames$Terms))

##mtest의 성능평가

x_test = as.matrix(m_dtmTest)
dataTest = as.data.frame(cbind(mtest$sentiment, x_test))
dataTest$V1 = as.factor(dataTest$V1)
colnames(dataTest)[1] = 'y'

pr = predict(m_r, newdata = dataTest, type = 'class') #rpart
table(pr, dataTest$y) # 오류율은 1,2에서 적은 수의 합/전체합, 정확률은 1-오류율

pf = predict(m_f, newdata = dataTest) #randomforest
table(pf, dataTest$y)

ps = predict(m_s, newdata= dataTest) #SVM
table(ps,dataTest$y)

#######################
## 영어 텍스트 마이닝을 이용한 한국어 처리
install.packages('wordcloud2')
library(tm)
library(XML)
library(wordcloud2)
library(SnowballC)
library(RCurl)
t = readLines('https://ko.wikipedia.org/wiki/%EB%B9%85_%EB%8D%B0%EC%9D%B4%ED%84%B0')
d = htmlParse(t, asText = TRUE)
clean_doc = xpathSApply(d, "//p", xmlValue)

## 전처리(stopword 필요없음)
doc = Corpus(VectorSource(clean_doc))
inspect(doc)
doc = tm_map(doc, content_transformer(tolower))
doc = tm_map(doc, removeNumbers)
doc = tm_map(doc, removePunctuation)
doc = tm_map(doc, stripWhitespace)

dtm = DocumentTermMatrix(doc)
inspect(dtm)
dim(dtm) #사전의 크기

## wordcloud 만들기
m = as.matrix(dtm)
v = sort(colSums(m), decreasing = TRUE)
d = data.frame(word = names(v), freq = v)
d1 = d[1:500, ] # 500개 단어만 표시
wordcloud2(d1)

##텍스트 마이닝 라이브러리 bareun
library(RProtoBuf) 
library(bareun)
apikey <- "koba로 시작하는 본인 API key 입력“
server <- "localhost“
set_api(apikey, server)
t <- tagger()
text <- "어두운 밤에 고양이를 보고 겁을 먹었다"
postag(t, text) #형태소와 품사태깅 출력
postag(t, text)[[1]][[4]] #1번째 문장의 4번째 형태소 출력
morphs(t) #형태소만 출력
nouns(t) #명사만 출력
verbs(t) #동사만 출력

## bareun가지고 단어구름 만들어보기

t = readLines('https://ko.wikipedia.org/wiki/%EB%B9%85_%EB%8D%B0%EC%9D%B4%ED%84%B0')
d = htmlParse(t, asText = TRUE)
clean_doc = xpathSApply(d, "//p", xmlValue)

# 형태소 추출
t <- tagger()
text <- clean_doc[[1]] #[[1]] 문서만 뽑아내기
result <- pos(t, text)[[1]] #형태소와 태깅 출력
nng <- result[str_detect(result,"/NNG")] #일반 명사
nnp <- result[str_detect(result,"/NNP")] #고유명사
vv <- result[str_detect(result,"/VV")] #동사
va <- result[str_detect(result,"/VA")] #형용사
mmn <- result[str_detect(result,"/MMN")] #수 관형사
sl <- result[str_detect(result,"/SL")] #외국어
sn <- result[str_detect(result,"/SN")] #숫자

nngt <- as.data.frame(table(nng)) #각 단어 별로 빈도수를 파악하기 위해 테이블 함수 적용
nnpt <- as.data.frame(table(nnp))
vvt <- as.data.frame(table(vv))
vat <- as.data.frame(table(va))
mmnt <- as.data.frame(table(mmn))
slt <- as.data.frame(table(sl))
snt <- as.data.frame(table(sn))

if(nrow(nngt)==0) nngt <- data.frame(morphs=numeric(),freq=numeric()) #형태소에 단어가 없을 경우if(nrow(nnpt)==0) nnpt <- data.frame(morphs=numeric(),freq=numeric())
if(nrow(vvt)==0) vvt <- data.frame(morphs=numeric(),freq=numeric())
if(nrow(vat)==0) vat <- data.frame(morphs=numeric(),freq=numeric())
if(nrow(mmnt)==0) mmnt <- data.frame(morphs=numeric(),freq=numeric())
if(nrow(slt)==0) slt <- data.frame(morphs=numeric(),freq=numeric())
if(nrow(snt)==0) snt <- data.frame(morphs=numeric(),freq=numeric())

names(nngt) <- c("morphs","freq") #열이름 지정
names(nnpt) <- c("morphs","freq")
names(vvt) <- c("morphs","freq")
names(vat) <- c("morphs","freq")
names(mmnt) <- c("morphs","freq")
names(slt) <- c("morphs","freq")
names(snt) <- c("morphs","freq")
cresult <- rbind(nngt,nnpt,vvt,vat,mmnt,slt,snt) #행을 기준으로 묶기
cresulta <- data.frame(doc=1,cresult) #데이터 프레임형태로 문서번호 넣기