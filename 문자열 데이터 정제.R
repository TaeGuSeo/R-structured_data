install.packages("stringr")
library(stringr)

fruits <- c("one apple", "two pears", "three bananas")

# 문자열에 있는 것들 대체하는 법

str_replace(fruits, "[aeiou]", "-")
# 출력값 "-ne apple" "tw- pears" "thr-e bananas“
str_replace_all(fruits, "[aeiou]", "-")
# 출력값 "-n- -ppl-" "tw- p--rs" "thr-- b-n-n-s"
str_replace_all(fruits, "[aeiou]", toupper)
# 출력값 "OnE ApplE" "twO pEArs" "thrEE bAnAnAs"
str_replace_all(fruits, "b", NA_character_)
# 출력값값 "one apple" "two pears" NA 

# 문자열 대체
sub("hat","lov",variable,fixed=T)# variable이라느 변수안에 hat이 들어간 문자를 모두 lov로 바꾸어라.

### 예제
# 첫번째행이 변수명이기에 header=T, as.is=T는 범주형 변환 비활성화 함.
data <- read.csv("Lec6_googleplaystore.csv",header=T,as.is=T)

# 2. Price 변수 데이터에 포함된 $기호를 제거하기
data$Price <- str_replace(data$Price,"\\$","")

# 3.Size 변수 데이터에 “Varies with device”로 표기된 데이터를 NA로 바꾸시오.
data$Size <- sub("Varies with device",NA,data$Size,fixed=T)

# 4. Size 변수 데이터에 M과 K는 각각 Mega(e6)와 Kilo(e3)를 의미한다. 값으로 바꾸시오
data$Size <- sub("M","e6",data$Size,fixed=T)
data$Size <- sub("k","e3",data$Size,fixed=T)

# 5. Size 변수를 숫자형 데이터로 변환하시오.
data$Size <- as.numeric(data$Size)

# 6. Installs 변수에 열에 기록된 숫자보다 더 크다는 의미의 + 기호가 포함된 관측값과 
#    1,000단위 숫자구분에 사용하는 ,가 포함된 관측값을 빈 문자로 처리하시오. (힌트 ‘\\+’ 와 ‘,’로 사용) 

data$Installs <-str_replace(data$Installs,"\\+","")
data$Installs <-str_replace_all(data$Installs,",","")

# 7.Installs 변수를 숫자형 데이터로 변환하시오. 

data$Installs <- as.numeric(data$Installs)

# 8. 데이터에서 생긴 결측값을 na.omit 함수를 이용하여 제거하시오

data <- na.omit(data)

# 9. 각 변수의 데이터형을 고려하여 형 변환을 시행하시오. (앞 테이블을 보고 character와 factor만 변환)
#category,content rating 랑 type는 글이기 때문에 factor로 변환, 0과 1인 price는 numeirc로
data$Category <- as.factor(data$Category)
data$Type <- as.factor(data$Type)
data$Price <- as.numeric(data$Price)
data$Content.Rating <- as.factor(data$Content.Rating)
data$Genres <- as.factor(data$Genres)

# 10. Last.Updated 변수를 lubridate 라이브러리의 함수를 이용하여 date 형으로 변환하시오. 
install.packages("lubridate")
library(lubridate)

data$Last.Updated <- dmy(data$Last.Updated)

# 11. 데이터를 적절하게 모델링하여 앱의 평점을 예측하는 기법을 만들어보자. 
#앱에 대한 사용자의 평점을예측하는 것은 0과 5사이의 연속적인 실수 값을 예측하는 문제에 해당한다. 
#설명변수로 Size,Content.Rating, Category를 사용한 결정 트리, 랜덤 포리스트 모델, SVM의 분류 기법을 
#적용하여 10-겹교차 검증을 시행하시오. 

#필요한 행만 가져오기
install.packages("dplyr")
library(dplyr)

data1 <- select(data,Rating,Size,Content.Rating,Category)

d <- data1[sample(nrow(data1)),]#순서섞기
k <- 10
q <- nrow(d)/k
l <- 1:nrow(d)
pe_total <- c()

for(i in 1:k){
  test <- ((i-1)*q+1):(i*q)
  test_data <- d[test,]
  train<- setdiff(l,test)
  train_data <- d[train,]
  
  r <- rpart(Rating~.,data=train_data)
  prd <- predict(r,newdata=test_data)
  pe <- (mean(pre-test_data$Rating)^2) #MSE
  pe_total=c(petotal,pe)
}

install.packages('rpart')
library(rpart)

mean(pe_total)
