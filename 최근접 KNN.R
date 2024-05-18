install.packages('class')
library(class)

train <- iris
test <- data.frame(Sepal.Length=c(5.11,7.01,6.32),Sepal.Width=c(3.51,3.2,3.31),Petal.Length=c(1.4,4.71,6.02),Petal.Width=c(0.19,1.4,2.49))

#설명변수 열만 넣어야하니까 [,1:4]
k <- knn(train[,1:4],test,train$Species,k=5)

#train 함수로 다양한 모델 구현해보기
install.packages('caret')
library(caret)

r <- train(Species~., data=iris, method="rpart")
rf <- train(Species~., data=iris, method="rf")
s <- train(Species~., data=iris, method="svmRadial")
kn <- train(Species~., data=iris, method="knn")

###############예제1
#다음 링크에는 UCLA 데이터가 있다. read.csv 함수를 이용하여 데이터를 불러온 후 각 변수들의 class를 확인하고 admit 변수를 범주형으로 변환하시오

install.packages("rpart")
install.packages('randomForest')
library(rpart)
library(randomForest)

ucla <- read.csv("https://stats.idre.ucla.edu/stat/binary.csv")
str(ucla)
ucla$admit <- as.factor(ucla$admit) #범주형으로 변환

#데이터에 rpart를 적용하여 결정 트리를 구하고 시각화 하시오.(리프 노드의 0,1은 각각 불합격과 합격)
r_ucla <- rpart(admit~.,data=ucla)
par(xpd=NA)
plot(r_ucla)
text(r_ucla,use.n=T) #그래프에 텍스트 보이기

#결정 트리로 훈련 집합에 대해 예측하고 테이블을 그려 정확률을 확인하시오
p_ucla <- predict(r_ucla,ucla,type="class")
table(p_ucla,ucla$admit) #accuracy = (249+54)/400 정확률계산

#랜덤 포레스트를 적용하고 print해보기
rf_ucla <- randomForest(admit~.,data=ucla)
print(rf_ucla)

#SVM적용하고 결정트리와 랜덤 포레스트의 정확률을 비교해보기
svm_ucla <- svm(admit~.,data=ucla)
tale(predict(svm_ucla,ucla,type="class"),ucla$admit) 

###############예제2
#Lec4_voice.csv 파일은 음성 신호를 듣고 여성인지 남성인지 알아내는 데 사용하는 데이터다. 
#이데이터는 음성 신호에서 추출한 20개의 특징을 설명 변수로 사용한다. label이라는 반응 변수는 여성 또는남성을 가르킨다. 
#반응 변수 label이 범주형이므로 전처리는 필요 없다. 데이터에 rpart를 적용하여 결정트리를 구하고 시각화 하시오. 

voice <- read.csv("Lec4_voice.csv")
str(voice)
r_voice <- rpart(label~.,data=voice)
plot(r_voice)
text(r_voice,use.n=T)

#결정 트리로 훈련 집합에 대해 예측하고 테이블을 그려 정확률을 확인하시오

p_voice <- predict(r_voice,voice,type="class")
table(p_voice,voice$label) # 정확도 (1551+1496)/3168

#랜덤 포리스트를 적용하고 print 함수를 이용하여 결과를 출력해보자

rf_voice <- randomForest(label~.,data=voice)
print(rf_voice)

#SVM을 적용하고 결정 트리와 랜덤 포리스트와 정확률을 비교하시오
install.packages('e1071')
library(e1071)

svm_voice <- svm(label~.,data=voice)
table(predict(svm_voice,voice,type="class"),voice$label)

#####################예제3
#랜덤 포리스트의 ntree 인수를 1,2,3,...,9,10으로 바꾸면서 정확률을 계산하고 이들 값을 그래프로 그리시오. 
#for 문을 이용하여 전체 과정을 자동으로 수행하는 프로그램을 작성하시오

v <- vector(length=10)
for(i in 1:10){
  rf_iris <- randomForest(Species~.,data=iris,ntree=i)
  cm <- table(predict(rf_iris,iris,),iris$Species)
  v[i] <- sum(diag(cm))/sum(cm)
}
plot(v,type="b",xlab="ntree",ylab="accuracy")
