#랜덤포레스트
install.packages("randomForest")
library('randomForest')

rf <- randomForest(Species~.,data=iris)
rf
#Number of trees:500 은 500개 결정 트리 만들어짐. error rate 4%는 오류율, 혼동행렬 50,47,46
summary(rf)
plot(rf)

#rf는 실행할때마다 달라질 수 있음. 달라지는걸 방지하기위해선 set.seed(1)미리 사용 후 함수 사용하면 됨.
set.seed(1)
rf <- randomForest(Species~.,data=iris)
legend("topright",colnames(rf$err.rate),col=1:4,cex=0.8,fill=1:4) #이건 시각화에 대한 함수

#varUsed는 랜포에 있는 결정 트리가 설명변수를 질문에 사용한 횟수.
varUsed(rf)
head(rf)
varImpPlot(rf) #varImpplot으로 설명 변수의 중요도를 나타내는 그래프를 그려줌. 우측으로 커질수록 중요한 변수이다.
treesize(rf)#리프 노드 갯수 출력

#모델에 데이터 대입
newd <- data.frame(Sepal.Length=c(5.11,7.01,6.32),Sepal.Width=c(3.51,3.20,3.31),Petal.Length=c(1.40,4.71,6.02),Petal.Width=c(0.91,1.40,2.49))
predict(rf,newd,type="prob")#prob는 확률
predict(rf,newd,type="vote",norm.votes=F)#type가 vote가되면 득표수
predict(rf,newd,type="vote",norm.votes=T)#norm.votes=T라해도 표준화한것으로 prob랑 같은거임

rf1 <- randomForest(Species~.,data=iris,ntree=20,nodesize=6,maxnodes=12) #ntree는 결정 트리의 개수결정, nodesize는 샘플의 최소 개수 설정, maxnodes는 리프 노드의 개수 제한
treesize(rf1)

newd1 <- data.frame(Sepal.Length=c(4.7,5.31,6.4,5.2,6.3),Sepal.Width=c(3.2,3.7,3.22,2.71,3.3),Petal.Length=c(1.3,1.5,4.5,3.9,6.1),Petal.Width=c(0.22,0.2,1.5,1.4,2.5))
predict(rf,newd1,type="prob")#득표율의 확률
predict(rf,newd1,type="vote",norm.votes=F)#norm.votes=F하면 득표수
predict(rf,newd1,type="response")#아예 결과로 보여주는 것

##################(SVM)
install.packages("e1071")
library(e1071)

s <- svm(Species~.,data=iris) #radial basis kernel 사용한것
p <- predict(s,iris)
table(p,iris$Species)#error=오류/전체

s2 <- svm(Species~.,data=iris,kernel="polynomial") #polynomial kernel 사용한것
p2 <- predict(s2,iris)
table(p2,iris$Species)

s3 <- svm(Species~.,data=iris,kernel="sigmoid") #polynomial kernel 사용한것
p3 <- predict(s3,iris)
table(p3,iris$Species)

## 위의 세가지의 커널을 사용했을 때 오류율이 작은 커널을 사용하는 것이 적합함.
#cost크게하면 오류율이 감소하지만 일반화 능력이 떨어짐. cost작게하면 그것의 반대.

s1_c <- svm(Species~.,iris,cost=100)
s_p1_c <- predict(s1_c,iris)
table(s_p1_c,iris$Species) #error의 값이 작아짐. 그대신 여백이 같이 작아졌기에 새로운 샘플이 들어왔을때 그것을 일반화 시키는 능력은 떨어짐
