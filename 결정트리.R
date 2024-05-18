install.packages('rpart')
library(rpart)
iris

r <- rpart(Species~.,data=iris)
printcp(r)
par(mfrow=c(1,1),xpd=NA)
plot(r)
text(r,use.n=TRUE)

#결정 트리를 가지고 훈련 집합에 대한 예측(class는 개수, prob는 확률), 혼동행렬은 table사용

p <- predict(r,iris,type='class')
table(p,iris$Species)

#prior(사전확률[정밀하게 비율을 계산해서 구한확률, 밑에선 setosa:versicolor:virginica의 비율이 1:1:8]) 사용

r_prior <- rpart(Species~., data=iris,parms=list(prior=c(0.1,0.1,0.8)))
plot(r_prior)
text(r_prior,use.n=TRUE)

#위의 오분류율은 전체 150개 중에 틀린것 전체 150개중에 12개임 그래서 오분류율은 12/150임. 가중치가 너무 virginica로 쏠려서 오분류율이 커짐.

##결정 트리 구현해보기 예재

#데이터프레임 구축
newd <- data.frame(Sepal.Length=c(5.11,7.01,6.32),Sepal.Width=c(3.51,3.2,3.31),Petal.Length=c(4.4,4.71,6.02),
                   Petal.Width=c(0.19,1.4,2.49))

#모델에 대입
print(newd)
predict(r,newdata=newd)



###과제(설명변수 중 Sepal.Length + Sepal.Width + Petal.Width만 사용하고 혼동행렬 구하기)
install.packages('rpart')
library(rpart)
iris

r <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Width, data = iris, method = "class")
printcp(r)
par(mfrow=c(1,1),xpd=NA)
plot(r)
text(r,use.n=TRUE)

p <- predict(r,iris,type='class')
table(p,iris$Species)
