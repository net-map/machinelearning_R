library(pracma)
library(dplyr)
library(e1071)


mydata <- read.table("winequality-red.csv", sep=";", header=TRUE)

Xi <- select(mydata,-quality)
yi <- select(mydata,quality)


#xii<-NULL
#for (i in 1:10){
#  xii <- cbind(xii,mapFeatures(Xi[,i],Xi[,i+1]))
#}
#
#Xi <-xii



print("Terminei de ajeitar os parametros!")

#separa em treino e teste/cross-val
index <- sample(1:nrow(Xi),round(0.75*nrow(Xi)))
trainX <- Xi[index,]
testX <- Xi[-index,]
trainY <- yi[index,]
testY <- yi[-index,]


#trainX <- normalize(trainX)

#prepare data to glm function
mydata2 <- data.frame(cbind(trainX,quality= trainY))
#mydata2$quality <- as.factor(mydata2$quality)
#prepare data to glm function
mydata3 <- data.frame(cbind(testX,quality= testY))
mydata3$quality <- as.factor(mydata3$quality)



#mylogit <-glm(quality ~., data = mydata2, family = binomial(link="logit"))




mylogit <-svm(quality ~., data = mydata2)




pred1 <- round(predict(mylogit))
AccTrain <- 100 - 100*mean(pred1==trainY)



pred1 <- round(predict(mylogit,newdata=mydata3))
AccTest<- 100 -100*mean(pred1==testY)




errors <- oneVsAll(trainX,trainY,11,testX,testY)
errorTrain <- errors[[1]]
errorTest <- errors[[2]]


print(paste("erro de treino eh",AccTrain,errorTrain))
print(paste("erro de teste eh",AccTest,errorTest))


#plotLearningCurveOnevsAll(trainX,trainY,11,testX,testY)