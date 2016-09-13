

#path <- "~/Documents/machinelearning_R/datasets"
path <- "raw-data"
zones<-zonas
#sample(zonas, 15, replace = FALSE, prob = NULL)
datasets <- prepareUCIdata2(path,1,2)
#justInside = TRUE
#datasets <- prepareUCIdata2(path,1,0)

train <- datasets$train_s
testIDZ <- datasets$test_s$idZ
trainIDZ <- datasets$train_s$idZ
test    <- dplyr::select(datasets$test_s,-idZ)
#train dataset without idZ
trainT <- dplyr::select(datasets$train_s,-idZ)

#TRAIN TREE
#
#
#
#
#
tree <- J48(idZ~.,data=train)
treeAda <- AdaBoostM1(idZ~. , data = train ,control = Weka_control(W = list(J48, M=5)))

#TRAIN NEURAL NET
#
#
#
#
nnData <- cbind(dplyr::select(train,-idZ),nnet::class.ind(train[,1]))

#adds `x` to every name in data
addq <- function(x) paste0("`", x, "`")

names(nnData) <- addq(names(nnData))

n <- names(nnData)
#gets indexes of dummy id columns 
indexId <- grep("^[[:punct:]][[:digit:]]*[[:punct:]]$",n)

lhseq <- paste(names(nnData[,indexId]),collapse="+")

rhseq <- paste(names(nnData[,-indexId]),collapse="+")

#creates formula
f<-as.formula(paste(lhseq,rhseq,sep = " ~ "))


#for some reason, remove quotes and it works
nnData <- cbind(dplyr::select(train,-idZ),nnet::class.ind(train[,1]))

neuron <- 210

nn <- neuralnet::neuralnet(f,data=nnData,hidden=c(neuron),linear.output=FALSE) 




#TRAIN SVM
#
#
#
#
#
SVM <- svm(idZ~.,data=train,probability=TRUE,scale=FALSE)


#TRAIN SMO
#
#
#
#
#
#
SMO <- SMO(idZ~.,data=train)




#########PREDICTIONS##########

factors<- nn$model.list$response
factors <- gsub("`",'',factors)


#KNN PREDICTION
knnModel<-kknn(formula=idZ ~. , k=4,distance=1, train=train,test=test,kernel="optimal")
knnModelTrain<-kknn(formula=idZ ~. , k=4,distance=1, train=train,test=trainT,kernel="optimal")

knnProb <- knnModel$prob
knnPrediction <- as.numeric(as.character(knnModel$fitted.values))
idZKNN <- knnPrediction

#results on training set for KNN
idZKNNt <-knnModelTrain$fitted.values


#DECISION TREE PREDICTION
predictionTree <- predict(tree,test,type="probability")
idZtree <-  as.numeric(as.character(factors[apply (predictionTree,1,function(x) which.max(x))]))
treeProb <-  predictionTree



#ADABOOST PREDICTION
#on test set
predictionTreeAda <- predict(treeAda,test,type="probability")
idZtreeAda <-  as.numeric(as.character(factors[apply (predictionTreeAda,1,function(x) which.max(x))]))
treeProbAda <-  predictionTreeAda

#on train set
predictionTreeAdat <- predict(treeAda,trainT,type="probability")

idZtreeAdat<-  as.factor(factors[apply (predictionTreeAdat,1,function(x) which.max(x))])





#SVM PREDICTION
svmProb <- attr(predict(SVM,test,probability=TRUE),"probabilities")  
idZSVM <- predict(SVM,test)


#NEURAL NET PREDICTION
nnProb<-neuralnet::compute(nn,test)$net.result
nnPrediction <-apply(nnProb,1,function(x) which.max(x))
idZNN <- as.numeric(as.character(factors[nnPrediction]))
#on train set
nnProbt<-neuralnet::compute(nn,trainT)$net.result
nnPredictiont <-apply(nnProbt,1,function(x) which.max(x))
idZNNt <- as.factor(factors[nnPredictiont])




#SMO PREDICTION

idZSMO <- predict(SMO,test)
idZSMOt <- predict(SMO,trainT)

smoProb <- predict(SMO,test,type="probability")



#COMPUTE BAYESIAN VOTE

#TRY WITH DIFFERENT COMBINATIONS OF PREDICTIONS
bayesianSum <- knnProb+smoProb+treeProbAda

idZBayas <- as.numeric(as.character(factors[apply (bayesianSum,1,function(x) which.max(x))]))


#COMPUTE SIMPLE VOTE

#TRY WITH DIFFERENT COMBINATIONS OF PREDICTIONS
results <- cbind(idZKNN,idZSMO,idZtreeAda)

idZVote <- apply(results,1,function (x) as.numeric(names(sort(table(x),decreasing = TRUE)[1])))


#CREATE DF WITH TRAIN AND TEST PREDICTIONS FROM ALL PREDICTORS
predDFTrain <- data.frame(trainIDZ,idZKNNt,idZNNt,idZSMOt,idZtreeAdat)
#changing names so predict method can know 
predDFTest <- data.frame("idZKNNt"=as.factor(idZKNN),"idZNNt"=as.factor(idZNN),"idZSMOt"=as.factor(idZSMO),"idZtreeAdat"=as.factor(idZtreeAda))

library("ipred")


#USE RANDOM FOREST TO MAKE PREDICTION WITH ENSEMBLE LEARNING
combModFit <- train(trainIDZ ~.,method="rpart",data=predDFTrain)
#lr2 <- glm(trainIDZ~., family=binomial, data=predDFTrain)
teste <- bagging(trainIDZ ~.,data=predDFTrain)

randomForestPred <- predict(combModFit,newdata=predDFTest)

rateRandomForest <- 1-mean(randomForestPred==testIDZ)



#COMPUTE CLASSIFICATION ~ERROR~ RATES


rateSVM <- 1- mean(idZSVM == testIDZ)
rateNN <- 1- mean(idZNN==testIDZ)
rateKNN <- 1- mean(idZKNN==testIDZ)
rateTree <- 1- mean(idZtree==testIDZ)
rateTreeAda <- 1- mean(idZtreeAda==testIDZ)
rateSMO <- 1- mean(idZSMO==testIDZ)

rateVote <- 1-mean(idZVote==testIDZ)
rateBayas <- 1-mean(idZBayas==testIDZ)
rateRandomForest <- 1-mean(randomForestPred==testIDZ)


cat("ERROS PARA TESTE ","\n")

cat("Erro de SVM",rateSVM,"\n")
cat("Erro de SMO",rateSMO,"\n")
cat("Erro de KNN",rateKNN,"\n")
cat("Erro de J48",rateTree,"\n")
cat("Erro de J48 + ADA",rateTreeAda,"\n")
cat("Erro de NN",rateNN,"\n")
cat("Erro de Voto Simples",rateVote,"\n")
cat("Erro de Voto com Peso",rateBayas,"\n")
cat("Erro de RF",rateRandomForest,"\n")



