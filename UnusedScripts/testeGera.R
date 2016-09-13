

#path <- "~/Documents/machinelearning_R/datasets"
path <- "raw-data"
zones<-zonas
#sample(zonas, 15, replace = FALSE, prob = NULL)
datasets <- prepareUCIdata2(path,1,2)
#justInside = TRUE
#datasets <- prepareUCIdata2(path,1,0)

train <- datasets$train_s
testIDZ <- datasets$test_s$idZ
test    <- dplyr::select(datasets$test_s,-idZ)


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
knnTrain<-kknn(formula=idZ ~. , k=4,distance=1, train=train,test=test,kernel="optimal")
knnProb <- knnTrain$prob
knnPrediction <- as.numeric(as.character(knnTrain$fitted.values))
idZKNN <- knnPrediction


#DECISION TREE PREDICTION
predictionTree <- predict(tree,test,type="probability")
idZtree <-  as.numeric(as.character(factors[apply (predictionTree,1,function(x) which.max(x))]))
treeProb <-  predictionTree

#ADABOOST PREDICTION
predictionTreeAda <- predict(treeAda,test,type="probability")
idZtreeAda <-  as.numeric(as.character(factors[apply (predictionTreeAda,1,function(x) which.max(x))]))
treeProbAda <-  predictionTreeAda






#SVM PREDICTION
svmProb <- attr(predict(SVM,test,probability=TRUE),"probabilities")  
idZSVM <- predict(SVM,test)


#NEURAL NET PREDICTION
nnProb<-neuralnet::compute(nn,test)$net.result
nnPrediction <-apply(nnProb,1,function(x) which.max(x))
idZNN <- as.numeric(as.character(factors[nnPrediction]))


#SMO PREDICTION

idZSMO <- predict(SMO,test)
smoProb <- predict(SMO,test,type="probability")



#COMPUTE BAYESIAN VOTE

#TRY WITH DIFFERENT COMBINATIONS OF PREDICTIONS
bayesianSum <- knnProb+smoProb+treeProbAda

idZBayas <- as.numeric(as.character(factors[apply (bayesianSum,1,function(x) which.max(x))]))


#COMPUTE SIMPLE VOTE

#TRY WITH DIFFERENT COMBINATIONS OF PREDICTIONS
results <- cbind(idZKNN,idZSMO,idZtreeAda)

idZVote <- apply(results,1,function (x) as.numeric(names(sort(table(x),decreasing = TRUE)[1])))



#linear regression layer to make prediction
library(VGAM)
library(caretEnsemble)
#CREATE DF WITH PREDICTIONS FROM ALL PREDICTORS

idZKNNf <- as.factor(idZKNN) 
idZNNf <- as.factor(idZNN)
idZtreeAdaf <- as.factor(idZtreeAda)


predDF <- data.frame(testIDZ,idZKNNf,idZNNf,idZSMO,idZtreeAdaf)

#get some of the ensemble DF to train ensemble model, the remaining to test
indexTrain <- sample(nrow(predDF), .9*nrow(predDF))

#USE RANDOM FOREST TO MAKE PREDICTION WITH ENSEMBLE LEARNING
combModFit <- train(testIDZ ~.,method="rf",data=predDF[indexTrain,])


randomForestPred <- predict(combModFit,predDF[-indexTrain,])

rateRandomForest <- 1-mean(randomForestPred==testIDZ[-indexTrain])
#lr2 <- glm(testIDZ~., family=binomial(link='logit'), data=predDF)


#USING NEURAL NET AS ENSEMBLE LAYER TO DECIDE BEETWEN OTHER MODELS

#n <- names(predDF)
#f <- as.formula(paste("testIDZ ~", paste(n[!n %in% "testIDZ"], collapse = " + ")))

#nnData <- apply(predDF,2,function(x) nnet::class.ind(x))

#nnData <- apply(predDF,2,function(x) as.numeric(x))

#nn <- neuralnet::neuralnet(f,data=nnData,hidden=c(neuron),linear.output=FALSE) 

#nnProb<-neuralnet::compute(nn,nnData)






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



