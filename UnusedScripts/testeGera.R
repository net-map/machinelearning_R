

path <- "~/Documents/machinelearning_R/datasets"
#datasets <- prepareUCIdata2(path,1,0,justInside = TRUE)
datasets <- prepareUCIdata2(path,1,0)

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
factors <- gsub('"','',factors)


#KNN PREDICTION
knnTrain<-kknn(formula=idZ ~. , k=4,distance=1, train=train,test=test,kernel="optimal")
knnProb <- knnTrain$prob
knnPrediction <- as.numeric(as.character(knnTrain$fitted.values))
idZKNN <- knnPrediction


#DECISION TREE PREDICTION
predictionTree <- predict(tree,test,type="probability")
idZtree <-  as.numeric(as.character(factors[apply (predictionTree,1,function(x) which.max(x))]))
treeProb <-  predictionTree


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
bayesianSum <- knnProb+treeProb+svmProb+nnProb+smoProb

idZBayas <- as.numeric(as.character(factors[apply (bayesianSum,1,function(x) which.max(x))]))


#COMPUTE SIMPLE VOTE

#TRY WITH DIFFERENT COMBINATIONS OF PREDICTIONS
results <- cbind(idZKNN,idZNN,idZtree,idZSMO)

idZVote <- apply(results,1,function (x) as.numeric(names(sort(table(x),decreasing = TRUE)[1])))




#COMPUTE CLASSIFICATION ~ERROR~ RATES


rateSVM <- 1- mean(idZSVM == testIDZ)
rateNN <- 1- mean(idZNN==testIDZ)
rateKNN <- 1- mean(idZKNN==testIDZ)
rateTree <- 1- mean(idZtree==testIDZ)
rateSMO <- 1- mean(idZSMO==testIDZ)

rateVote <- 1-mean(idZVote==testIDZ)
rateBayas <- 1-mean(idZBayas==testIDZ)


cat("ERROS PARA TESTE ")

cat("Erro de SVM",rateSVM)
cat("Erro de SMO",rateSMO)
cat("Erro de KNN",rateKNN)
cat("Erro de J48",rateTree)
cat("Erro de NN",rateNN)
cat("Erro de Voto Simples",rateVote)
cat("Erro de Voto com Peso",rateBayas)



