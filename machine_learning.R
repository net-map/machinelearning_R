library(ISLR)
library(pracma)
library(dplyr)
library(jsonlite)
library(reshape2)
library(e1071)
library(kknn)
library(cluster)
library(caret)
library(nnet)
library(neuralnet)



#computes manhattan distance (taxi-cab) between two vectors
manhattanDist <- function (v1, v2){
  
  return (sum(abs(v1-v2)))
  
}

#computes euclidean distance
euclideanDist <- function (v1, v2){
  
  return (sqrt(sum((v1-v2)^2)))
  
}



#t.test(College$PhD,College$Grad.Rate) para testar independencia

#used to cross-validate WIFI data using SVM
printSVMResults <- function(train,test,mylogit,kernelType){
  
  
  
  #print(paste("INFORMACOES DO SVM COM KERNEL:",kernelType,sep = " "))
  
  #print(summary(mylogit))

  
  #print("Com SVM, com os dados de treino, temos erro de:")
  trainError <- 100*(1-mean(train$idZ == predict(mylogit)))
  
  #print("Com SVM, com os dados de teste, temos erro de:")
  testError <- 100*(1-mean(test$idZ == predict(mylogit,dplyr::select(test,-idZ))))
  
  
  return (c(kernelType,trainError,testError))
  
}

MHmakeRandomString <- function(n, lenght)
{
  randomString <- c(1:n)                  # initialize vector
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                    lenght, replace=TRUE),
                             collapse="")
  }
  return(randomString)
}




#Use trained models to provide a single classification answer from testVector
#
#
#
#
#
#RSSID1 RSSID2 RSSID3 ...   idZ (id of Zone in which that measure was made)
# -30     -39    -29         2
#
#
#
singleTest <- function (NNmodel,SVMmodel,KNNmodel,testVector){
  
  #get names of columns used to train classifiers
  names <- NNmodel$model.list$variables
  factors <- NNmodel$model.list$response
  #remove quotes (if there are any)
  names <- gsub("`",'',names)
  factors <- gsub("`",'',factors)
  #creates dummy vector with BSSIDs used to train the classifier
  dummyVector <- t(as.data.frame(x=rep(-120,length(names)),names))
  
  #merge testVector with dummyVector in a way that if there is a BSSID missing in the testVector, it is created with -120
 
  commomNames <- intersect(names,names(testVector))
  #get values that are present in testVector
  testVector <- merge(dummyVector,testVector,by=commomNames,all.y=TRUE)
  
  testVector[is.na(testVector)] <- -120

  
  
  #NEURALNET PREDICTION
  nnPrediction <-apply(neuralnet::compute(nn,testVector)$net.result,1,function(x) which.max(x))
  
  #get idz computed
  idZNN <- factors[nnPrediction]
  
  
  #SVM PREDICTION
  svmPrediction <- as.numeric(predict(SVMmodel,testVector))
  
  #get idz computed
  idZSVM <- factors[svmPrediction]
  
  
  #KNN PREDICTION
  knnPrediction <- as.numeric(predict(KNNmodel,testVector))
  
  #get idz computed
  idZKNN <- factors[knnPrediction]
  
  
  results <- cbind(idZKNN,idZSVM,idZNN)
  
  
  #get most recurring result
  idZVote <-  as.numeric(names(sort(table(results),decreasing = TRUE)[1])) 
  
  #finally return calculated idZ
  return (idZVote)
  
}



crossValidateKNN <- function (trainingSet,validationSet,k){

  knnTrain<-train.kknn(idZ ~. , kmax=k,distance=1,kernel = c("rectangular", "triangular", "epanechnikov", "gaussian","rank", "optimal"), data=trainingSet)
  

  trainError <- 100*(1-mean(trainingSet$idZ == predict(knnTrain,trainingSet)))
 
 
  testError <- 100*(1-mean(validationSet$idZ == predict(knnTrain,validationSet)))
 
  return (c(trainError,testError))
  
  
}




crossValidateSVM <- function (trainingSet,validationSet,kernelType){
  
  #SUPPORT VECTOR MACHINE
  
  #We must separate data into X matrix for the features and Y for the response vector with the classes
  #suppressWarnings(attach(train_s))
  #detach(train_s)
  xi<- subset(trainingSet,select= - idZ)
  yi <- trainingSet$idZ
  mylogit <-svm(xi,yi,kernel = kernelType)
  
  #print("Com SVM, com os dados de treino, temos erro de:")
  trainError <- 100*(1-mean(trainingSet$idZ == predict(mylogit)))
  
  #print("Com SVM, com os dados de teste, temos erro de:")
  testError <- 100*(1-mean(validationSet$idZ == predict(mylogit,dplyr::select(validationSet,-idZ))))
  
  
  return(c(trainError,testError))
}





#performs a variety of ML tests on the WIFI dataset
#
#Dataset MUST be in the following form, as outputed by function prepareData:
#
#
#
#RSSID1 RSSID2 RSSID3 ...   idZ (id of Zone in which that measure was made)
# -30     -39    -29         2
# -20     -90    -20         3 
#  ...
#
#
#
#
#
tests <- function(train_s,test_s){
  
  
 
  #NEURALNETWORK


  
      
      #transforms factors in binary dummy vectors
      nnData <- cbind(dplyr::select(train_s,-idZ),nnet::class.ind(train_s$idZ))
      nnDatatest <- cbind(dplyr::select(test_s,-idZ),nnet::class.ind(test_s$idZ))
      
      addq <- function(x) paste0("`", x, "`")
      #adds `x` to every name in data
      names(nnData) <- addq(names(nnData))
      names(nnDatatest) <- addq(names(nnDatatest))
      
      n <- names(nnData)
      nTest <- names(nnDatatest)
      #gets indexes of dummy id columns 
      indexId <- grep("^[[:punct:]][[:digit:]]*[[:punct:]]$",n)
      indexIdTest <- grep("^[[:punct:]][[:digit:]]*[[:punct:]]$",nTest)
      
      lhseq <- paste(names(nnData[,indexId]),collapse="+")
      
      rhseq <- paste(names(nnData[,-indexId]),collapse="+")
      
      #creates formula
      f<-as.formula(paste(lhseq,rhseq,sep = " ~ "))
      
      #for some reason, remove quotes and it works
      nnData <- cbind(dplyr::select(train_s,-idZ),nnet::class.ind(train_s$idZ))
      nnDatatest <- cbind(dplyr::select(test_s,-idZ),nnet::class.ind(test_s$idZ))
      
      #TRAIN neuralnet!
      nnErrorList <- NULL
      
      neuron <- 10
      
      nn <- neuralnet::neuralnet(f,data=nnData,hidden=c(neuron,neuron-1,neuron-2,neuron-3),linear.output=FALSE) 
      assign("NeuralNet",nn,.GlobalEnv)
      
      
      trainRes <-apply(neuralnet::compute(nn,nnData[,-indexId])$net.result,1,function(x) which.max(x))
      testRes <-apply(neuralnet::compute(nn,nnDatatest[,-indexIdTest])$net.result,1,function(x) which.max(x))
      
      
      #remount idZ for train and test datasets
      idZtest <- factor(apply(nnDatatest[,indexIdTest], 1, function(x) which(x == 1)), labels= colnames(nnDatatest[,indexIdTest])) 
      idZ <- factor(apply(nnData[,indexId], 1, function(x) which(x == 1)), labels = colnames(nnData[,indexId])) 
      
      #create response factors from computed data
      
      print( colnames(nnDatatest[,indexIdTest]))
      print(testRes)
      
      idZtestres <- factor(testRes, labels  = colnames(nnDatatest[,indexIdTest])) 
      idZres <- factor(trainRes, labels= colnames(nnData[,indexId])) 
      
      
      resultsNN <- idZtestres
      
      
      trainError <- 100*(1-mean(idZ == idZres))
      testError <- 100*(1-mean(idZtest == idZtestres))
      
      nnError <- c(trainError,testError)
      
      
      
      #plot(nnErrorList[,2],nnErrorList[,3])
     
      #cor(nnErrorList[,2],nnErrorList[,3])
  
  
  #SUPPORT VECTOR MACHINE
  
  #We must separate data into X matrix for the features and Y for the response vector with the classes
  #suppressWarnings(attach(train_s))
  #detach(train_s)
  xi<- subset(train_s,select= - idZ)
  yi <- train_s$idZ
  
  print(yi)
  print(dim(yi))
  print(dim(xi))
  #TEST IN EACH KERNEL
  res <- c("KernelType","Train Error","Test Error")
  
  
  kernelType <- "linear" 
  mylogit1 <-svm(xi,yi,kernel = kernelType)
  assign("SVM",mylogit1,.GlobalEnv)
  resultsSVM <- predict(mylogit1,dplyr::select(test_s,-idZ))
  res <- rbind(res,printSVMResults(train_s,test_s,mylogit1,kernelType))
  SVMerror <- printSVMResults(train_s,test_s,mylogit1,kernelType)[2:3]
  kernelType <- "polynomial" 
  mylogit <-svm(xi,yi,kernel = kernelType)
  res <- rbind(res,printSVMResults(train_s,test_s,mylogit,kernelType))
  kernelType <- "radial" 
  mylogit <-svm(xi,yi,kernel = kernelType)
  res <- rbind(res,printSVMResults(train_s,test_s,mylogit,kernelType))
  kernelType <- "sigmoid" 
  mylogit <-svm(xi,yi,kernel = kernelType)
  res <- rbind(res,printSVMResults(train_s,test_s,mylogit,kernelType))
  
  
  
  
  assign("res",res,.GlobalEnv)
  
  
  
  # NN - Distance
  
  attach(train_s)
  #split dataset by groups of same idZ
  groups <- split(train_s,idZ)
  detach(train_s)
  
  
  testX <- subset(test_s,select=- idZ)
  testY <- test_s$idZ
  
  
  
  
  anslist <- NULL
  
  
  labelGroups <- vector(mode="numeric",length=(size(groups)[2]-1))
  
  #count of group
  count <- 1
  
  for (group in groups[-3]) {
     
    
    #transform from factor to number
    group$idZ <- as.numeric(as.character(group$idZ))
    
    #get idZ number of group 
    labelGroups[count]<-group$idZ[1]
    
    count<- count +1
    
    #for each group matrix remove idZ as we dont need it anymore
    group_idz <- dplyr::select(group,-idZ)
    
    temp <- matrix(nrow=nrow(testX))
    
    for( i in 1:nrow(testX) ){
          #sum manhatan distances between test vector and each vector from group matrix
          temp[i] <- sum(as.matrix(dist(rbind(testX[i,],group_idz),method = "manhattan"))[,1])
      
    }
    
     anslist<-cbind(anslist,temp)
    
     
  }
  
  
   #get the position of min value in each row
   minList <- apply(anslist,1, function(x) which.min(x) )  
   
   #get for each result the label it corresponds to
   resultsMAND <- labelGroups[minList]
   
   #compute error
   manhattanError <- 100* (1 -  mean(resultsMAND == testY))
  
  
   
   
   
  #K-nearest neighbours
  knnTrain<-train.kknn(idZ ~. , kmax=3,kernel = c("rectangular", "triangular", "epanechnikov", "gaussian","rank", "optimal"), data=train_s)
  
  assign("KNN",knnTrain,.GlobalEnv)
  
  #plot(knnTrain)
  
  
  cat("INFORMACOES DO KNN")
  print(summary(knnTrain))
  
  cat("Com KNN, com os dados de treino, temos erro de:")
  
  print(100*(1-mean(train_s$idZ == predict(knnTrain,train_s))))
  trainError <- 100*(1-mean(train_s$idZ == predict(knnTrain,train_s)))
  #print(100*classError(train_s$idZ,predict(knnTrain,train_s))$errorRate)
  cat("Com KNN, com os dados de teste, temos erro de:")
  
  print(100*(1-mean(test_s$idZ == predict(knnTrain,test_s))))
  testError <- 100*(1-mean(test_s$idZ == predict(knnTrain,test_s)))
  #print(100*classError(test_s$idZ,predict(knnTrain,test_s))$errorRate)
  
  resultsKNN <- predict(knnTrain,test_s)
  
  KNNerror <- c(trainError,testError)
  
  
  
  
  print("RESULTADOS PARA COMPARACAO")
  print("RESULTADOS KNN")
  print(resultsKNN)
  print("RESULTADOS MANHATTAN")
  print(resultsMAND)
  print("RESULTADOS SVM")
  print(resultsSVM)
  print("RESULTADOS REDE NEURAL")
  print(resultsNN)
  print("RESULTADOS ESPERADOS")
  print (test_s$idZ)
  
  
  
  
  tabela <- cbind(as.numeric(as.character(resultsKNN)),as.numeric(as.character(resultsSVM)),as.numeric(as.character(resultsNN)))
 
  #vote among results for the most recurrent label 
  
  resultsVote <- apply(tabela,1, function(x) as.numeric(names(sort(table(x),decreasing = TRUE)[1])) ) 
  #compute error of vote
  
  voteError <- 100*(1-mean(test_s$idZ==resultsVote))
  
  models<-c(knnTrain,nn,mylogit1)
  
  return (c(KNNerror,SVMerror,nnError,voteError))

}


crossValidateNN <- function (trainset,validateset,neuron){
  
  
  idZ <- trainset$idZ
  idZtest <- validateset$idZ
  
  
  #transforms factors in binary dummy vectors
  nnData <- cbind(dplyr::select(trainset,-idZ),nnet::class.ind(trainset$idZ))
  nnDatatest <- cbind(dplyr::select(validateset,-idZ),nnet::class.ind(validateset$idZ))
  
  addq <- function(x) paste0("`", x, "`")
  #adds `x` to every name in data
  names(nnData) <- addq(names(nnData))
  names(nnDatatest) <- addq(names(nnDatatest))
  
  n <- names(nnData)
  nTest <- names(nnDatatest)
  #gets indexes of dummy id columns 
  indexId <- grep("^[[:punct:]][[:digit:]]*[[:punct:]]$",n)
  indexIdTest <- grep("^[[:punct:]][[:digit:]]*[[:punct:]]$",nTest)
  
  lhseq <- paste(names(nnData[,indexId]),collapse="+")
  
  rhseq <- paste(names(nnData[,-indexId]),collapse="+")
  
  #creates formula
  f<-as.formula(paste(lhseq,rhseq,sep = " ~ "))
  
  #for some reason, remove quotes and it works
  nnData <- cbind(dplyr::select(trainset,-idZ),nnet::class.ind(trainset$idZ))
  nnDatatest <- cbind(dplyr::select(validateset,-idZ),nnet::class.ind(validateset$idZ))
  
  #TRAIN neuralnet!
  nnErrorList <- NULL
  
  
  
  nn <- neuralnet::neuralnet(f,data=nnData,hidden=c(neuron),linear.output=FALSE)
  
  
  trainRes <-apply(neuralnet::compute(nn,nnData[,-indexId])$net.result,1,function(x) which.max(x))
  testRes <-apply(neuralnet::compute(nn,nnDatatest[,-indexIdTest])$net.result,1,function(x) which.max(x))
  
  
  #remount idZ for train and test datasets
  #idZtest <- factor(apply(nnDatatest[,indexIdTest], 1, function(x) which(x == 1)), labels= colnames(nnDatatest[,indexIdTest])) 
  #idZ <- factor(apply(nnData[,indexId], 1, function(x) which(x == 1)), labels = colnames(nnData[,indexId])) 
  
  #create response factors from computed data
 
  factors <- nn$model.list$response
  factors <- gsub("`",'',factors)
  

  
  idZtestres <- factors[testRes]
  idZres <- factors[trainRes]
  
 
  
  resultsNN <- idZtestres
  
  
  trainError <- 100*(1-mean(idZ == idZres))
  testError <- 100*(1-mean(idZtest == idZtestres))
  
  print (c("O erro com ",neuron,"neuronios na HL teve resultado de ",testError))
  
  return (c(trainError,testError))
  
  
  
  
}






#
#
#TRAIN MODELS WITH TRAIN SET IN FORMAT SPECIFIED IN ANOTHER FUNCTIONS
#
#
#
trainModels <- function(train){
  
  
  
  #NEURALNETWORK
  
  #transforms factors in binary dummy vectors
  nnData <- cbind(dplyr::select(train,-idZ),nnet::class.ind(train$idZ))

  addq <- function(x) paste0("`", x, "`")
  #adds `x` to every name in data
  names(nnData) <- addq(names(nnData))

  n <- names(nnData)
  #gets indexes of dummy id columns 
  indexId <- grep("^[[:punct:]][[:digit:]]*[[:punct:]]$",n)

  lhseq <- paste(names(nnData[,indexId]),collapse="+")
  
  rhseq <- paste(names(nnData[,-indexId]),collapse="+")
  
  #creates formula
  f<-as.formula(paste(lhseq,rhseq,sep = " ~ "))
  
  #for some reason, remove quotes and it works
  nnData <- cbind(dplyr::select(train,-idZ),nnet::class.ind(train_s$idZ))

  #TRAIN neuralnet!

  neuron <- 10
  
  nn <- neuralnet::neuralnet(f,data=nnData,hidden=c(neuron,neuron-1,neuron-2,neuron-3),linear.output=FALSE) 
  assign("NeuralNet",nn,.GlobalEnv)
  
  
  #SUPPORT VECTOR MACHINE
  
  #We must separate data into X matrix for the features and Y for the response vector with the classes
  #suppressWarnings(attach(train_s))
  #detach(train_s)
  xi<- subset(train,select= - idZ)
  yi <- train$idZ
  
  
  
  kernelType <- "linear" 
  mylogit1 <-svm(xi,yi,kernel = kernelType)
  
  assign("SVM",mylogit1,.GlobalEnv)
 
  
  
  #
  #
  #K NEAREST NEIGHBOURS
  #
  #
  
  knnTrain<-train.kknn(idZ ~. , kmax=3,kernel = c("rectangular", "triangular", "epanechnikov", "gaussian","rank", "optimal"), data=train)
  
  assign("KNN",knnTrain,.GlobalEnv)
  


}


#apply the kalman filter on a vector of RSSI measures
kalmanFilter <- function (X){
  
  #X <- c(-38,-45,-36,-46,-37,-45,-39,-38,-38,-38,-38,-39)
  #if input is a dataframe or matrix, converts it to a one-dimensional vector
  X <-as.vector(X)
  
  #transforms into row vector if needed
  if(dim(X)[1]==1){
    X <- t(X)
  }
  
  #initial hunch for mean (first measure)
  muInit <- X[1]
  #initial variance
  pInit <- 50
  
  Qt <- var(X)
  
  Rt <- 0.008 
  
  
  
  
  newMu <- muInit
  newP <- pInit
  
  
  
  outMu <- newMu
  for(Xi in X){
    
    #PREDICTION PHASE
    
    mu<- newMu
    
    p <- newP + Rt
    
    #UPDATE PHASE
    Kgain <- p*(p+Qt)^-1
    
    newMu <- mu + Kgain*(Xi-mu)
    
    newP <- p - (Kgain*p)
    
    outMu <- cbind(outMu,newMu)
    
  }
  
  #plot(unlist(outMu[1,]),type="l",ylim = c(-70,-20),xlab = "# de Iterações", ylab="Potência(dB)" )
  #points(X)
  #cat ("Press [enter] to continue")
  #line <- readline()
  
  return (outMu[length(outMu)])
  
  
  
  
}


plotLearningCurveOnevsAll<- function (trainX,trainY,numlabels,testX,testY){
    #yt <- t(y) #transpoe vetor de entrada para coluna
  
    trainX <- data.matrix(trainX);
    trainY <- data.matrix(trainY);
    testX <- data.matrix(testX);
    testY <- data.matrix(testY);
    
  
    errorTrain <- NULL
    errorTest <- NULL
    
    countMin <- 2
    countMax <- dim(trainX)[1]
    countStep <- 1
    
    for (count in seq(countMin,countMax,countStep)){
      
      errors <- oneVsAll(trainX[1:count,],trainY[1:count,1],numlabels,testX,testY)
      errorTrain <- c(errorTrain,errors[[1]])
      errorTest <- c(errorTest,errors[[2]])
      
    }
  endX <- (countMax-countMin)/countStep
  jpeg('testErr.jpg')
  plot(0:endX,errorTest,type="p",ylab="Test Error",xlab="nMeasures")
  dev.off()
  jpeg('trainErr.jpg')
  plot(0:endX,errorTrain,type="p",ylab="Train Error",xlab="nMeasures")
  dev.off()
  
  
}




plotLearningCurveLogRegression<- function (trainX,trainY,testX,testY){
  #yt <- t(y) #transpoe vetor de entrada para coluna
  
  trainX <- data.matrix(trainX);
  trainY <- data.matrix(trainY);
  testX <- data.matrix(testX);
  testY <- data.matrix(testY);
  
  
  errorTrain <- NULL
  errorTest <- NULL
  
  countMin <- 2
  countMax <- dim(trainX)[1]
  countStep <- 10
  
  for (count in seq(countMin,countMax,countStep)){
    
    errors <- logisticRegression(trainX[1:count,],trainY[1:count,1],testX,testY)
    errorTrain <- c(errorTrain,errors[[1]])
    errorTest <- c(errorTest,errors[[2]])
    
  }
  endX <- (countMax-countMin)/countStep
  jpeg('testErr1.jpg')
  plot(0:endX,errorTest,type="p",ylab="Test Error",xlab="nMeasures")
  dev.off()
  jpeg('trainErr1.jpg')
  plot(0:endX,errorTrain,type="p",ylab="Train Error",xlab="nMeasures")
  dev.off()
  
  
}




logisticRegression <- function (X,y,testX,testY) {
 
  #data <- read.table("ex2data1.txt",sep=",")
  #X <- data[,1:2];
  #y <- data[,3];
  
  
  #transforma dados em matrizes matematicas
  X <- data.matrix(X);
  y <- data.matrix(y);
  
  #X <- mapFeatures(X[,1],X[,2]);
  X <- normalize(X)
  
  
  
  
  m <- dim(X)[1];
  n <- dim(X)[2];
  X <-cbind(1,X) #adicionar coluna de 1s
  theta_init <- matrix(0,n+1,1); #inicializa vetor de parametros
  
  
  cost <- costFunction(theta_init,X,y,0);
  grads <- grad(theta_init,X,y,0); 
 
  
  
  
  objOp <- optim(theta_init,fn=costFunction,gr=grad,X=X,y=y,lambda=0,method="BFGS" )
  result <- as.numeric(sigmoid(X%*%objOp$par)>0.5)
  
  
  
  trainErr <-100 - mean(y==result)*100
  
  
  testX <- data.matrix(testX);
  testY <- data.matrix(testY);
  
  testX <-cbind(1,testX) #adicionar coluna de 1s
  result2 <- as.numeric(sigmoid(testX%*%objOp$par)>0.5)
  
  
  
  testerr <- 100 - mean(testY==result2)*100
  
  
  
  return (res.list<-list(trainErr,testerr))
  
  
 
}


costFunction <- function(theta,X,y,lambda ){
  
  
  resultados <- sigmoid(X%*%theta);
  m <- dim(y)[1];
  
  
  J <- (1/m) *  (-y*log(resultados)-(1.-y)*log(1.-resultados));
  #J = sum(J)+ +(lambda*(1/(2*m))*sum(theta(2:end).^2));
  
  return (sum(J) + (lambda*(1/(2*m))*sum(theta[-1]^2)) )##retornar custo
  
}

grad <- function(theta, X, y, lambda){ 
  
  resultados <- sigmoid(X%*%theta);
  m <- dim(y)[1]
  
  
  grad <- (1/m) *  t(X)%*%(resultados-y);
  temp <- theta;
  temp[1]<-0;
  
  grad <- grad +  ((lambda/m)*temp);
  
  return (grad) #retorna vetor de derivadas parciais aka gradiente
}


oneVsAll <- function(trainX,trainY,numlabels,testX,testY){
  
  num_labels <- numlabels; #numero de classes de categorizacao
  
  
  trainX <- data.matrix(trainX);
  trainY <- data.matrix(trainY);
  

  

  #train <- mapFeatures(X[,1],X[,2]);
  
 
  
  
  m <- dim(trainX)[1];
  n <- dim(trainX)[2];
  
  
 #backupX <- trainX  
 #p <- 8
# trainX <- poly_par(trainX[,1],p) ;#cria matriz com parametros da primeira coluna de parametros

  
  # for( parCol in 2:n) {
     
   # tempM <- poly_par(backupX[,parCol],p) #cria matriz com parametros da n-esima coluna de parametros
  #   trainX <- cbind(trainX,tempM)
    
  #}
  
  trainX <- normalize(trainX); #normaliza vetor de exemplos, nao eh necessario sempre
  
  y<-trainY #only traning data
  

  #transforma dados em matrizes matematicas
  trainX <- data.matrix(trainX);
  trainY <- data.matrix(trainY);
  m <- dim(trainX)[1];
  n <- dim(trainX)[2];
  
 
  trainX <-cbind(1,trainX) #adicionar coluna de 1s
  theta_matrix <- matrix(0,num_labels,n+1) #matriz de parametros
  
  
  for (number in 1:num_labels ){ #regressao oneXall
    
    theta_init <- matrix(0,n+1,1); #inicializa vetor de parametros
    
    #print(typeof(X));
    #print(typeof(theta_init));
    
   
    
    objOp <- optim(theta_init,fn=costFunction,gr=grad,X=trainX,y=(trainY==number),lambda=0,method="BFGS")

    theta_matrix[number,] <- objOp$par;
  }
  
  
  #matrix <- Xi%*%t(theta_matrix)

result <- apply(trainX%*%t(theta_matrix),1,which.max) 
trerr <-100 -  mean(trainY==apply(trainX%*%t(theta_matrix),1,which.max))*100 #training error


testX <- data.matrix(testX);
testY <- data.matrix(testY);
testX <-cbind(1,testX) #adicionar coluna de 1s
testerr <- 100 - mean(testY==apply(testX%*%t(theta_matrix),1,which.max))*100 #test error
 
 return (res.list<-list(trerr,testerr));
  #return (theta_matrix)
  
 
  
  
  #return(theta_matrix)
 
}

normalize <- function(X){ #normalize every collumn of X, being each collum a parameter
  
  m <- dim(X)[1]; # lines
  n <- dim(X)[2]; #collumns
  Xnorm <- X;
  
  for (ncol in 1:n){ #iterate over colunas
    
    xCol <- X[,ncol];
    
    xCol <- (xCol - mean(xCol))/std(xCol); #normalize
    
    Xnorm[,ncol] <- xCol;
    
  }
  
  return (Xnorm);
  
}

poly_par<- function(Xi,p){ 
  #pega vetor de parametros X e retorna matriz com potencias desse parametro ate a potencia p
  #a matriz eh formada sendo cada que a primeira coluna eh X^1, a segunda X^2 e assim por diante
  
  X<-Xi
  for(power in 1:p){
    
    X <- cbind(X, Xi^power );
  }
  
  return (X[,-1]) #ignora coluna repetida pois segunda coluna eh x^1
}

mapFeatures<- function(X1,X2){
  # MAPFEATURE Feature mapping function to polynomial features
  #
  #   MAPFEATURE(X1, X2) maps the two input features
  #   to quadratic features used in the regularization exercise.
  #
  #   Returns a new feature array with more features, comprising of 
  #   X1, X2, X1.^2, X2.^2, X1*X2, X1*X2.^2, etc..
  #
  #   Inputs X1, X2 must be the same size
  #
  
  X1 <- data.matrix(X1);
  X2 <- data.matrix(X2);
  
  
  m <- dim(X1)[1]; # lines
  n <- dim(X1)[2]; #collumns
  

  degree <- 5;
 # out = ones(size(X1(:,1)));
  
  #out <- matrix(1,m,1);
  out <- NULL #NAO ESTA ADICIONANDO COLUNA DE 1s, PRESTAR ATENCAO NISSO
  
  
      for (i in 1:degree){
        
        for (j in 0:i){
          #out(:, end+1) = (X1.^(i-j)).*(X2.^j);
          out <- cbind(out,X1^(i-j)*(X2^j));
      }
      }
  
  return (out)
}


#
#data will be parsed from http://server-api-wifi-indoor.herokuapp.com
#Sample GET instructions can be found via that URL
#
#
#GET the data from all the zones and points in a facility in database and return a raw matrix with measures
#
getMLdata <- function (facility,user_id){
  
  userid <- user_id
  
  #GET list of facilities on server
  listaFacilities <- jsonlite::fromJSON(paste("http://server-api-wifi-indoor.herokuapp.com/facilities/user/",toString.default(userid),sep="")) 
  
  #GET id of desired facility
  id <- dplyr::filter(listaFacilities, name == facility)$id
  
  
  #Create url with id of desired facility
  urlZones <- paste("http://server-api-wifi-indoor.herokuapp.com/zones/facility/",toString.default(id),sep="")
  
  
  
  #GET list of zones in facility
  listZones <- jsonlite::fromJSON(urlZones)
  
  
  
  
  zonesID <- listZones$id
  
  templistWifi <- NULL
  listWifi <- NULL
  measuresList <- NULL
  listAP <- NULL
  
  #For each id, GET all the points
  for (idZ in zonesID) {
    #GET list of points in zone with id "id"
    print(paste("Pegando pontos da zona",idZ))
    urlPoints <- paste("http://server-api-wifi-indoor.herokuapp.com/points/zone/",toString.default(idZ),sep="") 
    listPoints <- jsonlite::fromJSON(urlPoints)
    pointsID <- listPoints$id
    print(paste(" ", "Foi achado o ponto ",pointsID))
    #empty WifiList for each zone before point loop
    templistWifi <- NULL
    #GET first measure in each point
    for (idP in pointsID){
      
      #GET list of measures in point with id "idP"
      print(paste("   ","Pegando measures do ponto: ",idP))
      urlMeasures <- paste("http://server-api-wifi-indoor.herokuapp.com/measures/point/",toString.default(idP),sep="")
      listMeasures <- jsonlite::fromJSON(urlMeasures)
      
      #GET FIRST MEASURE
      urlAP<- paste("http://server-api-wifi-indoor.herokuapp.com/access_points/measure/",toString.default(listMeasures$id[1]),sep="")
      
      listAP <- jsonlite::fromJSON(urlAP)
      
      listAP <- dplyr::select(listAP,c(bssid,rssi))
      
      #FOR second measure to the last
      for (mId in listMeasures$id[-1]){
        urlAP<- paste("http://server-api-wifi-indoor.herokuapp.com/access_points/measure/",toString.default(mId),sep="")
        listAPt <- jsonlite::fromJSON(urlAP)
        #merge by BSSID, will end with dataframe with bssid and multiple rows of RSSIs
        listAP <- merge(listAP,listAPt[c("bssid","rssi")],by="bssid")
        
      }
      
      #apply kalman filter for every measure
      k<- matrix(nrow=nrow(listAP))
      
      for(i in 1:nrow(listAP)){
        
        temp <- listAP[i,-1]
        
        k[i,] <- kalmanFilter(temp)
        
      }
      
      
      res <- cbind(listAP[,1],k)
      
      
      print(paste("     ","Foi pega a measure: ",listMeasures$id[1]))
      
      #WITH KALMANN FILTER
      listAccessPoints <- data.frame(bssid=res[,1],rssi=res[,2])
      #WITHOUT KALMANN FILTER
      #listAccessPoints <- data.frame(bssid=listAP[,1],rssi=listAP[,2])
      
      #only bssi and rssi matter for the machine learning algorithm
      
      #create vector with bssid and rssi: each with the id of the zone and measure they are part of
      
      templistWifi <- cbind(t(cbind(dplyr::select(listAccessPoints,c(bssid,rssi)),idZ,listMeasures$id[1])),templistWifi)
      
      
    }
    
    #create list across zones
    listWifi <- cbind(listWifi,templistWifi)
    
  }
  
  return (listWifi)
  
}


#convert dbM to Watts
dbMtoWatt <- function (value){
  
  return (10^(value/10))
}


#transforms the data parsed in to the matrix in which the machine learning will be applied
#Dataset will be outputed in the following form
#
#
#
#RSSID1 RSSID2 RSSID3 ...   idZ (id of Zone in which that measure was made)
# -30     -39    -29         2
# -20     -90    -20         3 
#  ...
#
prepareData <- function (listWifi){
  
  listWifi <- listWifi
  
  #transpose matrix 
  temp <- t(listWifi)
  
  #change matrix type to dataframe
  dfWifi <- data.frame(temp)
  
  colnames(dfWifi)[4] <- "measureID"
  
  
  #reshape dataframe for the machine learning algorithm
  mlWifi <- reshape2::dcast(dfWifi, measureID + idZ ~ bssid, value.var = 'rssi')
  
  #remove measure ID
  mlWifi[,1] <- NULL
  
  
  #convert strings to numbers
  mlWifi <- apply(mlWifi, 2, as.numeric)
  
  
  
  
  #convert measures from dbM to Watts 
  #TODO checar significancia dos algarismos na conversao
  #mlWifi[,-1] <- apply(mlWifi[,-1],2,Vectorize(dbMtoWatt))
  
  #substitute NAs with zero
  #we consider the signals which where not measured (i.e. the signal was too low) to be zero in power
  mlWifi[is.na(mlWifi)] <- -120
  
  
  #colnames(mlWifi) <- paste("t",colnames(mlWifi),"t",sep="")
  
  
  return (as.data.frame(mlWifi))
}



