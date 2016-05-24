# Machine learning
library(pracma)
library(dplyr)

#t.test(College$PhD,College$Grad.Rate) para testar independencia



#apply the kalman filter on a vector of RSSI measures
kalmanFilter <- function (X){
  
  #X <- c(-38,-45,-36,-46,-37,-45,-39,-38,-38,-38,-38,-39)
  #if input is a dataframe or matrix, converts it to a one-dimensional vector
  X <-as.vector(X)
  
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
  
  plot(unlist(outMu[1,]),type="l",ylim = c(-70,-20))
  points(X)
  return (outMu)
  
  
  
  
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



