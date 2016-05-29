library(ISLR)
library(pracma)
library(dplyr)
library(jsonlite)
library(reshape2)
library(e1071)
library(kknn)







#get the data from a facility in database and return a raw matrix with measures

getMLdata <- function (facility){


#GET list of facilities on server
listaFacilities <- jsonlite::fromJSON("http://server-api-wifi-indoor.herokuapp.com/facilities/user/1") 

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
     
         listAccessPoints <- data.frame(bssid=res[,1],rssi=res[,2])
         
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
  mlWifi[is.na(mlWifi)] <- -110

  
  #colnames(mlWifi) <- paste("t",colnames(mlWifi),"t",sep="")

  
  return (as.data.frame(mlWifi))
}

#
#
#
#
#
#
#
#
#main script
#
#
#
#
#
#
#





#Name of facility
#facility <- "Rua Lino Coutinho, 237, Ap. 43"
facility <- "Apartamento Adriano"


#rawData <- getMLdata(facility)

tidyData <- prepareData(rawData)

#zones id is a factor, not a number!
tidyData$idZ <- as.factor(tidyData$idZ)






# Scaling data
maxs <- apply(dplyr::select(tidyData,-idZ), 2, max) 
mins <- apply(dplyr::select(tidyData,-idZ), 2, min)
scaled <- as.data.frame(scale(dplyr::select(tidyData,-idZ), center = mins, scale = maxs - mins))
#concatenate with response vector again 
scaled$idZ <- tidyData$idZ

# Train-test random splitting for machine learning
# 30% for tests and the rest for training
#index <- sample(1:nrow(tidyData),round(0.7*nrow(tidyData)))
#train <- tidyData[index,]
#test <- tidyData[-index,]

#Train and test UNESCALED
train<- dplyr::filter(tidyData,idZ==5 |idZ==4)
test<- dplyr::filter(tidyData,idZ==6)
test$idZ <- c(4,4,4,4,4)
#Train and test SCALED
train_s<- dplyr::filter(scaled,idZ==5 |idZ==4)
test_s<- dplyr::filter(scaled,idZ==6)
test_s$idZ <- c(4,4,4,4,4)
#Now, we will apply many diferent machine learning algorithms and see which yelds the best classification accuracy




#LOGISTIC REGRESSION 

#number of classes
#labels <- 2


#trainX <- select(train,-idZ)
#testX <- select(test,-idZ)
#trainY <- select(train,idZ)
#testY <- select(test,idZ)

#error <- oneVsAll(trainX,trainY,labels,testX,testY)



#NEURALNETWORK


# NN training
library(neuralnet)

newNames <- replicate(length(train_s), paste(sample(LETTERS, 10, replace=TRUE), collapse=""))
train_sn <- train_s
test_sn <- test_s

names(train_sn) <- newNames
names(test_sn) <- newNames


#names(train_s) <- paste("(",names(train_s),")",sep="")
#names(train_s)  <-  gsub(":","",names(train_s))

colnames(train_sn)[length(train_sn)] <- "idZ"
n <- names(train_sn)
f <- as.formula(paste("idZ ~", paste(n[!n %in% "idZ"], collapse = " + ")))

#nn <- neuralnet(f,data=as.matrix(train_sn),hidden=25,linear.output=FALSE,threshold=0.01)

# Visual plot of the model
#plot(nn)




#SUPPORT VECTOR MACHINE

#We must separate data into X matrix for the features and Y for the response vector with the classes
suppressWarnings(attach(train_s))
xi<- subset(train_s,select= - idZ)
yi <- idZ


mylogit <-svm(xi,yi)
predict(mylogit)

print("INFORMACOES DO SVM")
print(summary(mylogit))

print("Com SVM, com os dados de treino, temos erro de:")
print(100*(1-mean(train_s$idZ == predict(mylogit))))

print("Com SVM, com os dados de teste, temos erro de:")
print(100*(1-mean(test_s$idZ == predict(mylogit,dplyr::select(test_s,-idZ)))))

#K-nearest neighbours



knnTrain<-train.kknn(idZ ~. , kmax=4,kernel = c("rectangular", "triangular", "epanechnikov", "gaussian","rank", "optimal"), data=train_s)


#plot(knnTrain)


print("INFORMACOES DO KNN")
print(summary(knnTrain))

print("Com KNN, com os dados de treino, temos erro de:")

print(100*(1-mean(train_s$idZ == predict(knnTrain,train_s))))

print("Com KNN, com os dados de teste, temos erro de:")

print(100*(1-mean(test_s$idZ == predict(knnTrain,test_s))))
