library(ISLR)
library(pracma)
library(dplyr)
library(jsonlite)
library(reshape2)
library(e1071)







#get the data from a facility in database and return a raw matrix with measures
getMLdata <- function (facility){


#GET list of facilities on server
listaFacilities <- jsonlite::fromJSON("http://server-api-wifi-indoor.herokuapp.com/facilities/user/1") 

#GET id of desired facility
id <- dplyr::filter(listaFacilities, name== facility)$id


#Create url with id of desired facility
urlZones <- paste("http://server-api-wifi-indoor.herokuapp.com/zones/facility/",toString.default(id),sep="")



#GET list of zones in facility
listZones <- jsonlite::fromJSON(urlZones)




zonesID <- listZones$id

templistWifi <- NULL
listWifi <- NULL
measuresList <- NULL

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
         #Get id of only first measure 
         measureID <- listMeasures$id[1]
         
         
         
         
         
         for (mId in listMeasures$id){
           
           urlAP<- paste("http://server-api-wifi-indoor.herokuapp.com/access_points/measure/",toString.default(mId),sep="")
           
           listAP <- jsonlite::fromJSON(urlAP)
           
           
           
          # measuresList <- cbind(measuresList,select(filter(listAP,ssid=="Lira"),rssi))
         }
           
         
         
         
         #TODO apply kalman filter for every measure
         
         
         print(paste("     ","Foi pega a measure: ",measureID))
         #GET lista of detected APs for this measure
         urlAccessPoints <- paste("http://server-api-wifi-indoor.herokuapp.com/access_points/measure/",toString.default(measureID),sep="")
         listAccessPoints <- jsonlite::fromJSON(urlAccessPoints)
         
         
         #only bssi and rssi matter for the machine learning algorithm
         
         #create vector with bssid and rssi: each with the id of the zone and measure they are part of
         
         templistWifi <- cbind(t(cbind(select(listAccessPoints,c(bssid,rssi)),idZ,measureID)),templistWifi)
         
         
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
  
  listWifi <- rawData
  
  #transpose matrix 
  temp <- t(listWifi)
  
  #change matrix type to dataframe
  dfWifi <- data.frame(temp)
  
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




# Train-test random splitting for machine learning
# 30% for tests and the rest for training

#zones id is a factor, not a number!
tidyData$idZ <- as.factor(tidyData$idZ)

colnames(tidyData)[3] <- "00:19:e0:a0:07:0e3"

index <- sample(1:nrow(tidyData),round(0.7*nrow(tidyData)))
train <- tidyData[index,]
test <- tidyData[-index,]

#Now, we will apply many diferent machine learning algorithms and see which yelds the best classification accuracy




#LOGISTIC REGRESSION 

#number of classes
#labels <- 2


trainX <- select(train,-idZ)
testX <- select(test,-idZ)
trainY <- select(train,idZ)
testY <- select(test,idZ)

#error <- oneVsAll(trainX,trainY,labels,testX,testY)



#NEURALNETWORK



# Scaling data for the NN
maxs <- apply(dplyr::select(tidyData,-idZ), 2, max) 
mins <- apply(dplyr::select(tidyData,-idZ), 2, min)
scaled <- as.data.frame(scale(dplyr::select(tidyData,-idZ), center = mins, scale = maxs - mins))

#concatenate with response vector again 
scaled$idZ <- tidyData$idZ


# Train-test scaled
train_s <- scaled[index,]
test_s <- scaled[-index,]




# NN training
library(neuralnet)
n <- names(train_)
f <- as.formula(paste("idZ ~", paste(n[!n %in% "idZ"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(7,5),linear.output=FALSE)

# Visual plot of the model
plot(nn)




#SUPPORT VECTOR MACHINE


attach(scaled)
mylogit <-svm(idZ~., data = scaled)

predict(mylogit,testY)








