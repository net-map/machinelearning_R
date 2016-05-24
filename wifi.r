library(ISLR)
library(pracma)
library(dplyr)
library(jsonlite)
library(reshape2)







#get the data and return a raw matrix with measures
getMLdata <- function (facility){


#GET list of facilities on server
listaFacilities <- jsonlite::fromJSON("http://server-api-wifi-indoor.herokuapp.com/facilities/user/2") 

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
  mlWifi[,-1] <- apply(mlWifi[,-1],2,Vectorize(dbMtoWatt))
  
  #substitute NAs with zero
  #we consider the signals which where not measured (i.e. the signal was too low) to be zero in power
  mlWifi[is.na(mlWifi)] <- 0
  
  return (mlWifi)
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
facility <- "casa do lira"

rawData <- getMLdata(facility)

tidyData <- prepareData(rawData)




# Train-test random splitting for machine learning
# 30% for tests and the rest for training
index <- sample(1:nrow(tidyData),round(0.7*nrow(tidyData)))
train <- tidyData[index,]
test <- tidyData[-index,]

#Now, we will apply many diferent machine learning algorithms and see which yelds the best classification accuracy

#LOGISTIC REGRESSION 






#NEURALNETWORK






#SUPPORT VECTOR MACHINE













