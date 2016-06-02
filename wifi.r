library(ISLR)
library(pracma)
library(dplyr)
library(jsonlite)
library(reshape2)
library(e1071)
library(kknn)


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
facility <- "lira house"
#user id
user <- 2

#rawData <- getMLdata(facility,user)

suppressWarnings( tidyData <- prepareData(rawData))

#zones id is a factor, not a number!
tidyData$idZ <- as.factor(tidyData$idZ)

#tests galore!
suppressWarnings(tests(tidyData))




