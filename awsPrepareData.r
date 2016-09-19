
setwd("../machinelearning_R")
path<-paste(getwd(),"serverFunctions.r",sep="/")

invisible(source(path))


#recieve input facility name from command line
args = commandArgs(trailingOnly=TRUE)
#Rscript awsPrepareData.r facilityName 
#default values
if (length(args)==0){
  args <- "Brocolândia"
}



invisible(source("serverFunctions.r"))

mongo <- mongo.create(host="52.67.105.105:27017",username="net.map",password = "p4gic0tb9f2m2yj37iav")

if(mongo.is.connected(mongo) == TRUE) {
  db <- "server_api_production"
  
  
  facilities <- paste(db,"facilities",sep = ".")
  
  #retrive list of facilities that match query and get 1st element which is the ID
  facilityID<-mongo.find.all(mongo,facilities,list(name=args))[[1]][[1]]
  

  zones <- paste(db,"zones",sep = ".")
  
  
  #retrive all zones from that facility
  listZones <- mongo.find.all(mongo,zones,list(facility_id=facilityID))
  
  rawData <- NULL
  
  #for each zone found
  for (z in 1:length(listZones)){
    
    #get ID of that zone from list of lists
    zoneID <- listZones[[z]][[1]]
    
    acquisition <- paste(db,"acquisitions",sep = ".")
    
    listAcquisitions <- mongo.find.all(mongo,acquisition,list(zone_id=zoneID))
    
    lista <- NULL
    
    for (a in 1:length(listAcquisitions)){
      listAP <- listAcquisitions[[a]]$access_points
      acquiID <- listAcquisitions[[a]]$`_id`
      #get relevant informantion from list
      temp <- lapply(listAP,function(x) return (c(x$BSSID,x$RSSI)))
      
      #reshape list and add relevant IDs
      temp2<- lapply(temp,montaLista,zoneID=zoneID,acquiID=acquiID)
      
      #create list in desirable format
      
      #unlist lists 
      for (l in 1:length(temp2)){
         lista <- rbind(lista,unlist(temp2[[l]]))
      }
      
        
      
    }
    
    rawData <- rbind(rawData,lista)
    
  }
  
  
  #we DONT want strings to be turned to factors!
  rawDataDF<-data.frame(rawData,stringsAsFactors = FALSE)
  
  names(rawDataDF)<- c("BSSID","RSSI","idZ","acquiID")
  
  rawDataDF$RSSI <- as.numeric(rawDataDF$RSSI)
  
  
  molten <- melt(rawDataDF,id.vars=c("idZ","acquiID","BSSID"), value.name = "RSSI",measure.vars = c("RSSI"))
  
  molten$variable <- NULL
  
  #gambiarra
  #molten <- molten[-14,]
  
  attach(rawDataDF)
  tidyData <- reshape2::dcast(molten,   acquiID+ idZ  ~ BSSID, value.var = 'RSSI')
  detach(rawDataDF)
  
  
}else{
  print("Could not connect to Mongo! DAMN IT LIRA")
}



#convert to numbers
#zones <- as.numeric(args)

#source("serverFunctions.r")


dataPath <- "prepared-data"
#remove Aquisition ID, as we don't really need it from now on
tidyData<- tidyData[,-1]


#transform idZ into factor!
tidyData[,1] <- as.factor(tidyData[,1])








saveRDS(tidyData,paste(dataPath,"/",args,".rds",sep=""))

