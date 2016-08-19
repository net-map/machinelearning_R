
#recieve input facility name from command line
args = commandArgs(trailingOnly=TRUE)
#default values
if (length(args)==0){
  args <- "Brocolândia"
}



mongo <- mongo.create(host="52.67.105.105:27017",username="net.map",password = "p4gic0tb9f2m2yj37iav")


if(mongo.is.connected(mongo) == TRUE) {
  db <- "server_api_production"
  
  
  facilities <- paste(db,"facilities",sep = ".")
  
  #retrive list of facilities that match query and get 1st element which is the ID
  facilityID<-mongo.find.all(mongo,facilities,list(name=args))[[1]][[1]]
  

  zones <- paste(db,"zones",sep = ".")
  
  
  #retrive all zones from that facility
  listZones <- mongo.find.all(mongo,zones,list(facility_id=facilityID))
  
  #for each zone found
  for (i in 1:length(listZones)){
    
    #get ID of that zone from list of lists
    zoneID <- listZones[[i]][[1]]
    
    acquisition <- paste(db,"acquisitions",sep = ".")
    
    listAcquisitions <- mongo.find.all(mongo,acquisition,list(zone_id=zoneID))
    
    
    tabela<-NULL
    for (i in 1:length(listAcquisitions)){
      
      listAP <- listAcquisitions[[i]]$access_points
      
      #get relevant informantion from list
      temp <- lapply(listAP,function(x) return (c(x$BSSID,x$RSSI)))
      
      #change names
      temp2<- lapply(temp,montaLista)
      
      #create list in desirable format
      lista <- NULL
      for (i in 1:length(temp2)){
         lista <- rbind(lista,temp2[[i]])
      }
      
        
      
      tabela <- merge(lista,tabela,by="BSSID")
    }
    
    
    
    
  }
  
  
  
  
}else{
  print("ERROOOOOOOOOOU")
}




#convert to numbers
zones <- as.numeric(args)

source("serverFunctions.r")


dataPath <- "raw-data"


datasets <- prepareUCIdata2(dataPath,args[1],args[2])


saveRDS(datasets,"prepared-data/UCIdata.rds")

