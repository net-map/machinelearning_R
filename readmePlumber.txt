
Para rodar o servidor de R:

Rscript initService.r



Funcion to train models:

	GET localhost:2000/train&facilityID=ID_SEM_ASPAS
	
	output: [OK]

Function to prepare data:

	GET localhost:2000/prepare&facilityID=ID_SEM_ASPAS
	
	output: [OK]


Function to make a single prediction:
	
	POST  
		Argumento JSON:
		 
		 {
    "facility_id": "5806a9a1bde5c63ae4c5e3f0",
    "access_points": [
      {
        "BSSID": "00:26:86:03:20:07",
        "RSSI": -31
      },
      {
        "BSSID": "00:27:22:a2:76:89",
        "RSSI": -89
      },
      {
        "BSSID": "14:cc:20:9c:b7:6b",
        "RSSI": -76
      },
      {
        "BSSID": "2c:c5:d3:2a:09:08",
        "RSSI": -84
      },
      {
        "BSSID": "2c:c5:d3:ea:09:08",
        "RSSI": -83
      }
    ]
  } 


		localhost:2000/singleTest

		output: [zoneName]
