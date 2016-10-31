Funcion to train models:

	GET localhost:2000/train&facilityID=ID_SEM_ASPAS

Function to prepare data:

	GET localhost:2000/prepare&facilityID=ID_SEM_ASPAS

Function to make a single prediction:
	
	GET --data  '{"facilityID":ID_SEM_ASPAS, "queueID":ID_SEM_ASPAS}' localhost:2000/singleTest
