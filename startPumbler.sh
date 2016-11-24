sudo kill -9 $(lsof -i :2000 -t)
Rscript initService.r &
