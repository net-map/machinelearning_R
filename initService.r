#!/usr/bin/Rscript

#invisible(library(Rserve))

#Rserve(args="--no-save",debug=TRUE)

library("plumber")

r <- plump("serverFunctions.r")


r$run(port=2000)
#run.Rserve()