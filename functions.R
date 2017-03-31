#functions.R

#Libraries ####
library(openxlsx)
library(magrittr)
library(data.table)
library(R6)


#functions
source("loadWorkbook.R")


#Classes ####
source(paste0(getwd(), "/classes/REPORTclass.R"))
source(paste0(getwd(), "/classes/RESULTclass.R"))
source(paste0(getwd(), "/classes/COMPARISONclass.R"))


