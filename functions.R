#functions.R

#Libraries ####
library(openxlsx)
library(magrittr)
library(data.table)
library(R6)


#functions
source("loadWorkbook.R")
source("t.test2.R")

#Classes ####
source(paste0(getwd(), "/classes/REPORTclass.R"))
source(paste0(getwd(), "/classes/RESULTclass.R"))
source(paste0(getwd(), "/classes/COMPARISONclass.R"))


#data ####
DescriptionLookup = read.csv("YearDescriptions.csv", stringsAsFactors = F)
