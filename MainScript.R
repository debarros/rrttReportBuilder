#MainScript.R
library(rrttReportBuilder)
library(dBtools)
library(roxygen2)

currentReport = generateReport(DataLocation = DataLocation)


DataLocation = "C:/Users/pauldeba/Downloads/week23 (2017-02-15) U3 Periodic Table"

library(openxlsx)
?loadWorkbook2


UpdateDescription()


currentReport$getResponseSet()

?VectorSentence

