#MainScript.R
library(rrttReportBuilder)
library(dBtools)
library(roxygen2)
library(openxlsx)
library(data.table)
library(stringr)
library(R6)


dBtools::UpdateDescription()


tests = read.csv()

DataLocation = "//stuthin2/Data/tests/2017-2018/ELA/Lit 12/week18 (2018-01-11) Midterm"
ComparisonFileName = "comparison and topic alignment.xlsx"
ReportFileName = "scores.xlsx"
TMS = "ScantronAS"
SMS = "PowerSchool"
UploadFilenames = c("upload_percentages.csv", "upload_totalpoints.csv")
template = NULL
messageLevel = 2

generateReport(DataLocation = DataLocation, TMS = TMS, messageLevel = messageLevel)
generateReport(DataLocation = DataLocation, TMS = TMS, template = template)

devtools::install_github()

report = currentReport
scoring = CurrentSpecial
curSpecScor
result = report$getResults()[[4]]
result = currentResult
report$getResults()
ItemInfo = report$getItemInfo()

report$getItemSummary()
report$getTopicSummary()

report$getUpLoadFiles()
report$getDataLocation()
report$getSources()
report$getSourceFileNames()
