#MainScript.R
library(rrttReportBuilder)
library(dBtools)
library(roxygen2)
library(openxlsx)
library(data.table)
library(stringr)
library(R6)


dBtools::UpdateDescription()



DataLocation = "C:/week04 (2017-09-26) U1 Numbers Variables Operations"
ComparisonFileName = "comparison and topic alignment.xlsx"
ReportFileName = "scores.xlsx"
TMS = "ScantronAS"
SMS = "PowerSchool"
UploadFilenames = c("upload_percentages.csv", "upload_totalpoints.csv")
template = NULL


generateReport(DataLocation = DataLocation, TMS = TMS)
generateReport(DataLocation = DataLocation, TMS = TMS, template = template)

devtools::install_github()

report = currentReport
scoring = CurrentSpecial
result = report$getResults()[[4]]
result = currentResult
report$getResults()
ItemInfo = report$getItemInfo()

report$getItemSummary()
report$getTopicSummary()

nchar(x$ItemName)

str(report$getItemInfo())
str(ItemInfo)
report$getNarrative()

report$getTopicAlignments()

result1 = report$getResults()[[1]]
ItemResp = result1$getItemResponses()

i = 1

DescriptionLookup = read.csv("YearDescriptions.csv")
devtools::use_data(DescriptionLookup, overwrite = T)

thisSource = "C:/week04 (2017-09-26) U1 Numbers Variables Operations\\exports/Burgess_p8_itemresponses.csv"
report$getDataLocation()
thisSource = "Burgess_p8_itemresponses.csv"

sourceLocation = "C:/week04 (2017-09-26) U1 Numbers Variables Operations\\exports/Burgess_p8_itemresponses.csv"

itemNames = ItemInfo$ItemName

x = report$getResults()
str(x, max.level = 1)


result$getSectionName()
result$getSummary()
result$getItemResponses()
result$getItemResponseScores()