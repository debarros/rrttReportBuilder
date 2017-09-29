#MainScript.R
library(rrttReportBuilder)
library(dBtools)
library(roxygen2)
library(openxlsx)


dBtools::UpdateDescription()

DataLocation = "C:/week04 (2017-09-26) U1 Numbers Variables Operations"
ComparisonFileName = "comparison and topic alignment.xlsx"
ReportFileName = "scores.xlsx"
TMS = "ScantronAS"
SMS = "PowerSchool"
UploadFilenames = c("upload_percentages.csv", "upload_totalpoints.csv")

generateReport(DataLocation = DataLocation)

generateReport(DataLocation = "J:/tests/2017-2018/Math/Alg 1/week04 (2017-09-26) U1 Numbers Variables Operations")

report = currentReport
report$getItemInfo()
str(report$getItemInfo())
str(ItemInfo)

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