#MainScript.R
library(rrttReportBuilder)
library(dBtools)
library(roxygen2)
library(openxlsx)
library(data.table)
library(stringr)
library(R6)
library(devtools)
library(knitr)
library(markdown)

dBtools::UpdateDescription()


tests = read.csv()


# DataLocation = "//stuthin2/Data/tests/2018-2019/Social Studies/Multi/week30 (2019-04-09) Civil Engagement (US and AP US)/modified data"
DataLocation = CopyClipboard()
ComparisonFileName = "comparison and topic alignment.xlsx"
ReportFileName = "scores.xlsx"
TMS = "ScantronAS"
SMS = "PowerSchool"
UploadFilenames = c("upload_percentages.csv", "upload_totalpoints.csv")
template = NULL
messageLevel = 4
HaltOnMultiResponse = T

generateReport(DataLocation = DataLocation, ComparisonFileName = ComparisonFileName, TMS = TMS, 
               HaltOnMultiResponse = HaltOnMultiResponse, messageLevel = messageLevel)

generateReport(DataLocation = DataLocation, TMS = TMS, template = template)

devtools::install_github()

report = currentReport
scoring = CurrentSpecial
i = 1
res = 1
thisResult = 1
stu = 1
subst = 1
subst = 1 + subst
curSpecScor
result = report$getResults()[[1]]
result = currentResult
result = thisResult
report$getResults()
ItemInfo = report$getItemInfo()
str(ItemInfo)

report$getSummary()
report$getItemSummary()
report$getTopicSummary()

report$getUpLoadFiles()
report$getDataLocation()
report$getSources()
report$getSourceFileNames()


result$getItemResponses()
result$getItemResponseScores()

curSpecScor$.__enclos_env__$private$SubsetSetup

CurrentSpecial$.__enclos_env__$private$SubsetAlign



install_github(repo = "awalker89/openxlsx")

(.packages())

result$.__enclos_env__$private$ItemResponses




UpdateDescription()
