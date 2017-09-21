#MainScript.R
library(rrttReportBuilder)
library(dBtools)
library(roxygen2)
library(openxlsx)


dBtools::UpdateDescription()



DataLocation = "J:/tests/2017-2018/ELA/AP Language/week01 (2017-09-06) I Wish My Teacher Knew Essay"
generateReport(DataLocation = DataLocation)

ComparisonFileName = "comparison and topic alignment.xlsx"
ReportFileName = "scores.xlsx"
TMS = "LinkIt"
SMS = "PowerSchool"
useLocalValues = T
useLocalNames = F

currentReport = generateReport(DataLocation = DataLocation)

report = currentReport

UpdateDescription()

report$checkTopics()
report$getTopicAlignments()

str(report$getItemInfo())

result1 = report$.__enclos_env__$private$Results[[1]]
sum(result1$getIR()[1,as.character(1:15)], na.rm = T)
result1$getIRS()
report$.__enclos_env__$private$ResponseSet
report$getItemInfo()
report$.__enclos_env__$private$Correlations
report$getUploadTab()
report$getResponses()
report$getItemScores()
report$.__enclos_env__$private$ItemResponseScores
report$getResponseSet()

155/165


report$getDataLocation()





DescriptionLookup = read.csv("YearDescriptions.csv")
devtools::use_data(DescriptionLookup, overwrite = T)
