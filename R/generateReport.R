#' @title Generate Report
#' @description Generate the excel score report and narrative
#' @param DataLocation a folder path
#' @param ComparisonFileName The name of the file with the test set up and comparison info
#' @param ReportFileName Desired name of the output excel file
#' @return an object of clas REPORT
generateReport = function(DataLocation = choose.dir(default = "J:/tests/2016-2017/"),
                          ComparisonFileName = "comparison and topic alignment.xlsx",
                          ReportFileName = "scores.xlsx"){
  currentReport = REPORT$new() #initiate a new report
  currentReport$setDataLocation(DataLocation)
  currentReport$setSources() #identify all the csv's in that folder
  currentReport$setTestName()
  currentReport$setItemInfo()
  currentReport$setResults()  #Get the actual results
  currentReport$setComparisonFileName
  currentReport$setComparisonLocation(paste0(DataLocation, "\\", ComparisonFileName))
  currentReport$enhanceItemInfo()
  currentReport$addItemScores()
  currentReport$addCorrelations()
  currentReport$addResponseFrequencies()
  currentReport$setUploadTab()
  currentReport$setSummary()
  currentReport$setTopicSummary()
  currentReport$setItemSummary()
  currentReport$setComparison()
  currentReport$setNarrative()
  currentReport$setTopicScores()
  currentReport$setHandouts()
  currentReport$exportNarrative()
  currentReport$exportReport(filename = ReportFileName)
  return(currentReport)
}