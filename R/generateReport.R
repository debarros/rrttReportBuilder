#' @title Generate Report
#' @description Generate the excel score report and narrative
#' @param DataLocation a folder path
#' @param ComparisonFileName The name of the file with the test set up and comparison info
#' @param ReportFileName Desired name of the output excel file
#' @param TMS Name of the testing management system.  Options currently limited to Linkit.
#' @param SMS Name of the student management system.  Options currently limited to PowerSchool.
#' @return an object of clas REPORT
generateReport = function(DataLocation = choose.dir(default = "J:/tests/2016-2017/"),
                          ComparisonFileName = "comparison and topic alignment.xlsx",
                          ReportFileName = "scores.xlsx",
                          TMS = "LinkIt",
                          SMS = "PowerSchool"){
  currentReport = REPORT$new() #initiate a new report
  currentReport$setDataLocation(DataLocation)
  currentReport$setSources() #identify all the csv's in that folder
  currentReport$setTestName()
  currentReport$setItemInfo()
  currentReport$setResults()  #Get the actual results
  currentReport$setComparisonFileName(ComparisonFileName)
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
  currentReport$exportNarrative() #save the html file with the narrative
  currentReport$exportReport(filename = ReportFileName) #save the xlsx score report file
  currentReport$exportUploads() #save the csv for uploading grades
  return(currentReport)
}