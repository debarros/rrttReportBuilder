#' @title Generate Report
#' @description Generate the excel score report and narrative
#' @param DataLocation a folder path
#' @param ComparisonFileName The name of the file with the test set up and comparison info
#' @param ReportFileName Desired name of the output excel file
#' @param TMS Name of the testing management system.  Options currently limited to Linkit.
#' @param SMS Name of the student management system.  Options currently limited to PowerSchool.
#' @param useLocalValues logical: should the item values in the test setup file be used?
#' @param useLocalNames logical: should the item names in the test setup file be used?
#' @param UploadFilenames character of upload filenames.  The first is for percentage scores and the second is for total points.
#' @return an object of class REPORT
generateReport = function(DataLocation = choose.dir(default = "J:/tests/2016-2017/"),
                          ComparisonFileName = "comparison and topic alignment.xlsx",
                          ReportFileName = "scores.xlsx",
                          TMS = "LinkIt", SMS = "PowerSchool",
                          useLocalValues = F, useLocalNames = F, 
                          UploadFilenames = c("upload_percentages.csv", "upload_totalpoints.csv")){
  currentReport = REPORT$new() #initiate a new report
  currentReport$setUpLoadFiles(UploadFilenames) # set the names of the upload files
  currentReport$setDataLocation(DataLocation) # set the folder for the test
  currentReport$setSources() #identify all the csv's in the exports subfolder
  currentReport$setTestName()
  currentReport$setItemInfo()
  currentReport$setResults()  #Get the actual results
  currentReport$setComparisonFileName(ComparisonFileName)
  currentReport$setComparisonLocation(paste0(DataLocation, "\\", ComparisonFileName))
  # apply local item names and values, set topic alignments, set the type and number of options 
  currentReport$enhanceItemInfo(report = currentReport, useLocalNames = useLocalNames, useLocalValues = useLocalValues) 
  currentReport$addItemScores()
  currentReport$loadSpecialScoring()  # Load special scoring rules
  currentReport$applySpecialScoring()  # Apply special scoring rules
  currentReport$addCorrelations() # add item correlations to ItemInfo and to Correlations
  currentReport$addResponseFrequencies() # add ReponseSet and add response frequencies to ItemInfo
  currentReport$setUploadTab()
  currentReport$setSummary()
  currentReport$setTopicSummary()
  currentReport$setItemSummary() # easy, difficult, powerful distractors, wheat from chaff, check key, overthinking, highly related
  currentReport$setComparison()
  currentReport$setNarrative()
  currentReport$setTopicScores()
  currentReport$setHandouts()
  currentReport$exportNarrative() #save the html file with the narrative
  currentReport$exportReport(filename = ReportFileName) #save the xlsx score report file
  currentReport$exportUploads() #save the CSV's for uploading grades
  return(currentReport)
} # /function