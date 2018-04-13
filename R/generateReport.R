#' @title Generate Report
#' @description Generate the excel score report and narrative
#' @param DataLocation a folder path
#' @param ComparisonFileName The name of the file with the test set up and comparison info
#' @param ReportFileName Desired name of the output excel file
#' @param TMS Name of the testing management system.  Options currently limited to Linkit and ScantronAS.
#' @param SMS Name of the student management system.  Options currently limited to PowerSchool.
#' @param UploadFilenames character of upload filenames.  The first is for percentage scores and the second is for total points.
#' @param template alternate template file
#' @param messageLevel integer - level of messages to print
#' @return an object of class REPORT
generateReport = function(DataLocation = choose.dir(default = "J:/tests/2016-2017/"),
                          ComparisonFileName = "comparison and topic alignment.xlsx",
                          ReportFileName = "scores.xlsx",
                          TMS = "LinkIt", SMS = "PowerSchool",
                          UploadFilenames = c("upload_percentages.csv", "upload_totalpoints.csv"),
                          template = NULL, 
                          messageLevel = 0){
  
  currentReport = REPORT$new(TMS = TMS)         # initiate a new report
  currentReport$setUpLoadFiles(UploadFilenames) # set the names of the upload files
  currentReport$setDataLocation(DataLocation)   # set the folder for the test
  currentReport$setSources(messageLevel = messageLevel - 1) # identify all the csv's in the exports subfolder
  currentReport$setTestName()
  currentReport$setComparisonFileName(ComparisonFileName)
  currentReport$setComparisonLocation(paste0(DataLocation, "\\", ComparisonFileName))
  currentReport$setItemInfo()
  currentReport$setResults()             # Get the actual results
  currentReport$addItemScores(messageLevel = messageLevel - 1)
  currentReport$loadSpecialScoring()     # Load special scoring rules
  currentReport$applySpecialScoring()    # Apply special scoring rules
  currentReport$addCorrelations()        # add item correlations to ItemInfo and to Correlations
  currentReport$addResponseFrequencies() # add ReponseSet and add response frequencies to ItemInfo
  currentReport$setUploadTab()
  currentReport$setSummary()
  currentReport$setTopicSummary()
  currentReport$setItemSummary()         # easy, difficult, powerful distractors, wheat from chaff, check key, overthinking, highly related
  currentReport$setComparison(messageLevel = messageLevel - 1)
  currentReport$setNarrative(messageLevel = messageLevel - 1)
  currentReport$setTopicScores()
  currentReport$setHandouts()
  currentReport$exportNarrative()                                            # save the html file with the narrative
  currentReport$exportReport(filename = ReportFileName, template = template) # save the xlsx score report file
  currentReport$exportUploads()                                              # save the CSV's for uploading grades
  
  return(currentReport)
  
} # /function
