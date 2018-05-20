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
  currentReport$setTestName(messageLevel = messageLevel - 1)
  currentReport$setComparisonFileName(ComparisonFileName)
  currentReport$setComparisonLocation(paste0(DataLocation, "\\", ComparisonFileName))
  currentReport$setItemInfo(messageLevel = messageLevel - 1)
  currentReport$setResults(messageLevel = messageLevel - 1)             # Get the actual results
  currentReport$addItemScores(messageLevel = messageLevel - 1)
  currentReport$loadSpecialScoring(messageLevel = messageLevel - 1)     # Load special scoring rules
  currentReport$applySpecialScoring(messageLevel = messageLevel - 1)    # Apply special scoring rules
  currentReport$addCorrelations(messageLevel = messageLevel - 1)        # add item correlations to ItemInfo and to Correlations
  currentReport$addResponseFrequencies(messageLevel = messageLevel - 1) # add ReponseSet and add response frequencies to ItemInfo
  currentReport$setUploadTab(messageLevel = messageLevel - 1)
  currentReport$setSummary(messageLevel = messageLevel - 1)
  currentReport$setTopicSummary(messageLevel = messageLevel - 1)
  currentReport$setItemSummary(messageLevel = messageLevel - 1) # easy hard distractors wheat/chaff check/key overthinking highly/related
  currentReport$setComparison(messageLevel = messageLevel - 1)
  currentReport$setNarrative(messageLevel = messageLevel - 1)
  currentReport$setTopicScores(messageLevel = messageLevel - 1)
  currentReport$setHandouts(messageLevel = messageLevel - 1)
  currentReport$exportNarrative(messageLevel = messageLevel - 1)  # save the html file with the narrative
  currentReport$exportReport(filename = ReportFileName, template = template, messageLevel = messageLevel - 1) # save the xlsx report file
  currentReport$exportUploads(messageLevel = messageLevel - 1)  # save the CSV's for uploading grades
  
  return(currentReport)
  
} # /function
