#MainScript.R


#dataLocation = choose.dir(default = "J:/tests/2016-2017/")  #select the folder
currentReport = REPORT$new() #initiate a new report
#currentReport$setDataLocation(dataLocation)
currentReport$setDataLocation("J:\\tests\\2016-2017\\Humanities\\H2\\week23 (2017-02-17) Cuba Africa Middle East")
currentReport$getDataLocation() #just to check
currentReport$setSources() #identify all the csv's in that folder
currentReport$getSources() #just to check
currentReport$setTestName()
currentReport$getTestName() #just to check
currentReport$setItemInfo()
currentReport$getItemInfo() #just to check
currentReport$setResults()  #Get the actual results
currentReport$getResults()  #just to check
currentReport$getResponses()  #just to check
currentReport$setComparisonLocation(paste0(currentReport$.__enclos_env__$private$DataLocation, "\\", currentReport$.__enclos_env__$private$ComparisonFileName))
currentReport$getComparisonLocation()  #just to check
currentReport$enhanceItemInfo()
#currentReport$setTopicAlignments() #gets run from inside enhanceItemInfo()
currentReport$getItemInfo() #just to check
currentReport$addItemScores()
currentReport$getItemInfo() #just to check
currentReport$getItemScores() #just to check
currentReport$addCorrelations()
currentReport$getCorrelations() #just to check
currentReport$getItemInfo() #just to check
currentReport$addResponseFrequencies()
currentReport$getResponseSet() #just to check
currentReport$getItemInfo() #just to check
currentReport$setUploadTab()
currentReport$getUploadTab() #just to check
currentReport$setSummary()
currentReport$getSummary() #just to check
currentReport$setTopicSummary()
currentReport$getTopicSummary() #just to check
currentReport$setItemSummary()
currentReport$getItemSummary() #just to check
currentReport$setComparison()
currentReport$getComparison() # just to check
currentReport$setNarrative()
currentReport$getNarrative() # just to check


currentReport$setTopicScores()
currentReport$getTopicScores() # just to check
currentReport$setHandouts()
currentReport$getHandouts() # just to check
View(currentReport$getHandouts())
currentReport$exportNarrative()



