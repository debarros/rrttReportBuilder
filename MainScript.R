#MainScript.R

source("functions.R")


#load the exported linkit student item response files ####

#Get the basic overview of stuff
#dataLocation = choose.dir(default = "J:/tests/2016-2017/")  #select the folder
currentReport = REPORT$new() #initiate a new report
currentReport$setDataLocation("J:\\tests\\2016-2017\\Humanities\\H2\\week23 (2017-02-17) Cuba Africa Middle East\\exports")
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
currentReport$getResponses2()  #just to check
currentReport$setComparisonLocation("J:/tests/2016-2017/Humanities/H2/week23 (2017-02-17) Cuba Africa Middle East/comparison and topic alignment.xlsx")
currentReport$getComparisonLocation()  #just to check
currentReport$enhanceItemInfo()
#currentReport$setTopicAlignments() #run from inside enhanceItemInfo()
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



