#MainScript.R




#load the exported linkit student item response files ####

#Get the basic overview of stuff
#dataLocation = choose.dir(default = "J:/tests/2016-2017/")  #select the folder
currentReport = REPORT$new()
currentReport$setDataLocation("J:\\tests\\2016-2017\\Humanities\\H2\\week23 (2017-02-17) Cuba Africa Middle East\\exports")
currentReport$setSources()
currentReport$setTestName()
currentReport$setItemInfo()
currentReport$setResults()  #Get the actual results
currentReport$setComparisonLocation("J:/tests/2016-2017/Humanities/H2/week23 (2017-02-17) Cuba Africa Middle East/comparison and topic alignment.xlsx")
currentReport$enhanceItemInfo()
#currentReport$setTopicAlignments() #run from inside enhanceItemInfo()
currentReport$addItemScores()
currentReport$addCorrelations()
currentReport$addResponseFrequencies()
currentReport$setUploadTab()

