#REPORTclass.R

#An instance of this class holds everything necessary for a score report on a single test

#How to deal with questions where the item name is the setup file differs from the item name in the export?
#This becomes an issue when running the enhanceItemInfo method
#At that point, the ItemInfo member of the report object and the ItemResponses member of each RESULT object 
# already have the item names from the export file
#There should probably be a an object that shows the correspondence between the two sets of item names

# need a reason to use setPassingScore - where is this encoded?  How is the call triggered?

REPORT = R6Class(
  
  classname = "REPORT",
  
  private = list(
    TMS = NULL, #The Testing Management System
    DataLocation = NULL, #address of the folder for the test
    ComparisonLocation = NULL, #address and filename of the comparison and topic alignment
    Sources = NULL, #character vector with the locations of the csv's
    TestName = NULL, #atomic character with the name of the test
    ItemInfo = NULL, #data.frame with info about the items, will be used to build the breakdown tab
    UploadTab = NULL, #data.frame that holds the stuff that goes in the upload tab
    Results = NULL, #list of objects of class RESULT
    TopicAlignments = NULL, #data.frame holding the topic alignments
    TopicSummary = NULL, #data.frame with stuff that would go on the Topic Chart Calculation tab
    TopicScores = NULL, #data.frame with one row per student and one column per topic
    Summary = NULL, #list with the overall stats from the Scores tab
    ItemSummary = NULL, #data.frame with info in the table at top of Item Summary tab. 1row/item, 1col/type
    Narrative = NULL, #Character vector. Text in cell A11 of Item Summary tab, but with markdown formatting
    Comparison = NULL, #a list of objects of class COMPARISON
    Handouts = NULL, #data.frame containing the information necessary to build the Handouts tab simply
    DistractorCutoffProportion = 0.25, #cutoff percentage for determining whether an item is a distrator
    OverThinkCutoff = 0.1, #cutoff correlation for determining whether an item is an overthinker
    EasyCutoff = 0.9, #cutoff score for determining whether an item is easy
    #lower bound, upper bound, and target proportion for criterion for determining whether an item is difficult
    DifficultCutoffParams = list("Lower" = 0.4, "Upper" = 0.5, "Proportion" = 0.2), 
    #rules for determining whether an item is Wheat From Chaff
    ChaffRules = data.frame(score = c(.3, .4), correlation = c(.3, .5)), 
    RelatedCutoffProportion = 0.2, #target proportion of items to count as highly related
    ResponseSet = NULL, #character vector hold the names of the different response frequencies columns
    Correlations = NULL, #correlations column from the ItemInfo
    ItemScores = NULL, #AverageScore column from the ItemInfo
    ItemResponseScores = NULL, #data.table with the score for every student on every item
    DropScores = NULL, #data.table with the score for each student after dropping each item
    PassingScore = 0.7, #passing score for the test
    ComparisonFileName = "comparison and topic alignment.xlsx", #test setup info filename (no file path)
    SpecialScoring = NULL, # list of special scoring rules
    HasSpecialScoring = NULL, # logical that indicates whether the test has special scoring
    HasStudentScoring = NULL, # logical that indicates whether the special scoring is different for different students
    SpecialScoringTable = NULL, # data.frame showing what special scoring rule to use for each student
    HasTopics = NULL # logical that indicates whether the test has topic alignments
  ), # /private
  
  public = list(
    initialize = function(TMS = "LinkIt"){private$TMS = TMS}, #default the Testing Management System to LinkIt
    setDataLocation = function(x){private$DataLocation = x},
    setComparisonFileName = function(x){private$ComparisonFileName = x},
    setComparisonLocation = function(x){private$ComparisonLocation = x},
    setSources = function(report = self){ setSources.REPORT(report) }, 
    setTestName = function(report = self){ setTestName.REPORT(report) }, 
    setItemInfo = function(report = self){ setItemInfo.REPORT(report) }, 
    enhanceItemInfo = function(report = self, useLocalNames = F){ enhanceItemInfo.REPORT(report, useLocalNames) }, 
    setTopicAlignments = function(d2, report = self){ setTopicAlignments.REPORT(d2, report) }, 
    addItemScores = function(report = self){ addItemScores.REPORT(report) },
    setUploadTab = function(report = self){ setUploadTab.REPORT(report) },
    setResults = function(report = self){ setResults.REPORT(report) }, 
    setPassingScore = function(x){private$PassingScore = x},
    addCorrelations = function(report = self){ addCorrelations.REPORT(report) }, 
    addResponseFrequencies = function(report = self){ addResponseFrequencies.REPORT(report) },
    getResponses = function(report = self){ return(getResponses.REPORT(report)) }, 
    badMessage = function(method, report = self){ return(badMessage.REPORT(method, report)) },
    getSources = function(){return(private$Sources)},
    getTestName = function(){return(private$TestName)},
    getItemInfo = function(){return(private$ItemInfo)},
    getUploadTab = function(){return(private$UploadTab)},
    getDataLocation = function(){return(private$DataLocation)},
    getComparisonLocation = function(){return(private$ComparisonLocation)},
    getResults = function(){return(private$Results)},
    getTopicSummary = function(){return(private$TopicSummary)},
    getSummary = function(){return(private$Summary)},
    getItemSummary = function(){return(private$ItemSummary)},
    getItemScores = function(){return(private$ItemScores)},
    getNarrative = function(){return(private$Narrative)},
    getComparison = function(){return(private$Comparison)},
    getHandouts = function(){return(private$Handouts)},
    getCorrelations = function(){return(private$Correlations)},
    getResponseSet = function(){return(private$ResponseSet)},
    getPassingScore = function(){return(private$PassingScore)},
    getTopicAlignments = function(report = self){ return(getTopicAlignments.REPORT(report)) }, 
    setSummary = function(report = self){ setSummary.REPORT(report) }, 
    setItemSummary = function(report = self){ setItemSummary.REPORT(report) }, 
    setTopicSummary = function(report = self){ setTopicSummary.REPORT(report) }, 
    setComparison = function(report = self){ setComparison.REPORT(report) }, 
    setNarrative = function(report = self){ setNarrative.REPORT(report) },
    exportNarrative = function(report = self){ exportNarrative.REPORT(report) }, 
    exportReport = function(report = self, filename = "scores.xlsx"){ exportReport.REPORT(report, filename) }, 
    setTopicScores = function(report = self){ setTopicScores.REPORT(report) }, 
    getTopicScores = function(){return(private$TopicScores)},
    setHandouts = function(report = self){ setHandouts.REPORT(report) }, 
    exportUploads = function(report = self){ exportUploads.REPORT(report) }, 
    applySpecialScoring = function(report = self){ applySpecialScoring.REPORT(report) },
    loadSpecialScoring = function(report = self){ loadSpecialScoring.REPORT(report) }, 
    exportUpdate = function(report = self, uploadFilePath){ exportUpdate.REPORT(report, uploadFilePath) }

  ) # /public
) # /REPORT R6 class
