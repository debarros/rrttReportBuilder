#REPORTclass.R

#An instance of this class holds everything necessary for a score report on a single test

# need a reason to use setPassingScore - where is this encoded?  How is the call triggered?

REPORT = R6Class(
  
  classname = "REPORT",
  
  private = list(
    
    # General parameters
    DistractorCutoffProportion = 0.25, #cutoff percentage for determining whether an item is a distrator
    OverThinkCutoff = 0.1, #cutoff correlation for determining whether an item is an overthinker
    EasyCutoff = 0.9, #cutoff score for determining whether an item is easy
    RelatedCutoffProportion = 0.2, #target proportion of items to count as highly related
    PassingScore = 0.7, #passing score for the test
    #     lower bound, upper bound, and target proportion for criterion for determining whether an item is difficult
    DifficultCutoffParams = list("Lower" = 0.4, "Upper" = 0.5, "Proportion" = 0.2), 
    #     rules for determining whether an item is Wheat From Chaff
    ChaffRules = data.frame(score = c(.3, .4), correlation = c(.3, .5)), 
    
    # General properties
    TMS = NULL, #The Testing Management System
    DataLocation = NULL, #address of the folder for the test
    ComparisonLocation = NULL, #address and filename of the comparison and topic alignment
    SourceFileNames = NULL, # character vectior with the filenames of the csv's
    Sources = NULL, #character vector with the locations of the csv's
    MissingSections = NULL, # character vector with the names of sections for which there is no data
    TestName = NULL, #atomic character with the name of the test
    ComparisonFileName = "comparison and topic alignment.xlsx", #test setup info filename (no file path)
    UpLdFileNames = NULL, # character with the names of the percentage and total points upload files
    HasTopics = NULL, # logical that indicates whether the test has topic alignments
    
    # Item stuff
    ItemInfo = NULL, #data.frame with info about the items, will be used to build the breakdown tab
    ResponseSet = NULL, #character vector hold the names of the different response frequencies columns
    Correlations = NULL, #correlations column from the ItemInfo
    ItemScores = NULL, #AverageScore column from the ItemInfo
    ItemResponseScores = NULL, #data.table with the score for every student on every item
    DropScores = NULL, #data.table with the score for each student after dropping each item
    
    # Report components
    UploadTab = NULL, #data.frame that holds the stuff that goes in the upload tab and upload_percentages export
    UploadTotalPoints = NULL, # data that holds the stuff that goes in the upload_totalpoints export
    Results = NULL, #list of objects of class RESULT
    TopicAlignments = NULL, #data.frame holding the topic alignments
    TopicSummary = NULL, #data.frame with stuff that would go on the Topic Chart Calculation tab
    TopicScores = NULL, #data.frame with one row per student and one column per topic
    Summary = NULL, #list with the overall stats from the Scores tab
    ItemSummary = NULL, #data.frame with info in the table at top of Item Summary tab. 1row/item, 1col/type
    Narrative = NULL, #Character vector. Text in cell A11 of Item Summary tab, but with markdown formatting
    Comparison = NULL, #a list of objects of class COMPARISON
    Handouts = NULL, #data.frame containing the information necessary to build the Handouts tab simply
    
    # Scoring info
    SpecialScoring = NULL, # list of special scoring rules
    HasSpecialScoring = NULL, # logical that indicates whether the test has special scoring
    HasStudentScoring = NULL, # logical that indicates whether the special scoring is different for different students
    SpecialScoringTable = NULL # data.frame showing what special scoring rule to use for each student
    
  ), # /private
  
  public = list(
    
    # Initialize method - new()
    initialize = function(TMS = "LinkIt"){private$TMS = TMS}, #default the Testing Management System to LinkIt
    
    # Methods to set members
    setComparison = function(report = self){ setComparison.REPORT(report) }, 
    setComparisonFileName = function(x){private$ComparisonFileName = x},
    setComparisonLocation = function(x){private$ComparisonLocation = x},
    setDataLocation = function(x){private$DataLocation = x},
    setItemInfo = function(report = self){ setItemInfo.REPORT(report) }, 
    setItemSummary = function(report = self){ setItemSummary.REPORT(report) }, 
    setHandouts = function(report = self){ setHandouts.REPORT(report) }, 
    setNarrative = function(report = self){ setNarrative.REPORT(report) },
    setPassingScore = function(x){private$PassingScore = x},
    setResults = function(report = self){ setResults.REPORT(report) }, 
    setSources = function(report = self){ setSources.REPORT(report) }, 
    setSummary = function(report = self){ setSummary.REPORT(report) }, 
    setTestName = function(report = self){ setTestName.REPORT(report) }, 
    setTopicAlignments = function(d2, report = self){ setTopicAlignments.REPORT(d2, report) }, 
    setUploadTab = function(report = self){ setUploadTab.REPORT(report) },
    setTopicSummary = function(report = self){ setTopicSummary.REPORT(report) }, 
    setTopicScores = function(report = self){ setTopicScores.REPORT(report) }, 
    setUpLoadFiles = function(filenames = c("upload_percentages.csv", "upload_totalpoints.csv")){
      private$UpLdFileNames = filenames},
    
    # Methods to return members
    getComparisonLocation = function(){return(private$ComparisonLocation)},
    getComparison = function(){return(private$Comparison)},
    getCorrelations = function(){return(private$Correlations)},
    getDataLocation = function(){return(private$DataLocation)},
    getHandouts = function(){return(private$Handouts)},
    getItemInfo = function(){return(private$ItemInfo)},
    getItemScores = function(){return(private$ItemScores)},
    getItemSummary = function(){return(private$ItemSummary)},
    getMissingSections = function(){return(private$MissingSections)},
    getNarrative = function(){return(private$Narrative)},
    getPassingScore = function(){return(private$PassingScore)},
    getResponses = function(report = self){ return(getResponses.REPORT(report)) }, 
    getResponseSet = function(){return(private$ResponseSet)},
    getResults = function(){return(private$Results)},
    getSources = function(){return(private$Sources)},
    getSpecialScoring = function(){private$SpecialScoring},
    getSpecialScoringTable = function(){return(private$SpecialScoringTable)},
    getSummary = function(){return(private$Summary)},
    getTestName = function(){return(private$TestName)},
    getTopicSummary = function(){return(private$TopicSummary)},
    getUploadTab = function(){return(private$UploadTab)},
    getUploadTotalPoints = function(){return(private$UploadTotalPoints)},
    getTMS = function(){return(private$TMS)},
    getTopicAlignments = function(report = self){ return(getTopicAlignments.REPORT(report)) }, 
    getTopicScores = function(){return(private$TopicScores)},
    getUpLoadFiles = function(){return(private$UpLdFileNames)},
    
    # Methods to quick set members
    
    # Export Methods
    exportNarrative = function(report = self){ exportNarrative.REPORT(report) }, 
    exportReport = function(report = self, filename = "scores.xlsx"){ 
      exportReport.REPORT(report, filename) }, 
    exportUpdate = function(uploadFilePath, report = self){ # called from exportReport
      exportUpdate.REPORT(uploadFilePath, report) },
    exportUploads = function(report = self){ exportUploads.REPORT(report) }, 
    
    # Other methods
    addCorrelations = function(report = self){ addCorrelations.REPORT(report) }, 
    addItemScores = function(report = self){ addItemScores.REPORT(report) },
    addResponseFrequencies = function(report = self){ addResponseFrequencies.REPORT(report) },
    applySpecialScoring = function(report = self){ applySpecialScoring.REPORT(report) },
    badMessage = function(method, report = self){ return(badMessage.REPORT(method, report)) },
    checkSpecScor = function(){return(private$HasSpecialScoring)},
    checkStudScor = function(){return(private$HasStudentScoring)},
    checkTopics = function(){return(private$HasTopics)},
    enhanceItemInfo = function(report = self, useLocalNames = F, useLocalValues = F){
      enhanceItemInfo.REPORT(report, useLocalNames, useLocalValues)}, 
    loadSpecialScoring = function(report = self){ loadSpecialScoring.REPORT(report) }, 
    updateIRandIRS = function(report = self){updateIRandIRS.REPORT(self)},
    updateItemInfo = function(x){private$ItemInfo = x}
    
  ) # /public
  
) # /REPORT R6 class
