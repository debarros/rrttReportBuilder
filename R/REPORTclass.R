#REPORTclass.R

#An instance of this class holds everything necessary for a score report on a single test

# need a reason to use setPassingScore - where is this encoded?  How is the call triggered?

REPORT = R6Class(
  
  classname = "REPORT",
  
  private = list(
    
    # General parameters
    DistractorCutoffProportion = 0.25,                # cutoff percentage for determining whether an item is a distrator
    EasyCutoff = 0.9,                                 # cutoff score for determining whether an item is easy
    ItemTypeCategories = c("MC","ER","WH","FI","FL"), # all the different item types that are acceptable
    OverThinkCutoff = 0.1,                            # cutoff correlation for determining whether an item is an overthinker
    RelatedCutoffProportion = 0.2,                    # target proportion of items to count as highly related
    PassingScore = 0.7,                               # passing score for the test
    #     rules for determining whether an item is Wheat From Chaff
    ChaffRules = data.frame(score = c(.3, .4), correlation = c(.3, .5)), 
    #     lower bound, upper bound, and target proportion for criterion for determining whether an item is difficult
    DifficultCutoffParams = list("Lower" = 0.4, "Upper" = 0.5, "Proportion" = 0.2), 
    
    # General properties
    ComparisonFileName = "comparison and topic alignment.xlsx", #test setup info filename (no file path)
    ComparisonLocation = NULL, # address and filename of the comparison and topic alignment
    DataLocation = NULL,       # address of the folder for the test
    HasTopics = NULL,          # logical that indicates whether the test has topic alignments
    MissingSections = NULL,    # character vector with the names of sections for which there is no data
    SourceFileNames = NULL,    # character vectior with the filenames of the csv's
    Sources = NULL,            # character vector with the locations of the csv's
    TestName = NULL,           # character(1) with the name of the test
    TMS = NULL,                # character(1) The Testing Management System
    UpLdFileNames = NULL,      # character(2) with the names of the percentage and total points upload files
    
    # Item stuff
    Correlations = NULL,        # correlations column from the ItemInfo
    DropScores = NULL,          # data.table with the score for each student after dropping each item
    ItemInfo = NULL,            # data.frame with info about the items, will be used to build the breakdown tab
    ItemResponseOptions = NULL, # list with character vector for each item containing its potential or observed responses
    ItemResponseScores = NULL,  # data.table with the score for every student on every item
    ItemScores = NULL,          # AverageScore column from the ItemInfo
    ResponseSet = NULL,         # character vector hold the names of the different response frequencies columns
    
    # Report components
    Comparison = NULL,        # a list of objects of class COMPARISON
    Handouts = NULL,          # data.frame containing the information necessary to build the Handouts tab simply
    ItemSummary = NULL,       # data.frame with info in the table at top of Item Summary tab. 1row/item, 1col/type
    Narrative = NULL,         # Character vector. Text in cell A11 of Item Summary tab, but with markdown formatting
    Results = NULL,           # list of objects of class RESULT
    Summary = NULL,           # list with the overall stats from the Scores tab
    TopicAlignments = NULL,   # data.frame holding the topic alignments
    TopicScores = NULL,       # data.frame with one row per student and one column per topic
    TopicSummary = NULL,      # data.frame with stuff that would go on the Topic Chart Calculation tab
    UploadTab = NULL,         # data.frame that holds the stuff that goes in the upload tab and upload_percentages export
    UploadTotalPoints = NULL, # data that holds the stuff that goes in the upload_totalpoints export
    
    # Scoring info
    HasSpecialScoring = NULL,  # logical that indicates whether the test has special scoring
    HasStudentScoring = NULL,  # logical that indicates whether the special scoring is different for different students
    SpecialScoring = NULL,     # list of special scoring rules
    SpecialScoringTable = NULL # data.frame showing what special scoring rule to use for each student
    
  ), # /private
  
  public = list(
    
    # Initialize method - new()
    initialize = function(TMS = "LinkIt"){private$TMS = TMS}, #default the Testing Management System to LinkIt
    
    # Methods to set members
    setComparison =         function(report = self){ setComparison.REPORT(report) }, 
    setComparisonFileName = function(x){private$ComparisonFileName = x},
    setComparisonLocation = function(x){private$ComparisonLocation = x},
    setDataLocation =       function(x){private$DataLocation = x},
    setItemInfo =           function(report = self){ setItemInfo.REPORT(report) }, 
    setItemSummary =        function(report = self){ setItemSummary.REPORT(report) }, 
    setHandouts =           function(report = self){ setHandouts.REPORT(report) }, 
    setNarrative =          function(report = self){ setNarrative.REPORT(report) },
    setPassingScore =       function(x){private$PassingScore = x},
    setResults =            function(report = self){ setResults.REPORT(report) }, 
    setSources =            function(report = self){ setSources.REPORT(report) }, 
    setSummary =            function(report = self){ setSummary.REPORT(report) }, 
    setTestName =           function(report = self){ setTestName.REPORT(report) }, 
    setTopicAlignments =    function(d2, report = self){ setTopicAlignments.REPORT(d2, report) }, 
    setUploadTab =          function(report = self){ setUploadTab.REPORT(report) },
    setTopicSummary =       function(report = self){ setTopicSummary.REPORT(report) }, 
    setTopicScores =        function(report = self){ setTopicScores.REPORT(report) }, 
    setUpLoadFiles =        function(x){private$UpLdFileNames = x},
    
    # Methods to return members
    getChaffRules =                 function(){return(private$ChaffRules)},
    getComparison =                 function(){return(private$Comparison)},
    getComparisonLocation =         function(){return(private$ComparisonLocation)},
    getCorrelations =               function(){return(private$Correlations)},
    getDataLocation =               function(){return(private$DataLocation)},
    getDifficultCutoffParams =      function(){return(private$DifficultCutoffParams)},
    getDistractorCutoffProportion = function(){return(private$DistractorCutoffProportion)},
    getEasyCutoff =                 function(){return(private$EasyCutoff)},
    getHandouts =                   function(){return(private$Handouts)},
    getItemInfo =                   function(){return(private$ItemInfo)},
    getItemResponseScores =         function(){return(private$ItemResponseScores)},
    getItemScores =                 function(){return(private$ItemScores)},
    getItemSummary =                function(){return(private$ItemSummary)},
    getItemTypeCategories =         function(){return(private$ItemTypeCategories)},
    getMissingSections =            function(){return(private$MissingSections)},
    getNarrative =                  function(){return(private$Narrative)},
    getOverThinkCutoff =            function(){return(private$OverThinkCutoff)},
    getPassingScore =               function(){return(private$PassingScore)},
    getRelatedCutoffProportion =    function(){return(private$RelatedCutoffProportion)},
    getResponses =                  function(report = self){ return(getResponses.REPORT(report)) }, 
    getResponseSet =                function(){return(private$ResponseSet)},
    getResults =                    function(){return(private$Results)},
    getSourceFileNames =            function(){return(private$SourceFileNames)},
    getSources =                    function(){return(private$Sources)},
    getSpecialScoring =             function(){return(private$SpecialScoring)},
    getSpecialScoringTable =        function(){return(private$SpecialScoringTable)},
    getSummary =                    function(){return(private$Summary)},
    getTestName =                   function(){return(private$TestName)},
    getTMS =                        function(){return(private$TMS)},
    getTopicAlignments =            function(report = self){ return(getTopicAlignments.REPORT(report)) }, 
    getTopicAlignmentsQuick =       function(){return(private$TopicAlignments)},
    getTopicScores =                function(){return(private$TopicScores)},
    getTopicSummary =               function(){return(private$TopicSummary)},
    getUpLoadFiles =                function(){return(private$UpLdFileNames)},
    getUploadTab =                  function(){return(private$UploadTab)},
    getUploadTotalPoints =          function(){return(private$UploadTotalPoints)},
    
    # Methods to quick set members
    setComparisonQuick =          function(x){private$Comparison = x},
    setCorrelationsQuick =        function(x){private$Correlations = x},
    setDropScoresQuick =          function(x){private$DropScores = x},
    setItemInfoQuick =            function(x){private$ItemInfo = x},
    setItemResponseOptionsQuick = function(x){private$ItemResponseOptions = x},
    setItemResponseScoresQuick =  function(x){private$ItemResponseScores = x},
    setItemScoresQuick =          function(x){private$ItemScores = x},
    setItemSummaryQuick =         function(x){private$ItemSummary = x},
    setHandoutsQuick =            function(x){private$Handouts = x},
    setHasSpecialScoringQuick =   function(x){private$HasSpecialScoring = x},
    setHasStudentScoringQuick =   function(x){private$HasStudentScoring = x},
    setHasTopicsQuick =           function(x){private$HasTopics = x},
    setMissingSectionsQuick =     function(x){private$MissingSections = x},
    setNarrativeQuick =           function(x){private$Narrative = x},
    setResponseSetQuick =         function(x){private$ResponseSet = x},
    setResultsQuick =             function(x){private$Results = x},
    setSpecialScoring =           function(x){private$SpecialScoring = x},
    setSpecialScoringTableQuick = function(x){private$SpecialScoringTable = x},
    setSourceFileNamesQuick =     function(x){private$SourceFileNames = x},
    setSourcesQuick =             function(x){private$Sources = x},
    setSummaryQuick =             function(x){private$Summary = x},
    setTestNameQuick =            function(x){private$TestName = x},
    setTopicAlignmentsQuick =     function(x){private$TopicAlignments = x},
    setTopicScoresQuick =         function(x){private$TopicScores = x},
    setTopicSummaryQuick =        function(x){private$TopicSummary = x}, 
    setUploadTabQuick =           function(x){private$UploadTab = x},
    setUploadTotalPointsQuick =   function(x){private$UploadTotalPoints = x},
    
    # Export Methods
    exportNarrative = function(report = self){ exportNarrative.REPORT(report) }, 
    exportReport =    function(filename = "scores.xlsx", template = NULL, report = self){ 
      exportReport.REPORT(filename, template, report) }, 
    exportUpdate =    function(uploadFilePath, report = self){ # called from exportReport
      exportUpdate.REPORT(uploadFilePath, report) },
    exportUploads =   function(report = self){ exportUploads.REPORT(report) }, 
    
    # Other methods
    addCorrelations =        function(report = self){ addCorrelations.REPORT(report) }, 
    addItemScores =          function(report = self, messageLevel = 0){ addItemScores.REPORT(report, messageLevel) },
    addResponseFrequencies = function(report = self){ addResponseFrequencies.REPORT(report) },
    applySpecialScoring =    function(report = self){ applySpecialScoring.REPORT(report) },
    badMessage =             function(method, report = self){ return(badMessage.REPORT(method, report)) },
    checkSpecScor =          function(){return(private$HasSpecialScoring)},
    checkStudScor =          function(){return(private$HasStudentScoring)},
    checkTopics =            function(){return(private$HasTopics)},
    enhanceItemInfo =        function(report = self, useLocalNames = F, useLocalValues = F){
      enhanceItemInfo.REPORT(report, useLocalNames, useLocalValues)}, 
    loadSpecialScoring =     function(report = self){ loadSpecialScoring.REPORT(report) }, 
    updateIRandIRS =         function(report = self){updateIRandIRS.REPORT(self)},
    updateItemInfo =         function(x){private$ItemInfo = x}
    
  ) # /public
  
) # /REPORT R6 class
