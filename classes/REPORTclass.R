#REPORTclass.R

#An instance of this class holds everything necessary for a score report on a single test

REPORT = R6Class(
  
  classname = "REPORT",
  
  private = list(
    TMS = NULL, #The Testing Management System
    DataLocation = NULL, #address of the folder where the exported csv's are
    ComparisonLocation = NULL, #address and filename of the comparison and topic alignment
    Sources = NULL, #character vector with the locations of the csv's
    TestName = NULL, #atomic character with the name of the test
    ItemInfo = NULL, #data.frame with info about the items, will be used to build the breakdown tab
    UploadTab = NULL, #data.frame that holds the stuff that goes in the upload tab
    Results = NULL, #list of objects of class RESULT
    TopicAlignments = NULL, #data.frame holding the topic alignments
    TopicSummary = NULL, #data.frame with stuff that would go on the Topic Chart Calculation tab
    Summary = NULL, #list with the overall stats from the Scores tab
    ItemSummary = NULL, #data.frame With  the info in the table at the top of the Item Summary tab.  One row per item, one column per type
    Narrative = NULL, #either atomic character or character vector.  Will be the text in cell A11 of the Item Summary tab
    Comparison = list(), #a list of objects of class COMPARISON
    Handouts = NULL, #data.frame containing the information necessary to build the Handouts tab simply
    DistractorCutoffProportion = 0.25, #cutoff percentage for determining whether an item is a distrator
    OverThinkCutoff = 0.1, #cutoff correlation for determining whether an item is an overthinker
    EasyCutoff = 0.9, #cutoff score for determining whether an item is easy
    DifficultCutoffParams = list("Lower" = 0.4, "Upper" = 0.5, "Proportion" = 0.2), #lower bound, upper bound, and target proportion for the criterion for determining whether an item is difficult
    ChaffRules = data.frame(score = c(.3, .4), correlation = c(.3, .5)), #rules for determining whether an item is Wheat From Chaff
    RelatedCutoffProportion = 0.2, #target proportion of items to count as highly related
    ResponseSet = NULL, #character vector hold the names of the different response frequencies columns
    Correlations = NULL, #correlations column from the ItemInfo
    ItemScores = NULL, #AverageScore column from the ItemInfo
    ItemResponseScores = NULL, #data.table with the score for every student on every item
    DropScores = NULL, #data.table with the score for each student after dropping each item
    addr = paste0(getwd(),"/classes/REPORTclass/"),
    PassingScore = 0.7 #passing score for the test
  ),
  
  public = list(
    initialize = function(TMS = "LinkIt"){private$TMS = TMS}, #default the Testing Management System to LinkIt
    setDataLocation = function(x){private$DataLocation = x},
    setComparisonLocation = function(x){private$ComparisonLocation = x},
    setSources = function(){source(paste0(private$addr,"setSources.R"), local = T)},
    setTestName = function(){source(paste0(private$addr,"setTestName.R"), local = T)},
    setItemInfo = function(){source(paste0(private$addr,"setItemInfo.R"), local = T)},
    enhanceItemInfo = function(){source(paste0(private$addr,"enhanceItemInfo.R"), local = T)},
    setTopicAlignments = function(d2){source(paste0(private$addr,"setTopicAlignments.R"), local = T)},
    addItemScores = function(){source(paste0(private$addr,"addItemScores.R"), local = T)},
    setUploadTab = function(){source(paste0(private$addr,"setUploadTab.R"), local = T)},
    setResults = function(){source(paste0(private$addr,"setResults.R"), local = T)},
    setItemSummary = function(x){source(paste0(private$addr,"setItemSummary.R"), local = T)},
    setPassingScore = function(x){private$PassingScore = x},
    addCorrelations = function(){source(paste0(private$addr,"addCorrelations.R"), local = T)},
    addResponseFrequencies = function(){source(paste0(private$addr,"addResponseFrequencies.R"), local = T)}, 
    getResponses = function(){source(paste0(private$addr,"getResponses.R"), local = T)}, 
    badMessage = function(method){source(paste0(private$addr,"badMessage.R"), local = T)}, 
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
    setSummary = function(){source(paste0(private$addr,"setSummary.R"), local = T)},
    
    
    
    getResponses2 = function(){
      #establish a list that will hold the Item Response data.frames
      ItemResponses = vector(mode = "list", length = length(private$Results))
      #load the item responses for each section in the list
      for(i in 1:length(private$Results)){
        ItemResponses[[i]] = private$Results[[i]]$getItemResponses()
      }
      ItemResponses2 = rbindlist(ItemResponses) #make a single data.table with all of the item responses from all of the sections
      return(ItemResponses2)
    },
    
    #Methods still to be made ####
    setTopicSummary = function(x){private$TopicSummary = x},
    setNarrative = function(x){private$Narrative = x},
    setComparison = function(x){private$Comparison = x},
    setHandouts = function(x){private$Handouts = x}
  )
)
