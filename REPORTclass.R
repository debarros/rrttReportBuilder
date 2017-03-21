#classDefinitions.R

REPORT = R6Class(
  
  classname = "REPORT",
  
  private = list(
    TMS = NA_character_, #The Testing Management System
    DataLocation = NA_character_, #address of the folder where the exported csv's are
    ComparisonLocation = NA_character_, #address and filename of the comparison and topic alignment
    Sources = NA_character_, #character vector with the locations of the csv's
    TestName = NA_character_, #atomic character with the name of the test
    ItemInfo = NA, #data.frame with info about the items, will be used to build the breakdown tab
    UploadTab = NA, #data.frame that holds the stuff that goes in the upload tab
    Results = list(), #list of objects of class RESULT
    TopicSummary = NA, #data.frame with stuff that would go on the Topic Chart Calculation tab
    Summary = NA, #not sure of the format.  Will be the overall stats from the Scores tab
    ItemSummary = NA, #not sure of the format.  Will be the info in the table at the top of the Item Summary tab
    Narrative = NA, #either atomic character or character vector.  Will be the text in cell A11 of the Item Summary tab
    Comparison = list(), #a list of objects of class COMPARISON
    Handouts = NA, #data.frame containing the information necessary to build the Handouts tab simply
    Correlations = NA #data.frame containing the itemscore/testscore correlations
  ),
  
  public = list(
    initialize = function(TMS = "LinkIt"){private$TMS = TMS}, #default the Testing Management System to LinkIt
    setDataLocation = function(x){private$dataLocation = x},
    setComparisonLocation = function(x){private$ComparisonLocation = x},
    setSources = function(x){private$Sources = x},
    setTestName = function(x){private$TestName = x},
    setItemInfo = function(x){private$ItemInfo = x},
    setUploadTab = function(x){private$UploadTab = x},
    setResults = function(x){private$Results = x},
    setTopicSummary = function(x){private$TopicSummary = x},
    setSummary = function(x){private$Summary = x},
    setItemSummary = function(x){private$ItemSummary = x},
    setNarrative = function(x){private$Narrative = x},
    setComparison = function(x){private$Comparison = x},
    setHandouts = function(x){private$Handouts = x},
    setCorrelations = function(x){private$Correlations = x}
    getDataLocation = function(){return(private$DataLocation)},
    getComparisonLocation = function(){return(private$ComparisonLocation)},
    getSources = function(){return(private$Sources)},
    getTestName = function(){return(private$TestName)},
    getItemInfo = function(){return(private$ItemInfo)},
    getUploadTab = function(){return(private$UploadTab)},
    getResults = function(){return(private$Results)},
    getTopicSummary = function(){return(private$TopicSummary)},
    getSummary = function(){return(private$Summary)},
    getItemSummary = function(){return(private$ItemSummary)},
    getNarrative = function(){return(private$Narrative)},
    getComparison = function(){return(private$Comparison)},
    getHandouts = function(){return(private$Handouts)},
    getCorrelations = function(){return(private$Correlations)}
  )
  
)










