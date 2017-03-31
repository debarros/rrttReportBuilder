#RESULTclass.R

#An instance of this class holds the set of results of a single test for a single section
#Each instance should be held within an instance of the REPORT class

RESULT = R6Class(
  
  classname = "RESULT",
  
  private = list(
    ItemResponses = NA, #data.frame showing the response each student made on each item
    SectionName = NA_character_, #atomic character with the name of the section
    ItemResponseScores = NA, #data.frame showing the points each student earned on each item
    Summary = NA, #not sure.  It will be the stuff that shows up to the right on the Scores tab
    TopicScores = NA, #data.frame with one row per student and one column per topic
    TopicSummary = NA, #data.frame representing one column from the Topic Chart Calculation tab and part of one row from the Topics tab
    DropScores = NA, #data.frame holding students' total points after dropping an item
    addr = paste0(getwd(),"/classes/RESULTclass/")
  ), #private
  
  public = list(
    initialize = function(SectionName){private$SectionName = SectionName},
    setItemResponses = function(sourceLocation, itemNames, itemValues){source(paste0(private$addr,"setItemResponses.R"), local = T)},
    setSectionName = function(x){private$SectionName= x},
    setItemResponseScores = function(ItemInfo){source(paste0(private$addr,"setItemResponseScores.R"), local = T)},
    setDropScores = function(ItemInfo){source(paste0(private$addr,"setDropScores.R"), local = T)},
    getItemResponses = function(){return(private$ItemResponses)},
    getSectionName = function(){return(private$SectionName)},
    getItemResponseScores = function(){return(private$ItemResponseScores)},
    getSummary = function(){return(private$Summary)},
    getTopicScores = function(){return(private$TopicScores)},
    getTopicSummary = function(){return(private$TopicSummary)},
    getDropScores = function(){return(private$DropScores)}
    
    #Methods yet to be made ####
    setSummary = function(x){private$Summary= x},
    setTopicScores = function(x){private$TopicScores= x},
    setTopicSummary = function(x){private$TopicSummary= x},
    
  ) #public
)
