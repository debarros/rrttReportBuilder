#RESULTclass.R

#An instance of this class holds the set of results of a single test for a single section
#Each instance should be held within an instance of the REPORT class

RESULT = R6Class(
  
  classname = "RESULT",
  
  private = list(
    ItemResponses = NA, #data.frame showing the response each student made on each item
    SectionName = NA_character_, #atomic character with the name of the section
    ItemScores = NA, #data.frame showing the points each student earned on each item
    Summary = NA, #not sure.  It will be the stuff that shows up to the right on the Scores tab
    TopicScores = NA, #data.frame with one row per student and one column per topic
    TopicSummary = NA, #data.frame representing one column from the Topic Chart Calculation tab and part of one row from the Topics tab
    DropScores = NA #data.frame holding students' total points after dropping an item
  ), #private
  
  public = list(
    initialize = function(SectionName){
      private$SectionName = SectionName
    },
    
    setItemResponses = function(sourceLocation, itemNames, itemValues){
      ItemResponses = read.csv(sourceLocation, skip = 13, header = F, stringsAsFactors = F) #read the item response info
      colnames(ItemResponses) = c("StudentID", "LastName","FirstName","TestDate","TotalPoints",itemNames) #set the column names 
      ItemResponses$score = ItemResponses$TotalPoints/sum(itemValues)*100
      private$ItemResponses = ItemResponses
    },
    
    setSectionName = function(x){private$SectionName= x},
    
    
    setResponseScores = function(ItemInfo){
      #create a dara.frame to hold the item scores
      ItemScores = setNames(as.data.frame(array(data = NA_integer_, dim = dim(private$ItemResponses))), colnames(private$ItemResponses)) 
      ItemScores[,1:5] = private$ItemResponses[,1:5] #pull in the student info from the results data.table
      
      #Calculate scores for each response on each item
      for(i in 1:nrow(ItemInfo)){
        if(ItemInfo$Type[i] == "MC"){
          ItemScores[,ItemInfo$ItemName[i]] = ItemInfo$Value[i]*(private$ItemResponses[,ItemInfo$ItemName[i], with = F] == ItemInfo$Answer[i])
        } else {
          ItemScores[,ItemInfo$ItemName[i]] = private$ItemResponses[,ItemInfo$ItemName[i], with = F]
        }
      }
      private$ItemScores = ItemScores
    }, #setResponseScores
    
    
    setSummary = function(x){private$Summary= x},
    setTopicScores = function(x){private$TopicScores= x},
    setTopicSummary = function(x){private$TopicSummary= x},
    setDropScores = function(x){private$DropScores= x},
    getItemResponses = function(){return(private$ItemResponses)},
    getSectionName = function(){return(private$SectionName)},
    getItemScores = function(){return(private$ItemScores)},
    getSummary = function(){return(private$Summary)},
    getTopicScores = function(){return(private$TopicScores)},
    getTopicSummary = function(){return(private$TopicSummary)},
    getDropScores = function(){return(private$DropScores)}
  ) #public
  
)