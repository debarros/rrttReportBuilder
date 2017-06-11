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
    setItemResponses = function(sourceLocation, itemNames, itemValues){
      ItemResponses = read.csv(sourceLocation, skip = 13, header = F, stringsAsFactors = F) #read the item response info
      colnames(ItemResponses) = c("StudentID", "LastName","FirstName","TestDate","TotalPoints",itemNames) #set the column names 
      # Set the basic score column.  Note that, if there is special scoring, that will be applied later.
      ItemResponses$score = ItemResponses$TotalPoints/sum(itemValues)*100
      #put the score column first
      ItemResponses = ItemResponses[,c(which(colnames(ItemResponses)=="score"),which(colnames(ItemResponses)!="score"))] 
      private$ItemResponses = ItemResponses
    },
    setSectionName = function(x){private$SectionName= x},
    setItemResponseScores = function(ItemInfo){
      ItemResp = private$ItemResponses
      #create a data.frame to hold the item scores
      ItemResponseScores = setNames(as.data.frame(
        array(data = NA_integer_, dim = dim(ItemResp))),
        colnames(ItemResp)) 
      ItemResponseScores[,1:6] = ItemResp[,1:6] #pull in the student info from the results data.table
      #Calculate scores for each response on each item
      for(i in 1:nrow(ItemInfo)){
        if(ItemInfo$Type[i] == "MC"){
          ItemResponseScores[,ItemInfo$ItemName[i]] = ItemInfo$Value[i]*(ItemResp[,ItemInfo$ItemName[i]] == ItemInfo$Answer[i])
        } else {
          ItemResponseScores[,ItemInfo$ItemName[i]] = ItemResp[,ItemInfo$ItemName[i]]
        }
      }
      private$ItemResponseScores = ItemResponseScores
    },
    
    setDropScores = function(ItemInfo){
      #Set up a dataframe to hold the scores of each students with each item dropped and then calculate those scores
      DropScores = private$ItemResponseScores 
      for(i in 1:nrow(DropScores)){
        for(j in ItemInfo$ItemName){
          DropScores[i,j] = DropScores$TotalPoints[i] - private$ItemResponseScores[i,j]
        }
      }
      private$DropScores = DropScores
    },
    
    getItemResponses = function(){return(private$ItemResponses)},
    getSectionName = function(){return(private$SectionName)},
    getItemResponseScores = function(){return(private$ItemResponseScores)},
    getSummary = function(){return(private$Summary)},
    getTopicScores = function(){return(private$TopicScores)},
    getTopicSummary = function(){return(private$TopicSummary)},
    getDropScores = function(){return(private$DropScores)},
    
    
    setSummary = function(x){private$Summary= x},
    setTopicScores = function(TopicAlignments, ItemInfo){
      # Does this section need na.rm?
      TopicNames = colnames(TopicAlignments)[-1]
      TopicAlignments$Value = ItemInfo$Value
      TopicScores = private$ItemResponses[,2:4]
      TopicScores[,TopicNames] = NA_real_
      for(i in TopicNames){
        itemset = TopicAlignments[,i]
        totalpoints = sum(TopicAlignments$Value[itemset])
        for(j in 1:nrow(TopicScores)){
          TopicScores[j,i] = sum(t(private$ItemResponseScores[j,TopicAlignments$ItemName[itemset]]))/totalpoints
        }
      }
      private$TopicScores = TopicScores
      self$setTopicSummary(TopicScores)
    },
    setTopicSummary = function(TopicScores){
      private$TopicSummary = apply(TopicScores[,-c(1:3)], 2, mean)
    },
    setIRSquick = function(x){private$ItemResponseScores = x},
    setIRquick = function(x){private$ItemResponses = x},
    getIR = function(x){return(private$ItemResponses)},
    getIRS = function(x){return(private$ItemResponseScores)}
  ) #public
)
