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
    Results = list(), #list of objects of class RESULT
    TopicAlignments = NULL, #data.frame holding the topic alignments
    TopicSummary = NULL, #data.frame with stuff that would go on the Topic Chart Calculation tab
    Summary = NULL, #not sure of the format.  Will be the overall stats from the Scores tab
    ItemSummary = NULL, #not sure of the format.  Will be the info in the table at the top of the Item Summary tab
    Narrative = NULL, #either atomic character or character vector.  Will be the text in cell A11 of the Item Summary tab
    Comparison = list(), #a list of objects of class COMPARISON
    Handouts = NULL #data.frame containing the information necessary to build the Handouts tab simply
  ),
  
  public = list(
    initialize = function(TMS = "LinkIt"){private$TMS = TMS}, #default the Testing Management System to LinkIt
    setDataLocation = function(x){private$dataLocation = x},
    setComparisonLocation = function(x){private$ComparisonLocation = x},
    
    setSources = function(){ #get a list of files
      if(is.null(private$DataLocation)){
        return("Need a data location first.")
      } else {
        private$Sources = list.files(private$DataLocation, full.names = T)  
      }
    }, #setSources
    
    setTestName = function(){ #get the name of the test
      if(is.null(private$Sources)){
        return("Need sources first.")
      } else {
        private$TestName = read.csv(private$Sources[1], header = F, nrows = 1, stringsAsFactors = F)[1,2] 
      }
    }, #setTestName
    
    setItemInfo = function(){
      if(is.null(private$Sources)){
        return("Need sources first.")
      } else {
        ItemInfo = read.csv(private$Sources[1], skip = 4, header = F, nrows = 3, stringsAsFactors = F)[,-(1:5)]  #get the basic item info
        ItemInfo =  set_rownames(setNames(as.data.frame(t(ItemInfo), stringsAsFactors = F), c("ItemName", "Value", "Answer")), NULL) #fix the iteminfo data.frame setup
        toFix = grepl(pattern = "[^a-zA-Z\\d\\s:]", x = ItemInfo$Answer) #which items have weird values in the Answer field?
        ItemInfo$Answer[toFix] = ItemInfo$Value[toFix] #Set those answers to just be the value of the respective questions
        ItemInfo$Value = as.numeric(ItemInfo$Value) #Set the value column to be numeric
        ItemInfo$AverageScore = NA_real_  
        ItemInfo$Correlation = NA_real_
        ItemInfo$Type = NA_character_
        ItemInfo$options = NA_integer_
        private$ItemInfo = ItemInfo
      }
    }, #setItemInfo
    
    
    enhanceItemInfo = function(){ #load the complete item info from the comparison and topic alignment file
      badmessage = ""
      if(is.null(private$ComparisonLocation)){ badmessage = paste0(badmessage, "Need Comparison Location first.  ")}
      if(is.null(private$ItemInfo)){ badmessage = paste0(badmessage, "Need Item Info first.  ")}
      if(nchar(badmessage) > 0){
        return(badmessage)
      } else {
        d2 = read.xlsx(xlsxFile = private$ComparisonLocation, sheet = "Topic Alignment", startRow = 2, colNames = F)
        d2 = d2[1:(which(is.na(d2[,3]))[1] - 1),3:ncol(d2)] #remove unnecessary columns and rows
        d2 = t(d2) #transpose it
        colnames(d2) = d2[1,] #use the first row as the column names
        d2 = d2[-1,] #remove the first row
        row.names(d2) = NULL #remove the row names
        d2 = as.data.frame(d2, stringsAsFactors = F) #convert it to a data.frame
        self$setTopicAlignments(d2) #set the topic alignments
        d2$isMC = grepl("mc",d2$`Type:`, ignore.case = T) #determine which questions are MC
        d2$`Value:` = as.integer(d2$`Value:`) #convert the Value column to integer
        d2$options = d2$`Value:` + 1 #default the number of options to what it should be for ER questions
        d2$options[d2$isMC] = substr(d2$`Type:`[d2$isMC], 3, nchar(d2$`Type:`[d2$isMC])) #set the number of options for MC questions
        d2$type = "ER" #default the question type to ER
        d2$type[d2$isMC] = "MC" #set the question type to MC for MC questions
        private$ItemInfo$Type = d2$type[match(private$ItemInfo$ItemName, d2$`Question #:`)] #set the type 
        private$ItemInfo$options = as.integer(d2$options[match(private$ItemInfo$ItemName, d2$`Question #:`)]) #set the number of options
      }
    } #enhanceItemInfo
    
    
    setTopicAlignments = function(d2){
      Topics = d2[,5:ncol(d2)] #set up a data.frame to hold topic info
      colnames(Topics)[1] = "ItemName" #set the name of the first column
      private$TopicAlignments = Topics
    } #setTopicAlignments
    
    
    addItemScores = function(){ #add the ItemResponseScores data.frame to each result and add item average scores to the ItemInfo
      badmessage = ""
      if(length(private$results) == 0){badmessage = paste0(badmessage, "Need Results first.  ")}
      if(is.null(private$ComparisonLocation)){ badmessage = paste0(badmessage, "Need Comparison Location first.  ")}
      if(is.null(private$ItemInfo)){ badmessage = paste0(badmessage, "Need Item Info first.  ")}
      if(nchar(badmessage) > 0){
        return(badmessage)
      } else {
        #establish a list that will hold the Item Response Scores data.frames
        ItemResponseScores = vector(mode = "list", length = length(private$results))
        #calculate the item response scores for each section and load them in the list
        for(i in 1:length(private$results)){
          private$results[i]$setResponseScores(private$ItemInfo)
          ItemResponseScores[i] = private$results[i]$getItemScores
        }
        ItemResponseScores = rbindlist(ItemResponseScores) #make a single data.table with all of the item response scores from all of the sections
        #Calculate the average score for each question
        for(i in 1:nrow(private$ItemInfo)){
          private$ItemInfo$AverageScore[i] = mean(ItemResponseScores[,private$ItemInfo$ItemName[i]])/private$ItemInfo$Value[i]*100
        } 
      }
    } #addItemScores
    
    
    setUploadTab = function(x){private$UploadTab = x},
    
    setResults = function(){
      if(is.null(private$Sources)){
        return("Need sources first.")
      } else {
        results = vector(mode = "list", length = length(private$Sources)) #set up a list to hold the response sets for the various sections
        names(results) = paste0("a", 1:length(private$Sources)) #add names to the list so they can be set later
        for (i in 1:length(private$Sources)){  #for each source/section
          SectionName = read.csv(private$Sources[i], skip = 1, header = F, nrows = 1, stringsAsFactors = F)[1,2] #get the section name
          thisResult = RESULT$new(SectionName)
          thisResult$setItemResponses(private$Sources[i], private$ItemInfo$ItemName, private$ItemInfo$Value)
          results[[i]] = thisResult #put the response info in the list
          names(results)[i] = SectionName #set the element name in the list to be the name of the section
        }  
      }
    }, #setResults
    
    setTopicSummary = function(x){private$TopicSummary = x},
    setSummary = function(x){private$Summary = x},
    setItemSummary = function(x){private$ItemSummary = x},
    setNarrative = function(x){private$Narrative = x},
    setComparison = function(x){private$Comparison = x},
    setHandouts = function(x){private$Handouts = x},
    
    
    addCorrelations = function(x){
      badmessage = ""
      if(length(private$results) == 0){badmessage = paste0(badmessage, "Need Results first.  ")}
      if(is.null(private$ComparisonLocation)){ badmessage = paste0(badmessage, "Need Comparison Location first.  ")}
      if(is.null(private$ItemInfo)){ badmessage = paste0(badmessage, "Need Item Info first.  ")}
      if(nchar(badmessage) > 0){
        return(badmessage)
      } else {
        #establish a list that will hold the DropScores data.frames
        DropScores = vector(mode = "list", length = length(private$results))
        #calculate the item response scores for each section and load them in the list
        for(i in 1:length(private$results)){
          private$results[i]$setDropScores(private$ItemInfo)
          DropScores[i] = private$results[i]$getDropScores
        }
        DropScores = rbindlist(DropScores) #make a single data.table with all of the dropscores from all of the sections
        #Calculate the correlations
        for(i in 1:nrow(private$ItemInfo)){
          private$ItemInfo$Correlation[i] = cor(DropScores$TotalPoints, DropScores[private$ItemInfo$ItemName[i]])
        }
      }
    }, #addCorrelations
    
    
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










