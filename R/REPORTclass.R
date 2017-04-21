#REPORTclass.R

#An instance of this class holds everything necessary for a score report on a single test

#This file is way too long.  It should be broken up into separate files, where each one adds a method using $set()
# See details at https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html#adding-members-to-an-existing-class

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
    Comparison = NULL, #a list of objects of class COMPARISON
    Handouts = NULL, #data.frame containing the information necessary to build the Handouts tab simply
    DistractorCutoffProportion = 0.25, #cutoff percentage for determining whether an item is a distrator
    OverThinkCutoff = 0.1, #cutoff correlation for determining whether an item is an overthinker
    EasyCutoff = 0.9, #cutoff score for determining whether an item is easy
    #lower bound, upper bound, and target proportion for the criterion for determining whether an item is difficult
    DifficultCutoffParams = list("Lower" = 0.4, "Upper" = 0.5, "Proportion" = 0.2), 
    ChaffRules = data.frame(score = c(.3, .4), correlation = c(.3, .5)), #rules for determining whether an item is Wheat From Chaff
    RelatedCutoffProportion = 0.2, #target proportion of items to count as highly related
    ResponseSet = NULL, #character vector hold the names of the different response frequencies columns
    Correlations = NULL, #correlations column from the ItemInfo
    ItemScores = NULL, #AverageScore column from the ItemInfo
    ItemResponseScores = NULL, #data.table with the score for every student on every item
    DropScores = NULL, #data.table with the score for each student after dropping each item
    PassingScore = 0.7 #passing score for the test
  ),
  
  public = list(
    initialize = function(TMS = "LinkIt"){private$TMS = TMS}, #default the Testing Management System to LinkIt
    setDataLocation = function(x){private$DataLocation = x},
    setComparisonLocation = function(x){private$ComparisonLocation = x},
    
    setSources = function(){
      if(is.null(private$DataLocation)){
        return("Need a data location first.")
      } else {
        private$Sources = list.files(private$DataLocation, full.names = T)  
      }
    },
    
    setTestName = function(){
      if(is.null(private$Sources)){
        return("Need sources first.")
      } else {
        private$TestName = read.csv(private$Sources[1], header = F, nrows = 1, stringsAsFactors = F)[1,2] 
      }
    },
    
    setItemInfo = function(){
      if(is.null(private$Sources)){
        return("Need sources first.")
      } else {
        ItemInfo = read.csv(private$Sources[1], skip = 4, header = F, nrows = 3, stringsAsFactors = F)[,-(1:5)]  #get the basic item info
        #fix the iteminfo data.frame setup
        ItemInfo =  magrittr::set_rownames(setNames(as.data.frame(t(ItemInfo), stringsAsFactors = F), c("ItemName", "Value", "Answer")), NULL) 
        toFix = grepl(pattern = "[^a-zA-Z\\d\\s:]", x = ItemInfo$Answer) #which items have weird values in the Answer field?
        ItemInfo$Answer[toFix] = ItemInfo$Value[toFix] #Set those answers to just be the value of the respective questions
        ItemInfo$Value = as.numeric(ItemInfo$Value) #Set the value column to be numeric
        ItemInfo$AverageScore = NA_real_  
        ItemInfo$Correlation = NA_real_
        ItemInfo$Type = NA_character_
        ItemInfo$options = NA_integer_
        private$ItemInfo = ItemInfo
      }
    },
    
    enhanceItemInfo = function(){
      badmessage = ""
      if(is.null(private$ComparisonLocation)){ badmessage = paste0(badmessage, "Need Comparison Location first.  ")}
      if(is.null(private$ItemInfo)){ badmessage = paste0(badmessage, "Need Item Info first.  ")}
      if(nchar(badmessage) > 0){
        return(badmessage)
      } else {
        d2 = openxlsx::read.xlsx(xlsxFile = private$ComparisonLocation, sheet = "Topic Alignment", startRow = 2, colNames = F)
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
    },
    
    setTopicAlignments = function(d2){
      Topics = d2[,5:ncol(d2)] #set up a data.frame to hold topic info
      colnames(Topics)[1] = "ItemName" #set the name of the first column
      for(i in 2:ncol(Topics)){
        Topics[,i] = as.logical(as.numeric(Topics[,i]))
      }
      private$TopicAlignments = Topics
    },
    
    addItemScores = function(){
      badmessage = ""
      if(length(private$Results) == 0){badmessage = paste0(badmessage, "Need Results first.  ")}
      if(is.null(private$ComparisonLocation)){ badmessage = paste0(badmessage, "Need Comparison Location first.  ")}
      if(is.null(private$ItemInfo)){ badmessage = paste0(badmessage, "Need Item Info first.  ")}
      if(nchar(badmessage) > 0){
        return(badmessage)
      } else {
        #establish a list that will hold the Item Response Scores data.frames
        ItemResponseScores = vector(mode = "list", length = length(private$Results))
        #calculate the item response scores for each section and load them in the list
        for(i in 1:length(private$Results)){
          private$Results[[i]]$setItemResponseScores(private$ItemInfo)
          ItemResponseScores[[i]] = private$Results[[i]]$getItemResponseScores()
        }
        #make a single data.table with all of the item response scores from all of the sections
        ItemResponseScores = data.table::rbindlist(ItemResponseScores) 
        #Calculate the average score for each question
        for(i in 1:nrow(private$ItemInfo)){
          private$ItemInfo$AverageScore[i] = mean(ItemResponseScores[[private$ItemInfo$ItemName[i]]])/private$ItemInfo$Value[i]*100
        }
        private$ItemScores = private$ItemInfo$AverageScore
        private$ItemResponseScores = ItemResponseScores
      }
    },
    
    setUploadTab = function(){
      ItemResponses = as.data.frame(self$getResponses())
      UploadTab = data.frame(StudentID = ItemResponses$StudentID)
      UploadTab$StudentName = paste0(ItemResponses$LastName, ", ",ItemResponses$FirstName)
      UploadTab$Percentage = ItemResponses$score
      private$UploadTab = UploadTab
    },
    
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
        private$Results = results
      }
    },
    
    setPassingScore = function(x){private$PassingScore = x},
    
    addCorrelations = function(){
      badmessage = ""
      if(length(private$Results) == 0){badmessage = paste0(badmessage, "Need Results first.  ")}
      if(is.null(private$ComparisonLocation)){ badmessage = paste0(badmessage, "Need Comparison Location first.  ")}
      if(is.null(private$ItemInfo)){ badmessage = paste0(badmessage, "Need Item Info first.  ")}
      if(nchar(badmessage) > 0){
        return(badmessage)
      } else {
        #establish a list that will hold the DropScores data.frames
        DropScores = vector(mode = "list", length = length(private$Results))
        #calculate the item response scores for each section and load them in the list
        for(i in 1:length(private$Results)){
          private$Results[[i]]$setDropScores(private$ItemInfo)
          DropScores[[i]] = private$Results[[i]]$getDropScores()
        }
        DropScores = data.table::rbindlist(DropScores) #make a single data.table with all of the dropscores from all of the sections
        #Calculate the correlations between the student scores on the item and the student total scores after dropping the item
        for(i in 1:nrow(private$ItemInfo)){
          thisItem = private$ItemInfo$ItemName[i]
          private$ItemInfo$Correlation[i] = cor(private$ItemResponseScores[[thisItem]], DropScores[[thisItem]])
        }
      }
      private$Correlations = private$ItemInfo$Correlation
      private$DropScores = DropScores
    },
    
    addResponseFrequencies = function(){
      badmessage = ""
      if(length(private$Results) == 0){badmessage = paste0(badmessage, "Need Results first.  ")}
      if(is.null(private$ComparisonLocation)){ badmessage = paste0(badmessage, "Need Comparison Location first.  ")}
      if(is.null(private$ItemInfo)){ badmessage = paste0(badmessage, "Need Item Info first.  ")}
      if(nchar(badmessage) > 0){
        return(badmessage)
      } else {
        ItemResponses = self$getResponses()
        
        #Set default values for the number of letter options and number of point options
        topletter = 0
        toppoint = 0
        
        #Find the max number of letter and point options in the test
        if("MC" %in% private$ItemInfo$Type){topletter = max(private$ItemInfo$options, na.rm = T)}
        if("ER" %in% private$ItemInfo$Type){toppoint = max(private$ItemInfo$Value[private$ItemInfo$Type == "ER"])} 
        
        #Find the max number of response columns needed and the number of columns that will represent both numbers and letters
        totalset = max(topletter, toppoint+1)
        overlapset = min(topletter, toppoint+1)
        
        #Build the set of response column names
        responseSet = paste0(
          c(LETTERS[1:topletter], rep("", times = totalset - topletter)),
          c(rep("/", times = overlapset), rep("", times = totalset - overlapset)),
          c(as.character(0:toppoint), rep("", times = totalset - (toppoint+1) )))
        
        basecolumn = ncol(private$ItemInfo) #How many columns does ItemInfo have already?
        private$ItemInfo[,responseSet] = "" #Initialize those columns
        
        #This nested loop should be rewritten using lapply or something
        for(i in 1:nrow(private$ItemInfo)){ #for every item
          for(j in 1:private$ItemInfo$options[i]){ #for every possible response for that item
            if(private$ItemInfo$Type[i] == "ER"){ #if it's an ER item, count how many times that point level was awarded
              private$ItemInfo[i,j+basecolumn] = sum(ItemResponses[,private$ItemInfo$ItemName[i], with = F] == j-1)
            } else { #if it's an MC item, count how many times that letter was used
              private$ItemInfo[i,j+basecolumn] = sum(ItemResponses[,private$ItemInfo$ItemName[i], with = F] == LETTERS[j])
            }
          }
        }
        private$ResponseSet = responseSet
      }
    }, 
    
    getResponses = function(){
      #establish a list that will hold the Item Response data.frames
      ItemResponses = vector(mode = "list", length = length(private$Results))
      #load the item responses for each section in the list
      for(i in 1:length(private$Results)){
        ItemResponses[[i]] = private$Results[[i]]$getItemResponses()
      }
      ItemResponses = data.table::rbindlist(ItemResponses) #make a single data.table with all of the item responses from all of the sections
      return(ItemResponses)
    }, 
    
    badMessage = function(method){
      badmessage = ""
      if(is.null(private$DataLocation)){
        badmessage = paste0(badmessage, "Need a data location first.  ")
      }
      if(method %in% c("setSources", "getDataLocation")){
        return(badmessage)
      }
      
      if(is.null(private$Sources)){
        badmessage = paste0(badmessage, "Need sources first.  ")
      }
      if(method %in% c("setTestName", "getSources")){
        return(badmessage)
      }
      
      if(is.null(private$TestName)){
        badmessage = paste0(badmessage, "Need the test name first.  ")
      }
      if(method %in% c("setItemInfo", "getTestName")){
        return(badmessage)
      }
      
      if(is.null(private$ItemInfo)){
        badmessage = paste0(badmessage, "Need Item Info first.  ")
      }
      if(method %in% c("setResults", "getItemInfo")){
        return(badmessage)
      }
      
      if(length(private$Results) == 0){
        badmessage = paste0(badmessage, "Need results first.  ")
      }
      if(method %in% c("setComparisonLocation", "getResults")){
        return(badmessage)
      }
      
      if(is.null(private$ComparisonLocation)){
        badmessage = paste0(badmessage, "Need Comparison Location first.  ")
      }
      if(method %in% c("getComparisonLocation")){
        return(badmessage)
      }
      
      if(is.null(private$TopicAlignments)){
        badmessage = paste0(badmessage, "Need Topic Alignments first.  ")
      }
      if(method %in% c("addItemScores", "getTopicAlignments")){
        return(badmessage)
      }
      
      if(is.null(private$ItemScores)){
        badmessage = paste0(badmessage, "Need item scores first.  ")
      }
      if(method %in% c("addCorrelations", "getItemScores")){
        return(badmessage)
      }
      
      if(is.null(private$Correlations)){
        badmessage = paste0(badmessage, "Need item correlations first.  ")
      }
      if(method %in% c("addResponseFrequencies", "getCorrelations")){
        return(badmessage)
      }
      
      if(is.null(private$ResponseSet)){
        badmessage = paste0(badmessage, "Need Response Frequencies first.  ")
      }
      if(method %in% c("setUploadTab")){
        return(badmessage)
      }
      
      if(is.null(private$UploadTab)){
        badmessage = paste0(badmessage, "Need upload tab first.  ")
      }
      if(method %in% c("getUploadTab")){
        return(badmessage)
      }
    }, 
    
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
    getTopicAlignments = function(){return(private$TopicAlignments)},
    
    setSummary = function(){
      Summarize = vector(mode = "list")
      Summarize$Average = mean(private$UploadTab$Percentage)
      Summarize$Median = median(private$UploadTab$Percentage)
      Summarize$High = max(private$UploadTab$Percentage)
      Summarize$Low = min(private$UploadTab$Percentage)
      Summarize$Q1 = quantile(x = private$UploadTab$Percentage, probs = .25)
      Summarize$Q3 = quantile(x = private$UploadTab$Percentage, probs = .75)
      Summarize$Tenth = quantile(x = private$UploadTab$Percentage, probs = .10)
      Summarize$Ninetieth = quantile(x = private$UploadTab$Percentage, probs = .90)
      Summarize$SD = sd(private$UploadTab$Percentage)
      Summarize$N = nrow(private$UploadTab)
      Summarize$NPassed = sum(private$UploadTab$Percentage >= private$PassingScore)
      Summarize$PassRate = Summarize$NPassed / Summarize$N
      private$Summary = Summarize
    },
    
    setItemSummary = function(){
      badmessage = ""
      if(is.null(private$DataLocation)){ badmessage = paste0(badmessage, "Need Data Location first.  ")}
      if(length(private$Results) == 0){badmessage = paste0(badmessage, "Need Results first.  ")}
      if(is.null(private$ComparisonLocation)){ badmessage = paste0(badmessage, "Need Comparison Location first.  ")}
      if(is.null(private$ItemInfo)){ badmessage = paste0(badmessage, "Need Item Info first.  ")}
      if(is.null(private$ResponseSet)){ badmessage = paste0(badmessage, "Need Response Frequencies first.  ")}
      if(nchar(badmessage) > 0){
        return(badmessage)
      } else {
        
        # set the parameters
        DifficultCutoff = 100* min(
          private$DifficultCutoffParams$Lower, 
          max(private$DifficultCutoffParams$Upper, 
              quantile(private$ItemInfo$AverageScore, private$DifficultCutoffParams$Proportion)))
        
        # find the 1-RelatedCutoffProportion quantile of the item correlations
        RelatedCutoff = quantile(private$ItemInfo$Correlation, 1 - private$RelatedCutoffProportion)   
        DistractorCutoffCount = nrow(private$ItemInfo) * private$DistractorCutoffProportion
        
        #set up the ItemSummary data.frame
        ItemSummary = data.frame(ItemName = private$ItemInfo$ItemName) 
        
        #for MC items only, if at least one wrong answer was selected by at least private$DistractorCutoffProportion percent of students
        #this should be altered to not be a loop
        ItemSummary$PowerDistrators = FALSE
        for(i in 1:nrow(ItemSummary)){
          if(private$ItemInfo$Type[i] == "MC"){
            wrongSet = grep(pattern = private$ItemInfo$Answer[i], x = private$ResponseSet, value = T, invert = T) 
            ItemSummary$PowerDistrators[i] = any(private$ItemInfo[i,wrongSet] > DistractorCutoffCount)
          }
        }
        
        
        #Correlation < private$OverThinkCutoff
        ItemSummary$OverThinking = private$ItemInfo$Correlation <  private$OverThinkCutoff
        
        # average score < DifficultCutoff
        ItemSummary$Difficult = private$ItemInfo$AverageScore < DifficultCutoff
        
        # average score >= private$EasyCutoff
        ItemSummary$Easy = private$ItemInfo$AverageScore >= private$EasyCutoff * 100
        
        #item fits one of the ChaffRules (score < ChaffRules$score and correlation > ChaffRules$correlation)
        #this should be altered to not be a loop
        ItemSummary$WheatFromChaff = 0
        for(i in 1:nrow(private$ChaffRules)){
          ItemSummary$WheatFromChaff = ItemSummary$WheatFromChaff + 
            ((private$ItemInfo$AverageScore < private$ChaffRules$score[i]) & 
               (private$ItemInfo$Correlation > private$ChaffRules$correlation[i]))
        }
        ItemSummary$WheatFromChaff = as.logical(ItemSummary$WheatFromChaff)
        
        # correlation greater than RelatedCutoff
        ItemSummary$HighlyRelated = private$ItemInfo$Correlation > RelatedCutoff
        
        #for MC items only, (score is 0) OR (item is both Difficult and Overthinking)
        ItemSummary$CheckKey = private$ItemInfo$Type == "MC" & 
          (private$ItemInfo$AverageScore == 0 | (ItemSummary$Difficult & ItemSummary$OverThinking))
        
        private$ItemSummary = ItemSummary
      }
    },
    
    setTopicSummary = function(){
      TopicNames = colnames(private$TopicAlignments)[-1]
      TopicScores = vector(mode = "list", length = length(private$Results))
      sectionNames = c("All", names(private$Results))
      TopicSummary = magrittr::set_rownames(magrittr::set_colnames(
        as.data.frame.matrix(matrix(data = NA_real_, nrow = ncol(private$TopicAlignments)-1, ncol = length(sectionNames))),
        sectionNames),TopicNames)
      
      for(i in 1:length(private$Results)){
        private$Results[[i]]$setTopicScores(private$TopicAlignments,private$ItemInfo)
        TopicScores[[i]] = private$Results[[i]]$getTopicScores()
        TopicSummary[,names(private$Results)[i]] = private$Results[[i]]$getTopicSummary()
      }
      
      
      TopicScores = data.table::rbindlist(TopicScores)
      for(i in TopicNames){
        itemset = private$TopicAlignments[,i]
        totalpoints = sum(private$TopicAlignments$Value[itemset])
        TopicSummary$All[rownames(TopicSummary) == i] = mean(unlist(TopicScores[,i, with = F]))
      }
      private$TopicSummary = TopicSummary
    }, 
    
    
    
    #Methods still to be made ####
    
    setComparison = function(){
      
      #get general comparison info  
        
        
        
      
    },
    
    
    
    
    setHandouts = function(x){private$Handouts = x},
    setNarrative = function(x){private$Narrative = x}
  )
)
