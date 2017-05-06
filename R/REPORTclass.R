#REPORTclass.R

#An instance of this class holds everything necessary for a score report on a single test

#This file is way too long.  Methods should be moved to separate files and set up as functions

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
    ChaffRules = data.frame(score = c(.3, .4), 
                            correlation = c(.3, .5)), #rules for determining whether an item is Wheat From Chaff
    RelatedCutoffProportion = 0.2, #target proportion of items to count as highly related
    ResponseSet = NULL, #character vector hold the names of the different response frequencies columns
    Correlations = NULL, #correlations column from the ItemInfo
    ItemScores = NULL, #AverageScore column from the ItemInfo
    ItemResponseScores = NULL, #data.table with the score for every student on every item
    DropScores = NULL, #data.table with the score for each student after dropping each item
    PassingScore = 0.7, #passing score for the test
    ComparisonFileName = "comparison and topic alignment.xlsx" #test setup info filename (no file path)
  ), # /private
  
  public = list(
    initialize = function(TMS = "LinkIt"){private$TMS = TMS}, #default the Testing Management System to LinkIt
    setDataLocation = function(x){private$DataLocation = x},
    setComparisonFileName = function(x){private$ComparisonFileName = x},
    setComparisonLocation = function(x){private$ComparisonLocation = x},
    
    setSources = function(){
      if(is.null(private$DataLocation)){
        return("Need a data location first.")
      } else {
        private$Sources = list.files(paste0(private$DataLocation,"\\exports"), full.names = T)  
      }
    }, # /setSources
    
    setTestName = function(){
      if(is.null(private$Sources)){
        return("Need sources first.")
      } else {
        private$TestName = read.csv(private$Sources[1], header = F, nrows = 1, stringsAsFactors = F)[1,2] 
      }
    }, # /setTestName
    
    setItemInfo = function(){
      if(is.null(private$Sources)){
        return("Need sources first.")
      } else {
        ItemInfo = read.csv(private$Sources[1], 
                            skip = 4, 
                            header = F, 
                            nrows = 3, 
                            stringsAsFactors = F)[,-(1:5)]  #get the basic item info
        #fix the iteminfo data.frame setup
        ItemInfo =  magrittr::set_rownames(
          setNames(
            as.data.frame(t(ItemInfo), stringsAsFactors = F), 
            c("ItemName", "Value", "Answer")), 
          NULL
        ) 
        #which items have weird values in the Answer field?
        toFix = grepl(pattern = "[^a-zA-Z\\d\\s:]", x = ItemInfo$Answer) 
        #Set those answers to just be the value of the respective questions
        ItemInfo$Answer[toFix] = ItemInfo$Value[toFix] 
        ItemInfo$Value = as.numeric(ItemInfo$Value) #Set the value column to be numeric
        ItemInfo$AverageScore = NA_real_  
        ItemInfo$Correlation = NA_real_
        ItemInfo$Type = NA_character_
        ItemInfo$options = NA_integer_
        private$ItemInfo = ItemInfo
      }
    }, # /setItemInfo
    
    enhanceItemInfo = function(){
      badmessage = ""
      if(is.null(private$ComparisonLocation)){ 
        badmessage = paste0(badmessage, "Need Comparison Location first.  ")
      }
      if(is.null(private$ItemInfo)){ 
        badmessage = paste0(badmessage, "Need Item Info first.  ")
      }
      if(nchar(badmessage) > 0){
        return(badmessage)
      } else {
        d2 = openxlsx::read.xlsx(xlsxFile = private$ComparisonLocation, 
                                 sheet = "Topic Alignment", 
                                 startRow = 2, 
                                 colNames = F)
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
        #set the number of options for MC questions
        d2$options[d2$isMC] = substr(d2$`Type:`[d2$isMC], 3, nchar(d2$`Type:`[d2$isMC])) 
        d2$type = "ER" #default the question type to ER
        d2$type[d2$isMC] = "MC" #set the question type to MC for MC questions
        private$ItemInfo$Type = d2$type[match(private$ItemInfo$ItemName, d2$`Question #:`)] #set the type 
        #set the number of options
        private$ItemInfo$options = as.integer(d2$options[match(private$ItemInfo$ItemName, d2$`Question #:`)]) 
      }
    }, # /enhanceItemInfo
    
    setTopicAlignments = function(d2){
      Topics = d2[,5:ncol(d2)] #set up a data.frame to hold topic info
      colnames(Topics)[1] = "ItemName" #set the name of the first column
      for(i in 2:ncol(Topics)){
        Topics[,i] = as.logical(as.numeric(Topics[,i]))
      }
      private$TopicAlignments = Topics
    }, # /setTopicAlignments
    
    addItemScores = function(){
      badmessage = ""
      if(length(private$Results) == 0){
        badmessage = paste0(badmessage, "Need Results first.  ")
      }
      if(is.null(private$ComparisonLocation)){
        badmessage = paste0(badmessage, "Need Comparison Location first.  ")
      }
      if(is.null(private$ItemInfo)){ 
        badmessage = paste0(badmessage, "Need Item Info first.  ")
      }
      if(nchar(badmessage) > 0){
        return(badmessage)
      } else {
        #establish a list that will hold the Item Response Scores data.frames
        ItReScores = vector(mode = "list", length = length(private$Results))
        #calculate the item response scores for each section and load them in the list
        for(i in 1:length(private$Results)){
          private$Results[[i]]$setItemResponseScores(private$ItemInfo)
          ItReScores[[i]] = private$Results[[i]]$getItemResponseScores()
        }
        #make a single data.table with all of the item response scores from all of the sections
        ItReScores = data.table::rbindlist(ItReScores) 
        #Calculate the average score for each question
        for(i in 1:nrow(private$ItemInfo)){
          private$ItemInfo$AverageScore[i] = mean(
            ItReScores[[private$ItemInfo$ItemName[i]]])/private$ItemInfo$Value[i]
        }
        private$ItemScores = private$ItemInfo$AverageScore
        private$ItemResponseScores = ItReScores
      }
    }, # /addItemScores
    
    setUploadTab = function(){
      ItemResponses = as.data.frame(self$getResponses())
      UploadTab = data.frame(StudentID = ItemResponses$StudentID)
      UploadTab$StudentName = paste0(ItemResponses$LastName, ", ",ItemResponses$FirstName)
      UploadTab$Percentage = round(ItemResponses$score, digits = 2)
      private$UploadTab = UploadTab
    }, # /setUploadTab
    
    setResults = function(){
      if(is.null(private$Sources)){
        return("Need sources first.")
      } else {
        #set up list to hold the response sets for the various sections
        results = vector(mode = "list", length = length(private$Sources)) 
        #add names to the list so they can be set later
        names(results) = paste0("a", 1:length(private$Sources)) 
        for (i in 1:length(private$Sources)){  #for each source/section
          SectionName = read.csv(file = private$Sources[i], 
                                 skip = 1, 
                                 header = F, 
                                 nrows = 1, 
                                 stringsAsFactors = F)[1,2] #get the section name
          thisResult = RESULT$new(SectionName)
          thisResult$setItemResponses(private$Sources[i], private$ItemInfo$ItemName, private$ItemInfo$Value)
          results[[i]] = thisResult #put the response info in the list
          names(results)[i] = SectionName #set the element name in the list to be the name of the section
        }
        private$Results = results
      }
    }, # /setResults
    
    setPassingScore = function(x){private$PassingScore = x},
    
    addCorrelations = function(){
      badmessage = ""
      if(length(private$Results) == 0){
        badmessage = paste0(badmessage, "Need Results first.  ")
      }
      if(is.null(private$ComparisonLocation)){ 
        badmessage = paste0(badmessage, "Need Comparison Location first.  ")
      }
      if(is.null(private$ItemInfo)){ 
        badmessage = paste0(badmessage, "Need Item Info first.  ")
      }
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
        # make a single data.table with all of the dropscores from all of the sections
        DropScores = data.table::rbindlist(DropScores) 
        # Calculate the correlations between the student scores on the item 
        # and the student total scores after dropping the item
        for(i in 1:nrow(private$ItemInfo)){
          thisItem = private$ItemInfo$ItemName[i]
          private$ItemInfo$Correlation[i] = cor(private$ItemResponseScores[[thisItem]], DropScores[[thisItem]])
        }
      }
      private$Correlations = private$ItemInfo$Correlation
      private$DropScores = DropScores
    }, # /addCorrelations
    
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
        
        # Find the max number of response columns needed and the number 
        # of columns that will represent both numbers and letters
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
            #if it's an ER item, count how many times that point level was awarded
            if(private$ItemInfo$Type[i] == "ER"){ 
              private$ItemInfo[i,j+basecolumn] = sum(ItemResponses[,private$ItemInfo$ItemName[i], with = F] == j-1)
            } else { #if it's an MC item, count how many times that letter was used
              private$ItemInfo[i,j+basecolumn] = sum(
                ItemResponses[,private$ItemInfo$ItemName[i], with = F] == LETTERS[j])
            }
          }
        }
        private$ResponseSet = responseSet
      }
    },  # /addResponseFrequencies
    
    getResponses = function(){
      #establish a list that will hold the Item Response data.frames
      ItemResponses = vector(mode = "list", length = length(private$Results))
      #load the item responses for each section in the list
      for(i in 1:length(private$Results)){
        ItemResponses[[i]] = private$Results[[i]]$getItemResponses()
      }
      #make a data.table with all of the item responses from all of the sections
      ItemResponses = data.table::rbindlist(ItemResponses) 
      return(ItemResponses)
    },  # /getResponses
    
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
    },  # /badMessage
    
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
    
    getTopicAlignments = function(){
      TopicAlignments =  private$TopicAlignments
      for(i in 2:ncol(TopicAlignments)){
        TopicAlignments[,i] = as.integer(TopicAlignments[,i])
      }
      return(TopicAlignments)
    }, # /getTopicAlignments
    
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
      Summarize$TestName = private$TestName
      Summarize$Sections = length(private$Results)
      Summarize$Items = nrow(private$ItemInfo)
      private$Summary = Summarize
    }, # /setSummary
    
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
        DifficultCutoff = min(
          private$DifficultCutoffParams$Lower, 
          max(private$DifficultCutoffParams$Upper, 
              quantile(private$ItemInfo$AverageScore, private$DifficultCutoffParams$Proportion)))
        
        # find the 1-RelatedCutoffProportion quantile of the item correlations
        RelatedCutoff = quantile(private$ItemInfo$Correlation, 1 - private$RelatedCutoffProportion)   
        DistractorCutoffCount = nrow(private$ItemInfo) * private$DistractorCutoffProportion
        
        #set up the ItemSummary data.frame
        ItemSummary = data.frame(ItemName = private$ItemInfo$ItemName, stringsAsFactors = F) 
        
        # For MC items only, check if at least one wrong answer was selected by 
        # at least private$DistractorCutoffProportion percent of students.
        # This should be altered to not be a loop.
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
        ItemSummary$Easy = private$ItemInfo$AverageScore >= private$EasyCutoff
        
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
    }, # /setItemSummary
    
    setTopicSummary = function(){
      TopicNames = colnames(private$TopicAlignments)[-1]
      TopicScores = vector(mode = "list", length = length(private$Results))
      sectionNames = c("All Classes", names(private$Results))
      TopicSummary = magrittr::set_rownames(
        magrittr::set_colnames(
          as.data.frame.matrix(
            matrix(data = NA_real_, 
                   nrow = ncol(private$TopicAlignments)-1, 
                   ncol = length(sectionNames))),
          sectionNames),
        TopicNames)
      
      for(i in 1:length(private$Results)){
        private$Results[[i]]$setTopicScores(private$TopicAlignments,private$ItemInfo)
        TopicScores[[i]] = private$Results[[i]]$getTopicScores()
        TopicSummary[,names(private$Results)[i]] = private$Results[[i]]$getTopicSummary()
      }
      
      
      TopicScores = data.table::rbindlist(TopicScores)
      for(i in TopicNames){
        itemset = private$TopicAlignments[,i]
        totalpoints = sum(private$TopicAlignments$Value[itemset])
        TopicSummary$`All Classes`[rownames(TopicSummary) == i] = mean(unlist(TopicScores[,i, with = F]))
      }
      private$TopicSummary = TopicSummary
    }, # /setTopicSummary
    
    setComparison = function(){
      
      d2 = openxlsx::read.xlsx(xlsxFile = private$ComparisonLocation, 
                               sheet = "Overall Comparison", 
                               startRow = 2, 
                               colNames = F)
      CompHeader = d2[1:8,-1]
      row.names(CompHeader) = CompHeader[,1]
      CompHeader = CompHeader[,2*(1:(ncol(CompHeader)/2))]
      CompHeader = CompHeader[1:nrow(CompHeader),apply(X = !is.na(CompHeader), MARGIN = 2, FUN = any), drop = FALSE]
      if(ncol(CompHeader)>0){
        Comparisons = vector(mode = "list", length = ncol(CompHeader))
        
        d3 = openxlsx::read.xlsx(xlsxFile = private$ComparisonLocation, 
                                 sheet = "Topic Comparison", 
                                 startRow = 4, 
                                 colNames = T)
        i = 1
        for(i in 1:ncol(CompHeader)){
          
          Comparisons[[i]] = COMPARISON$new()
          Comparisons[[i]]$setDescription(DescriptionLookup$Description[DescriptionLookup$Year == CompHeader[5,i]])
          Comparisons[[i]]$setSummary(CompHeader[,i], row.names(CompHeader))
          ItemComparisons = magrittr::set_colnames(
            d2[-c(1:9),c(1,(2*i),(1+2*i))],
            c("This test item", "Prior test item","Prior test score"))
          ItemComparisons$`Prior test score` = as.numeric(ItemComparisons$`Prior test score`)
          ItemComparisons$Higher = private$ItemScores > ItemComparisons[,3] + 0.1
          ItemComparisons$Lower =  private$ItemScores < ItemComparisons[,3] - 0.1
          Comparisons[[i]]$setItemComparisons(ItemComparisons)
          
          # If there is a topic comparison:
          if(nrow(d3) != 0){
            TopicComparisons = d3[,c(1,i+1)]
            TopicComparisons$Higher = private$TopicSummary$`All Classes` > TopicComparisons[,2] + 0.1
            TopicComparisons$Lower = private$TopicSummary$`All Classes` < TopicComparisons[,2] - 0.1
            Comparisons[[i]]$setTopicComparisons(TopicComparisons)
          }
          
          # If there is an overall comparison:
          if(!is.na(CompHeader[1,i])){ 
            #use t.test2 here
            tTestSummary = t.test2(
              m1 = private$Summary$Average, 
              m2 = as.numeric(CompHeader[1,i]), 
              s1 = private$Summary$SD, 
              s2 = as.numeric(CompHeader[2,i]), 
              n1 = private$Summary$N, 
              n2 = as.numeric(CompHeader[3,i])
            )
            Comparisons[[i]]$setGrowth(tTestSummary$`Difference of means`)
            Comparisons[[i]]$setTtest(tTestSummary$t)
            Comparisons[[i]]$setPvalue(tTestSummary$`p-value`)
            significance = "not a significant difference, and is probably due to chance."
            if(tTestSummary$`p-value`<0.1){
              significance = "a somewhat significant difference, and could be due to chance."
            }
            if(tTestSummary$`p-value`<0.05){
              significance = "a very significant difference, and is unlikely to be due to chance."
            }
            if(tTestSummary$`p-value`<0.01){
              significance = "an extremely significant difference, and could not be due to chance."
            }
            Comparisons[[i]]$setSignificance(significance)
          }
        }
        private$Comparison = Comparisons
      }
    }, # /setComparison
    
    setNarrative = function(){
      narrative = paste0("Here are your scores and analysis for **", private$TestName,"**.  ")
      narrative = c(narrative,"", "* The score distribution ")
      # If there are check key items, add the line
      if(sum(private$ItemSummary$CheckKey) > 0){
        x = "* **Check the answer key for the following question"
        if(sum(private$ItemSummary$CheckKey) > 1){
          x = paste0(x,"s")
        } 
        x = paste0(x,": ", VectorSentence(private$ItemSummary$ItemName,private$ItemSummary$CheckKey), "**")
        narrative = c(narrative, x)
      }
      # If there are powerful distractors, add the line
      if(sum(private$ItemSummary$PowerDistrators) > 0){
        x = "* The following question"
        if(sum(private$ItemSummary$PowerDistrators) > 1){
          x = paste0(x, "s")
        }
        x = paste0(x, 
                   " had powerful distractors: ", 
                   VectorSentence(as.character(private$ItemSummary$ItemName), private$ItemSummary$PowerDistrators), 
                   ".  Looking at those wrong answers might help you understand where students are making mistakes.")
        narrative = c(narrative, x)
      }
      # If there are overthinking items, add the line
      if(sum(private$ItemSummary$OverThinking) > 0){
        x = "* The following question"
        if(sum(private$ItemSummary$OverThinking) > 1){
          x = paste0(x, 
                     "s were missed just as often by your high scoring students as your low scoring students.  ",
                     "This indicates that they are potential overthinking questions: ")
        } else {
          x = paste0(x, 
                     " was missed just as often by your high scoring students as your low scoring students.  ",
                     "This indicates that it is a potential overthinking question: ")
        }
        x = paste0(x, VectorSentence(private$ItemSummary$ItemName, private$ItemSummary$OverThinking), ".")
        narrative = c(narrative, x)
      }
      # If there are checkdifficult items, add the line
      if(sum(private$ItemSummary$Difficult) > 0){
        x = "* Your students found the following question"
        if(sum(private$ItemSummary$Difficult) > 1){
          x = paste0(x, "s")
        } 
        x = paste0(x, 
                   " very difficult: ",
                   VectorSentence(private$ItemSummary$ItemName, private$ItemSummary$Difficult), 
                   ".")
        narrative = c(narrative, x)
      }
      # If there are easy items, add the line
      if(sum(private$ItemSummary$Easy) > 0){
        x = "* Your students found the following question"
        if(sum(private$ItemSummary$Easy) > 1){
          x = paste0(x, "s")
        } 
        x = paste0(x, " very easy: ",VectorSentence(private$ItemSummary$ItemName, private$ItemSummary$Easy), ".")
        narrative = c(narrative, x)
      }
      # If there are wheat from chaff items, add the line
      if(sum(private$ItemSummary$WheatFromChaff) > 0){
        a = "  Those are very difficult, but the best students get them right."
        x = paste0("* ",VectorSentence(private$ItemSummary$ItemName, private$ItemSummary$WheatFromChaff))
        if(sum(private$ItemSummary$WheatFromChaff) > 1){
          x = paste0(x, " might be wheat from chaff questions.", a)
        } else {
          x = paste0(x, " might be a wheat from chaff question.", a)
        }
        narrative = c(narrative, x)
      }
      # If there are highly related items, add the line
      if(sum(private$ItemSummary$HighlyRelated) > 0){
        x = "* The highly related item"
        if(sum(private$ItemSummary$HighlyRelated) > 1){
          x = paste0(x, "s were ")
        } else {
          x = paste0(x, " was ")
        }
        x = paste0(x, 
                   VectorSentence(private$ItemSummary$ItemName, private$ItemSummary$HighlyRelated), 
                   ".  Those are questions to keep, since they are good indicators of student knowledge.")
        narrative = c(narrative, x)
      }
      # Add lines for boxplots and topics
      narrative = c(narrative, "* Boxplots", "* Topics")
      # Add sections for the comparisons, if they exist
      if(length(private$Comparison) > 0){
        for(i in length(private$Comparison):1){
          ItemComparisons = private$Comparison[[i]]$getItemComparisons()
          TopicComparisons = private$Comparison[[i]]$getTopicComparisons()
          desc = private$Comparison[[i]]$getDescription()
          growth = private$Comparison[[i]]$getGrowth()
          if(growth > 0){
            growthDirection = "higher"
          } else {
            growthDirection = "lower"
          }
          growth = abs(round(growth))
          if(growth == 1){
            growthDirection = paste0(" point ", growthDirection)
          } else {
            growthDirection = paste0(" points ", growthDirection)
          }
          x = paste0("* Compared to ", 
                     desc, 
                     " your students scored about ", 
                     growth, 
                     growthDirection, 
                     " on average.  This is ", 
                     private$Comparison[[i]]$getSignificance())
          narrative = c(narrative, x)
          
          if(sum(ItemComparisons$Higher)>0){
            x = paste0("    * Compared to ", 
                       desc, 
                       ", your students did noticeably better on question")
            if(sum(ItemComparisons$Higher)>1){
              x = paste0(x, "s")
            }
            x = paste0(x, 
                       " ",
                       VectorSentence(ItemComparisons$`This test item`, ItemComparisons$Higher))
            narrative = c(narrative, x)
          }
          
          if(sum(ItemComparisons$Lower)>0){
            x = paste0("    * Compared to ", 
                       desc, 
                       ", your students did noticeably worse on question")
            if(sum(ItemComparisons$Lower)>1){
              x = paste0(x, "s")
            }
            x = paste0(x, 
                       " ",
                       VectorSentence(ItemComparisons$`This test item`, ItemComparisons$Lower))
            narrative = c(narrative, x)
          }
          
          if(sum(TopicComparisons$Higher)>0){
            x = paste0("    * Compared to ", 
                       desc, 
                       ", your students did noticeably better on ", 
                       VectorSentence(TopicComparisons$Topic, TopicComparisons$Higher))
            narrative = c(narrative, x)
          }
          
          if(sum(TopicComparisons$Lower)>0){
            x = paste0("    * Compared to ", 
                       desc, 
                       ", your students did noticeably worse on ", 
                       VectorSentence(TopicComparisons$Topic, TopicComparisons$Lower))
            narrative = c(narrative, x)
          }
        }
      }
      # Add the closing line
      narrative = c(narrative,"", "Let me know if you need anything else.")
      private$Narrative = narrative
    },# /setNarrative
    
    exportNarrative = function(){
      fileConn <- file(paste0(private$DataLocation,"\\narrative.Rmd"))
      writeLines(c("---",'title: "Report Narrative"', "output: html_document",
                   "---","   ",private$Narrative), 
                 fileConn)
      close(fileConn)
      rmarkdown::render(input = paste0(private$DataLocation,"\\narrative.Rmd"),
                        output_format = "html_document", 
                        output_file = "narrative.html", 
                        output_dir = private$DataLocation,
                        quiet = T)
    }, # /exportNarrative
    
    exportReport = function(filename = "scores.xlsx"){
      wb1 = loadWorkbook2(file = system.file("extdata", 
                                             "template", 
                                             package = "rrttReportBuilder"), 
                          isUnzipped = T)
      for(i in 1:length(private$Results)){
        openxlsx::writeData(wb = wb1, 
                            sheet = "Responses", 
                            x = names(private$Results)[i], 
                            startCol = 1, 
                            startRow = 100*(i-1) + 1)
        openxlsx::writeData(wb = wb1, 
                            sheet = "Responses", 
                            x = private$Results[[i]]$getItemResponses(), 
                            startCol = 1, 
                            startRow = 100*(i-1) + 2)
      }
      
      for(i in 1:length(private$Results)){
        openxlsx::writeData(wb = wb1, 
                            sheet = "ItemScores", 
                            x = names(private$Results)[i], 
                            startCol = 1, 
                            startRow = 100*(i-1) + 1)
        openxlsx::writeData(wb = wb1, 
                            sheet = "ItemScores", 
                            x = private$Results[[i]]$getItemResponseScores(), 
                            startCol = 1, 
                            startRow = 100*(i-1) + 2)
      }
      
      openxlsx::writeData(wb = wb1, 
                          sheet = "ItemInfo", 
                          x = private$ItemInfo)
      openxlsx::writeData(wb = wb1, 
                          sheet = "Summary", 
                          x = data.frame(private$Summary), 
                          startRow = 1)
      openxlsx::writeData(wb = wb1, 
                          sheet = "Upload", 
                          x = private$UploadTab, 
                          startRow = 1)
      openxlsx::writeData(wb = wb1, 
                          sheet = "TopicAlignments", 
                          x = self$getTopicAlignments(), 
                          startRow = 1)
      openxlsx::writeData(wb = wb1, 
                          sheet = "TopicSummary", 
                          x = private$TopicSummary, 
                          startRow = 1, 
                          rowNames = T)
      ItemSummary = data.frame(private$Narrative)
      ItemSummary = ItemSummary[4:(nrow(ItemSummary)-2),,drop=F]
      ItemSummary = ItemSummary[!(ItemSummary[,1] %in% c("* Boxplots", "* Topics")),,drop=F]
      openxlsx::writeData(wb = wb1, 
                          sheet = "Item_Summary", 
                          x = ItemSummary, 
                          startRow = 3, 
                          colNames = F)
      
      for(i in 1:length(private$Results)){
        openxlsx::writeData(wb = wb1, 
                            sheet = "TopicScores", 
                            x = names(private$Results)[i], 
                            startCol = 1, 
                            startRow = 100*(i-1) + 1)
        openxlsx::writeData(wb = wb1, 
                            sheet = "TopicScores", 
                            x = private$Results[[i]]$getTopicScores(), 
                            startCol = 1, 
                            startRow = 100*(i-1) + 2)
      }
      
      openxlsx::writeData(wb = wb1, 
                          sheet = "Raw Handout Data", 
                          x = private$Handouts, 
                          startRow = 1)
      
      numberOfComparisons = length(private$Comparison)
      if(numberOfComparisons > 0){
        
        for(i in numberOfComparisons:1){
          colShift = 14*(numberOfComparisons - i)
          CompSummary = private$Comparison[[i]]$getSummary()
          openxlsx::writeData(wb = wb1, sheet = "Comparison", x = as.numeric(CompSummary$Total), 
                              startCol = 3 + colShift, startRow = 4)
          openxlsx::writeData(wb = wb1, sheet = "Comparison", x = as.numeric(CompSummary$sd), 
                              startCol = 3 + colShift, startRow = 5)
          openxlsx::writeData(wb = wb1, sheet = "Comparison", x = as.numeric(CompSummary$n), 
                              startCol = 3 + colShift, startRow = 6)
          openxlsx::writeData(wb = wb1, sheet = "Comparison", x = CompSummary$`Compare test:`, 
                              startCol = 11 + colShift, startRow = 3)
          openxlsx::writeData(wb = wb1, sheet = "Comparison", x = CompSummary$`Year:`, 
                              startCol = 11 + colShift, startRow = 4)
          openxlsx::writeData(wb = wb1, sheet = "Comparison", x = CompSummary$`Taken by:`, 
                              startCol = 11 + colShift, startRow = 5)
          openxlsx::writeData(wb = wb1, sheet = "Comparison", x = CompSummary$`Compari-bility:`, 
                              startCol = 11 + colShift, startRow = 6)
          openxlsx::writeData(wb = wb1, sheet = "Comparison", 
                              x = as.numeric(private$Comparison[[i]]$getGrowth()), 
                              startCol = 3 + colShift, startRow = 8)
          Items = private$Comparison[[i]]$getItemComparisons()
          openxlsx::writeData(wb = wb1, sheet = "Comparison", x = Items$`Prior test item`, 
                              startCol = 1 + colShift, startRow = 14, colNames = F)
          openxlsx::writeData(wb = wb1, sheet = "Comparison", x = Items$`Prior test score`, 
                              startCol = 3 + colShift, startRow = 14, colNames = F)
          openxlsx::writeData(wb = wb1, sheet = "Comparison", x = private$Comparison[[i]]$getSignificance(), 
                              startCol = 3 + colShift, startRow = 9)
          Topics = private$Comparison[[i]]$getTopicComparisons()
          openxlsx::writeData(wb = wb1, sheet = "Comparison", x = Topics$Average.score, 
                              startCol = 10 + colShift, startRow = 14, colNames = F)
        }
      }
      openxlsx::saveWorkbook(wb1, paste0(private$DataLocation,"\\",filename))
    }, # /exportReport method
    
    setTopicScores = function(){
      #establish a list that will hold the Topic Scores data.frames
      TopicScores = vector(mode = "list", length = length(private$Results))
      #pull the topic scores for each section and load them in the list
      for(i in 1:length(private$Results)){
        TopicScores[[i]] = private$Results[[i]]$getTopicScores()
      }
      #make a single data.table with all of the item response scores from all of the sections
      TopicScores = data.table::rbindlist(TopicScores) 
      private$TopicScores = TopicScores
    }, # /setTopicScores method
    
    getTopicScores = function(){return(private$TopicScores)},
    
    setHandouts = function(){
      ItemInfo = private$ItemInfo
      ItemResponseScores = private$ItemResponseScores
      ItemResponses = self$getResponses()
      Handouts = as.data.frame(ItemResponses, stringsAsFactors = F)[,1:5]
      colnames(ItemResponseScores) = paste0(colnames(ItemResponseScores),"_Score")
      colnames(ItemResponses) = paste0(colnames(ItemResponses),"_Response")
      
      for(i in 7:ncol(ItemResponses)){
        ItemResponseScores[,i] = ItemResponseScores[,i]/ItemInfo$Value[i-6]
        Handouts = cbind.data.frame(Handouts, 
                                    ItemResponses[,i, drop = F], 
                                    stringsAsFactors = F)
        Handouts = cbind.data.frame(Handouts, 
                                    ItemResponseScores[,i, drop = F], 
                                    stringsAsFactors = F)
      } # /for
      
      topicNames = row.names(private$TopicSummary)
      for(i in 1:length(topicNames)){
        Handouts = cbind.data.frame(Handouts, 
                                    NA, 
                                    stringsAsFactors = F)
        Handouts = cbind.data.frame(Handouts, 
                                    private$TopicScores[,topicNames[i], drop = F], 
                                    stringsAsFactors = F)
      } # /for
      
      private$Handouts = Handouts
    }, # /setHandouts method
    
    exportUploads = function(){
      write.csv(x = private$UploadTab, 
                file = paste0(private$DataLocation,"\\","upload.csv"), 
                row.names = F)
    } # /exportUploads method
    
  ) # /public
) # /REPORT R6 class
