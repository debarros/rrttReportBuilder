COMPARISON = R6Class(
  
  classname = "COMPARISON", 
  
  private = list(
    Description = "last year", #description of the relationship to the prior year test
    Summary = list(), #info in the header of the Overall Comparison tab for 1 test
    ItemComparisons = NA, #data.frame showing the item correspondences and scores in prior year (lower part of Overall Comparison tab)
    TopicComparisons = NA, #data.frame showing the prior year scores on each of the topics
    HigherItems = list(), #list of the item names where the current test had noticeably higher scores than the prior test
    LowerItems = list(), #list of the item names where the current test had noticeably lower scores than the prior test
    HigherTopics = list(), #list of the topics where the current test had noticeably higher scores than the prior test
    LowerTopics = list(), #list of the topics where the current test had noticeably lower scores than the prior test
    Growth = NA_real_, #current year average - prior year average
    Ttest = NA_real_, #value of the t test for mean differences using the df adjustment for unequal variances
    Pvalue = NA_real_, #p value associated with the t test stat
    Significance = NA_character_ #description of the signficance of the t test
  ),
  
  public = list(
    initialize = function(){},
    setDescription = function(x){private$Description= x},
    setSummary = function(x){private$Summary= x},
    setItemComparisons = function(x){private$ItemComparisons= x},
    setTopicComparisons = function(x){private$TopicComparisons= x},
    setHigherItems = function(x){private$HigherItems= x},
    setLowerItems = function(x){private$LowerItems= x},
    setHigherTopics = function(x){private$HigherTopics= x},
    setLowerTopics = function(x){private$LowerTopics= x},
    setGrowth = function(x){private$Growth= x},
    setTtest = function(x){private$Ttest= x},
    setPvalue = function(x){private$Pvalue= x},
    setSignificance = function(x){private$Significance= x},
    getDescription = function(){return(private$Description)},
    getSummary = function(){return(private$Summary)},
    getItemComparisons = function(){return(private$ItemComparisons)},
    getTopicComparisons = function(){return(private$TopicComparisons)},
    getHigherItems = function(){return(private$HigherItems)},
    getLowerItems = function(){return(private$LowerItems)},
    getHigherTopics = function(){return(private$HigherTopics)},
    getLowerTopics = function(){return(private$LowerTopics)},
    getGrowth = function(){return(private$Growth)},
    getTtest = function(){return(private$Ttest)},
    getPvalue = function(){return(private$Pvalue)},
    getSignificance = function(){return(private$Significance)},
  )
  
)