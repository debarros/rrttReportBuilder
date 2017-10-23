#COMPARISONclass.R

#An instance of this class holds the summary comparison information for a prior administration of a single test
#Each instance should be held within an instance of the REPORT class.
#Some parts of it are just based on the prior test.
#Other parts are actual comparisons, based on both the prior test and all of the results

COMPARISON = R6Class(
  
  classname = "COMPARISON", 
  
  private = list(
    Description = "last year", # description of the relationship to the prior year test
    Growth = NULL,           # current year average - prior year average (or NULL if no overal comparison is wanted)
    ItemComparisons = NULL,  # data.frame showing the item correspondences and scores in prior year (lower part of Overall Comparison tab) and whether this year was higher or lower
    Pvalue = NULL,           # p value associated with the t test stat
    Significance = NULL,     # description of the signficance of the t test
    Summary = list(),        # info in the header of the Overall Comparison tab for 1 test
    TopicComparisons = NULL, # data.frame showing the prior year scores on each of the topics and whether this year was higher or lowee
    Ttest = NULL             # value of the t test for mean differences using the df adjustment for unequal variances
  ), #private
  
  public = list(
    initialize = function(){},
    setDescription = function(x){private$Description= x},
    
    setSummary = function(VAL, NAM){
      VAL = as.list(VAL)
      names(VAL) = NAM      
      private$Summary= VAL
    },
    
    setItemComparisons = function(x){private$ItemComparisons= x},
    setTopicComparisons = function(x){private$TopicComparisons= x},
    setGrowth = function(x){private$Growth= x},
    setTtest = function(x){private$Ttest= x},
    setPvalue = function(x){private$Pvalue= x},
    setSignificance = function(x){private$Significance= x},
    
    getDescription = function(){return(private$Description)},
    getSummary = function(){return(private$Summary)},
    getItemComparisons = function(){return(private$ItemComparisons)},
    getTopicComparisons = function(){return(private$TopicComparisons)},
    getGrowth = function(){return(private$Growth)},
    getTtest = function(){return(private$Ttest)},
    getPvalue = function(){return(private$Pvalue)},
    getSignificance = function(){return(private$Significance)}
  ) #public
  
)