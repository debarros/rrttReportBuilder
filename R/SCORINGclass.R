#SCORINGclass.R

#An instance of this class the set of rules necessary to apply special scoring
#Each instance should be held within an instance of the REPORT class

SCORING = R6Class(
  
  classname = "SCORING",
  
  private = list(
    IsSpecial = NULL, # logical, is there special scoring?
    IsSubset = NULL, # logical, is there subset scoring?
    SubsetAlign = NULL, # data.frame aligning items to subsets and providing their weights in the subsets
    SubsetSetup = NULL, # data.frame showing what type of scoring to apply to the subset and the weight of each in calculating the overall score
    OverallSetup = NULL # data.frame showing what type of scoring to apply to the overall score
  ), # /private
  
  public = list(
    initialize = function(x){ # x is a dataframe with 1 column and 2 rows taking values "Yes" or "No"
      private$IsSpecial = x[1,1] == "Yes"
      private$IsSubset = x[2,1] == "Yes"
    },
    
    setSubsetAlign = function(x){
      x$`Subset weight` = as.numeric(x$`Subset weight`)
      private$SubsetAlign = x
    },
    
    setSubsetSetup = function(x){
      x$`Score weight` = as.numeric(x$`Score weight`)
      private$SubsetSetup = x
    },
    
    setOverallSetup = function(x){
      private$OverallSetup = x
    },
    
    checkSpecial = function(){private$IsSpecial},
    checkSubset = function(){private$IsSubset},
    getSubsetAlign = function(){private$SubsetAlign},
    getSubsetSetup = function(){private$SubsetSetup},
    getOverallSetup = function(){private$OverallSetup}
    
  ) # /public
) # /class definition
