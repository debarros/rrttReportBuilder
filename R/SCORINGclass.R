#SCORINGclass.R

# An instance of this class holds the set of rules necessary to apply one special scoring system
# Each instance should be held within an instance of the REPORT class
# One report might have several special scoring objects

SCORING = R6Class(
  
  classname = "SCORING",
  
  private = list(
    IsSpecial = NULL,    # logical, is there special scoring?
    IsSubset = NULL,     # logical, is there subset scoring?
    Lookups = NULL,      # named list of data.frames holding the lookup tables (if they are needed)
    LookupFunctions = c("Lookup score", "Lookup table", "Regents curve"), # Which special scoring methods require a lookup table?
    OverallSetup = NULL, # data.frame showing what type of scoring to apply to the overall score
    SubsetAlign = NULL,  # data.frame aligning items to subsets and providing their weights in the subsets
    SubsetSetup = NULL   # data.frame showing what type of scoring to apply to the subset and the weight of each in calculating the overall score
  ), # /private
  
  public = list(
    initialize = function(x){ # x is a dataframe with 1 column and 2 rows taking values "Yes" or "No"
      private$IsSpecial = x[1,1] == "Yes"
      private$IsSubset = x[2,1] == "Yes"
    },
    
    
    setLookups = function(ComparisonLocation, scoring = self){setLookups.SCORING(ComparisonLocation, scoring)},
    
    
    setOverallSetup = function(x){private$OverallSetup = x},
    
    setSubsetAlign = function(x){
      x$`Subset weight` = as.numeric(x$`Subset weight`)
      private$SubsetAlign = x
    },
    
    setSubsetSetup = function(x){
      x$`Score weight` = as.numeric(x$`Score weight`)
      private$SubsetSetup = x
    },
    
    checkLookups = function(){
      ret = FALSE
      overallFunctions = unlist(private$OverallSetup$`Score function`)
      subsetFunctions = unlist(private$SubsetSetup$`Score function`)
      allFunctions = c(overallFunctions, subsetFunctions)
      if(length(intersect(private$LookupFunctions, allFunctions)) > 0){
        ret = TRUE
      }
      return(ret)
    },
    
    checkSpecial = function(){private$IsSpecial},
    checkSubset = function(){private$IsSubset},
    getLookupFunctions = function(){private$LookupFunctions},
    getLookups = function(){private$Lookups},
    getSubsetAlign = function(){private$SubsetAlign},
    getSubsetSetup = function(){private$SubsetSetup},
    getOverallSetup = function(){private$OverallSetup},
    
    setLookupsQuick = function(x){private$Lookups = x}
    
  ) # /public
) # /class definition
