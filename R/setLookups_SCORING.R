# setLookups_SCORING.R

setLookups.SCORING = function(ComparisonLocation, scoring, messageLevel = 0){
 
  OvFuncs = scoring$getOverallSetup()       # Get all the scoring functions used in the overall setup on this test
  SubFuncs = scoring$getSubsetSetup()       # Get all the scoring functions used in the subset setup on this test
  LookFuncs = scoring$getLookupFunctions()  # Get the names of all the functions that require lookup tables
  
  OvFuncs = OvFuncs[OvFuncs$`Score function` %in% LookFuncs,]     # Get any lookup functions in the overall setup
  OvLookTabs = unlist(OvFuncs$`Score parameter 1`)                # Get the names of the tabs containing the lookup tables
  
  SubFuncs = SubFuncs[SubFuncs$`Subset function` %in% LookFuncs,] # Get any lookup functions in the subset setup
  SubLookTabs = unlist(SubFuncs$`Subset parameter 1`)             # Get the names of the tabs containing the lookup tables
  
  allLookupTabs = unique(c(OvLookTabs, SubLookTabs))              # Get the names of all tabs with lookup tables
  
  Lookups = vector(mode = "list", length = length(allLookupTabs)) # Setup a list to hold the lookup tables
  names(Lookups) = allLookupTabs                                  # Name the elements by the names of tabs with the tables
  for(i in 1:length(Lookups)){                                    # For each lookup table,
    x = openxlsx::read.xlsx(ComparisonLocation, allLookupTabs[i]) # Load the lookup table
    Lookups[[allLookupTabs[i]]] = x                               # Put it in the list
  } # /for
  
  scoring$setLookupsQuick(Lookups)                                # Add the list of lookup tables to the scoring object
   
} # /function
