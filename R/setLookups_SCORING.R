# setLookups_SCORING.R

setLookups.SCORING = function(ComparisonLocation, scoring){
 
  overallFunctions = scoring$getOverallSetup()
  subsetFunctions = scoring$getSubsetSetup()
  LookupFunctions = scoring$getLookupFunctions()
  
  overallFunctions = overallFunctions[overallFunctions$`Score function` %in% LookupFunctions,]
  overallLookupTabs = unlist(overallFunctions$`Score parameter 1`)
  
  subsetFunctions = subsetFunctions[subsetFunctions$`Score function` %in% LookupFunctions,]
  subsetLookupTabs = unlist(subsetFunctions$`Score parameter 1`)
  
  allLookupTabs = unique(c(overallLookupTabs, subsetLookupTabs))
  
  Lookups = vector(mode = "list", length = length(allLookupTabs))
  names(Lookups) = allLookupTabs
  for(i in 1:length(Lookups)){
    Lookups[[allLookupTabs[i]]] = openxlsx::read.xlsx(xlsxFile = ComparisonLocation, sheet = allLookupTabs[i])
  } # /for
  
  scoring$setLookupsQuick(Lookups)
   
}
