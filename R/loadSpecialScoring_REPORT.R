# Load Special Scoring

loadSpecialScoring.REPORT = function(report){
  
  # pull the needed info from the report
  ComparisonLocation = report$getComparisonLocation()
  
  # initialize variables
  HasSpecialScoring = F
  HasStudentScoring = F
  SpecialScoring = NULL
  StudentRuleTable = NULL
  
  # Determine whether there is student specific scoring
  CheckStudentScoring = openxlsx::read.xlsx(
    xlsxFile = ComparisonLocation, sheet = "Scoring by Student", 
    rows = 3:4, cols = 2:3, colNames = F, rowNames = T)
  
  if(CheckStudentScoring[1,1] == "Yes"){                                      # if there is student specific scoring,
    HasSpecialScoring = T                                                     # Set HasSpecialScoring to True
    HasStudentScoring = T                                                     # Set HasStudentScoring to True
    StudentRuleTable = openxlsx::read.xlsx(                                   # Load the table showing which special scoring to use for each student
      xlsxFile = ComparisonLocation, sheet = "Scoring by Student", 
      startRow = 6, cols = 2:3, colNames = T)
    RuleTabs = unique(c(CheckStudentScoring[2,1], StudentRuleTable$Tab.Name)) # Get the set of unique special scoring rules
    SpecialScoring = vector(mode = "list", length = length(RuleTabs))         # Create a list to hold the special scoring rules
    SpecialScoring = setNames(SpecialScoring, RuleTabs) 
    
  }  else {                                                               # If there is no student specific special scoring
    HasStudentScoring = FALSE                                             # Set HasStudentScoring to False
    CheckSpecialScoring = openxlsx::read.xlsx(                            # Look for special scoring
      xlsxFile = ComparisonLocation, sheet = CheckStudentScoring[2,1], 
      rowNames = T, colNames = F,rows = 3:4, cols = 2:3)
    if(CheckSpecialScoring[1,1] == "Yes"){                                # if there is special scoring,
      HasSpecialScoring = TRUE                                            # set HasSpecialScoring to True
      SpecialScoring = vector(mode = "list", length = 1)                  # Create the special scoring list with only 1 entry
      SpecialScoring = setNames(SpecialScoring, CheckStudentScoring[2,1])  
    } else {                                                              # if there is no special scoring
      HasSpecialScoring = FALSE                                           # Set HasSpecialScoring to False
    } # /if-else special scoring
  } # /if-else student specific scoring
  
  # If there is any special scoring, load the special scoring rules
  if(HasSpecialScoring){
    
    for(i in 1:length(SpecialScoring)){
      
      # set up the list that holds the special scoring info
      CurrentSpecial = SCORING$new(openxlsx::read.xlsx(
        xlsxFile = ComparisonLocation, sheet = names(SpecialScoring)[i], 
        rowNames = T, colNames = F, rows = 3:4, cols = 2:3))
      
      # Second element: which items are in which subsets, and what are their weights
      CurrentSpecial$setSubsetAlign(as.data.frame(
        t(openxlsx::read.xlsx(
          xlsxFile = ComparisonLocation, sheet = names(SpecialScoring)[i], 
          rowNames = T, colNames = F, rows = 8:10)), 
        stringsAsFactors = F))
      
      # Third element: for each subset, what function should be applied, and what is its weight?
      CurrentSpecial$setSubsetSetup(as.data.frame(
        t(openxlsx::read.xlsx(
          xlsxFile = ComparisonLocation, sheet = names(SpecialScoring)[i], 
          rowNames = T, colNames = F, rows = 14:21)),
        stringsAsFactors = F))
      
      # Fourth element: special scoring function to apply to the overall score
      CurrentSpecial$setOverallSetup(as.data.frame(
        t(openxlsx::read.xlsx(
          xlsxFile = ComparisonLocation, sheet = names(SpecialScoring)[i], 
          rowNames = T, colNames = F, rows = 25:30, cols = 2:3)), 
        stringsAsFactors = F))
      
      # Fifth element: if there are any lookup tables needed, load them.
      if(CurrentSpecial$checkLookups()){
        CurrentSpecial$setLookups(ComparisonLocation)
      }
      
      # Store the current SpecialScoring in the list
      SpecialScoring[[i]] = CurrentSpecial
      
    } # /for each special scoring tab
  } # /if there is special scoring
  
  report$setSpecialScoring(SpecialScoring)
  report$setHasSpecialScoringQuick(HasSpecialScoring)
  report$setHasStudentScoringQuick(HasStudentScoring)
  report$setSpecialScoringTableQuick(StudentRuleTable)
  
} # /function
