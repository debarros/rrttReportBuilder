# Load Special Scoring

loadSpecialScoring.REPORT = function(report){
  # Determine whether there is student specific scoring
  CheckStudentScoring = openxlsx::read.xlsx(
    xlsxFile = report$.__enclos_env__$private$ComparisonLocation, 
    sheet = "Scoring by Student", 
    rows = 3:4, cols = 2:3,
    colNames = F, rowNames = T)
  
  if(CheckStudentScoring[1,1] == "Yes"){ # if there is student specific scoring,
    
    report$.__enclos_env__$private$HasSpecialScoring = TRUE
    report$.__enclos_env__$private$HasStudentScoring = TRUE
    
    # Load the table showing which special scoring to use for each student
    StudentRuleTable = openxlsx::read.xlsx(
      xlsxFile = report$.__enclos_env__$private$ComparisonLocation, 
      sheet = "Scoring by Student", 
      startRow = 6, cols = 2:3,
      colNames = T)
    report$.__enclos_env__$private$SpecialScoringTable = StudentRuleTable
    
    # Get the set of unique special scoring rules
    RuleTabs = unique(c(CheckStudentScoring[2,1], StudentRuleTable$Tab.Name))
    
    # Create a list to hold the special scoring rules
    SpecialScoring = setNames(vector(mode = "list", length = length(RuleTabs)), RuleTabs)
    
  }  else { # If there is no student specific special scoring
    
    report$.__enclos_env__$private$HasStudentScoring = FALSE
    
    CheckSpecialScoring = openxlsx::read.xlsx(
      xlsxFile = report$.__enclos_env__$private$ComparisonLocation, 
      sheet = CheckStudentScoring[2,1], 
      rowNames = T, colNames = F,
      rows = 3:4, cols = 2:3)
    
    if(CheckSpecialScoring[1,1] == "Yes"){ # if there is special scoring,
      report$.__enclos_env__$private$HasSpecialScoring = TRUE
      # Create the special scoring list with only 1 entry
      SpecialScoring = setNames(vector(mode = "list", length = 1), CheckStudentScoring[2,1])  
    } else { # if there is no special scoring
      report$.__enclos_env__$private$HasSpecialScoring = FALSE
    } # /if-else special scoring
    
  } # /if-else student specific scoring
  
  # If there is any special scoring, load the special scoring rules
  if(report$.__enclos_env__$private$HasSpecialScoring){
    
    for(i in 1:length(SpecialScoring)){
      
      # set up the list that holds the special scoring info
      CurrentSpecial = SCORING$new(openxlsx::read.xlsx(
        xlsxFile = report$.__enclos_env__$private$ComparisonLocation, 
        sheet = names(SpecialScoring)[i], 
        rowNames = T, colNames = F,
        rows = 3:4, cols = 2:3))
      
      # Second element: which items are in which subsets, and what are their weights
      CurrentSpecial$setSubsetAlign(as.data.frame(
        t(openxlsx::read.xlsx(
          xlsxFile = report$.__enclos_env__$private$ComparisonLocation, 
          sheet = names(SpecialScoring)[i], 
          rowNames = T, colNames = F,
          rows = 8:10)), 
        stringsAsFactors = F))
      
      # Third element: for each subset, what function should be applied, and what is its weight?
      CurrentSpecial$setSubsetSetup(as.data.frame(
        t(openxlsx::read.xlsx(
          xlsxFile = report$.__enclos_env__$private$ComparisonLocation, 
          sheet = names(SpecialScoring)[i], 
          rowNames = T, colNames = F,
          rows = 14:21)),
        stringsAsFactors = F))
      
      # Fourth element: special scoring function to apply to the overall score
      CurrentSpecial$setOverallSetup(as.data.frame(
        t(openxlsx::read.xlsx(
          xlsxFile = report$.__enclos_env__$private$ComparisonLocation, 
          sheet = names(SpecialScoring)[i], 
          rowNames = T, colNames = F,
          rows = 25:30, cols = 2:3)), 
        stringsAsFactors = F))
      
      SpecialScoring[[i]] = CurrentSpecial
      
    } # /for each special scoring tab
    
    report$.__enclos_env__$private$SpecialScoring = SpecialScoring  
    
  } # /if there is special scoring
} # /function
