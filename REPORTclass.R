#classDefinitions.R

REPORT = R6Class(
  
  classname = "REPORT",
  
  private = list(
    dataLocation = NA_character_, #address of the folder where the exported csv's are
    comparisonLocation = NA_character_, #address and filename of the comparison and topic alignment
    sources = NA_character_, #character vector with the locations of the csv's
    TestName = NA_character_, #atomic character with the name of the test
    ItemInfo = NA, #data.frame with info about the items, will be used to build the breakdown tab
    UploadTab = NA, #data.frame that holds the stuff that goes in the upload tab
    results = list(), #list of objects of class RESULT
    TopicSummary = NA, #data.frame with stuff that would go on the Topic Chart Calculation tab
    Summary = NA, #not sure of the format.  Will be the overall stats from the Scores tab
    ItemSummary = NA, #not sure of the format.  Will be the info in the table at the top of the Item Summary tab
    Narrative = NA, #either atomic character or character vector.  Will be the text in cell A11 of the Item Summary tab
    Comparison = list(), #a list of objects of class COMPARISON
    Handouts = NA, #data.frame containing the information necessary to build the Handouts tab simply
    Correlations = NA #data.frame containing the itemscore/testscore correlations
  ),
  
  public = list(
    initialize = function(){}
  )
  
)










