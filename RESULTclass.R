RESULT = R6Class(
  
  classname = "RESULT",
  
  private = list(
    ItemResponses = NA, #data.frame
    SectionName = NA_character_, #atomic character with the name of the section
    ItemScores = NA, #data.frame
    Summary = NA, #not sure.  It will be the stuff that shows up to the right on the Scores tab
    TopicScores = NA, #data.frame with one row per student and one column per topic
    TopicSummary = NA, #data.frame representing one column from the Topic Chart Calculation tab and part of one row from the Topics tab
    DropScores = NA #data.frame holding students' total points after dropping an item
  ),
  
  public = list(
    initialize = function(){}
  )
  
)