# exportUpdate_REPORT

exportUpdate.REPORT = function(uploadFilePath, report) {
      #get the existing scores and add an identifying code
      ExistingScores = read.csv(uploadFilePath, stringsAsFactors = F)
      ExistingScores$code = apply(X = ExistingScores, MARGIN = 1, FUN = paste0, collapse = "-")
      
      #get all scores and add an identifying code
      AllScores = report$.__enclos_env__$private$UploadTab
      AllScores$code = apply(X = AllScores, MARGIN = 1, FUN = paste0, collapse = "-")
      
      #get the subset of scores that are new or different
      NewScores = AllScores[!(AllScores$code %in% ExistingScores$code),]
      
      if(nrow(NewScores) > 0){ #If there are new scores, write a new score update
        #Pad student names to make them the same length
        maxName = max(nchar(NewScores$StudentName)) 
        NewScores$StudentName = stringr::str_pad(string = NewScores$StudentName, width = maxName, side = "right")
        
        out = paste0("Updated scores for ",report$.__enclos_env__$private$TestName,":  \n") #Put in the header line for the score update email
        for(i in 1:nrow(NewScores)){ #for each score append a line with that student's name, id, and score
          out = paste0(out, "\t",NewScores$StudentName[i], "\t", NewScores$StudentID[i], "\t", NewScores$Percentage[i], "  \n")
        } # /for loop
        #write the score update emails to a text file
        write(out, paste0(report$.__enclos_env__$private$DataLocation,"\\","ScoreUpdates.txt"))  
      } # /if
}