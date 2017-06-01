# setItemInfo_REPORT.R

setItemInfo.REPORT = function(report) {
      if(is.null(report$.__enclos_env__$private$Sources)){
        return("Need sources first.")
      } else {
        ItemInfo = read.csv(report$.__enclos_env__$private$Sources[1], 
                            skip = 4, 
                            header = F, 
                            nrows = 3, 
                            stringsAsFactors = F)[,-(1:5)]  #get the basic item info
        #fix the iteminfo data.frame setup
        ItemInfo =  magrittr::set_rownames(
          setNames(
            as.data.frame(t(ItemInfo), stringsAsFactors = F), 
            c("ItemName", "Value", "Answer")), 
          NULL
        ) 
        #which items have weird values in the Answer field?
        toFix = grepl(pattern = "[^a-zA-Z\\d\\s:]", x = ItemInfo$Answer) 
        #Set those answers to just be the value of the respective questions
        ItemInfo$Answer[toFix] = ItemInfo$Value[toFix] 
        ItemInfo$Value = as.numeric(ItemInfo$Value) #Set the value column to be numeric
        ItemInfo$AverageScore = NA_real_  
        ItemInfo$Correlation = NA_real_
        ItemInfo$Type = NA_character_
        ItemInfo$options = NA_integer_
        report$.__enclos_env__$private$ItemInfo = ItemInfo
      }
}