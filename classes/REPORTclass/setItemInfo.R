#setItemInfo.R

if(is.null(private$Sources)){
  return("Need sources first.")
} else {
  ItemInfo = read.csv(private$Sources[1], skip = 4, header = F, nrows = 3, stringsAsFactors = F)[,-(1:5)]  #get the basic item info
  ItemInfo =  set_rownames(setNames(as.data.frame(t(ItemInfo), stringsAsFactors = F), c("ItemName", "Value", "Answer")), NULL) #fix the iteminfo data.frame setup
  toFix = grepl(pattern = "[^a-zA-Z\\d\\s:]", x = ItemInfo$Answer) #which items have weird values in the Answer field?
  ItemInfo$Answer[toFix] = ItemInfo$Value[toFix] #Set those answers to just be the value of the respective questions
  ItemInfo$Value = as.numeric(ItemInfo$Value) #Set the value column to be numeric
  ItemInfo$AverageScore = NA_real_  
  ItemInfo$Correlation = NA_real_
  ItemInfo$Type = NA_character_
  ItemInfo$options = NA_integer_
  private$ItemInfo = ItemInfo
}