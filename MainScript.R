#MainScript.R




#load the exported linkit student item response files ####

#Get the basic overview of stuff
#dataLocation = choose.dir(default = "J:/tests/2016-2017/")  #select the folder
currentReport = REPORT$new()
currentReport$setDataLocation("J:\\tests\\2016-2017\\Humanities\\H2\\week23 (2017-02-17) Cuba Africa Middle East\\exports")
currentReport$setSources()
currentReport$setTestName()
currentReport$setItemInfo()
currentReport$setResults()  #Get the actual results
currentReport$setComparisonLocation("J:/tests/2016-2017/Humanities/H2/week23 (2017-02-17) Cuba Africa Middle East/comparison and topic alignment.xlsx")
currentReport$enhanceItemInfo()
currentReport$addItemScores()
currentReport$addCorrelations()



#------------------------------------#
#response frequencies ####
#Set default values for the number of letter options and number of point options
topletter = 0
toppoint = 0

#Find the max number of letter and point options in the test
if("MC" %in% ItemInfo$Type){topletter = max(ItemInfo$options, na.rm = T)}
if("ER" %in% ItemInfo$Type){toppoint = max(ItemInfo$Value[ItemInfo$Type == "ER"])} 

#Find the max number of response columns needed and the number of columns that will represent both numbers and letters
totalset = max(topletter, toppoint+1)
overlapset = min(topletter, toppoint+1)

#Build the set of response column names
responseSet = paste0(
  c(LETTERS[1:topletter], rep("", times = totalset - topletter)),
  c(rep("/", times = overlapset), rep("", times = totalset - overlapset)),
  c(as.character(0:toppoint), rep("", times = totalset - (toppoint+1) )))

basecolumn = ncol(ItemInfo) #How many columns does ItemInfo have already?
ItemInfo[,responseSet] = "" #Initialize those columns

for(i in 1:nrow(ItemInfo)){ #for every item
  for(j in 1:ItemInfo$options[i]){ #for every possible response for that item
    if(ItemInfo$Type[i] == "ER"){ #if it's an ER item, count how many times that point level was awarded
      ItemInfo[i,j+basecolumn] = sum(allresults[,ItemInfo$ItemName[i], with = F] == j-1)
    } else { #if it's an MC item, count how many times that letter was used
      ItemInfo[i,j+basecolumn] = sum(allresults[,ItemInfo$ItemName[i], with = F] == LETTERS[j])
    }
  }
}
#------------------------------------#







#-----------------------------------#
#Create the upload tab ####
UploadTab = allresults[,c("StudentID")]
UploadTab$StudentName = paste0(allresults$LastName, ", ",allresults$FirstName)
UploadTab$Percentage = allresults$score
#-----------------------------------#