#MainScript.R
library(openxlsx)
library(magrittr)
library(data.table)
source("loadWorkbook.R")


#load the exported linkit student item response files ####

#Get the basic overview of stuff
dataLocation = choose.dir(default = "J:/tests/2016-2017/")  #select the folder
sources = list.files(dataLocation, full.names = T) #get a list of files
TestName = read.csv(sources[1], header = F, nrows = 1, stringsAsFactors = F)[1,2] #get the name of the test
ItemInfo = read.csv(sources[1], skip = 4, header = F, nrows = 3, stringsAsFactors = F)[,-(1:5)]  #get the basic item info
ItemInfo =  set_rownames(setNames(as.data.frame(t(ItemInfo), stringsAsFactors = F), c("ItemName", "Value", "Answer")), NULL) #fix the iteminfo data.frame setup
toFix = grepl(pattern = "[^a-zA-Z\\d\\s:]", x = ItemInfo$Answer) #which items have weird values in the Answer field?
ItemInfo$Answer[toFix] = ItemInfo$Value[toFix] #Set those answers to just be the value
ItemInfo$Value = as.numeric(ItemInfo$Value) #Set the value column to be numeric

#Get the actual results
results = vector(mode = "list", length = length(sources)) #set up a list to hold the response sets for the various sections
names(results) = paste0("a", 1:length(sources)) #add names to the list so they can be set later
for (i in 1:length(sources)){  #for each source/section
  SectionName = read.csv(sources[i], skip = 1, header = F, nrows = 1, stringsAsFactors = F)[1,2] #get the section name
  x = read.csv(sources[1], skip = 13, header = F, stringsAsFactors = F) #read the item response info
  colnames(x) = c("StudentID", "LastName","FirstName","TestDate","TotalPoints",ItemInfo$ItemName) #set the column names 
  x$score = x$TotalPoints/sum(ItemInfo$Value)*100
  results[[i]] = x #put the response info in the list
  names(results)[i] = SectionName #set the element name in the list to be the name of the section
}

#Combine and analyze the item response data ####
allresults = rbindlist(results) #make a single data.table with all of the item responses from all of the section
ItemScores = setNames(as.data.frame(array(data = NA_integer_, dim = dim(allresults))), colnames(allresults)) #create a dara.frame to hold the item scores
ItemScores[,1:5] = allresults[,1:5] #pull in the student info from the results data.table

#load the complete item info from the comparison and topic alignment file
d2 = read.xlsx( 
  xlsxFile = "J:/tests/2016-2017/Humanities/H2/week23 (2017-02-17) Cuba Africa Middle East/comparison and topic alignment.xlsx", 
  sheet = "Topic Alignment", startRow = 2, colNames = F)
d2 = d2[1:(which(is.na(d2[,3]))[1] - 1),3:ncol(d2)] #remove unnecessary columns and rows
d2 = t(d2) #transpose it
colnames(d2) = d2[1,] #use the first row as the column names
d2 = d2[-1,] #remove the first row
row.names(d2) = NULL #remove the row names
d2 = as.data.frame(d2, stringsAsFactors = F) #convert it to a data.frame

Topics = d2[,5:ncol(d2)] #set up a data.frame to hold topic info
colnames(Topics)[1] = "ItemName" #set the name of the first column


d2$isMC = grepl("mc",d2$`Type:`, ignore.case = T) #determine which questions are MC
d2$`Value:` = as.integer(d2$`Value:`) #convert the Value column to integer
d2$options = d2$`Value:` + 1 #default the number of options to what it should be for ER questions
d2$options[d2$isMC] = substr(d2$`Type:`[d2$isMC], 3, nchar(d2$`Type:`[d2$isMC])) #set the number of options for MC questions
d2$type = "ER" #default the question type to ER
d2$type[d2$isMC] = "MC" #set the question type to MC for MC questions

#Move necessary info into ItemInfo
ItemInfo$Type = d2$type[match(ItemInfo$ItemName, d2$`Question #:`)] #set the type
ItemInfo$options = as.integer(d2$options[match(ItemInfo$ItemName, d2$`Question #:`)]) #set the number of options

#Calculate scores for each response on each item
for(i in 1:nrow(ItemInfo)){
  if(ItemInfo$Type[i] == "MC"){
    ItemScores[,ItemInfo$ItemName[i]] = ItemInfo$Value[i]*(allresults[,ItemInfo$ItemName[i], with = F] == ItemInfo$Answer[i])
  } else {
    ItemScores[,ItemInfo$ItemName[i]] = allresults[,ItemInfo$ItemName[i], with = F]
  }
}


#initialize some variables we need in ItemInfo
ItemInfo$AverageScore = NA_real_  
ItemInfo$Correlation = NA_real_

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



#Calculate the average score for each question
for(i in 1:nrow(ItemInfo)){ItemInfo$AverageScore[i] = mean(ItemScores[,ItemInfo$ItemName[i]])/ItemInfo$Value[i]*100}


#Set up a dataframe to hold the scores of each students with each item dropped and then calculate those scores
DropScores = ItemScores 
for(i in 1:nrow(DropScores)){
  for(j in ItemInfo$ItemName){
    DropScores[i,j] = DropScores$TotalPoints[i] - ItemScores[i,j]
  }
}


#Calculate the correlations
for(i in 1:nrow(ItemInfo)){ItemInfo$Correlation[i] = cor(DropScores$TotalPoints, DropScores[ItemInfo$ItemName[i]])}


#Create the upload tab
UploadTab = allresults[,c("StudentID")]
UploadTab$StudentName = paste0(allresults$LastName, ", ",allresults$FirstName)
UploadTab$Percentage = allresults$score





