#MainScript.R
library(openxlsx)
library(magrittr)
library(data.table)
source("loadWorkbook.R")



dataLocation = choose.dir(default = "J:/tests/2016-2017/")
sources = list.files(dataLocation, full.names = T)
TestName = read.csv(sources[1], header = F, nrows = 1, stringsAsFactors = F)[1,2]
ItemInfo = read.csv(sources[1], skip = 4, header = F, nrows = 3, stringsAsFactors = F)[,-(1:5)]
ItemInfo =  set_rownames(setNames(as.data.frame(t(ItemInfo), stringsAsFactors = F), c("ItemName", "Value", "Answer")), NULL)

toFix = grepl(pattern = "[^a-zA-Z\\d\\s:]", x = ItemInfo$Answer)
ItemInfo$Answer[toFix] = ItemInfo$Value[toFix]

results = vector(mode = "list", length = length(sources))
names(results) = paste0("a", 1:length(sources))
for (i in 1:length(sources)){
  SectionName = read.csv(sources[i], skip = 1, header = F, nrows = 1, stringsAsFactors = F)[1,2]
  x = read.csv(sources[1], skip = 13, header = F, stringsAsFactors = F)
  colnames(x) = c("StudentID", "LastName","FirstName","TestDate","TotalPoints",ItemInfo$ItemName)
  results[[i]] = x
  names(results)[i] = SectionName
}

allresults = rbindlist(results)
ItemScores = setNames(as.data.frame(array(data = NA_integer_, dim = dim(allresults))), colnames(allresults))
ItemScores[,1:5] = allresults[,1:5]


