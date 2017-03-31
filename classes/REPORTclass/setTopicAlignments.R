#setTopicAlignments.R

Topics = d2[,5:ncol(d2)] #set up a data.frame to hold topic info
colnames(Topics)[1] = "ItemName" #set the name of the first column
private$TopicAlignments = Topics