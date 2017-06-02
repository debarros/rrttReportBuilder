# setTopicAlignments_REPORT

setTopicAlignments.REPORT = function(d2, report) {
      if(ncol(d2) > 5){ #check to see if there are topics at all
        report$.__enclos_env__$private$HasTopics = T
        Topics = d2[,5:ncol(d2)] #set up a data.frame to hold topic info
        colnames(Topics)[1] = "ItemName" #set the name of the first column
        for(i in 2:ncol(Topics)){
          Topics[,i] = as.logical(as.numeric(Topics[,i]))
        } # /for
        report$.__enclos_env__$private$TopicAlignments = Topics
      } else {
        report$.__enclos_env__$private$HasTopics = F
        report$.__enclos_env__$private$TopicAlignments = NULL
      } # /if-else
}