# getResponses_REPORT

getResponses.REPORT = function(report) {
#establish a list that will hold the Item Response data.frames
      ItemResponses = vector(mode = "list", length = length(report$.__enclos_env__$private$Results))
      #load the item responses for each section in the list
      for(i in 1:length(report$.__enclos_env__$private$Results)){
        ItemResponses[[i]] = report$.__enclos_env__$private$Results[[i]]$getItemResponses()
      }
      #make a data.table with all of the item responses from all of the sections
      ItemResponses = data.table::rbindlist(ItemResponses) 
      return(ItemResponses)
}