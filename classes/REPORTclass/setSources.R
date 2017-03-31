
  if(is.null(private$DataLocation)){
    return("Need a data location first.")
  } else {
    private$Sources = list.files(private$DataLocation, full.names = T)  
  }
