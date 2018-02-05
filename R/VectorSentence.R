#' @title Vector Sentence
#' @description Create a natural language, comma separated list
#' @param x a vector of values, some subset of which are to be listed in the ouput string
#' @param y a logical vector of the same length as /code{x}, indicating whether each element should be included
#' @param OxfordComma logical, should the Oxford (or serial) comma be used?
#' @param End what word or phrase should be inserted prior to the final element in the list?
#' @param hyphenate what is the min # of consecutive elements that will be hyphenated? If <2, no hyphenation.
#' @return atomic character
#' @note This functions creates a string that lists the selected elements of a vector.  The elements will be separated by commas, including the Oxford comma (if OxfordComma = T). If there are runs of consecutive elements selected, they will be grouped using hyphenation (if hyphenate > 1). Hyphenation refers to abbreviating a run of consecutive elements by using just the first and last, separated by a hyphen
#' @examples
#' # Sample Data 1
#' x = c(as.character(1:4), "5a", "5b", "5c", as.character(6:41),"D1", "D2", "D3a", "D4a", "D5", "D6", "Essay", letters[5:20])
#' y = sample(c(T, F), size = length(x), replace = TRUE) #elements to use
#' if (sum(y) < length(y)/2){y = !y}  #make sure at least half of the elements are True
#' names(y) = x
#' 
#' # Test Run 1
#' # look at the stuff
#' VectorSentence(x,y) #basic call
#' VectorSentence(x,y, hyphenate = 4, End = "and also ") #change the minimum hyphenation and the final words
#' VectorSentence(x,y, hyphenate = 8, End = "and also ")
#' VectorSentence(x, y, OxfordComma = F, End = "and ", hyphenate = 2) #turn off the oxford comma and hyphenate pairs
#' VectorSentence(x, y, OxfordComma = T, End = "", hyphenate = 4) #eliminate the final words
#' VectorSentence(x, y, OxfordComma = F, End = "", hyphenate = 3) #no final words or oxford comma
#' VectorSentence(x, y, OxfordComma = T, End = "", hyphenate = 1) #no hyphenation at all
#' 
#' # Sample Data 2
#' y = rep(sample(c(T, F), size = round(length(x)/2), replace = TRUE), each =2)
#' names(y) = x
#' 
#' # Test Run 2
#' y
#' VectorSentence(x,y)
#' VectorSentence(x,y, hyphenate = 3, End = "and also ")
#' VectorSentence(x,y, hyphenate = 8, End = "and also ")
#' VectorSentence(x, y, OxfordComma = F, End = "and ", hyphenate = 2)
#' VectorSentence(x, y, OxfordComma = T, End = "", hyphenate = 4)
#' VectorSentence(x, y, OxfordComma = F, End = "", hyphenate = 3)
#' VectorSentence(x, y, OxfordComma = T, End = "", hyphenate = 1)
#' 
#' # Sample Data 3
#' x = c("a","b","c","d")
#' 
#' # Test Run 3
#' VectorSentence(x)
#' VectorSentence(x, hyphenate = 5)
#' VectorSentence(x, OxfordComma = F, End = "", hyphenate = 1)
#' VectorSentence(x, OxfordComma = F, End = "and lastly ", hyphenate = 1)
#' VectorSentence(x, OxfordComma = F, End = "and lastly ", hyphenate = 2)
#' 
#' # Sample Data 4
#' x = letters[1:10]
#' y = c(T,T,T,T,F,T,T,T,T,T)
#' 
#' # Test Run 4
#' VectorSentence(x,y)
#' VectorSentence(x,y, hyphenate = 5)
#' VectorSentence(x,y, OxfordComma = F, End = "", hyphenate = 1)
#' VectorSentence(x,y, OxfordComma = F, End = "and lastly ", hyphenate = 1)
#' VectorSentence(x,y, OxfordComma = F, End = "and lastly ", hyphenate = 2)
#' VectorSentence(x,y, OxfordComma = T, End = "and lastly ", hyphenate = 2)
VectorSentence = function(x, y = NA, OxfordComma = T, End = "and ", hyphenate = 3){
  
  #If y is not supplied, set it to be all true
  if(length(y) == 1){
    if(is.na(y)){y = rep(T, times = length(x))}
  }
  
  #If y has NA's, remove those positions from both x and y
  if(sum(is.na(y)) > 0){
    x = x[!is.na(y)]
    y = y[!is.na(y)]
  }
  
  #If the set of elements to be strung together is small, deal with it right away
  if(sum(y, na.rm = T) == 0){return(NULL)}
  if(sum(y, na.rm = T) == 1){return(x[y])}
  if(sum(y, na.rm = T) == 2){return(paste0(x[y][1]," ",End,x[y][2]))}
  
  #Create the variables that that will be used later in the function
  x1 = 1:length(x)          #create a vector of the indices of the values
  x2 = x1[y]                #create a vector of just the indices of the values that will be included in the output
  x3 = c(x2[1],head(x2,-1)) #make a vector of the elements of x2, shifted forward by 1 space
  
  #create a data.frame to be used in building the sentence
  start = which(!(x2 == x3 + 1))                          #find the elements s.t. the prior element comes right before it
  end = data.frame(end = c(tail(start,-1)-1,length(x2))) #find the last elements in consecutive runs
  elements = cbind(start, end)                           #make them into a data frame
  
  #At this point in the function, the elements data.frame holds only the indices of the indices of x2
  #Add columns to get the corresponding values of x2
  elements$startIndex = x2[elements$start]
  elements$endIndex = x2[elements$end]
  
  #Now the elements data.frame has the values of x2
  #However, those are just the subset of x1 to be used, and x1 is just the indices of x
  #Add columns to get the values from x
  elements$startValue = x[elements$startIndex]
  elements$endValue = x[elements$endIndex]
  
  elements$type = NA  #Add new variable to hold the type of entry in the comma separated string
  
  #Determine the types
  elements$type[elements$start == elements$end] = "single"
  elements$type[elements$start < elements$end] = "comma"
  if(hyphenate > 1){elements$type[(elements$start - 1) <= (elements$end - hyphenate)] = "hyphenate"}
  
  #Create the entry list
  Entries = list()
  for (i in 1:nrow(elements)){
    if(elements$type[i] == "single"){Entries[[i]] = elements$startValue[i]
    } else if(elements$type[i] == "comma"){Entries[[i]] = x[seq.int(elements$startIndex[i],elements$endIndex[i])]
    } else {Entries[[i]] = paste0(elements$startValue[i],"-",elements$endValue[i])}
  } #end of for loop
  Entries = unlist(Entries) #Convert the Entries list to a character vector
  
  if(length(Entries) == 1){return(Entries)}  #if there is only one element left, return it
  if(length(Entries) == 2){return(paste0(Entries[1]," ",End,Entries[2]))}  #if there are only 2 elements left, return them
  
  if(OxfordComma){End = paste0(", ",End)}else{End = paste0(" ",End)} #modify the final words in light of OxfordComma
  
  #Return the comma separated string, completely assembled
  paste0(paste0(Entries[1:(length(Entries)-1)],collapse = ", "),End,Entries[length(Entries)])
  
} #end of function
