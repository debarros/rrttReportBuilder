# setTestName_REPORT

# The while loop in the ScantronAS section below is designed to 
# 1) Try each source to get the test name, and exit with the test name when it is found
# 2) Exit with an error if all the source files have been checked already

setTestName.REPORT = function(report, messageLevel = 0) {

  if(messageLevel > 0){
    message("Running setTestName.REPORT")
  }
  
  # pull the needed info from the report
  TMS = report$getTMS()         # name of the testing management system
  Sources = report$getSources() # file paths to source data
  
  # Initialize the TestName
  TestName = NA_character_
  
  if(TMS == "LinkIt"){                        # If the TMS is LinkIt,
    TestName = read.csv(Sources[1],           # Read the first source file
                        header = F, 
                        nrows = 1,
                        stringsAsFactors = F)
    TestName = TestName[1,2]                  # Get the test name
    
  } else if(TMS == "ScantronAS"){             # If the TMS is ScantronAS,
    i = 0                                     # Set the counter to 0.
    while(is.na(TestName)){                   # Until a test name is found,
      i = i+1                                 # increment the counter
      if(i > length(Sources)){                # If the sources have all be checked,
        stop("There is no data")              # halt and report an error.
      }                                       # Otherwise,
      x = read.csv(Sources[i], 
                   stringsAsFactors = F)      # Read the next source file
      if(!is.null(x)){                        # if there is data in the source file,
        TestName = x[1,3]                     # grab the test name
      } # /if there is data in the source file
    } # /while 
    
  } else if(TMS == "ASAP"){                   # If the TMS is the Level 1 ASAP system for regents data,
    TestName = read.csv(Sources[1],           # Read the first source file
                        stringsAsFactors = F)
    TestName = TestName[1,6]                  # Get the test name
    
  } else {                                    # If the TMS is not one of the known ones, 
    stop(paste0("Unknown TMS: ",TMS))         # halt and throw an error
  } # /if-else
  
  report$setTestNameQuick(TestName)           # Store the test name
  
} # /function
