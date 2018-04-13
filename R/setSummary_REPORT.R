# setSummary_REPORT

setSummary.REPORT = function(report, messageLevel = 0) {
  
  # pull the relevant parts of the report
  UploadTab = report$getUploadTab()
  nResults = length(report$getResults())
  ItemInfo = report$getItemInfo()
  PassingScore = report$getPassingScore()
  TestName = report$getTestName()
  
  # build the summary
  Summarize = vector(mode = "list")
  Summarize$Average = mean(UploadTab$Percentage, na.rm = T)
  Summarize$Median = median(UploadTab$Percentage, na.rm = T)
  Summarize$High = max(UploadTab$Percentage, na.rm = T)
  Summarize$Low = min(UploadTab$Percentage, na.rm = T)
  Summarize$Q1 = quantile(x = UploadTab$Percentage, probs = .25, na.rm = T)
  Summarize$Q3 = quantile(x = UploadTab$Percentage, probs = .75, na.rm = T)
  Summarize$Tenth = quantile(x = UploadTab$Percentage, probs = .10, na.rm = T)
  Summarize$Ninetieth = quantile(x = UploadTab$Percentage, probs = .90, na.rm = T)
  Summarize$SD = sd(UploadTab$Percentage, na.rm = T)
  Summarize$N = nrow(UploadTab)
  Summarize$NPassed = sum(UploadTab$Percentage >= PassingScore, na.rm = T)
  Summarize$PassRate = Summarize$NPassed / Summarize$N
  Summarize$TestName = TestName
  Summarize$Sections = nResults
  Summarize$Items = nrow(ItemInfo)
  
  # Add the summare to the report
  report$setSummaryQuick(Summarize)
  
} # /setSummary.REPORT function
