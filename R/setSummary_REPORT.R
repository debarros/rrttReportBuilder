# setSummary_REPORT

setSummary.REPORT = function(report) {
      Summarize = vector(mode = "list")
      Summarize$Average = mean(report$.__enclos_env__$private$UploadTab$Percentage, na.rm = T)
      Summarize$Median = median(report$.__enclos_env__$private$UploadTab$Percentage, na.rm = T)
      Summarize$High = max(report$.__enclos_env__$private$UploadTab$Percentage, na.rm = T)
      Summarize$Low = min(report$.__enclos_env__$private$UploadTab$Percentage, na.rm = T)
      Summarize$Q1 = quantile(x = report$.__enclos_env__$private$UploadTab$Percentage, probs = .25, na.rm = T)
      Summarize$Q3 = quantile(x = report$.__enclos_env__$private$UploadTab$Percentage, probs = .75, na.rm = T)
      Summarize$Tenth = quantile(x = report$.__enclos_env__$private$UploadTab$Percentage, probs = .10, na.rm = T)
      Summarize$Ninetieth = quantile(x = report$.__enclos_env__$private$UploadTab$Percentage, probs = .90, na.rm = T)
      Summarize$SD = sd(report$.__enclos_env__$private$UploadTab$Percentage, na.rm = T)
      Summarize$N = nrow(report$.__enclos_env__$private$UploadTab)
      Summarize$NPassed = sum(report$.__enclos_env__$private$UploadTab$Percentage >= report$.__enclos_env__$private$PassingScore, na.rm = T)
      Summarize$PassRate = Summarize$NPassed / Summarize$N
      Summarize$TestName = report$.__enclos_env__$private$TestName
      Summarize$Sections = length(report$.__enclos_env__$private$Results)
      Summarize$Items = nrow(report$.__enclos_env__$private$ItemInfo)
      report$.__enclos_env__$private$Summary = Summarize
}