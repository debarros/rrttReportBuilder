#setSummary.R

Summarize$Average = mean(private$UploadTab$Percentage)
Summarize$Median = median(private$UploadTab$Percentage)
Summarize$High = max(private$UploadTab$Percentage)
Summarize$Low = min(private$UploadTab$Percentage)
Summarize$Q1 = quantile(x = private$UploadTab$Percentage, probs = .25)
Summarize$Q3 = quantile(x = private$UploadTab$Percentage, probs = .75)
Summarize$Tenth = quantile(x = private$UploadTab$Percentage, probs = .10)
Summarize$Ninetieth = quantile(x = private$UploadTab$Percentage, probs = .90)
Summarize$SD = sd(private$UploadTab$Percentage)
Summarize$N = nrow(private$UploadTab$Percentage)
Summarize$NPassed = sum(private$UploadTab$Percentage >= private$PassingScore)
Summarize$PassRate = Summarize$NPassed / Summarize$N
private$Summary = Summarize