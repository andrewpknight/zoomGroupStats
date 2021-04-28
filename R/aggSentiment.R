#' Helper function to aggregate sentiment variables
#' This is just a simple helper function that is used to aggregate the 
#' sentiment variables
#' @param inputData data.frame that has been output from textSentiment function
#' @param speakerId string that indicates the name of the variable containing the speaker identity
#' @import data.table
#' @return A list with data.frames. The first gives sentiment variables at the corpus level of analysis
#' The second gives sentiment variables at the speaker level of analysis.
#' @export
#'
#' @examples
#' agg.out = aggSentiment(inputData=sample_transcript_sentiment_processed, speakerId="userName")
aggSentiment = function(inputData, speakerId) {
  
  sentClass <- positive <- sd <- negative <- neutral <- mixed <- NULL
  
  sent.dt = data.table::data.table(inputData)
  
  # Create the transcript level sentiment information
  sent.tr = data.frame(sent.dt[, list(text_positive_pct = sum(sentClass=="POSITIVE")/.N, text_positive_x = mean(positive, na.rm=T), text_positive_sd = sd(positive, na.rm=T), text_negative_pct = sum(sentClass=="NEGATIVE")/.N, text_negative_x = mean(negative, na.rm=T), text_negative_sd = sd(negative, na.rm=T), text_neutral_pct = sum(sentClass=="NEUTRAL")/.N, text_neutral_x = mean(neutral, na.rm=T), text_neutral_sd = sd(neutral, na.rm=T), text_mixed_pct = sum(sentClass=="MIXED")/.N, text_mixed_x = mean(mixed, na.rm=T), text_mixed_sd = sd(mixed, na.rm=T))])
  
  # Create the user level sentiment information	
  sent.ind = data.frame(sent.dt[, list(text_positive_pct = sum(sentClass=="POSITIVE")/.N, text_positive_x = mean(positive, na.rm=T), text_positive_sd = sd(positive, na.rm=T), text_negative_pct = sum(sentClass=="NEGATIVE")/.N, text_negative_x = mean(negative, na.rm=T), text_negative_sd = sd(negative, na.rm=T), text_neutral_pct = sum(sentClass=="NEUTRAL")/.N, text_neutral_x = mean(neutral, na.rm=T), text_neutral_sd = sd(neutral, na.rm=T), text_mixed_pct = sum(sentClass=="MIXED")/.N, text_mixed_x = mean(mixed, na.rm=T), text_mixed_sd = sd(mixed, na.rm=T)), by=list(get(speakerId))])
  names(sent.ind)[1] = speakerId
  
  return(list(sent.tr, sent.ind))
  
}

