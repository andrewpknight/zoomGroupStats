#' Helper function to aggregate sentiment variables
#' This is just a simple helper function that is used to aggregate the 
#' sentiment variables
#' @param inputData data.table 
#' @param speakerId character
#' @import data.table
#' @return list
#' @export
#'
#' @examples
#' agg.out = aggSentiment(inputData=sample_transcript_sentiment_processed, speakerId="userName")
aggSentiment = function(inputData, speakerId) {
  
  sent.dt = data.table::data.table(inputData)
  
  # Create the transcript level sentiment information
  sent.tr = data.frame(sent.dt[, list(text_positive_pct = sum(sentClass=="POSITIVE")/.N, text_positive_x = mean(positive, na.rm=T), text_positive_sd = sd(positive, na.rm=T), text_negative_pct = sum(sentClass=="NEGATIVE")/.N, text_negative_x = mean(negative, na.rm=T), text_negative_sd = sd(negative, na.rm=T), text_neutral_pct = sum(sentClass=="NEUTRAL")/.N, text_neutral_x = mean(neutral, na.rm=T), text_neutral_sd = sd(neutral, na.rm=T), text_mixed_pct = sum(sentClass=="MIXED")/.N, text_mixed_x = mean(mixed, na.rm=T), text_mixed_sd = sd(mixed, na.rm=T))])
  
  # Create the user level sentiment information	
  sent.ind = data.frame(sent.dt[, list(text_positive_pct = sum(sentClass=="POSITIVE")/.N, text_positive_x = mean(positive, na.rm=T), text_positive_sd = sd(positive, na.rm=T), text_negative_pct = sum(sentClass=="NEGATIVE")/.N, text_negative_x = mean(negative, na.rm=T), text_negative_sd = sd(negative, na.rm=T), text_neutral_pct = sum(sentClass=="NEUTRAL")/.N, text_neutral_x = mean(neutral, na.rm=T), text_neutral_sd = sd(neutral, na.rm=T), text_mixed_pct = sum(sentClass=="MIXED")/.N, text_mixed_x = mean(mixed, na.rm=T), text_mixed_sd = sd(mixed, na.rm=T)), by=list(get(speakerId))])
  names(sent.ind)[1] = speakerId
  
  return(list(sent.tr, sent.ind))
  
}

