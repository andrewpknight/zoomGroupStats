#' Helper function to aggregate sentiment variables
#' This is just a simple helper function that is used to aggregate the 
#' sentiment variables
#' @param inputData data.frame that has been output from textSentiment function
#' @param meetingId string that indicates the name of the variable containing the meeting ID
#' @param indivId string that indicates the name of the variable containing the speaker identity
#' @param sentMethod string that indicates what type of sentiment analysis to aggregate--must be either 'aws' or 'syuzhet'
#' @import data.table
#' @return A list with data.frames. The first gives sentiment variables at the corpus level of analysis
#' The second gives sentiment variables at the speaker level of analysis.
#' @export
#'
#' @examples
#' agg.out = aggSentiment(inputData=sample_transcript_sentiment_aws, 
#' meetingId="batchMeetingId", indivId = "userId", sentMethod="aws")
#' 
#' agg.out = aggSentiment(inputData=sample_transcript_sentiment_syu, 
#' meetingId="batchMeetingId", indivId = "userId", sentMethod="syuzhet")
aggSentiment = function(inputData, meetingId, indivId, sentMethod) {
  
  aws_sentClass <- sd <- NULL
  sentDt = data.table::data.table(inputData)
  if(sentMethod == "aws") {
    aws_sentClasses = c("POSITIVE", "NEGATIVE", "MIXED", "NEUTRAL")
    awsContVars = paste0("aws_", tolower(aws_sentClasses))
    awsClassVars = paste0(awsContVars, "_class")
    sentDt[, (awsClassVars) := lapply(aws_sentClasses, function(x) aws_sentClass == x)]
    if(!is.null(meetingId) && !is.null(indivId)) {    
      agg1 = data.frame(sentDt[, as.list(unlist(lapply(.SD, function(x) list(mean = mean(x, na.rm=T), sd=sd(x, na.rm=T), sum=sum(x, na.rm=T), pct=sum(x, na.rm=T)/.N)))), by=list(get(meetingId), get(indivId)), .SDcols=c(awsContVars, awsClassVars)])
      names(agg1)[1:2]= c(meetingId, indivId)
      agg1 = agg1[, c(meetingId, indivId, paste0(awsContVars, ".mean"), paste0(awsContVars, ".sd"), paste0(awsClassVars, ".sum"), paste0(awsClassVars, ".pct"))]	
    } else if(!is.null(meetingId)) {
      agg1 = data.frame(sentDt[, as.list(unlist(lapply(.SD, function(x) list(mean = mean(x, na.rm=T), sd=sd(x, na.rm=T), sum=sum(x, na.rm=T), pct=sum(x, na.rm=T)/.N)))), by=list(get(meetingId)), .SDcols=c(awsContVars, awsClassVars)])
      names(agg1)[1]= c(meetingId)
      agg1 = agg1[, c(meetingId, paste0(awsContVars, ".mean"), paste0(awsContVars, ".sd"), paste0(awsClassVars, ".sum"), paste0(awsClassVars, ".pct"))]	
    } else if(!is.null(indivId)) {
      agg1 = data.frame(sentDt[, as.list(unlist(lapply(.SD, function(x) list(mean = mean(x, na.rm=T), sd=sd(x, na.rm=T), sum=sum(x, na.rm=T), pct=sum(x, na.rm=T)/.N)))), by=list(get(indivId)), .SDcols=c(awsContVars, awsClassVars)])
      names(agg1)[1]= c(indivId)
      agg1 = agg1[, c(indivId, paste0(awsContVars, ".mean"), paste0(awsContVars, ".sd"), paste0(awsClassVars, ".sum"), paste0(awsClassVars, ".pct"))]	
    } else {
      stop("You did not enter either a meetingId or an indivId")		
    }
    sentOut = agg1
  }
  
  if(sentMethod == "syuzhet") {
    syuVars = paste0("syu_",c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive"))
    
    if(!is.null(meetingId) && !is.null(indivId)) {
      agg1 = data.frame(sentDt[, as.list(unlist(lapply(.SD, function(x) list(sum=sum(x, na.rm=T), pct=sum(x, na.rm=T)/.N)))), by=list(get(meetingId), get(indivId)), .SDcols=syuVars])
      names(agg1)[1:2]= c(meetingId, indivId)
      agg1 = agg1[, c(meetingId, indivId, paste0(syuVars, ".sum"), paste0(syuVars, ".pct"))]	
    } else if(!is.null(meetingId)) {
      
      agg1 = data.frame(sentDt[, as.list(unlist(lapply(.SD, function(x) list(sum=sum(x, na.rm=T), pct=sum(x, na.rm=T)/.N)))), by=list(get(meetingId)), .SDcols=syuVars])
      names(agg1)[1]= c(meetingId)
      agg1 = agg1[, c(meetingId, paste0(syuVars, ".sum"), paste0(syuVars, ".pct"))]	
    } else if(!is.null(indivId)) {
      agg1 = data.frame(sentDt[, as.list(unlist(lapply(.SD, function(x) list(sum=sum(x, na.rm=T), pct=sum(x, na.rm=T)/.N)))), by=list(get(indivId)), .SDcols=syuVars])
      names(agg1)[1]= c(indivId)
      agg1 = agg1[, c(indivId, paste0(syuVars, ".sum"), paste0(syuVars, ".pct"))]	
    } else {
      stop("You did not enter either a meetingId or an indivId")
    }
    sentOut = agg1
  }
  return(sentOut)
}