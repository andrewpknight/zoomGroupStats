#' Helper function to aggregate sentiment variables
#' 
#' Used to aggregate the sentiment variables to the individual
#' and meeting levels
#' @param inputData data.frame that has been output from textSentiment function
#' @param meetingId string that indicates the name of the variable containing the meeting ID
#' @param speakerId string that indicates the name of the variable containing the speaker identity
#' @param sentMethod string that indicates what type of 
#' sentiment analysis to aggregate--must be either 'aws' or 'syuzhet'
#' @import data.table
#' @return A data.frame giving the sentiment metrics aggregated to the requested level. If only meetingId
#' is specified, metrics are aggregated to that level. If only speakerId is specified, metrics
#' are aggregated to the individual level across any meetings. If both meetingId and speakerId
#' are specified, metrics are aggregated to the level of the individual within meeting.  
#' @export
#'
#' @examples
#' agg.out = aggSentiment(inputData=sample_transcript_sentiment_aws, 
#' meetingId="batchMeetingId", speakerId = "userId", sentMethod="aws")
#' 
#' agg.out = aggSentiment(inputData=sample_chat_sentiment_syu, 
#' meetingId="batchMeetingId", speakerId = "userName", sentMethod="syuzhet")
aggSentiment = function(inputData, meetingId=NULL, speakerId=NULL, sentMethod) {
  
  aws_sentClass <- sd <- NULL
  sentDt = data.table::data.table(inputData)
  
  if(sentMethod == "aws") {
    
    aws_sentClasses = c("POSITIVE", "NEGATIVE", "MIXED", "NEUTRAL")
    awsContVars = paste0("aws_", tolower(aws_sentClasses))
    awsClassVars = paste0(awsContVars, "_class")
    
    if(sum(awsContVars %in% names(inputData)) == 0) {
        
      stop("You have requested aws sentiment metrics, but your input data does not include aws output. Either change sentMethod to 'none' or first run textSentiment on your input data and provide the correct output data frame.")
      
    }
    
    sentDt[, (awsClassVars) := lapply(aws_sentClasses, function(x) aws_sentClass == x)]
    if(!is.null(meetingId) && !is.null(speakerId)) {    
      agg1 = data.frame(sentDt[, as.list(unlist(lapply(.SD, function(x) list(mean = mean(x, na.rm=T), sd=sd(x, na.rm=T), sum=sum(x, na.rm=T), pct=sum(x, na.rm=T)/.N)))), by=list(get(meetingId), get(speakerId)), .SDcols=c(awsContVars, awsClassVars)])
      names(agg1)[1:2]= c(meetingId, speakerId)
      agg1 = agg1[, c(meetingId, speakerId, paste0(awsContVars, ".mean"), paste0(awsContVars, ".sd"), paste0(awsClassVars, ".sum"), paste0(awsClassVars, ".pct"))]	
    } else if(!is.null(meetingId)) {
      agg1 = data.frame(sentDt[, as.list(unlist(lapply(.SD, function(x) list(mean = mean(x, na.rm=T), sd=sd(x, na.rm=T), sum=sum(x, na.rm=T), pct=sum(x, na.rm=T)/.N)))), by=list(get(meetingId)), .SDcols=c(awsContVars, awsClassVars)])
      names(agg1)[1]= c(meetingId)
      agg1 = agg1[, c(meetingId, paste0(awsContVars, ".mean"), paste0(awsContVars, ".sd"), paste0(awsClassVars, ".sum"), paste0(awsClassVars, ".pct"))]	
    } else if(!is.null(speakerId)) {
      agg1 = data.frame(sentDt[, as.list(unlist(lapply(.SD, function(x) list(mean = mean(x, na.rm=T), sd=sd(x, na.rm=T), sum=sum(x, na.rm=T), pct=sum(x, na.rm=T)/.N)))), by=list(get(speakerId)), .SDcols=c(awsContVars, awsClassVars)])
      names(agg1)[1]= c(speakerId)
      agg1 = agg1[, c(speakerId, paste0(awsContVars, ".mean"), paste0(awsContVars, ".sd"), paste0(awsClassVars, ".sum"), paste0(awsClassVars, ".pct"))]	
    } else {
      stop("You did not enter either a meetingId or an speakerId")		
    }
    sentOut = agg1
  }
  
  if(sentMethod == "syuzhet") {
    syuVars = paste0("syu_",c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive"))
    
    if(sum(syuVars %in% names(inputData)) == 0) {
      
      stop("You have requested syuzhet sentiment metrics, but your input data does not include syuzhet output. Either change sentMethod to 'none' or first run textSentiment on your input data and provide the correct output data frame.")
      
    }    
    
    if(!is.null(meetingId) && !is.null(speakerId)) {
      agg1 = data.frame(sentDt[, as.list(unlist(lapply(.SD, function(x) list(sum=sum(x, na.rm=T), pct=sum(x, na.rm=T)/.N)))), by=list(get(meetingId), get(speakerId)), .SDcols=syuVars])
      
      names(agg1)[1:2]= c(meetingId, speakerId)
      agg1 = agg1[, c(meetingId, speakerId, paste0(syuVars, ".sum"), paste0(syuVars, ".pct"))]	
      
    } else if(!is.null(meetingId)) {
      
      agg1 = data.frame(sentDt[, as.list(unlist(lapply(.SD, function(x) list(sum=sum(x, na.rm=T), pct=sum(x, na.rm=T)/.N)))), by=list(get(meetingId)), .SDcols=syuVars])
      names(agg1)[1]= c(meetingId)
      agg1 = agg1[, c(meetingId, paste0(syuVars, ".sum"), paste0(syuVars, ".pct"))]	
    } else if(!is.null(speakerId)) {
      agg1 = data.frame(sentDt[, as.list(unlist(lapply(.SD, function(x) list(sum=sum(x, na.rm=T), pct=sum(x, na.rm=T)/.N)))), by=list(get(speakerId)), .SDcols=syuVars])
      names(agg1)[1]= c(speakerId)
      agg1 = agg1[, c(speakerId, paste0(syuVars, ".sum"), paste0(syuVars, ".pct"))]	
    } else {
      stop("You did not enter either a meetingId or an speakerId")
    }
    sentOut = agg1
  }
  return(sentOut)
}