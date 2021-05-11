#' Simple conversational turn-taking analysis
#' 
#' Generate a very basic analysis of the conversational turntaking in 
#' either a Zoom transcript or a Zoom chat file.
#'
#' @param inputData data.frame output from either processZoomChat or processZoomTranscript
#' @param inputType string of either 'chat' or 'transcript'
#' @param meetingId string giving the name of the meeting identifier
#' @param speakerId string giving the name of the variable with the identity of the speaker
#'
#' @return list of four data.frames giving different levels of analysis for turn taking:
#' \itemize{
#'     \item rawTurn - This data.frame gives a dataset with a 
#'     lagged column so that you could calculate custom metrics
#'     \item aggTurnsDyad - This gives a dyad-level dataset so that 
#'     you know whose speech patterns came before whose
#'     \item aggTurnsSpeaker - This gives a speaker-level dataset
#'     with metrics that you could use to assess each given
#'     person's influence on the conversation
#'     \item aggTurnsSpeaker_noself - This is a replication of 
#'     the aggTurnsSpeaker dataset, but it excludes turns where
#'     a speaker self-follows (i.e., Speaker A => Speaker A)
#' }
#' @export
#'
#' @examples
#' turn.out = turnTaking(inputData=sample_transcript_processed, 
#' inputType='transcript', meetingId='batchMeetingId', 
#' speakerId='userName')
#' 
#' turn.out = turnTaking(inputData=sample_chat_processed, 
#' inputType='chat', meetingId='batchMeetingId', 
#' speakerId='userName')
#' 
turnTaking = function(inputData, inputType, meetingId, speakerId) {
  
  turnGap<-sd<-speakerCurrent<-speakerBefore<-numTurns<-turnGap_x<-NULL
  
  ## This should be done on a meeting-by-meeting basis. Will do in a crude brute force way for now
  
  uniqueMeets = unique(inputData[,meetingId])
  
  if(length(uniqueMeets) == 1) pbMin=0 else pbMin=1  
  pb = utils::txtProgressBar(min=pbMin, max=length(uniqueMeets), style=3)
  for(m in 1:length(uniqueMeets)) {
    utils::setTxtProgressBar(pb, m)
    meetData = inputData[inputData[,meetingId] == uniqueMeets[m], ]
    
    # Get the names of the unique speakers in this file
    uniqueSpeakers = sort(unique(meetData[,speakerId]))
    
    #Create lagged variables
    meetData$speakerCurrent = meetData[,speakerId]
    
    if(inputType == "transcript") {
      meetData = meetData[order(meetData$utteranceEndSeconds), ]
      meetData[, c("speakerBefore", "priorUtteranceEndSeconds")] = dplyr::lag(meetData[, c("speakerCurrent", "utteranceEndSeconds")])
      meetData$turnGap = meetData$utteranceStartSeconds - meetData$priorUtteranceEndSeconds
    } else if(inputType == "chat") {
      meetData = meetData[order(meetData$messageTime), ]  
      meetData[, c("speakerBefore", "priorMessageTime")] = dplyr::lag(meetData[, c("speakerCurrent", "messageTime")])
      meetData$turnGap = as.numeric(difftime(meetData$messageTime, meetData$priorMessageTime, units="secs"))
    }
  
    turnDyd = meetData[,c("speakerCurrent", "speakerBefore", "turnGap")]
    turnDyd.dt = data.table::data.table(turnDyd)
    turnDyd.agg = data.frame(turnDyd.dt[, list(numTurns = .N, turnGap_x = mean(turnGap, na.rm=T), turnGap_sd = sd(turnGap, na.rm=T)), by=list(speakerCurrent, speakerBefore)])
    
    # Add zeros for pairs that didn't occur
    for(b in uniqueSpeakers) {
      for(c in uniqueSpeakers) {
        if(nrow(turnDyd.agg[turnDyd.agg$speakerBefore == b & turnDyd.agg$speakerCurrent == c, ]) == 0) {
          turnDyd.agg[nrow(turnDyd.agg)+1, ] = c(c, b, 0, NA, NA)
        }
      }
    }
    turnDyd.agg[,3:5] = lapply(turnDyd.agg[,3:5], as.numeric)
    
    ######## Create an individual level dataset focused for now on influence ########
    turnDyd.dt2 = data.table::data.table(turnDyd.agg)
        
    turnDyd.agg2 = data.frame(turnDyd.dt2[!is.na(speakerBefore), list(turnsAfterSpeaker = sum(numTurns), turnGapAfterSpeaker_x = mean(turnGap_x, na.rm=T), turnGapAfterSpeaker_sd = sd(turnGap_x, na.rm=T)), list(speakerBefore)])
    totalTurns = sum(turnDyd.agg[!is.na(turnDyd.agg$speakerBefore), "numTurns"])
    turnDyd.agg2$turnsAfterSpeaker_pct = turnDyd.agg2$turnsAfterSpeaker/totalTurns
    
    # Do a version of this that excludes the self references
    turnDyd.agg_noself = data.frame(turnDyd.dt2[!is.na(speakerBefore) & (speakerCurrent != speakerBefore), list(turnsAfterSpeaker = sum(numTurns), turnGapAfterSpeaker_x = mean(turnGap_x, na.rm=T), turnGapAfterSpeaker_sd = sd(turnGap_x, na.rm=T)), list(speakerBefore)])
    
    totalTurns_noself = sum(turnDyd.agg[!is.na(turnDyd.agg$speakerBefore) & (turnDyd.agg$speakerCurrent != turnDyd.agg$speakerBefore), "numTurns"])
    
    turnDyd.agg_noself$turnsAfterSpeaker_pct = turnDyd.agg_noself$turnsAfterSpeaker/totalTurns_noself  	
    
    if(nrow(turnDyd) > 0) {
      turnDyd[,meetingId] =  uniqueMeets[m]
    }
    if(nrow(turnDyd.agg) > 0){
      turnDyd.agg[,meetingId] =  uniqueMeets[m]  
    }
    if(nrow(turnDyd.agg2) > 0){
      turnDyd.agg2[,meetingId] =  uniqueMeets[m]  
    }
    if(nrow(turnDyd.agg_noself) > 0){
      turnDyd.agg_noself[,meetingId] =  uniqueMeets[m]	
    }
        
    if(m == 1) {
      res1 = turnDyd
      res2 = turnDyd.agg
      res3 = turnDyd.agg2
      res4 = turnDyd.agg_noself
    } else {
      res1 = rbind(res1, turnDyd)
      res2 = rbind(res2, turnDyd.agg)
      res3 = rbind(res3, turnDyd.agg2)
      res4 = rbind(res4, turnDyd.agg_noself)						
    }
  }
  close(pb)
  ## output a few things
  o.list = list("rawTurns" = res1, "aggTurnsDyad" = res2, "aggTurnsSpeaker" = res3, "aggTurnsSpeaker_noself" = res4)
  return(o.list)
}