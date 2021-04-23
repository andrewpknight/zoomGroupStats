#' Simple conversational turn-taking analysis
#' 
#' Generate a very basic analysis of the conversational turntaking in 
#' either a Zoom transcript or a Zoom chat file.
#'
#' @param inputData data.frame output from either processZoomChat or processZoomTranscript
#' @param inputType string of either 'chat' or 'transcript'
#' @param speakerId string giving the name of the variable with the identity of the speaker
#'
#' @return list of four data.frames. The named items provide different views of turntaking. 
#' @export
#'
#' @examples
#' turn.out = turnTaking(inputData=sample_transcript_processed, 
#' inputType='transcript', 
#' speakerId='userName')
#' 
turnTaking = function(inputData, inputType, speakerId) {
  # Get the names of the unique speakers in this file
  uniqueSpeakers = sort(unique(inputData[,speakerId]))

#Create lagged variables
inputData$speakerCurrent = inputData[,speakerId]

if(inputType == "transcript") {
  inputData[, c("speakerBefore", "priorUtteranceEndSeconds")] = dplyr::lag(inputData[, c("speakerCurrent", "utteranceEndSeconds")])
  inputData$turnGap = inputData$utteranceStartSeconds - inputData$priorUtteranceEndSeconds
} else if(inputType == "chat") {
  inputData[, c("speakerBefore", "priorMessageTime")] = dplyr::lag(inputData[, c("speakerCurrent", "messageTime")])
  inputData$turnGap = as.numeric(difftime(inputData$messageTime, inputData$priorMessageTime, units="secs"))
}

turnDyd = inputData[,c("speakerCurrent", "speakerBefore", "turnGap")]
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

## output a few things
o.list = list("rawTurns" = turnDyd, "aggTurnsDyad" = turnDyd.agg, "aggTurnsSpeaker" = turnDyd.agg2, "aggTurnsSpeaker_noself" = turnDyd.agg_noself)
}