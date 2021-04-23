#' Process Zoom transcript file
#'
#' @param fname charracter 
#' @param recordingStartDateTime character
#' @param languageCode character
#'
#' @return data.frame
#' @export
#'
#' @examples
#' tr.out = processZoomTranscript(
#' fname=system.file('extdata', "sample_transcript.vtt", package = 'zoomGroupStats'),
#' recordingStartDateTime = '2020-04-20 13:30:00', 
#' languageCode = 'en')
processZoomTranscript = function(fname, recordingStartDateTime, languageCode) {

  # Function to parse the time to create a total number of seconds
  timeCalc = function(incTime) {
    inc_hours = as.numeric(substr(incTime,1,2))
    inc_mins = as.numeric(substr(incTime,4,5))	
    inc_secs = as.numeric(substr(incTime,7,12))		
    inc_total_secs = inc_hours*60*60 + inc_mins*60 + inc_secs
  }
  
  # Parse the transcript file -- vtt is a structured format. 
  f = readLines(fname)
  
  # there are three main pieces of data for each marked "utterance" - an id, a window of time, and the text
  utteranceId = as.integer(f[seq(3,length(f), 4)])
  utteranceWindow = f[seq(4,length(f), 4)]
  utteranceText = f[seq(5,length(f), 4)]
  
  # Parse the time window into two separate elements
  utteranceStartTime = unlist(strsplit(utteranceWindow, " --> "))[seq(1, length(utteranceWindow)*2, 2)]
  utteranceEndTime = unlist(strsplit(utteranceWindow, " --> "))[seq(2, length(utteranceWindow)*2, 2)]	
  
  # Preserve this to use in a dynamic conversation analysis
  utteranceStartSeconds = timeCalc(utteranceStartTime)
  utteranceEndSeconds = timeCalc(utteranceEndTime)
  
  # Now turn these into actual datetime values
  recordingStartDateTime = as.POSIXct(recordingStartDateTime)
  utteranceStartTime = recordingStartDateTime + utteranceStartSeconds
  utteranceEndTime = recordingStartDateTime + utteranceEndSeconds
  
  # Create a time window (in seconds) for the utterances -- how long is each in seconds
  utteranceTimeWindow = as.numeric(difftime(utteranceEndTime, utteranceStartTime, units="secs"))
  
  # Parse the utterance message itself
  utteranceMessage = substring(utteranceText, regexpr("[:]", utteranceText)+2)
  
  # Get the user name that spoke the text
  userName = substr(utteranceText, 1, regexpr("[:]", utteranceText)-1)	
  
  # Prepare the output file
  res.out = data.frame(utteranceId, utteranceStartSeconds, utteranceStartTime, utteranceEndSeconds, utteranceEndTime, utteranceTimeWindow, userName, utteranceMessage, stringsAsFactors=F)
  
  # Mark as unidentified any user with a blank username
  res.out$userName = ifelse(res.out$userName == "" | is.na(res.out$userName), "UNIDENTIFIED", res.out$userName)		
  
  # Add the language code
  res.out$utteranceLanguage = languageCode
  
  return(res.out)
}