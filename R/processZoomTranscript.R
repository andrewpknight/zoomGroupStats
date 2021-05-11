#' Process Zoom transcript file
#'
#'# Zoom Recording Transcript File Processing
#' This function parses the data from the transcript file (.vtt) that is downloaded from the Zoom website. 
#' NOTE: This is the file that accompanies a recording to the cloud.
#' 
#' @param fname String that is the path to the exported Zoom .vtt transcript chat file
#' @param recordingStartDateTime String that is the timestamp when the recording was started in YYYY-MM-DD HH:MM:SS
#' @param languageCode String denoting the language
#'
#' @return data.frame where each record is an utterance in the transcript, with columns:
#' \itemize{
#'    \item utteranceId - Numeric identifier for each utterance in the transcript
#'    \item utteranceStartSeconds - number of seconds from the start of the recording when utterance began
#'    \item utteranceStartTime - POSIXct timestamp of the start of the utterance, using recordingStartDateTime as the zero
#'    \item utteranceEndSeconds - number of seconds from the start of the recording when utterance ended
#'    \item utteranceEndTime - POSIXct timestamp of the end of the utterance, using recordingStartDateTime as the zero    
#'    \item utteranceTimeWindow - number of seconds that this utterance lasted
#'    \item userName - Zoom display name of the person who spoke this utterance
#'    \item utteranceMessage - transcribed spoken words of this utterance
#'    \item utteranceLanguage - language code for this utterance
#' }
#' @export
#'
#' @examples
#' tr.out = processZoomTranscript(
#' fname=system.file('extdata', 'meeting001_transcript.vtt', package = 'zoomGroupStats'), 
#' recordingStartDateTime = '2020-04-20 13:30:00', languageCode = 'en')
processZoomTranscript = function(fname, recordingStartDateTime="1970-01-01 00:00:00", languageCode="en") {
  
  # Parse the transcript file -- vtt is a structured format. 
  f = readLines(fname)
  
  # there are three main pieces of data for each marked "utterance" - an id, a window of time, and the text
  utteranceId = as.integer(f[seq(3,length(f), 4)])
  utteranceWindow = f[seq(4,length(f), 4)]
  utteranceText = f[seq(5,length(f), 4)]
  
  # Parse the time window into two separate elements
  utteranceStartTime = unlist(strsplit(utteranceWindow, " --> "))[seq(1, length(utteranceWindow)*2, 2)]
  utteranceEndTime = unlist(strsplit(utteranceWindow, " --> "))[seq(2, length(utteranceWindow)*2, 2)]	
  
  utteranceStartSeconds = as.numeric(lubridate::seconds(lubridate::hms(utteranceStartTime)))
  utteranceEndSeconds = as.numeric(lubridate::seconds(lubridate::hms(utteranceEndTime)))
  
  # Now turn these into actual datetime values
  recordingStartDateTime = as.POSIXct(recordingStartDateTime, tz=Sys.timezone())
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
