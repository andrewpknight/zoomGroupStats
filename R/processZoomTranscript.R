############################################################
# processZoomTranscript Function
############################################################

# Zoom Recording Transcript File Processing
# This function parses the data from the transcript file (.vtt) that is downloaded from the Zoom website. 
# NOTE: This is the file that accompanies a recording to the cloud. 

# example call:				tr.out = processZoomTranscript(fname="~/Desktop/transcript.vtt", recordingStartDateTime="2020-04-01 17:56:34", languageCode="en")

# INPUT:
# fname: 					the path to the local file where the transcript file (vtt) is saved.
# recordingStartDateTime: 	the time that the recording was launched. Format is YYYY-MM-DD HH:MM:SS
# languageCode:				the code for the language (e.g., en)

# Note: I plan to fix at a later point in time the timing issue. Specifically, it is not clear
# where in Zoom's system I can get the actual time that the recording was started. This 
# is a problem for linking the transcript file up with the chat file.
# One workaround for now (for research) would be to set recordings to auto-start. This is not ideal, though.
# we should be able to know when the recording was started. It is embedded in the video, so could pull from there.

# OUTPUT: 
# utteranceId:				an incremented numeric identifier for a marked speech utterance
# utteranceStartSeconds:	the number of seconds from the start of the recording (when it starts)
# utteranceStartTime:		the timestamp for the start of the utterance
# utteranceEndSeconds:		the number of seconds from the start of the recording (when it ends)
# utteranceEndTime:			the timestamp for the end of the utterance
# utteranceTimeWindow:		the number of seconds that the utterance took
# userName: 				the name attached to the utterance
# utteranceMessage:			the text of the utterance
# utteranceLanguage:		the language code for the transcript

processZoomTranscript = function(fname, recordingStartDateTime, languageCode) {
	require(stringr)

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