############################################################
# processZoomChat Function
############################################################

# Zoom Chat File Processing
# This function parses the data from the chatfile that is downloaded from the Zoom website. 
# NOTE: This is the file that accompanies a recording. This is not the file 
# that you download directly within the window. It is also not the one that is 
# saved locally on your computer. This is the file that you can access after a session 
# if you record in the cloud. 

# example call:				ch.out = processZoomChat(fname="~/Desktop/chat.txt", sessionStartDateTime="2020-04-01 17:56:34", languageCode="en")

# INPUT ARGUMENTS: 
# fname: 					the path to the local file where the chat file (txt) is saved.
# sessionStartDateTime: 	the time that the actual session was launched. Format is YYYY-MM-DD HH:MM:SS
# languageCode:				the code for the language (e.g., en)

# OUTPUT: 
# messageId:				an incremented numeric identifier for a message in the chat
# messageTime:				a timestamp for the message, based on the start of the Zoom session
# userName:					the name attached to the message
# message:					the text of the message
# messageLanguage:			the language code for the chat

processZoomChat = function(fname, sessionStartDateTime, languageCode) {

	require(stringr)

	# NOTE: Need to fix this to remove any stray tabs from this file before bringing it in. I have run into a few edge cases where participants use tabs in their messages and it screws up the file. Need to experiment with this and introduce (likely) a brute force parser for this file. 

	ch = read.delim(fname, sep="\t", stringsAsFactors=F, header=F, col.names=c("messageIncrement", "userName", "message"), quote="")


	####################################
	# First thing do to is to create a message_time variable

	# This is user-supplied and could come from the usermeeting report that can be downloaded
	sessionStartDateTime = as.POSIXct(sessionStartDateTime)

	# This is the value embedded in the chat record. It is an HH:MM:SS delta from the start of the session
	# I'm doing a crude parse to just get the total number of seconds that the delta is. This is then
	# used as the increment from the sessionStartDateTime
	ch$incHours = as.numeric(substr(ch$messageIncrement,1,2))
	ch$incMins = as.numeric(substr(ch$messageIncrement,4,5))	
	ch$incSecs = as.numeric(substr(ch$messageIncrement,7,8))		
	ch$incTotalSecs = ch$incHours*60*60 + ch$incMins*60 + ch$incSecs
	ch$messageTime = sessionStartDateTime + ch$incTotalSecs

	####################################
	# Chat transcripts do not handle soft returns well (i.e., if the same person uses a soft line break 
	# for multiple lines in a single message that is submitted to the system). 
	# This is a crude way to identify them based on someone having an invalid message time. 
	# For now, will assign that text to the last marked user name in the dataset, 
	# pasting the messages together into a single line (separated by a space. )

	# Create a flag to mark erroneous records based on the message time variable. This should be made stronger
	# and cleaner eventually
	ch$flag = is.na(as.integer(substr(ch$messageIncrement,1,1))) + (nchar(ch$messageIncrement) != 8)

	# Assign the value in the message_increment variable to the message variable. This is because
	# the parsing of the file is fucked up when there are soft returns in someone's chat message
	ch$message = ifelse(ch$flag > 0, ch$messageIncrement, ch$message)

	# Go through the records from the bottom up to paste the message on the one it 
	# should be part of
	for(i in nrow(ch):1) {
		if(ch[i,"flag"] > 0) {
			ch[(i-1), "message"] = paste(ch[(i-1), "message"], ch[i, "message"], sep=" ")
		}
	}

	# now drop the unnecessary records
	ch = ch[ch$flag == 0, ]

	# get rid of whitespace at the beginning and end
	ch$message = str_trim(ch$message, "both")

	# Add a language variable, which is user-supplied for now
	ch$messageLanguage = languageCode

	# Add a simple numeric incrementing identifier for the messages that people submitted
	ch$messageId = 1:nrow(ch)

	# Get rid of the superfluous colon at the end of the usernames
	ch$userName = substr(ch$userName, 1, nchar(ch$userName)-1)

	# Mark as unidentified any user with a blank username
	ch$userName = ifelse(ch$userName == "" | is.na(ch$userName), "UNIDENTIFIED", ch$userName)	

	# Clean up the ordering of variables that are returned and return
	return(ch[, c("messageId", "messageTime", "userName", "message", "messageLanguage")])
}