############################################################
# textConversationAnalysis Function
############################################################
# Conversation Analysis Function
# This function takes in the output of one of the other functions (either processZoomChat or processZoomTranscript) and produces # a set of conversation measures. I don't know conversation analysis, so this is just a rough cut of things I was
# curious about. If you are a conversation expert and want to contribute, let me know!
#
# Example Call: 	o = textConversationAnalysis(inputData=outputOfOtherFunctions, inputType="chat", speakerID = "user_name", sentiment=TRUE, sentiDone=FALSE)



# INPUT ARGUMENTS:

# inputData			The output from either the processZoomChat or processZoomTranscript functions
# inputType			either "chat" or "transcript"
# speakerId			The name of the variable in inputData that contains the unique identfier (e.g., "user_name")
# doSentiment 		Boolean to indicate whether you want to run a sentiment analysis on this input file
# sentiDone			Boolean to indicate whether the file already has the sentiment variables included

# OUTPUT: 
# this function outputs a list with two items. One is a set of measures aggregated for the overall file (either chat or transcript). The second is a set of measures aggregated to the level of the individual speaker. The function presumes that speakerId is a unique identifier and treats each value as the ID for a speaker.

# Note that any time measures in the output are represented as seconds.  

# Most variable names are self-explanatory, but I'll highlight a few: 

#### For the overall transcript output ####

# utterance_gap_x: 		This is the average number of seconds between one person's utterance and the next person's utterance
# utterance_gap_sd: 	The SD of the utterance gaps
# burstiness_raw:		This is a measure of how concentrated (in time) utterances are. It is not adjusted for # of utterances

#### For the speaker-level transcript output ####

# utterance_gap_x: 		This is the average number of seconds (from the last utterance) that pass before this person makes an utterance

#### For the overall chat output ####

# message_gap_x: 		This is the average number of seconds between one person's message and the next person's message
# message_gap_sd: 		The SD of the message gaps
# burstiness_raw:		This is a measure of how concentrated (in time) chat messages are. It is not adjusted for # of messages

#### For the speaker-level chat output ####

# message_gap_x: 		This is the average number of seconds (from the last message) that pass before this person sends a message


############################################################


textConversationAnalysis = function(inputData, inputType, speakerId, doSentiment=FALSE, sentiDone=FALSE) {
	require(data.table)
	########################################
	# IF THE USER REQUESTED AN ANALYSIS OF A TRANSCRIPT FILE, DO THE FOLLOWING
	########################################			

	if(inputType=="transcript") {

		########################################
		# Do the sentiment analysis if it was requested
		########################################
		if(doSentiment==TRUE) {
			if(sentiDone==FALSE) {
				inputData = textSentiment(inputData=inputData, idVar="utteranceId", textVar="utteranceMessage", languageCodeVar="utteranceLanguage")
			}
			tab_denom = nrow(inputData[!is.na(inputData$sentClass), ])
			utterance_positive_pct = nrow(inputData[inputData$sentClass=="POSITIVE", ])/tab_denom
			utterance_neutral_pct = nrow(inputData[inputData$sentClass=="NEUTRAL", ])/tab_denom
			utterance_negative_pct = nrow(inputData[inputData$sentClass=="NEGATIVE", ])/tab_denom
			utterance_mixed_pct = nrow(inputData[inputData$sentClass=="MIXED", ])/tab_denom	

			utterance_mixed_x = mean(inputData$mixed, na.rm=T)					
			utterance_neutral_x = mean(inputData$neutral, na.rm=T)					
			utterance_negative_x = mean(inputData$negative, na.rm=T)					
			utterance_positive_x = mean(inputData$positive, na.rm=T)										

			utterance_mixed_sd = sd(inputData$mixed, na.rm=T)					
			utterance_neutral_sd = sd(inputData$neutral, na.rm=T)					
			utterance_negative_sd = sd(inputData$negative, na.rm=T)					
			utterance_positive_sd = sd(inputData$positive, na.rm=T)		
			sent.cols = cbind(utterance_positive_pct, utterance_positive_x, utterance_positive_sd, utterance_neutral_pct, utterance_neutral_x, utterance_neutral_sd, utterance_negative_pct, utterance_negative_x, utterance_negative_sd, utterance_mixed_pct, utterance_mixed_x, utterance_mixed_sd)		
		}

		########################################
		# Create a transcript-level output
		########################################

		# First, get some overall statistics - all time units are in seconds
		totalRecordedTime = as.numeric(difftime(max(inputData$utteranceEndTime), min(inputData$utteranceStartTime), units="secs"))
		utterance_time_window_sum = sum(inputData$utteranceTimeWindow)
		silent_time_sum = totalRecordedTime-utterance_time_window_sum
		utterance_time_window_x = mean(inputData$utteranceTimeWindow, na.rm=T)
		utterance_time_window_sd = sd(inputData$utteranceTimeWindow, na.rm=T)

		numUniqueSpeakers = length(unique(inputData[,speakerId]))
		numUtterances = nrow(inputData)		

		# Second, if there is more than one utterance, get the information for burstiness, which is calculated as the CV of 
		# the gap between utterances (so concentration of speech)
		inputData$utteranceGap = NA
		if(nrow(inputData) >= 2) {

			# Figure out the gap from one utterance to the next
			for(i in 2:nrow(inputData)) {
				# start time of current utterance - end time of prior utterance (in seconds)
				inputData[i, "utteranceGap"] = as.numeric(difftime(inputData[i, "utteranceStartTime"], inputData[(i-1), "utteranceEndTime"], units="secs"))
			}

			utterance_gap_x = mean(inputData$utteranceGap, na.rm=T)
			utterance_gap_sd = sd(inputData$utteranceGap, na.rm=T)		

			burstinessRaw = (sd(inputData$utteranceGap, na.rm=T)-mean(inputData$utteranceGap, na.rm=T))/(sd(inputData$utteranceGap, na.rm=T)+mean(inputData$utteranceGap, na.rm=T))
		} else {

			utterance_gap_x = NA
			utterance_gap_sd = NA
			burstinessRaw = NA
		}


		transcript_out = cbind(totalRecordedTime, numUtterances, numUniqueSpeakers, utterance_time_window_sum, silent_time_sum, utterance_time_window_x, utterance_time_window_sd, utterance_gap_x, utterance_gap_sd, burstinessRaw)

		# If we've just done the sentiment analysis above, add the output from that to this object
		if(doSentiment==TRUE) transcript_out = cbind(transcript_out, sent.cols)		

		########################################
		# Create an individual-level output
		# Note, the presumption is that userName is something unique -- This may not be the case if 
		# different peopl have the exact same userName in the Zoom Transcript
		########################################
		dt = data.table(inputData)

		if(doSentiment == TRUE || sentiDone == TRUE) {
			agg.dt = dt[,list(utterance_time_window_sum = sum(utteranceTimeWindow, na.rm=T), num_utterances = .N, utterance_time_x = mean(utteranceTimeWindow, na.rm=T), utterance_time_sd = sd(utteranceTimeWindow, na.rm=T), utterance_gap_x = mean(utteranceGap, na.rm=T), utterance_gap_sd = sd(utteranceGap, na.rm=T),
				utterance_positive_pct = sum(sentClass=="POSITIVE")/.N, utterance_positive_x = mean(positive, na.rm=T), utterance_positive_sd = sd(positive, na.rm=T), 
				utterance_negative_pct = sum(sentClass=="NEGATIVE")/.N, utterance_negative_x = mean(negative, na.rm=T), utterance_negative_sd = sd(negative, na.rm=T), 				 		
				utterance_neutral_pct = sum(sentClass=="NEUTRAL")/.N, utterance_neutral_x = mean(neutral, na.rm=T), utterance_neutral_sd = sd(neutral, na.rm=T), 				 						
				utterance_mixed_pct = sum(sentClass=="MIXED")/.N, utterance_mixed_x = mean(mixed, na.rm=T), utterance_mixed_sd = sd(mixed, na.rm=T)				
				), by=list(get(speakerId))]
			names(agg.dt)[1] = speakerId

		} else {
			agg.dt = dt[,list(utterance_time_window_sum = sum(utteranceTimeWindow, na.rm=T), num_utterances = .N, utterance_time_x = mean(utteranceTimeWindow, na.rm=T), utterance_time_sd = sd(utteranceTimeWindow, na.rm=T), utterance_gap_x = mean(utteranceGap, na.rm=T), utterance_gap_sd = sd(utteranceGap, na.rm=T)), by=list(get(speakerId))]
			names(agg.dt)[1] = speakerId
		}

		agg.out = data.frame(agg.dt)

		res.out = list("TRANSCRIPT-LEVEL" = data.frame(transcript_out, stringsAsFactors=F), "SPEAKER-LEVEL" = agg.out)

	########################################
	# IF THE USER REQUESTED AN ANALYSIS OF A CHAT FILE, DO THE FOLLOWING
	########################################		

	} else if(inputType=="chat") {

		########################################
		# Do the sentiment analysis if it was requested
		########################################
		if(doSentiment==TRUE) {
			if(sentiDone==FALSE) {
				inputData = textSentiment(inputData=inputData, idVar="messageId", textVar="message", languageCodeVar="messageLanguage")
			}
			tab_denom = nrow(inputData[!is.na(inputData$sentClass), ])
			message_positive_pct = nrow(inputData[inputData$sentClass=="POSITIVE", ])/tab_denom
			message_neutral_pct = nrow(inputData[inputData$sentClass=="NEUTRAL", ])/tab_denom
			message_negative_pct = nrow(inputData[inputData$sentClass=="NEGATIVE", ])/tab_denom
			message_mixed_pct = nrow(inputData[inputData$sentClass=="MIXED", ])/tab_denom	

			message_mixed_x = mean(inputData$mixed, na.rm=T)					
			message_neutral_x = mean(inputData$neutral, na.rm=T)					
			message_negative_x = mean(inputData$negative, na.rm=T)					
			message_positive_x = mean(inputData$positive, na.rm=T)										

			message_mixed_sd = sd(inputData$mixed, na.rm=T)					
			message_neutral_sd = sd(inputData$neutral, na.rm=T)					
			message_negative_sd = sd(inputData$negative, na.rm=T)					
			message_positive_sd = sd(inputData$positive, na.rm=T)		
			sent.cols = cbind(message_positive_pct, message_positive_x, message_positive_sd, message_neutral_pct, message_neutral_x, message_neutral_sd, message_negative_pct, message_negative_x, message_negative_sd, message_mixed_pct, message_mixed_x, message_mixed_sd)	
		}

		########################################
		# Create a chat-level output
		########################################
		inputData$messageNumChars = nchar(inputData$message)

		# First, get some overall statistics - all time units are in seconds
		totalRecordedTime = as.numeric(difftime(max(inputData$messageTime), min(inputData$messageTime), units="secs"))
		message_numchars_sum = sum(inputData$messageNumChars)
		message_numchars_x = mean(inputData$messageNumChars)
		message_numchars_sd = sd(inputData$messageNumChars)				

		numUniqueMessagers = length(unique(inputData[,speakerId]))
		numMessages = nrow(inputData)		

		# Second get the information for burstiness, which is calculated as the CV of 
		# the gap between messages (so concentration of speech)

		# Figure out the gap from one message to the next
		inputData$messageGap = NA
		if(numMessages > 1) {
			for(i in 2:nrow(inputData)) {
				# start time of current utterance - end time of prior utterance (in seconds)
				inputData[i, "messageGap"] = as.numeric(difftime(inputData[i, "messageTime"], inputData[(i-1), "messageTime"], units="secs"))
			}			
		}

		message_gap_x = mean(inputData$messageGap, na.rm=T)
		message_gap_sd = sd(inputData$messageGap, na.rm=T)		

		burstinessRaw = (sd(inputData$messageGap, na.rm=T)-mean(inputData$messageGap, na.rm=T))/(sd(inputData$messageGap, na.rm=T)+mean(inputData$messageGap, na.rm=T))

		chat_out = cbind(totalRecordedTime, numMessages, message_numchars_sum, numUniqueMessagers, message_gap_x, message_gap_sd, burstinessRaw)
		if(doSentiment==TRUE) chat_out = cbind(chat_out, sent.cols)		

		########################################
		# Create an individual-level output
		# Note, the presumption is that user_name is something unique -- hopefully it is!
		########################################		

		dt = data.table(inputData)

		if(doSentiment == TRUE || sentiDone == TRUE) {
			agg.dt = dt[,list(message_numchars_sum = sum(messageNumChars, na.rm=T), num_messages = .N, message_numchars_x = mean(messageNumChars), message_numchars_sd = sd(messageNumChars), message_gap_x = mean(messageGap, na.rm=T), message_gap_sd = sd(messageGap, na.rm=T),
				message_positive_pct = sum(sentClass=="POSITIVE")/.N, message_positive_x = mean(positive, na.rm=T), message_positive_sd = sd(positive, na.rm=T), 
				message_negative_pct = sum(sentClass=="NEGATIVE")/.N, message_negative_x = mean(negative, na.rm=T), message_negative_sd = sd(negative, na.rm=T), 				 		
				message_neutral_pct = sum(sentClass=="NEUTRAL")/.N, message_neutral_x = mean(neutral, na.rm=T), message_neutral_sd = sd(neutral, na.rm=T), 				 						
				message_mixed_pct = sum(sentClass=="MIXED")/.N, message_mixed_x = mean(mixed, na.rm=T), message_mixed_sd = sd(mixed, na.rm=T)				
				), by=list(get(speakerId))]
			names(agg.dt)[1] = speakerId

		} else {
			agg.dt = dt[,list(message_numchars_sum = sum(messageNumChars, na.rm=T), num_messages = .N, message_numchars_x = mean(messageNumChars), message_numchars_sd = sd(messageNumChars), message_gap_x = mean(messageGap, na.rm=T), message_gap_sd = sd(messageGap, na.rm=T)				
				), by=list(get(speakerId))]
			names(agg.dt)[1] = speakerId
		}

		agg.out = data.frame(agg.dt)
		res.out = list("CHAT-LEVEL" = chat_out, "USER-LEVEL" = agg.out)		
	}
}