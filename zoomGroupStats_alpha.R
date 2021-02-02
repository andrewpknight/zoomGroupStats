makeTimeWindows = function(inputData, inputType, windowSize) {
	inputData=inputData[order(inputData$utterance_id), ]
	if(inputType == "transcript") {
		
		inputData$window_id = NA
		inputData$window_start_seconds = NA
		inputData$window_end_seconds = NA		
		count = 1
		for(i in ceiling(max(inputData$utterance_end_seconds)/windowSize):0) {
			window_start_seconds = i*windowSize
			window_end_seconds = (i+1)*windowSize

			inputData$window_id = ifelse(inputData$utterance_end_seconds >= window_start_seconds & inputData$utterance_end_seconds <= window_end_seconds, (i+1), inputData$window_id) 

			inputData$window_start_seconds = ifelse(inputData$utterance_end_seconds >= window_start_seconds & inputData$utterance_end_seconds <= window_end_seconds, window_start_seconds, inputData$window_start_seconds) 

			inputData$window_end_seconds = ifelse(inputData$utterance_end_seconds >= window_start_seconds & inputData$utterance_end_seconds <= window_end_seconds, window_end_seconds, inputData$window_end_seconds) 			

			time_window_info = data.frame((i+1), window_start_seconds, window_end_seconds)

			names(time_window_info) = c("window_id", "window_start_seconds", "window_end_seconds")

			if(count == 1) {
				res.line = time_window_info
			} else {
				res.line = rbind(res.line, time_window_info)
			}
			count = count + 1
		}

	}
	return(list(inputData, res.line[order(res.line$window_id), ]))
}


windowedTextConversationAnalysis = function(inputData, inputType, sentiment, speakerId, windowSize, sentiDone) {
	t.out.windowed = makeTimeWindows(inputData, inputType, windowSize)
	inputData = t.out.windowed[[1]]

	# Create a blank set that gives each user an opportunity to have an aggregate
	# metric during each of the time windows
	participants = sort(unique(inputData[,speakerId]))
	user_name = rep(participants,max(inputData$window_id))
	window_id = sort(rep(1:max(inputData$window_id), length(participants)))
	p3 = data.frame(user_name, window_id, stringsAsFactors=F)
	p4 = merge(p3, t.out.windowed[[2]], by="window_id")

	# Now, loop through the time windows 
	count = 1

	for(win in 1:max(inputData$window_id)) {

		windowed.input = inputData[inputData$window_id == win, ]

		if(nrow(windowed.input) > 0) {

			res.line = textConversationAnalysis(inputData=windowed.input, inputType="transcript", sentiment=sentiment, speakerId = speakerId, sentiDone=sentiDone)

			grp.res.line = res.line[[1]]
			grp.res.line$window_id = win

			ind.res.line = res.line[[2]]
			ind.res.line$window_id = win

			if(count == 1) {
				grp.res.out = grp.res.line
				ind.res.out = ind.res.line		
			} else {
				grp.res.out = rbind(grp.res.out, grp.res.line)
				ind.res.out = rbind(ind.res.out, ind.res.line)
			}
			count = count + 1
		}
	}	

	p5 = merge(p4, ind.res.out, by=c("user_name", "window_id"), all.x=T)
	p5$utterance_time_window_sum = ifelse(is.na(p5$utterance_time_window_sum), 0, p5$utterance_time_window_sum)
	p5$num_utterances = ifelse(is.na(p5$num_utterances), 0, p5$num_utterances)

	grp1 = merge(t.out.windowed[[2]], grp.res.out, by=c("window_id"), all.x=T)
	grp1$utterance_time_window_sum = ifelse(is.na(grp1$utterance_time_window_sum), 0, grp1$utterance_time_window_sum)
	grp1$num_utterances = ifelse(is.na(grp1$num_utterances), 0, grp1$num_utterances)	
	grp1$total_recorded_time = ifelse(is.na(grp1$total_recorded_time), 0, grp1$total_recorded_time)		

	grp1$num_unique_speakers = ifelse(is.na(grp1$num_unique_speakers), 0, grp1$num_unique_speakers)			

	
	return(list("TRANSCRIPT-LEVEL" = grp1, "SPEAKER-LEVEL" = p5))	
}
