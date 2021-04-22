turnTaking = function(inputData, inputType, speakerId) {
	require(data.table)
	if(inputType=="transcript") {
		t.out = inputData

		# Get the names of the unique speakers in this file
		uniqueUsers = sort(unique(t.out[,speakerId]))

		# Create an empty matrix that will hold counts of the utterances
		userMatrix = matrix(data=rep(0, length(uniqueUsers)^2), nrow=length(uniqueUsers), ncol=length(uniqueUsers))
		rownames(userMatrix) = uniqueUsers
		colnames(userMatrix) = uniqueUsers		

		# Create an empty dyad-level dataset that will be easy to use and do any of the norming that
		# might be useful for SRM

		dyadData = data.frame(list(speakerBefore=sort(rep(uniqueUsers, length(uniqueUsers))), speakerCurrent = rep(uniqueUsers, length(uniqueUsers)), numUtterances = rep(0, length(uniqueUsers)^2)))

		# Go through the transcript, utterance-by-utterance to count the number of times a given speaker followed
		# someone in the matrix
		# Skipping the first and last utterances so that it is a balanced matrix
		for(i in 2:(nrow(t.out)-1)) {

			# this is name of the person who preceded the current speaker
			speakerBefore = t.out[(i-1), speakerId]
			speakerCurrent = t.out[i, speakerId]

			# Augment that cell in the matrix. The matrix is such that the row is the preceding speaker (from) and the 
			# column is the current speaker (to)
			userMatrix[speakerBefore, speakerCurrent] = userMatrix[speakerBefore, speakerCurrent] + 1

			# Augment that row in the dyadic dataset
			dyadData[dyadData$speakerBefore == speakerBefore & dyadData$speakerCurrent == speakerCurrent, "numUtterances"] = dyadData[dyadData$speakerBefore == speakerBefore & dyadData$speakerCurrent == speakerCurrent, "numUtterances"] + 1
		}

		# Create a few objects that give different normed versions of this
		rawCount = userMatrix
		rawCountNoDiag = userMatrix
		diag(rawCountNoDiag) = NA

		# This is the percent of utterances across the whole matrix (excluding the diagonal)
		pctByConvo = rawCountNoDiag/sum(rawCountNoDiag, na.rm=T)

		# This is the percent of utterances normed by the speaker. So it asks 
		# what percent of a given speakers utterances follow a given person
		pctBySpeaker = rawCountNoDiag
		for(c in 1:ncol(rawCountNoDiag)) {

			pctBySpeaker[,c] = rawCountNoDiag[,c]/sum(rawCountNoDiag[,c], na.rm=T)
		}

		# Create the normed variables for dyadData - excluding self for these
		dyadDt = data.table(dyadData)
		speakerAgg = data.frame(dyadDt[speakerBefore != speakerCurrent, list(sumSpeakerCurrentUtterances = sum(numUtterances)), by=list(speakerCurrent)])
		dyadData = merge(dyadData, speakerAgg, by="speakerCurrent")
		dyadData[dyadData$speakerBefore == dyadData$speakerCurrent, "sumSpeakerCurrentUtterances"] = NA		
		dyadData$pctBySpeakerCurrent = dyadData$numUtterances/dyadData$sumSpeakerCurrentUtterances
		dyadData$pctByConvo = dyadData$numUtterances/sum(dyadData[dyadData$speakerBefore != speakerCurrent, "numUtterances"])
		dyadData$pctByConvo = ifelse(dyadData$speakerBefore == dyadData$speakerCurrent, NA, dyadData$pctByConvo)

		# Create an individual-level file that represents the average percent of a speaker's utterances (and of the conversation) that come after one of the people
		dyadDt2 = data.table(dyadData)
		speakerAgg2 = data.frame(dyadDt2[speakerBefore != speakerCurrent, list(numAfterSpeaker = sum(numUtterances, na.rm=T), pctAfterSpeaker = mean(pctBySpeakerCurrent, na.rm=T), pctAfterConvo = mean(pctByConvo, na.rm=T)), by=list(speakerBefore)])
		names(speakerAgg2)[1] = speakerId



	}
	o.list = list("rawCount" = rawCount, "rawCountNoDiag" = rawCountNoDiag, "pctByConvo" = pctByConvo, "pctBySpeaker" = pctBySpeaker, "dyadData" = dyadData, "indivData" = speakerAgg2)
	return(o.list)
}