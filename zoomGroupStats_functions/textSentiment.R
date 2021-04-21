############################################################
# textSentiment Function
############################################################
# Text-based sentiment analysis function
# This function takes in the output of the chat and transcript functions. It then
# conducts a sentiment analysis on an identified chunk of text
# and returns the values. 
# To use this function, you must have an aws account that with privileges for the comprehend service
# However you authenticate for AWS, you should do so before running the function.

# example call:			sent.out = textSentiment(inputData=tr.out, idVar = "utteranceId", textVar = "utteranceMessage", languageCodeVar = "utteranceLanguage")

# INPUT:
# inputData: 			the input data frame
# idVar: 				the name of the id variable for the text 
# textVar:				the name of the variable with the text in it
# languageCodeVar:		the variable containing the language code for each text chunk

# OUTPUT: This function returns the inputData plus the following variables
# sentClass: 			the text-based sentiment classification of the message
# mixed:				the confidence level for the text being mixed sentiment
# negative: 			the confidence level for the text being negative sentiment
# neutral: 				the confidence level for the text being neutral sentiment
# positive: 			the confidence level for the text being positive sentiment

# Note: This function currently does this in a brute force way. In the future, I will
# build this so that it batches chunks of text to run, rather than looping through.

textSentiment = function(inputData, idVar, textVar, languageCodeVar){
	require(paws)
	require(reshape2)
	# Identify the AWS service comprehend: 
	# AS STATED ABOVE--YOU MUST HAVE AN AUTHENTICATED ACCOUNT WITH THE RIGHT PRIVILIGES
	svc = comprehend()

	# Loop through each record of the inputData
	for(i in 1:nrow(inputData)) {

		# Run the sentiment detection function from AWS Comprehend on this chunk of text
		sent = svc$detect_sentiment(Text = inputData[i,textVar], LanguageCode=inputData[i,languageCodeVar])

		# Create a simple 
		res.line = cbind(inputData[i,idVar],unlist(sent$SentimentScore), sent$Sentiment)
		if(i == 1) {
			res.out = res.line
		} else {
			res.out = rbind(res.out, res.line)
		}		
	}

	# Now, clean up the output so that it comes as a dataframe
	d.res = data.frame(res.out, stringsAsFactors=F)
	names(d.res) = c(idVar, "sentValue", "sentClass")
	d.res$sentType = unlist(lapply(strsplit(row.names(d.res), '[.]'), '[[',1))

	d.res.melt = suppressMessages({reshape2::melt(d.res, idVars=c(idVar, sentClass, "sentType"), variable.name="sentVariable", value.name="sentValue")})

	d.res.wide = reshape2::dcast(d.res.melt, get(idVar) + sentClass ~ sentType, value.var="sentValue")
	d.res.wide[,c("Mixed", "Negative", "Neutral", "Positive")] = lapply(d.res.wide[,c("Mixed", "Negative", "Neutral", "Positive")], as.numeric)
	names(d.res.wide) = c(idVar, "sentClass", "mixed", "negative", "neutral", "positive")

	d.mrg = merge(inputData, d.res.wide, by=idVar, all.x=T)
	return(d.mrg)
}
