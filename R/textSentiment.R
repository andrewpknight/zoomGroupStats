#' Conduct a sentiment analysis on text data
#' 
#' This function takes in the output of the chat and transcript functions. It then
#' conducts a sentiment analysis on an identified chunk of text
#' and returns the values. 
#' To use the aws option, you must have an aws account that with privileges for the comprehend service
#' However you authenticate for AWS, you should do so before running calling the function
#' with this option in sentMethods
#'
#' @param inputData data.frame that has been output by either the processZoomTranscript or processZoomChat functions 
#' @param idVars vector with the name of variables that give the unique identifiers for this piece of text
#' @param textVar name of variable that contains the text
#' @param sentMethods a vector specifying the types of sentiment analysis-currently
#' either "aws" or "syuzhet"
#' @param languageCodeVar name of variable that contains the language code
#'
#' @return returns a list containing as data.frames the output of the sentiment analyses
#' that were requested in sentMethods. For each output data.frame, the first columns
#' are the idVars specified to enable combining back with the original inputData
#' @export
#'
#' @examples
#' sent.out = textSentiment(inputData=sample_transcript_processed, idVar=c('utteranceId'), 
#' textVar='utteranceMessage', sentMethods='syuzhet', languageCodeVar='utteranceLanguage')

textSentiment = function(inputData, idVars, textVar, sentMethods, languageCodeVar){
  aws_sentClass <- NULL  
  returnList = list()
  if("aws" %in% sentMethods) {		
    svc = paws::comprehend()
    aws.o = lapply(inputData[, textVar], function(x, y=inputData[,languageCodeVar]) svc$detect_sentiment(Text = x, LanguageCode=y))
    aws.o.data = do.call(rbind, lapply(aws.o, FUN=function(x) {	
      aws_sentClass = x$Sentiment
      aws_positive = x$SentimentScore$Positive
      aws_negative = x$SentimentScore$Negative
      aws_neutral = x$SentimentScore$Neutral
      aws_mixed = x$SentimentScore$Mixed
      return(data.frame(aws_sentClass, aws_positive, aws_negative, aws_neutral, aws_mixed))
    })) 
    awsOutput = cbind(inputData[,c(idVars)], aws.o.data)
    names(awsOutput)[1:length(idVars)] = idVars
    returnList[["aws"]] = awsOutput
  }
  
  if("syuzhet" %in% sentMethods) {
    syuzhetData = inputData
    syuzhetData[,textVar] = gsub("[^[:alnum:][:space:]']", "", syuzhetData[,textVar])
    syuzhetData$wordCount = stringr::str_count(syuzhetData[,textVar], '\\w+')
    syu.o.data = do.call(rbind, lapply(syuzhetData[,textVar], syuzhet::get_nrc_sentiment))		
    names(syu.o.data) = paste("syu", names(syu.o.data), sep="_")    
    syuOutput = cbind(syuzhetData[,c(idVars, "wordCount")], syu.o.data)
    names(syuOutput)[1:length(idVars)] = idVars
    returnList[["syuzhet"]] = syuOutput
  }	
  return(returnList)
}