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
#' @param idVars vector with the name of variables that give the unique identifiers for this piece of text. Usually this will be a the meeting id variable and the text id variable (e.g., utteranceId, messageId)
#' @param textVar name of variable that contains the text
#' @param sentMethods a vector specifying the types of sentiment analysis-currently
#' either "aws" or "syuzhet"
#' @param appendOut boolean indicating whether you want the sentiment results
#' merged to the inputData in your output
#' @param languageCodeVar name of variable that contains the language code
#'
#' @return returns a list containing as data.frames the output of the sentiment analyses
#' that were requested in sentMethods. For each output data.frame, the first columns
#' are the idVars specified to enable combining back with the original inputData
#' @export
#'
#' @examples
#' sent.out = textSentiment(inputData=sample_chat_processed,
#' idVars=c('batchMeetingId', 'messageId'), 
#' textVar='message', sentMethods='syuzhet',appendOut=TRUE,
#' languageCodeVar='messageLanguage')
#' 
#' \dontrun{
#' sent.out = textSentiment(inputData=sample_transcript_processed, 
#' idVars=c('batchMeetingId','utteranceId'), 
#' textVar='utteranceMessage', sentMethods=c('aws','syuzhet'), 
#' appendOut=TRUE, languageCodeVar='utteranceLanguage')
#' }
#' 
textSentiment = function(inputData, idVars, textVar, sentMethods, appendOut=FALSE, languageCodeVar){
  aws_sentClass <- NULL  

  if(length(idVars[!(idVars %in% names(inputData))]) > 0) {
    stop("One or more idVars is not in inputData: ",paste(idVars[!(idVars %in% names(inputData))], sep=", "))
  }  
  

  
  
  returnList = list()
  if("aws" %in% sentMethods) {	
    message("Running AWS machine learning-based sentiment analysis")
    if(nrow(inputData) >= 100) {
      message("Your input data has ",nrow(inputData), " records. Conducting a sentiment analysis on a dataset of this size using aws will take time. Be patient while the function does its work.")
    }
    
    svc = paws::comprehend()
    aws.o = pbapply::pblapply(inputData[, textVar], function(x, y=inputData[,languageCodeVar]) svc$detect_sentiment(Text = x, LanguageCode=y))
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
    
    if(appendOut) {
      awsOutput = merge(inputData, awsOutput, by=idVars)
      awsOutput = dplyr::arrange(awsOutput, get(idVars[1]), get(idVars[2]))            
    }
    
    returnList[["aws"]] = awsOutput
  }
  
  if("syuzhet" %in% sentMethods) {
    message("Running syuzhet lexicon-based sentiment analysis")    
    if(nrow(inputData) >= 1000) {
      message("Your input data has ",nrow(inputData), " records. Conducting a sentiment analysis on a dataset of this size, even with the lexicon-based syuzhet, will take time. Be patient while the function does its work.")
    }    
    
    syuzhetData = inputData
    syuzhetData[,textVar] = gsub("[^[:alnum:][:space:]']", "", syuzhetData[,textVar])
    syuzhetData$wordCount = stringr::str_count(syuzhetData[,textVar], '\\w+')
    syu.o.data = do.call(rbind, pbapply::pblapply(syuzhetData[,textVar], syuzhet::get_nrc_sentiment))		
    names(syu.o.data) = paste("syu", names(syu.o.data), sep="_")    
    syuOutput = cbind(syuzhetData[,c(idVars, "wordCount")], syu.o.data)
    names(syuOutput)[1:length(idVars)] = idVars
    
    if(appendOut) {
      syuOutput = merge(inputData, syuOutput, by=idVars)
      syuOutput = dplyr::arrange(syuOutput, get(idVars[1]), get(idVars[2]))      
    }
    
    returnList[["syuzhet"]] = syuOutput
  }	
  return(returnList)
}

