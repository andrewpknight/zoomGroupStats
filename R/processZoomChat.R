#' Process a Zoom chat file
#' 
#' Parses the data from the chatfile that is downloaded from the Zoom Cloud recording
#' site. Note that this is the file that accompanies a recording. This is not the file 
#' that you might download directly within a given Zoom session, nor is it the one
#' that is saved locally on your computer. This is the file that you can access 
#' after a session if you record in the cloud.
#'
#' @param fname String that is the path to the downloaded Zoom .txt chat file
#' @param sessionStartDateTime String that is the start of the session in YYYY-MM-DD HH:MM:SS
#' @param languageCode String denoting the language
#'
#' @return data.frame where each record is a message submission in the chat, containing columns:
#' \itemize{
#'     \item messageId - Numeric identifier for each message, only unique within a given meeting
#'     \item messageSeconds - When message was posted, in number of seconds from start of session 
#'     \item messageTime - When message was posted as POSIXct, using the supplied sessionStartDateTime
#'     \item userName - Display name of user who posted the message
#'     \item message - Text of the message that was posted
#'     \item messageLanguage - Language code for the message
#' }
#' @export
#'
#' @examples
#' ch.out = processZoomChat(
#' fname=system.file('extdata', "meeting001_chat.txt", package = 'zoomGroupStats'), 
#' sessionStartDateTime = '2020-04-20 13:30:00', 
#' languageCode = 'en')
processZoomChat = function(fname, sessionStartDateTime="1970-01-01 00:00:00", languageCode="en") {
  
  # NOTE: Need to fix this to remove any stray tabs from this file before bringing it in. I have run into a few edge cases where participants use tabs in their messages and it screws up the file. Need to experiment with this and introduce (likely) a brute force parser for this file. 
  
  ch = utils::read.delim(fname, sep="\t", stringsAsFactors=F, header=F, col.names=c("messageIncrement", "userName", "message"), quote="")
  
  ####################################
  # Chat transcripts do not handle soft returns well (i.e., if the same person uses a soft line break 
  # for multiple lines in a single message that is submitted to the system). 
  # This is a crude way to identify them based on someone having an invalid message time. 
  # For now, will assign that text to the last marked user name in the dataset, 
  # pasting the messages together into a single line (separated by a space. )
  
  # Create a flag to mark erroneous records based on the message time variable. This should be made stronger
  # and cleaner eventually
  ch$flag = ifelse(!(grepl('(?:[01]\\d|2[0123]):(?:[012345]\\d):(?:[012345]\\d)', ch$messageIncrement)) | ch$userName=="", TRUE, FALSE)
  
  # Assign the value in the message_increment variable to the message variable. This is because
  # the parsing of the file is screwed up when there are soft returns in someone's chat message
  ch$message = ifelse(ch$flag, ch$messageIncrement, ch$message)
  
  # Go through the records from the bottom up to paste the message on the one it 
  # should be part of
  for(i in nrow(ch):1) {
    if(ch[i,"flag"]) {
      ch[(i-1), "message"] = paste(ch[(i-1), "message"], ch[i, "message"], sep=" ")
    }
  }
  
  # now drop the unnecessary records
  ch = ch[!ch$flag, ]
  
  # Apply date and time
  sessionStartDateTime = as.POSIXct(sessionStartDateTime, tz=Sys.timezone())
  
  ch$messageSeconds = as.numeric(lubridate::seconds(lubridate::hms(ch$messageIncrement)))
  ch$messageTime = sessionStartDateTime + ch$messageSeconds
  
  
  # get rid of whitespace at the beginning and end
  ch$message = gsub("^\\s+|\\s+$", "", ch$message)
  
  # Add a language variable, which is user-supplied for now
  ch$messageLanguage = languageCode
  
  # Add a simple numeric incrementing identifier for the messages that people submitted
  ch$messageId = 1:nrow(ch)
  
  # Get rid of the superfluous colon at the end of the usernames
  ch$userName = substr(ch$userName, 1, nchar(ch$userName)-1)
  
  # Mark as unidentified any user with a blank username
  ch$userName = ifelse(ch$userName == "" | is.na(ch$userName), "UNIDENTIFIED", ch$userName)	
  
  # Clean up the ordering of variables that are returned and return
  return(ch[, c("messageId", "messageSeconds", "messageTime", "userName", "message", "messageLanguage")])
}