#' Create a file to aid in adding a unique identifier to link to the zoom user name
#' 
#' A major challenge in analyzing virtual meetings is reconciling the display
#' name that zoom users in chat and transcript. This function outputs a data.frame
#' that can be helpful in manually adding a new unique identifier to use in 
#' further data anlaysis.
#'
#' @param zoomOutput the output from running processZoomOutput 
#'
#' @return a data.frame that has unique values for the zoom display name
#' that show up across any files that are available, including
#' participants, transcript, and chat. If the user gives the participants
#' file, it will separate display name changes and include all versions. If
#' there are emails attached to display names, it will include those.
#'
#' @export
#'
#' @examples
#' rosetta.out = createZoomRosetta(processZoomOutput(fileRoot=
#' file.path(system.file('extdata', package = 'zoomGroupStats'),"meeting001")))
#' \dontrun{
#' rosetta.out = createZoomRosetta(processZoomOutput(fileRoot="~/zoomMeetings/meeting001"))
#' }
createZoomRosetta = function(zoomOutput) {
  
  uniqueTranscriptNames = c()
  uniqueChatNames = c()	
  uniquePartNames = c()
  
  if(!is.null(zoomOutput$transcript)) {
    uniqueTranscriptNames = unique(zoomOutput$transcript$userName)
  }
  
  if(!is.null(zoomOutput$chat)) {
    uniqueChatNames = unique(zoomOutput$chat$userName)
  }	
  
  if(!is.null(zoomOutput$partInfo)) {
    
    uniqParts = zoomOutput$partInfo[,c("userName", "userEmail")]
    uniqParts$userName2 = lapply(uniqParts$userName, function(x) regmatches(x, gregexpr("(?<=\\().*?(?=\\))", x, perl=T))[[1]])
    uniqParts$userName = unlist(gsub("\\s*\\([^\\)]+\\)","",as.character(uniqParts$userName)))		
    
    uniqParts$nameChange = unlist(lapply(uniqParts$userName2, function(x) ifelse(length(x) > 0, 1, 0)))
    
    appendNames1 = uniqParts[uniqParts$nameChange==1, c("userName2", "userEmail", "nameChange")]
    names(appendNames1)[1] = "userName"
    appendNames1$userName = unlist(appendNames1$userName)
    
    appendNames2 = uniqParts[uniqParts$nameChange==1, c("userName", "userEmail", "nameChange")]
    
    uniqParts = unique(rbind(uniqParts[uniqParts$nameChange==0, c("userName", "userEmail", "nameChange")], appendNames1, appendNames2) )
    
    uniquePartNames=unique(uniqParts$userName)
  }
  
  allNames = data.frame(userName=unique(c(uniqueTranscriptNames, uniqueChatNames, uniquePartNames)), stringsAsFactors=F)
  if(!is.null(zoomOutput$partInfo)) {
    allNames = merge(allNames, uniqParts[, c("userName", "userEmail")], by="userName", all.x=T)
  }
  return(allNames)
}