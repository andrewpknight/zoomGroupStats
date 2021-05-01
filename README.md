
# zoomGroupStats

<!-- badges: start -->
<!-- badges: end -->

zoomGroupStats provides utilities for processing and analyzing the files that are exported from recorded Zoom meetings. This includes analyzing data captured through video cameras and microphones, the text-based chat, and meta-data. You can analyze aspects of the conversation among meeting participants, aspects of their public chatting behavior, and their facial expressions throughout the meeting. 

## Installing & Getting Started

This is a work in progress. I am still making substantive changes to the package and actively developing the documentation. The quickest way to get going with this is to install the github version of the package and follow along with the [vignette](http://apknight.org/docs/articles/process-zoom-files.html) that I am developing. 

``` r
library(devtools)
devtools::install_github("https://github.com/andrewpknight/zoomGroupStats", build_vignettes=TRUE)
library(zoomGroupStats)
vignette("process-zoom-files")
```

As I'm building this, I'm publishing [ongoing documentation for zoomGroupStats on my website](http://apknight.org/docs/).

An alternative approach to use while the package is being built is to source older versions of the functions alone directly from my website:

``` r
source("http://apknight.org/zoomGroupStats.R")
```

## Examples

The vignette referenced above provides far more detail about how to use this package, but here are some sample bits of code: 

### Process information for a batch of meetings

```r
batchOut = batchProcessZoomOutput(batchInput="./myMeetingsBatch.xlsx", exportZoomRosetta="./myMeetings_rosetta_original.xlsx")
```

### Add a unique individual identifier

```r
batchOutIds = importZoomRosetta(zoomOutput=batchOut, zoomRosetta="./myEditedRosetta.xlsx", 
meetingId="batchMeetingId")
```

### Conduct sentiment analysis on transcribed audio data

``` r 
 transcriptSent = textSentiment(inputData=batchOutIds$transcript, idVar=c('utteranceId'), textVar='utteranceMessage', sentMethods=c('aws', 'syuzhet'), appendOut=TRUE, languageCodeVar='utteranceLanguage')
```

### Conduct sentiment analysis on chat data

``` r 
 transcriptSent = textSentiment(inputData=batchOutIds$chat, idVar=c('messageId'), textVar='message', sentMethods=c('aws', 'syuzhet'), appendOut=TRUE, languageCodeVar='messageLanguage')
```

### Conduct conversation analysis on transcribed audio data

``` r 
convo.out = textConversationAnalysis(inputData=transcriptSent$aws, inputType='transcript', meetingId='batchMeetingId', speakerId='indivId', sentMethod="aws")
```

### Analyze the facial expressions in the video feed

``` r 
vid.out = videoFaceAnalysis(inputVideo="sample_gallery_video.mp4", recordingStartDateTime="2020-04-20 13:30:00", sampleWindow=30, facesCollectionID="group-r")
```