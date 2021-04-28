test_that("Chat parsing yields correct records", {
  ch.out = processZoomChat(fname=system.file('extdata', "sample_chat.txt", package = 'zoomGroupStats'), sessionStartDateTime = '2020-04-20 13:30:00',languageCode = 'en')
  
  expect_equal(nrow(ch.out), 10)
  expect_equal(length(unique(ch.out$userName)), 5)
  expect_equal(lubridate::is.POSIXct(ch.out$messageTime), TRUE)
  
})
