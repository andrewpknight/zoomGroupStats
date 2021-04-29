test_that("zoomOutput grabs available files", {
  z.out = processZoomOutput(fileRoot=paste(system.file('extdata', package='zoomGroupStats'), "sample", sep="/"))
  expect_equal(length(z.out), 4)
  expect_equal(is.null(z.out$meetInfo), FALSE)
  expect_equal(is.null(z.out$partInfo), FALSE)
  expect_equal(is.null(z.out$transcript), FALSE)
  expect_equal(is.null(z.out$chat), FALSE)  
})
