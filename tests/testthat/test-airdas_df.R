context("airdas_df class")

test_that("coerce from data.frame to airdas_df", {
  y.proc <- airdas_process(system.file("airdas_sample.das", package = "swfscAirDAS"))

  y.df <- data.frame(y.proc)
  y1 <- as_airdas_df(y.df)
  y2 <- as_airdas_df(y.df[, c(2, 1, 3:ncol(y.df))])
  
  expect_s3_class(y1, c("airdas_df", "data.frame"), exact = TRUE)
  expect_s3_class(y2, c("airdas_df", "data.frame"), exact = TRUE)
  expect_equal(y.proc, y1)
  
  # expect_error(as_airdas_df(y.proc[, -1]))
  expect_error(as_airdas_df(y.df[, -1]))
  # expect_error(as_airdas_df(y.df[1, ])) 
  #TODO^: what row checks to do
})

# test_that("subset airdas_df object", {
#   y.proc <- airdas_process(system.file("airdas_sample.das", package = "swfscAirDAS"))
#   
# })
