test_that("airdas_read output has expected column names and classes", {
  y.read <- airdas_read(system.file("airdas_sample.das", package = "swfscAirDAS"))
  y.read2 <- as_airdas_dfr(data.frame(y.read))
  
  # Same as in as_airdas_dfr()
  exp.class <- list(
    Event = "character",
    EffortDot = "logical",
    DateTime = c("POSIXct", "POSIXt"),
    Lat = "numeric",
    Lon = "numeric",
    Data1 = "character",
    Data2 = "character",
    Data3 = "character",
    Data4 = "character",
    Data5 = "character",
    Data6 = "character",
    Data7 = "character",
    EventNum = "integer",
    file_das = "character",
    line_num = "integer"
  )
  
  expect_identical(exp.class, lapply(y.read, class))
  expect_identical(exp.class, lapply(y.read2, class))
})


test_that("airdas_process output has expected column names and classes", {
  y.proc <- airdas_process(system.file("airdas_sample.das", package = "swfscAirDAS"))
  y.proc2 <- as_airdas_df(data.frame(y.proc))
  
  # Same as in as_airdas_df()
  exp.class <- list(
    Event = "character",
    DateTime = c("POSIXct", "POSIXt"),
    Lat = "numeric",
    Lon = "numeric",
    OnEffort = "logical",
    Trans = "character",
    Bft = "numeric",
    CCover = "numeric",
    Jelly = "numeric",
    HorizSun = "numeric",
    HKR = "character",
    ObsL = "character",
    ObsB = "character",
    ObsR = "character",
    Rec = "character",
    AltFt = "numeric",
    SpKnot = "numeric",
    VLI = "character",
    VLO = "character",
    VB = "character",
    VRI = "character",
    VRO = "character",
    Data1 = "character",
    Data2 = "character",
    Data3 = "character",
    Data4 = "character",
    Data5 = "character",
    Data6 = "character",
    Data7 = "character",
    EffortDot = "logical", 
    EventNum = "integer",
    file_das = "character",
    line_num = "integer"
  )
  
  expect_identical(exp.class, lapply(y.proc, class))
  expect_identical(exp.class, lapply(y.proc2, class))
})

test_that("airdas_sight output has expected column names and classes", {
  y.proc <- airdas_process(system.file("airdas_sample.das", package = "swfscAirDAS"))
  y.sight <- airdas_sight(y.proc)
  # TODO: multi
  # y.sight.multi <- airdas_sight(y.proc, mixed.multi = TRUE)
  
  exp.name <- c(
    "Event", "DateTime", "Lat", "Lon", "OnEffort", 
    "Trans", "Bft", "CCover", "Jelly", "HorizSun", "HKR", 
    "ObsL", "ObsB", "ObsR", "Rec", "AltFt", "SpKnot", 
    "VLI", "VLO", "VB", "VRI", "VRO", "EffortDot", "EventNum", "file_das", "line_num", 
    "SightNo", "Obs", "Angle", "SightStd", "Sp", "Gs", 
    "TurtleSizeFt", "TurtleDirection", "TurtleTail"
  )
  
  # exp.name.nomulti <- c(
  #   "Sp1", "Sp2","Sp3", "Sp4", "Sp1Perc", "Sp2Perc", "Sp3Perc", "Sp4Perc",
  #   "GsSp1", "GsSp2", "GsSp3", "GsSp4", "ResightCourse",
  #   "TurtleSp", "TurtleNum", "TurtleJFR", "TurtleAge", "TurtleCapt",
  #   "BoatType", "BoatNum"
  # )
  
  # exp.name.multi <- c(
  #   "Species", "GsSpecies", "ResightCourse",
  #   "TurtleSp", "TurtleNum", "TurtleJFR", "TurtleAge", "TurtleCapt",
  #   "BoatType", "BoatNum"
  # )
  
  expect_identical(exp.name, names(y.sight))
  # expect_identical(c(exp.name, exp.name.nomulti), names(y.sight))
  # expect_identical(c(exp.name, exp.name.multi), names(y.sight.multi))
})

