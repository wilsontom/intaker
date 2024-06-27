
test_that("package", {

  raw_data <- readr::read_csv(system.file('extdata/example_data.csv', package = 'intaker'))


  intaker_ob <- openIntake24(raw_data)

  expect_true(isS4(intaker_ob))


})
