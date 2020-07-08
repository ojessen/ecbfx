context("Get FX")

test_that("Currency Pairs", {
  expect_named(get_ecb_fx(cur = "CHF",start_date =  "2000-01-01", end_date =  "2018-10-01"),
                  c("Datum", "D.CHF.EUR"))
  expect_s3_class(get_ecb_fx("GBP", lubridate::ymd(20000101), freq = "M", type = "E"), "data.frame")

  expect_equal(get_ecb_fx("CHF")$Datum[1], lubridate::ymd(19990104))
  expect_error(get_ecb_fx("CHx"))
})
