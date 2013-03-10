context("PCR simulation")

test_that("simPCR", {

  # Three alleles with sizes and heights.

  expect_that(simPCR(ncells=0), equals(0))
  expect_that(simPCR(), throws_error())
  expect_that(simPCR(ncells=NA), throws_error())

  # TODO: More tests...
  #simPCR(ncells=100, probEx=1, probAlq=1, probPCR=1, cyc=1, 
  #                 tDetect=1, dip=TRUE, KH=1, sim=1, debugInfo=TRUE)
  
})