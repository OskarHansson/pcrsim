context("EPG generation")

test_that("heightToPeak", {

  # Three alleles with sizes and heights.
  peakw <- 1
  mydf <- data.frame(Size=c(100,120,130), Height=c(5000,4500,4000))
  res <- heightToPeak(data=mydf, width=peakw, keepNA=TRUE)
  
  # TEST 01--------------------------------------------------------------------
  # Normal data.

  # Test some faulty arguments.
  expect_that(heightToPeak(data=mydf, width="String"), throws_error())  
  expect_that(heightToPeak(data=c(12,14)), throws_error())  
  expect_that(heightToPeak(data=mydf, keepNA="String"), throws_error())  
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))

  # Check length.
  expect_that(nrow(res), equals(9))
  
  # Check peak width.
  expect_that(res$Size[3]-res$Size[1], is_equivalent_to(peakw))

  # Check that expected columns exist.  
  expect_true(any(grepl("Size", names(res))))
  expect_true(any(grepl("Height", names(res))))
  
  # Check for NA's.
  expect_false(any(is.na(res$Size)))
  expect_false(any(is.na(res$Height)))
  

  # TEST 02--------------------------------------------------------------------
  # Data with a missing height and an additional column for sample name.
  
  # Three alleles with sizes and heights (NA in heigt).
  # Sample name colum with text as strings (stringsAsFactors=FALSE).
  # Wider peaks.
  name <- "TEST"
  peakw <- 2.5
  mydf <- data.frame(Sample.Name=name, Size=c(100,120,130), 
                     Height=c(5000,NA,4000), stringsAsFactors=FALSE)
  res <- heightToPeak(data=mydf, width=peakw, keepNA=TRUE)

  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check length.
  expect_that(nrow(res), equals(9))

  # Check sample name (error if returned as factor level).
  expect_that(res$Sample.Name[1], is_equivalent_to(name))

  # Check peak width.
  expect_that(res$Size[3]-res$Size[1], is_equivalent_to(peakw))
  
  # Check that expected columns exist.  
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Size", names(res))))
  expect_true(any(grepl("Height", names(res))))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Size)))
  expect_false(any(is.na(res$Height)))
  
  # TEST 03--------------------------------------------------------------------
  # keepNA=FALSE and an additional column for numerical sample name.
  
  # Three alleles with sizes and heights (NA in heigt).
  # Sample name colum with numerical names (stringsAsFactors=TRUE).
  name <- 123456
  peakw <- 1/3
  mydf <- data.frame(Sample.Name=name, Size=c(100,120,130), Height=c(5000,NA,NA))
  res <- heightToPeak(data=mydf, width=peakw, keepNA=FALSE)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))

  # Check length.
  expect_that(nrow(res), equals(3))
  
  # Check sample name (error if returned as factor level).
  expect_that(res$Sample.Name[1], is_equivalent_to(name))
  
  # Check peak width.
  expect_that(res$Size[3]-res$Size[1], is_equivalent_to(peakw))
  
  # Check that expected columns exist.  
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Size", names(res))))
  expect_true(any(grepl("Height", names(res))))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Size)))
  expect_false(any(is.na(res$Height)))
  

  # TEST 04--------------------------------------------------------------------
  # keepNA=FALSE and an additional column for numerical sample name.
  # Completely negative sample.
  
  # Three alleles with sizes and heights (NA in heigt).
  # Sample name colum with numerical names (stringsAsFactors=TRUE).
  name <- 123456
  mydf <- data.frame(Sample.Name=name, Size=c(100,120,130), Height=c(NA,NA,NA))
  res <- heightToPeak(data=mydf, keepNA=FALSE)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))
  
  # Check length.
  expect_that(nrow(res), equals(0))

  # Check that expected columns exist.  
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Size", names(res))))
  expect_true(any(grepl("Height", names(res))))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Size)))
  expect_false(any(is.na(res$Height)))
  
  # NEW DATA.
  res <- heightToPeak(data=mydf, keepNA=TRUE)

  # Check length.
  expect_that(nrow(res), equals(9))
  
  # Check that expected columns exist.  
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Size", names(res))))
  expect_true(any(grepl("Height", names(res))))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Size)))
  expect_false(any(is.na(res$Height)))

})