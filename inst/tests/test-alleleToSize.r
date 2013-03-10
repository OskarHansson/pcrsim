context("EPG generation")

test_that("alleleToSize", {

  # Use same kit name for all tests.
  kit <- "ESX17"

  # Use same markers for all tests.
  markersFat <- c("AMEL", "D3S1358",  "TH01", "D21S11", "D18S51",
                "D10S1248", "D1S1656", "D2S1338", "D16S539",
                "D22S1045", "vWA", "D8S1179", "FGA",
                "D2S441", "D12S391", "D19S433", "SE33")
  markers <- rep(markersFat, each=2)
  
  # TEST 01--------------------------------------------------------------------
  # ESX17 positive control sample in 'fat' format.
  
  sample <- ("ESX17 PC fat")
  allele1 <- c("X", 17, 6, 29, 16,
               13, 12, 22, 9,
               16, 16, 14, 20,
               10, 18, 13, 15)
  allele2 <- c("Y", 18, 9.3, 31.2, 18,
               15, 13, 25, 13,
               16, 19, 15, 23,
               14, 23, 14, 16)

  esx17df <- data.frame(Sample.Name=sample,
                         Marker=markersFat,
                         Allele.1=allele1,
                         Allele.2=allele2)

  res <- alleleToSize(data=esx17df, kit=kit, debugInfo=FALSE)

  # Warn when 'wide' format is passed to function.
  expect_that(alleleToSize(data=esx17df, kit=kit, debugInfo=FALSE), gives_warning())
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))

  # Check that expected columns exist.  
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Allele", names(res))))
  expect_true(any(grepl("Size", names(res))))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Allele)))
  expect_false(any(is.na(res$Size)))
  
  
  # TEST 02--------------------------------------------------------------------
  # ESX17 positive control sample in 'slim' format.
  
  sample <- ("ESX17 PC slim")

  alleles <- c("X","Y", 17, 18, 6, 9.3, 29, 31.2, 16, 18,
               13, 15, 12, 13, 22, 25, 9, 13,
               16, 16, 16, 19, 14, 15, 20, 23,
               10, 14, 18, 23, 13, 14, 15, 16)

  esx17df <- data.frame(Sample.Name=sample,
                         Marker=markers,
                         Allele=alleles)
  
  res <- alleleToSize(data=esx17df, kit=kit, debugInfo=FALSE)
  
  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))

  # Check that expected columns are in data frame.  
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Allele", names(res))))
  expect_true(any(grepl("Size", names(res))))

  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_false(any(is.na(res$Allele)))
  expect_false(any(is.na(res$Size)))

  
  # TEST 03--------------------------------------------------------------------
  # ESX17 with some missing alleles (NA).
  
  sample <- ("ESX17 PC with NA")
  
  alleles <- c("X","Y", NA, 18, 6, 9.3, 29, 31.2, 16, 18,
                     13, 15, 12, 13, 22, NA, 9, 13,
                     16, 16, 16, 19, 14, 15, NA, 23,
                     10, 14, 18, 23, 13, 14, 15, NA)
  
  esx17df <- data.frame(Sample.Name=sample,
                           Marker=markers,
                           Allele=alleles)
  
  res <- alleleToSize(data=esx17df, kit=kit, debugInfo=FALSE)

  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))

  # Check that expected columns are in data frame.  
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Allele", names(res))))
  expect_true(any(grepl("Size", names(res))))
  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_true(any(is.na(res$Allele)))
  expect_true(any(is.na(res$Size)))
  
  
  # TEST 04--------------------------------------------------------------------
  # ESX17 without any alleles (blank result).

  sample <- ("ESX17 PC all NA")
  
  alleles <- rep(NA,17*2)

  esx17df <- data.frame(Sample.Name=sample,
                        Marker=markers,
                        Allele=alleles)

  res <- alleleToSize(data=esx17df, kit=kit, debugInfo=FALSE)

  # Check return class.  
  expect_that(class(res), matches(class(data.frame())))

  # Check that expected columns are in data frame.  
  expect_true(any(grepl("Sample.Name", names(res))))
  expect_true(any(grepl("Marker", names(res))))
  expect_true(any(grepl("Allele", names(res))))
  expect_true(any(grepl("Size", names(res))))

  
  # Check for NA's.
  expect_false(any(is.na(res$Sample.Name)))
  expect_false(any(is.na(res$Marker)))
  expect_true(all(is.na(res$Allele)))
  expect_true(all(is.na(res$Size)))
  
  
})