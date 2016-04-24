## Test environments
* local Windows 7 (64-bit) install, R 3.2.4, RStudio 0.99.893
* win-builder: R version 3.3.0 beta (2016-04-14 r70486) and R version 3.2.5 (2016-04-14)


## R CMD check results
There were no ERRORs or WARNINGs. 

There was 2 NOTES in the local test environment:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Oskar Hansson <oskar.hansson@fhi.no>'
  New submission
  
  Package was archived on CRAN
  
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2014-02-07: the maintainer did not wish
      to fix its check failure.
    
  OSKAR: The simulation framework has now been completely re-structured.

* checking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped
  
  OSKAR: I think this is harmless.

There was 1 NOTE in the win-builder environment:

* Possibly mis-spelled words in DESCRIPTION:
  electropherograms (16:36)
  electrophoresis (15:63)

  OSKAR: Above words are correct.
  

## Downstream dependencies
There are currently no downstream dependencies for this package.