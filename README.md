
<!-- README.md is generated from README.Rmd. Please edit that file -->

# swfscAirDAS

<!-- badges: start -->

[![CRAN
version](http://www.r-pkg.org/badges/version/swfscAirDAS)](https://cran.r-project.org/package=swfscAirDAS)
[![R-CMD-check](https://github.com/smwoodman/swfscAirDAS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/smwoodman/swfscAirDAS/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This package contains functions designed for processing and analyzing
aerial survey DAS data (AirDAS) generated by the Southwest Fisheries
Science Center using one of the following programs: PHOCOENA, CARETTA,
or TURTLE (such as TURTLEP or TURTLE4D). Functionality includes reading
AirDAS data into a data frame, processing this data (extracting state
and condition information for each AirDAS event), and summarizing
sighting and effort information. Plans for future development include:
generating tables summarizing the information in the AirDAS file,
plotting AirDAS data, and generating files needed for doing density
estimation. Learn more in `vignette("swfscAirDAS")`

## Installation

You can install the released version of swfscAirDAS from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("swfscAirDAS")
```

You can install the developmental version of swfscAirDAS from
[GitHub](https://github.com/). To use the development version of
swfscAirDAS, it is recommended to install the development version of
[swfscDAS](https://smwoodman.github.io/swfscDAS/index.html) as well:

``` r
# install.packages("remotes")
remotes::install_github("smwoodman/swfscDAS", build_vignettes = TRUE)
remotes::install_github("smwoodman/swfscAirDAS", build_vignettes = TRUE)
```

Please contact the developer if you have any issues.

## AirDAS format and checks

swfscAirDAS accepts AirDAS data from several different programs, and
thus in several different formats: PHOCOENA, CARETTA, and TURTLE. You
can download PDFs describing these formats at the following links:

- [PHOCOENA data format
  PDF](https://github.com/smwoodman/swfscAirDAS/blob/master/inst/AirDAS_Format_PHOCOENA.pdf)
- [CARETTA data format
  PDF](https://github.com/smwoodman/swfscAirDAS/blob/master/inst/AirDAS_Format_CARETTA.pdf)
- [TURTLE data format
  PDF](https://github.com/smwoodman/swfscAirDAS/blob/master/inst/AirDAS_Format_TURTLE.pdf)

These PDFs are also included in the package; see
[`airdas_format_pdf`](https://smwoodman.github.io/swfscAirDAS/reference/airdas_format_pdf.html)
for more details.

In addition, the package contains the function
[`airdas_check`](https://smwoodman.github.io/swfscAirDAS/reference/airdas_check.html),
which checks whether the provided file adheres to expected format of the
provided file type. This function is designed to 1) be used for data
QA/QC after performing a survey and 2) ensure the data format meets all
of the assumptions made by the rest of the functions in the package. A
PDF describing the format checks is included in the package; you can
also
[`download the PDF here`](https://github.com/smwoodman/swfscAirDAS/blob/master/inst/AirDAS_check.pdf)

## Disclaimer

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.
