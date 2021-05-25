
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covaxr

<!-- badges: start -->
<!-- badges: end -->

This package provides R users with a simple interface to Ontario’s
Provincial COVID-19 Vaccination Booking System for getting data about
vaccination locations and open appointments. These functions make the
exact same API calls that your browser does when you use the system.
This package is, and will always be, limited to read-only functions to
help users get information about locations and open appointments, and
helper functions for working with this data.

These functions need a personal API key to connect to the provincial
system. You can get your personal API key by logging into the provincial
system and monitoring the `Network` tab on the Chrome developer console
and finding the `vaccineData` parameter in the XHR API calls.

## Installation

You can install the developer version of covaxr from
[GitHub](https://www.github.com) with:

``` r
devtools::install_github("chris31415926535/covaxr")
```

## Example

This example shows how you would find the COVID vaccination locations
near Toronto, and then find all open appointments between today and
September 1.

``` r
library(magrittr)
library(covaxr)

vax_data <- "YOUR_ONLINE_API_IDENTIFIER"

apts <- covaxr::get_locations(lat = 43.692556,
                              lng =  -79.377056,
                              fromDate = as.character(Sys.Date()),
                              doseNumber = 1,
                              vaccineData = vax_data) %>%
        covaxr::check_locations_covax(end_date = "2021-09-01")
```

Results are returned in tidy nested tibbles.

The function `covaxr::map_locations()` takes the output of
`covaxr::get_locations` and makes an interactive map using the
**leaflet** package.

## Twitter Bot

This package is (as of May 2021) powering a Twitter bot that looks for
appointments near Ottawa within the next 3 days. See it in action here:

-   <https://twitter.com/OttawaCovidApts>
