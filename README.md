
<!-- README.md is generated from README.Rmd. Please edit that file -->

# covaxr

<!-- badges: start -->
<!-- badges: end -->

This package provides R users with a simple interface to Ontario’s
Provincial COVID-19 Vaccination Booking System for getting data about
vaccination locations and open appointments, and a “bot-in-a-box”
function for creating a twitter bot that finds and tweets out empty
appointment slots with a single function call. These functions make the
exact same API calls that your browser does when you use the system.
This package is, and will always be, limited to read-only functions to
help users get information about locations and open appointments, and
helper functions for working with and sharing this data.

These functions need a personal API key to connect to the provincial
system. You can get your personal API key by logging into the provincial
system and monitoring the `Network` tab on the Chrome developer console
and finding the `vaccineData` parameter in the XHR API calls.

This is an unofficial project that is in no way associated with or
endorsed by the Province of Ontario. This package comes with no warranty
or guarantees. Use at your own risk!

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

This package is (as of May 2021 and, sadly, again as of December 2021)
powering a Twitter bot that looks for appointments near Ottawa within
the next 30 days. See it in action here:

-   <https://twitter.com/OttCovidApts>

The package also provides the function `covaxr::covax_twitter_bot()`
that lets you make an appointment-seeking bot with a single function
call. You can run it in local mode and print updates to the console, or
if you set up a Twitter developer account you can post updates to
Twitter.

Here’s the function call that’s powering
[@OttCovidApts](https://twitter.com/OttCovidApts):

``` r
covaxr::covax_twitter_bot(twitter_app_name = "CovidAptsOtt",
                          twitter_consumer_key = "put_your_own_values_here_:)",
                          twitter_consumer_secret = "put_your_own_values_here_:)",
                          twitter_access_token = "put_your_own_values_here_:)",
                          twitter_access_secret = "put_your_own_values_here_:)",
                          local_mode = FALSE,      # offline testing mode, no tweeting
                          covax_api_key = covax_api_key,         # your personal API key
                          lat = 45,
                          lon = -75,
                          dose_number = 2,        # dose 1 or 2?
                          days_in_future = 30,     # how many days in future
                          search_radius = 100,     # search radius in km
                          update_delay = 1,       # minutes between updates
                          verbose = TRUE)
```
