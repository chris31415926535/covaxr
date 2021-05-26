#' Get a list of local vaccination locations
#'
#' This function returns a data frame with the names, locations, and identifiers
#' of the COVID-19 vaccination locations that are available for a given
#' vaccine dose, place, and date.
#'
#' This function requires a personal API identifier, called `vaccineData`, that
#' the Ontario covax API uses to grant access to the system. You can get your
#' id by logging into the [provincial booking system](https://vaccine.covaxonbooking.ca/)
#' using Chrome and opening the developer console by pressing `Ctrl-Shift-J`
#' (on a Windows machine--consult Google for advice on other machines)
#'
#' * https://vaccine.covaxonbooking.ca/
#'
#' @param lat Latitude of the location to search from.
#' @param lng Longitude of the location to search from.
#' @param fromDate Date to begin looking, in format YYYY-MM-DD.
#' @param doseNumber Vaccine dose, either 1 or 2.
#' @param vaccineData Personal API identifier.
#'
#' @return A tibble with the name, address, locations, and identifiers of all
#' COVID-19 vaccination clinics matching the input criteria. In addition the
#' returned object has class `covax` that includes metadata about the request--
#' including the private API access key--that is used by other package functions.
#'
#' @examples
#' \dontrun{
#' vax_data <- "YOUR_ONLINE_API_IDENTIFIER"
#' apts <- covaxr::get_locations(lat = 43.692556,
#'                               lng =  -79.377056,
#'                               fromDate = as.character(Sys.Date()),
#'                               doseNumber = 1,
#'                               vaccineData = vax_data) %>%
#'         covaxr::check_locations_covax(end_date = "2021-09-01")
#' }
#' @export
get_locations <- function(lat, lng, fromDate = NA, doseNumber, vaccineData){

  if (is.na(fromDate)) fromDate <- Sys.Date()
  fromDate <- as.character(fromDate)

  doseNumber <- as.numeric(doseNumber)
  if (!doseNumber %in% c(1,2)) stop ("Parameter `doseNumber` must be an integer value of 1 or 2.")

  url <- "https://api.covaxonbooking.ca/public/locations/search"


  resp <- httr::POST(url = url,
                     body = list("vaccineData" = vaccineData,
                                 "fromDate" = fromDate,
                                 "location" = list("lng" = lng,
                                                   "lat" = lat)
                                 ,"url" = "https://vaccine.covaxonbooking.ca/manage/location-select"
                                 ,"doseNumber"= doseNumber
                     ),
                     encode = "json")

  search <- httr::content(resp)

  locations <- search$locations # check availability at a location over time

  names <- purrr::map_chr(locations, purrr::pluck, "name")
  addresses <-purrr::map_chr(locations, purrr::pluck, "displayAddress")
  ids <- purrr::map_chr(locations, purrr::pluck, "extId")
  lngs <- purrr::map_dbl(locations, purrr::pluck, "location", "lng")
  lats <- purrr::map_dbl(locations, purrr::pluck, "location", "lat")


  locations <- tibble::tibble(name = names,
                              address = addresses,
                              id = ids,
                              lat = lats,
                              lng = lngs)

  locations <- structure(locations,
            class = c("covax","tbl", "tbl_df", "data.frame"),
            lat = lat,
            lng = lng,
            fromDate = fromDate,
            doseNumber = doseNumber,
            vaccineData = vaccineData
  )

  return (locations)
}

#' Map COVID-19 vaccination locations
#'
#' This function makes an interactive map of local COVID-19 vaccination stations
#' using the **leaflet** package. It accepts input from
#'
#' @param locs A tibble containing COVID-19 vaccination locations obtained
#' through `covaxr::get_locations()`.
#'
#' @return A **leaflet** map.
#' @export
#'
#' @examples
#' \dontrun{
#' locations <- covaxr::get_locations(lat = 43.692556,
#'                       lng =  -79.377056,
#'                       fromDate = as.character(Sys.Date()),
#'                       doseNumber = 1,
#'                       vaccineData = vax_data)
#'
#' covaxr::map_locations(locations)
#' }
map_locations <- function(locs){

  labs <- paste0("<b>",locs$name,"</b><br>",locs$address) %>%
    purrr::map(htmltools::HTML)

  map <- leaflet::leaflet(locs)
  map <- leaflet::addTiles(map)
  map <- leaflet::addMarkers(map, label = labs)

  map
}



#' Check appointment availability for a time range at one location
#'
#' @param location_id A location's id.
#' @param start_date The first date to check for appointments, format YYYY-MM-DD.
#' @param end_date The last date to check for appointments, format YYYY-MM-DD.
#' @param vaccineData Your personal covaxon API identifier.
#' @param doseNumber 1 or 2 for first or second dose.
#' @param verbose Boolean: would you like updates to the console?
#'
#' @return A tibble containing appointment availabilities.
#'
#' @export
check_location <- function(location_id, start_date = NA, end_date = NA, vaccineData, doseNumber, verbose = TRUE){

  doseNumber <- as.numeric(doseNumber)
  if (!doseNumber %in% c(1,2)) stop ("Parameter `doseNumber` must be an integer value of 1 or 2.")

  Sys.sleep(0.2)
  if (verbose) message(location_id)

  # by default we look for open slots from today to three days into the future
  if (is.na(start_date)) start_date <- as.character(Sys.Date())
  if (is.na(end_date)) end_date <- as.character(Sys.Date() + lubridate::days(3))


  location_url <- paste0("https://api.covaxonbooking.ca/public/locations/",location_id,"/availability")

  resp_avail <- httr::POST(url = location_url,
                           body = list("vaccineData" = vaccineData,
                                       "startDate" = start_date,
                                       "endDate" = end_date
                                       ,"url" = "https://vaccine.covaxonbooking.ca/manage/appointment-select"
                                       ,"doseNumber" = doseNumber
                           ),
                           encode = "json")

  avail <- httr::content(resp_avail, encoding = "UTF-8", type = "application/json")

  # set up empty one in case there's nothing to return
  result <- tibble::tribble(~date, ~available, ~vaccineData)

  # if we got good results, use those instead
  if (length(avail$availability) > 0){
    result <- avail$availability %>%
      tibble::enframe() %>%
      tidyr::unnest_wider(value)%>%
      dplyr::filter(available) %>%
      dplyr::select(-name)
  }

  return (result)
}


#' Check appointment availability for a time range at one or more locations
#'
#' This function checks a
#' list of locations to see if there are any available COVID-19 vaccination
#' appointments in a given time range. You can specify the last date to check;
#' if no last date is given, it looks for appointments within the next 3 days.
#'
#' It accepts the results of `covaxr::get_locations()`, and you can pipe to it
#' directly with `%>%`.
#'
#' It returns the input tibble plus extra columns with information about the
#'  appointments it found, if any. Details are in nested tibbles in the `available`
#'  column. If you just want to know how many days have vacancies, or which
#'  days have vacancies, those values are provided in `num_days` and `days`.
#'
#' @param locations A set of locations returned from `covaxr::get_locations()`.
#' This will be a tibble with class `covax` and stored metadata.
#' @param end_date The last date to check for appointments, format YYYY-MM-DD.
#' @param verbose Boolean: would you like updates to the console?
#'
#' @return The input tibble with additional columns for the date, time, and
#' number of appointments that were found at each location.
#' @export
check_locations_covax <- function(locations, end_date = NA, verbose = TRUE){
  if (!"covax" %in% class(locations)) stop ("Input must use results of `covaxr::get_locations()`.")

  results <- list()
  for (i in 1:nrow(locations)) results[[i]] <- tibble::tribble(~date, ~available)



  start_date <- attr(locations, "fromDate")
  dose_number <- attr(locations, "doseNumber")
  dose_number <- as.numeric(dose_number)
  vaccineData <- attr(locations, "vaccineData")

  # if (!doseNumber %in% c(1,2)) stop ("Parameter `doseNumber` must be an integer value of 1 or 2.")

  # For each location, we're going to query the API and find out if it has
  # appointments available. I know for-loops are gauche in R but it was the
  # easiest way to get on with it.
  for (i in 1:nrow(locations)){
    Sys.sleep(0.2)
    location_id <- locations$id[[i]]

    if (verbose) message(paste0(i,": ", location_id))

    # by default we look for open slots from today to three days into the future
    if (is.na(start_date)) start_date <- as.character(Sys.Date())
    if (is.na(end_date)) end_date <- as.character(Sys.Date() + lubridate::days(3))


    location_url <- paste0("https://api.covaxonbooking.ca/public/locations/",location_id,"/availability")

    resp_avail <- httr::POST(url = location_url,
                             body = list("vaccineData" = vaccineData,
                                         "startDate" = start_date,
                                         "endDate" = end_date
                                         ,"url" = "https://vaccine.covaxonbooking.ca/manage/appointment-select"
                                         ,"doseNumber" = dose_number
                             ),
                             encode = "json")

    avail <- httr::content(resp_avail, encoding = "UTF-8", type = "application/json")

    # set up empty one in case there's nothing to return
    result <- tibble::tribble(~date, ~available)#, ~vaccineData)

    # if we got good results, use those instead
    if (length(avail$availability) > 0){
      result <- avail$availability %>%
        tibble::enframe() %>%
        tidyr::unnest_wider(value)%>%
        dplyr::filter(available) %>%
        dplyr::select(-name, -vaccineData, -available)
    }

    result <- result %>%
      dplyr::mutate(apt_times = purrr::map(date, get_apt_times, location_id),
                    num_apts = purrr::map_dbl(apt_times, nrow)) %>%
      dplyr::filter(num_apts > 0)
    results[[i]] <- result

  }

  # put all of our results back on the original
  output <- tibble::add_column(locations, available = results) %>%
    dplyr::mutate(num_days = purrr::map_dbl(available, nrow))
           # days = purrr::map_chr(available, function(x) {
           #   days <- dplyr::pull(x, "date")
           #   stringr::str_flatten(days, collapse = ", ")
           # })
    #)

  output <- output %>%
    dplyr::filter(num_days > 0) %>%
    dplyr::select(-num_days) %>%
    tidyr::unnest(available)

  return (output)
}



get_apts <- function(slots_found){
  apts_available <- slots_found %>%
    dplyr::mutate(num_apts = purrr::map2_dbl(id, date, get_num_apts)) %>%
    dplyr::select(name, address, date, num_apts) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::arrange(date) %>%
    dplyr::filter(num_apts > 0)

  return (apts_available)

}




# check availability in time range at one location
get_num_apts <- function(location_id, location_date, verbose = TRUE) {
  if (verbose) message(paste0(location_id," - ", location_date))
  location_url <- paste0("https://api.covaxonbooking.ca/public/locations/",location_id,"/date/",location_date,"/slots")

  t <- httr::POST(url =location_url,
                  body = list("vaccineData" = "WyJhMWQ0dDAwMDAwMDFqZGtBQUEiXQ==",
                              "doseNumber" = "1"
                              ,"url" = "https://vaccine.covaxonbooking.ca/manage/appointment-select"),
                  encode = "json")

  a <- httr::content(t)

  apts <- a$slotsWithAvailability %>%
    enframe() %>%
    unnest_wider(value)

  if (nrow(apts) > 0) {
    apts <- apts %>%
      mutate(apt_datetime = paste0(location_date," ", localStartTime),
             apt_datetime = lubridate::ymd_hms(apt_datetime),
             in_the_future = (apt_datetime > Sys.time())) %>%
      filter(in_the_future)
  }

  num_apts <- nrow(apts)

  return (num_apts)
}


# check availability in time range at a vector of dates for one location
get_apt_times <- function(location_dates, location_id, verbose = TRUE) {

  output <- tibble::tibble()

  for (j in 1:length(location_dates)){
    location_date <- location_dates[[j]]

    if (verbose) message(paste0(location_id," - ", location_date))
    location_url <- paste0("https://api.covaxonbooking.ca/public/locations/",location_id,"/date/",location_date,"/slots")

    t <- httr::POST(url =location_url,
                    body = list("vaccineData" = "WyJhMWQ0dDAwMDAwMDFqZGtBQUEiXQ==",
                                "doseNumber" = "1"
                                ,"url" = "https://vaccine.covaxonbooking.ca/manage/appointment-select"),
                    encode = "json")

    a <- httr::content(t)

    apts <- a$slotsWithAvailability %>%
      tibble::enframe() %>%
      tidyr::unnest_wider(value)

    if (nrow(apts) > 0) {
      apt <- apts %>%
        dplyr::mutate(apt_datetime = paste0(location_date," ", localStartTime),
                      apt_datetime = lubridate::ymd_hms(apt_datetime),
                      in_the_future = (apt_datetime > Sys.time())) %>%
        dplyr::filter(in_the_future) %>%
        dplyr::select(apt_datetime)

      output <- dplyr::bind_rows(output, apt)
    }

    #  num_apts <- nrow(apts)

  }


  return (output)
}


# function to post tweet text
post_tweet <- function(tweet_text, verbose = TRUE){
  if (verbose) message("New update. Tweeting:")
  message(paste0("   ", new_tweet))

  tweet_resp <- tryCatch(
    {
      #twitteR::tweet(new_tweet)
      rtweet::post_tweet(status = tweet_text)
    },
    error=function(cond) {
      message ("Error tweeting. May be the same text repeated.")
      message(cond)
      # Choose a return value in case of error
      return(cond)
    }
  )

  return (tweet_text)
}


# function to take a list of locations + appointments and create tweet text
create_tweet_text <- function(apts_available){

  # remove a bunch of extra text from the location names
  fortweet <- apts_available %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(nam = stringr::str_remove(name, " FULL/PLEIN"),
                  nam = stringr::str_remove(nam, "May|Jun.*"),
                  nam = stringr::str_remove(nam, " - $"),
                  nam = stringr::str_remove(nam, "\\(.*\\)"),
                  nam = stringr::str_remove(nam, "\\("),
                  nam = stringr::str_trim(nam),
                  dat = paste0(lubridate::month(date, label = TRUE), " ", lubridate::day(date))) %>%
    dplyr::mutate(txt = paste0(dat,": ", nam, ": ",num_apts," apts")) %>%
    dplyr::select(txt)

  # format the time
  the_time <- format(Sys.time(), "%X")

  # trim the tweet so that we have room to put the time at the end
  # twitter doesn't like it if you post the exact same tweet more than once
  new_tweet <- fortweet %>%
    dplyr::pull(txt) %>%
    stringr::str_flatten(collapse = "\n") %>%
    stringr::str_trunc(width = (138 - nchar(the_time)), ellipsis = "") %>%
    paste0(., "\n", the_time)

  return(new_tweet)
}


#' @importFrom magrittr %>%


