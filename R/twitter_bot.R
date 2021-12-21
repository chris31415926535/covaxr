# twitter_bot.R


#' Bot-In-A-Box for Tweeting Open Ontario COVID-19 Vaccination Appointments
#'
#' @description
#' This function is a complete bot-in-a-box that will search Ontario's
#' provincial vaccine booking system for empty slots and tweet them out. It uses
#' the **covaxr** package's `get_locations()` and `check_locations_covax()`
#' functions for reading the booking system, and the **rtweet** package to tweet
#' them out. *Please note this is an unofficial product, in no way affiliated
#' with the Government of Ontario, and it comes with no warranty and is to be
#' used at your own risk.*
#'
#' If you're just looking for yourself, you can run it in "local mode" and
#' updates will be printed to the console without tweeting.
#'
#'  I've described the function parameters below, but please note you need
#'  two things for this to work:
#'
#'   - **A Provincial API Key.** If you have an Ontario Health Card and can log
#'   into the provincial COVID-19 booking system, then you have an API key. You
#'   can find it by monitoring Chrome's developer console under the 'Network'
#'   tab and looking for the value it passes for `vaccineData` when it makes API
#'   calls. *The function won't work without one.*
#'   - **Twitter Credentials.** The **rtweet** package authors put together a
#'   good guide on using OAuth with Twitter, and [you can read it here]
#'   (https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html.)
#'   - **A latitude & longitude.** The provincial API will return locations near
#'   here.
#'   - **A dose number.** 1 or 2; at this point 1 is more urgent and dose 2
#'   values may not be reliable.
#'
#'
#' @param twitter_app_name The name of your Twitter app (only if not in local mode).
#' @param twitter_consumer_key Your Twitter consumer key (only if not in local mode).
#' @param twitter_consumer_secret Your Twitter consumer secret key (only if not in local mode).
#' @param twitter_access_token Your Twitter access token (only if not in local mode).
#' @param twitter_access_secret Your twitter access secret key (only if not in local mode).
#' @param local_mode Boolean. Should the bot run in local mode and only print
#' results to the console (default), or should it post updates to Twitter?
#' @param covax_api_key Your personal provincial booking system API key.
#' @param lat The latitude to search.
#' @param lon The longitude to search.
#' @param dose_number The does number to search for (1 or 2).
#' @param days_in_future How many days in the future to look (default is 3).
#' @param search_radius Radius in km to search around supplied latitude and longitude.
#' @param update_delay How many minutes to wait between updates.
#' @param verbose Boolean. Would you like lots of updates to the console?
#'
#' @return Returns nothing; runs until terminated.
#' @export
covax_twitter_bot <- function(twitter_app_name = NA,
                              twitter_consumer_key = NA,
                              twitter_consumer_secret = NA,
                              twitter_access_token = NA,
                              twitter_access_secret = NA,
                              local_mode = TRUE,      # offline testing mode, no tweeting
                              covax_api_key,         # your personal API key
                              lat = 45.38,
                              lon = -75.69,
                              dose_number = 1,           # dose 1 or 2?
                              days_in_future = 3,    # how many days in future
                              search_radius = 25,    # how many km to search around lat/lon
                              update_delay = 1,       # minutes between updates
                              verbose = TRUE){

  if (!local_mode) {
    token <-  rtweet::create_token(
      app = twitter_app_name,
      consumer_key = twitter_consumer_key,
      consumer_secret = twitter_consumer_secret,
      access_token = twitter_access_token,
      access_secret = twitter_access_secret,
      set_renv= FALSE)
  }

  while (TRUE) {
    if (verbose) message("Beginning new sweep:")

    locations <- covaxr::get_locations(lat = lat,
                                       lng = lon,
                                       fromDate = as.character(Sys.Date()),
                                       doseNumber = dose_number,
                                       vaccineData = covax_api_key,
                                       search_radius = search_radius)

    # if we find any locations, we process them and tweet if applicable
    if (nrow(locations) > 0){
      end_date <- as.character(Sys.Date() + lubridate::days(days_in_future))

      appointments <- covaxr::check_locations_covax(locations,
                                                    end_date = end_date,
                                                    verbose = verbose)

      # if there are appointments
      # make a tweet if we're not testing
      # print to console if we're testing
      if (nrow(appointments) > 0)
      {
        tweet_text <- create_tweet_text(appointments)
        if (!local_mode) post_tweet(tweet_text, token)
        if (local_mode) message(paste0("Testing, not tweeting:\n",tweet_text))
      }
    } # end if nrow(locations)> 0

    if (verbose) message(paste0("Pausing for ",update_delay," minutes..."))
    Sys.sleep(60 * update_delay)
  }


}
