#' Twitter Token Generator
#' 
#' Generates a twitter token to access the API. 
#' You are assumed to have set up an app at apps.twitter.com.
#' This function requires the httr package
#' @param consumer_key Character. The consumer key for your app as a string
#' @param consumer_secret Character. The consumer secret for your app as a string
#' @param access_token Character. The access token for your app as a string
#' @param access_secret Character. The access secret for your app as a string
#' @export
#' @examples
#' twitToken(consumer_key, consumer_secret, access_token, access_secret)
twitToken <- function(consumer_key, consumer_secret, access_token, access_secret){
    require(httr)
    
    options("httr_oauth_cache"=FALSE)
    app <- httr::oauth_app("twitter", key = consumer_key, secret = consumer_secret)
    credentials <- list(oauth_token = access_token, oauth_token_secret = access_secret)
    twitter_token <- httr::Token1.0$new(endpoint = NULL, params = list(as_header = TRUE), 
                                        app = app, credentials = credentials)
    
    return(twitter_token)
}
