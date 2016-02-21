#' Twitter Request Limit
#' 
#' Retrieves number of requests remaining from your twitter token.
#' Requests are renewed every-so-often (typically 15 minutes) based on Twitter's API user agreement.
#' @param twitter_token Your twitter access token.
#' @export
#' @examples
#' twitter_token <- twitToken(consumer_key, consumer_secret, access_token, access_secret)
#' limit <- twitLimit(twitter_token, query = 'search')
twitLimit <- function(twitter_token){
    require(httr)
    url <- "https://api.twitter.com/1.1/application/rate_limit_status.json"
    response <- httr::GET(url, httr::config(token = twitter_token))
    if (response$status_code == 200) {
        return(httr::content(response))
    } else {
        #return the negative status code if error thrown
        message('Error: remaining requests status has thrown error code ', 
                response$status_code)
        return(0)
    }
}
