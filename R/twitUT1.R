#' Request Twitter User Timeline Once
#' 
#' Make a single request to extract user timeline tweets with query specifications.
#' @param twitter_token Your twitter_token; e.g. from twitToken() function.
#' @param query A user_timeline query; e.g. from twitUTQuery() function.
#' @export
#' @examples
#' twitter_token <- twitToken(consumer_key, consumer_secret, access_token, access_secret)
#' query <- twitUTQuery(screen_name = 'thomas_laetsch')
#' user.data <- twitUT1(twitter_token, query)
#' user.json.data <- httr::content(user.data)
twitUT1 <- function(twitter_token, query){
    require(httr)
    url <- "https://api.twitter.com/1.1/statuses/user_timeline.json"
    user.data <- httr::GET(url, query = query, httr::config(token = twitter_token))
    return(user.data)
}