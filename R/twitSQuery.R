#' Twitter Search Tweets Query Maker
#' 
#' Sets up URL encoded query list to query for user timelines in the statuses/user_timeline request.
#' Most commonly use as support function for twitUT1.
#' @param q Character. Keyword query to search tweets. 
#' @param count (defaults to 100) Integer. Number of tweets to return per request; max = 100.
#' @param geocode (optional) Character. Pull tweets via geolocation within given search radius. 
#' @param language (optional) Character. Pull tweets of a specific language.
#' @param result_type (optional) Character. Pull tweets which are recent, popular, or mixed.
#' @param until (optional) Character. Pull tweets appearing before given date.
#' @param since_id (optional) Integer. Request results with tweet id greater (newer) than this value.
#' @param max_id (optional) Integer. Request results with tweet id less (older) than or equal to this value.
#' @seealso \link{https://dev.twitter.com/rest/public/search}
#' @export
#' @examples 
#' twitter_token <- twitToken(consumer_key, consumer_secret, access_token, access_secret)
#' query <- twitSQuery(q = 'thomas laetsch ua OR ucsd OR uconn OR nyu')
#' user.data <- twitS1(twitter_token, query)
#' user.json.data <- httr::content(user.data)
twitSQuery <- function(q='', count = 100, 
                         geocode=NULL, lang=NULL, 
                         result_type=NULL, until=NULL,
                         since_id=NULL, max_id=NULL){
    
    ## twitter maximum value per request = 100
    count <- max(1,min(count,100))
    
    ## set up parameters, note that count = 200 is the max allowed
    params <- list(q=q, count=count)
    
    if (!is.null(geocode)){
        params[["geocode"]] <- geocode
    }
    if (!is.null(lang)){
        params[["lang"]] <- lang
    }
    if (!is.null(result_type)){
        params[["result_type"]] <- result_type
    }
    if (!is.null(until)){
        params[["until"]] <- until
    }
    if (!is.null(since_id)){
        params[["since_id"]] <- since_id
    }
    if (!is.null(max_id)){
        params[["max_id"]] <- max_id
    }
    
    query <- lapply(params, function(x) URLencode(as.character(x)))
    
    return(query)
}