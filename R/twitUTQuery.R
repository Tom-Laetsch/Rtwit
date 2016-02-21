#' Twitter User Timeline Query Maker
#' 
#' Sets up URL encoded query list to query for user timelines in the statuses/user_timeline request.
#' Most commonly use as support function for twitUT1.
#' @param screen_name (optional if user_id is specified) Character. Screen name of user to request the timeline from.
#' @param user_id (optional if screen_name is specified) Integer. User ID of user to request the timeline from.
#' @param count (optional, default = 200) Integer. Number of tweets per request; max = 200.
#' @param since_id (optional) Integer. Request results with tweet id greater (newer) than this value.
#' @param max_id (optional) Integer. Request results with tweet id less (older) than or equal to this value.
#' @param trim_user (optional) Logical. If true returns only user_id in user object. False returns complete user object.
#' @param exclude_replies (optional) Logical. If true prevents replies on user timeline from being returned.
#' @param contributor_details (optional) Logical. If true, contributor's screen_name is also returned, instead of only user_id.
#' @seealso \link{https://dev.twitter.com/rest/reference/get/statuses/user_timeline}
#' @export
#' @examples 
#' twitter_token <- twitToken(consumer_key, consumer_secret, access_token, access_secret)
#' query <- twitUTQuery(screen_name = 'thomas_laetsch')
#' user.data <- twitUT1(twitter_token, query)
#' user.json.data <- httr::content(user.data)

twitUTQuery <- function(screen_name=NULL, user_id=NULL, 
                        count = 200, since_id=NULL,
                        max_id=NULL, trim_user="true",
                        exclude_replies=NULL, contributor_details=NULL){
    ## make sure the query includes either screen name or id
    if(is.null(screen_name) & is.null(user_id)){
        message('Error: screen_name or id must be entered')
        return(NULL)
    }
    
    ## twitter maximum value per request = 200
    count <- max(1,min(count,200))
    
    ## set up parameters, note that count = 200 is the max allowed
    params <- list(count = count, trim_user=trim_user)
    if (!is.null(screen_name)){
        params[["screen_name"]] <- screen_name
    }
    else if (!is.null(user_id)){
        params[["user_id"]] <- user_id
    }
    if (!is.null(since_id)){
        params[["since_id"]] <- since_id
    }
    if (!is.null(max_id)){
        params[["max_id"]] <- max_id
    }
    if (!is.null(exclude_replies)){
        params[["exclude_replies"]] <- exclude_replies
    }
    if (!is.null(contributor_details)){
        params[["contributor_details"]] <- contributor_details
    }
    
    query <- lapply(params, function(x) URLencode(as.character(x)))
    
    return(query)
}