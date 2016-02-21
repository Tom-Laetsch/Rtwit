#' Write many Twitter User Timeline requests to JSON file
#' 
#' Used to extract many repeated requests to pull a user's timeline and save the results to a json file.
#' Will automatically move from most recent toward older tweets while pulling data. 
#' @param filename Character. File path to json file to store data.
#' @param twitter_token Your twitter_token; e.g. from twitToken() function.
#' @param n (optional) Integer. Maximum total tweets to pull from the timeline.
#' @param screen_name, user_id, since_id, max_id, trim_user, exclude_replies, contributor_details 
#' To set up query. See ?twitUTQuery for details
#' @param verbose (defaults TRUE) Logical. If true, displays request limit at each request.
#' @param append_old (defaults FALSE) Logical. If true will append an already extant json file; otherwise it creates/overwrites file.
#' @export
#' @examples 
#' twitter_token <- twitToken(consumer_key, consumer_secret, access_token, access_secret)
#' twitUTmany2json(filename='~/thomas_laetsch.json', twitter_token = twitter_token, screen_name = 'thomas_laetsch')

twitUTmany2json <- function(filename, twitter_token,  n=5000, 
                            screen_name=NULL, user_id=NULL, since_id=NULL,
                            max_id=NULL, trim_user="true",
                            exclude_replies=NULL, contributor_details=NULL,
                            verbose=TRUE, append_old = FALSE){
    require(jsonlite)
    require(httr)
    
    ## setup everything
    if(is.null(since_id)) since_id <- 1
    count <- 200
    if(n < count) count <- n
    
    ## request limit remaining
    ret <- twitLimit(twitter_token)
    limit <- ret$resources$statuses$`/statuses/user_timeline`$remaining
    
    ## setup query for request
    query <- twitUTQuery(screen_name=screen_name, user_id=user_id, 
                         count = count, since_id=since_id,
                         max_id=max_id, trim_user=trim_user,
                         exclude_replies=exclude_replies, 
                         contributor_details=contributor_details)
    if(is.null(query)) stop("Invalid query.")
    
    ## get json data from request
    json.data <- httr::content(twitUT1(twitter_token,query))
    
    ## number of tweets in inaugural request
    ntweets <- length(json.data)
    if(ntweets < 1) stop("No tweets avaiable")
    
    ## set max_id for last tweet of request
    max_id <- json.data[[ntweets]]$id_str
    max_id_old <- "not set"
    
    ## connect to file first time
    if(append_old){ 
        ## append an already extant file
        conn <- file(filename,'a')
    }else{
        ## start from scratch
        conn <- file(filename,'w')
    }
    ## write data to json file
    ret <- lapply(json.data, function(x) writeLines(jsonlite::toJSON(x, null="null"), con=conn))
    close(conn)
    
    if(verbose) message('Request limit: ',limit)
    limit <- limit-1
    
    while(limit > 0 & ntweets < n & 
          !identical(max_id,max_id_old) &
          max_id > as.numeric(since_id)){
        
        ## setup query for next request
        query <- twitUTQuery(screen_name=screen_name, user_id=user_id, 
                             count = count, since_id=since_id,
                             max_id=max_id, trim_user=trim_user,
                             exclude_replies=exclude_replies, 
                             contributor_details=contributor_details)
        if(is.null(query)) break
        
        ## get json data from request
        json.data <- httr::content(twitUT1(twitter_token,query))
        len <- length(json.data)
        if(length(json.data$error) != 0){
            message("Error extracting last request.")
            break
        } 
        
        ## append data to json file
        conn <- file(filename, 'a')
        ret <- lapply(json.data, function(x) writeLines(jsonlite::toJSON(x, null="null"), con=conn))
        close(conn)
        
        ## total number of tweets extracted so far
        ntweets <- ntweets + len
        
        ## setup next iteration request
        max_id_old <- max_id
        max_id <- json.data[[len]]$id_str
        count <- min(200,n-ntweets)
        if(verbose) message("Request limit: ", limit)
        limit <- limit-1
    }
    message("Total tweets extracted: ", ntweets)
    return(T)
}