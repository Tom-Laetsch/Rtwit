#' Write many Twitter search requests to JSON file
#' 
#' Used to extract many repeated requests to pull searchs and save the results to a json file.
#' Will automatically move from most recent toward older tweets while pulling data. 
#' @param filename Character. File path to json file to store data.
#' @param twitter_token Your twitter_token; e.g. from twitToken() function.
#' @param n (optional) Integer. Maximum total tweets to pull from the timeline.
#' @param q='', geocode, lang, result_type, until, since_id, max_id 
#' To set up query. See ?twitSQuery for details
#' @param verbose (defaults TRUE) Logical. If true, displays request limit at each request.
#' @param append_old (defaults FALSE) Logical. If true will append an already extant json file; otherwise it creates/overwrites file.
#' @export
#' @examples 
#' twitter_token <- twitToken(consumer_key, consumer_secret, access_token, access_secret)
#' twitSmany2json(filename='~/thomas_laetsch.json', twitter_token = twitter_token, q = 'thomas laetsch uconn OR nyu')

twitSmany2json <- function(filename, twitter_token, n=5000, 
                           q='', geocode=NULL,  lang=NULL, 
                           result_type=NULL, until=NULL,
                           since_id=NULL, max_id=NULL,
                           verbose=TRUE, append_old = FALSE){
    require(jsonlite)
    require(httr)
    
    ## setup everything
    if(is.null(since_id)) since_id <- 1
    count <- 100
    if(n < count) count <- n
    
    ## request limit remaining
    ret <- twitLimit(twitter_token)
    limit <- ret$resources$search$`/search/tweets`$remaining
    if(verbose) message('Request limit: ',limit)
    
    ## setup query for request
    query <- twitSQuery(q=q, geocode=geocode, lang=lang, 
                          result_type=result_type, until=until,
                          since_id=since_id, max_id=max_id)
    if(is.null(query)) stop("Invalid query.")
    
    ## get json data from request
    j.data <- httr::content(twitS1(twitter_token,query))
    json.data <- j.data$statuses
    
    ## number of tweets in inaugural request
    ntweets <- length(json.data)
    if(ntweets < 1) stop("No tweets avaiable")
    
    ## set max_id to that of last tweet on request
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
    ## store first request to json file
    ret <- lapply(json.data, function(x) writeLines(jsonlite::toJSON(x, null="null"), con=conn))
    close(conn)
    
    ## one fewer requests
    limit <- limit-1
    
    ## iterate over requests until out of requests or done
    while(limit > 0 & ntweets < n & 
          !identical(max_id,max_id_old) &
          max_id > as.numeric(since_id)){
        
        ## setup query for next request
        query <- twitSQuery(q=q, geocode=geocode, lang=lang, 
                              result_type=result_type, until=until,
                              since_id=since_id, max_id=max_id)
        if(is.null(query)) break
        
        ## get json data from request
        j.data <- httr::content(twitS1(twitter_token,query))
        json.data <- j.data$statuses
        
        ## get number of tweets returned with request
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
        
        ## set up next iteration request
        max_id_old <- max_id
        max_id <- json.data[[len]]$id_str
        count <- min(100,n-ntweets)
        if(verbose) message("Request limit: ", limit)
        limit <- limit-1
    }
    message(ntweets, " tweets extracted from your query: ",q)
    return(q)
}