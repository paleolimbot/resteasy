
# these functions do all the work for the in-memory getters

# post_query wraps around methods that take 'params' as payload (POST, PATCH, PUT)
post_query <- function(endpoint, params, cache, parser, cache_set, cache_get, content_as,
                       encoding, quiet = FALSE, method = httr::POST,
                       encode = "multipart", ...) {

  # cache_get and cache_set are slightly different because 'params' aren't encoded in the url
  url_object <- list(endpoint, params)

  # call rest_query
  rest_query(url_string = endpoint, cache = cache, parser = parser,
             cache_set = function(cache, url, data) {
               cache_set(cache, url_object, data)
             },
             cache_get = function(cache, url) {
               cache_get(cache, url_object)
             },
             content_as = content_as, encoding = encoding, quiet = quiet,
             method = method, body = params, encode = encode, ...)
}

rest_query <- function(url_string, cache, parser, cache_set, cache_get, content_as,
                       encoding, method, quiet=FALSE, ...) {

  # make sure encoding is a string of length 1 or NULL
  if(!is.null(encoding) && (!is.character(encoding) || (length(encoding) > 1))) {
    stop("'encoding' must be a character vector of length 1")
  }

  # make sure quiet is true or false
  if(!is.logical(quiet)) stop("'quiet' must be TRUE or FALSE")

  # get cache (will throw error if cache can't be converted to a cache)
  cache <- as.cache(cache)

  # make sure parser is a function
  parser <- match.fun(parser)

  # check cache
  result <- cache_get(cache, url_string)

  # if cached result exists
  if(!is.null(result)) {
    if(!quiet) message("Using cached information for ", url_string)
    # return parsed result
    return(parser(result))
  }

  # if there is no cached result, query the URL
  if(!quiet) message("Retrieving information from ", url_string)
  connect <- try(method(url_string, ...), silent=TRUE)

  # check for fail
  if(class(connect) != "try-error") {

    # try to get content
    if(!quiet) httr::warn_for_status(connect)
    result <- httr::content(connect, as=content_as, encoding=encoding)

    # store response information, if cache is not full
    if(!cache_full(cache)) {
      cache_set(cache, url_string, result)
    }

    # return parsed
    parser(result)
  } else {
    stop("Unable to connect to ", url_string, ": ", as.character(connect))
  }
}
