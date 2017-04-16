#' Query an endpoint and cache the data
#'
#' @param ...,params Key/value pairs to send to the query (non url-encoded)
#' @param endpoint,.endpoint The URL endpoint to send the query
#' @param cache,.cache An environment, directory name, NULL (disable cache), or NA for internal cache
#' @param parser,.parser A function that the (character) results will be passed through
#' @param quiet,.quiet Use quiet=TRUE to supress messaging
#' @param encoding,.encoding The encoding to use (defaults to auto, see httr::\link[httr]{content})
#'
#' @return The result of parser on the text resulting from the call
#' @export
#'
#' @rdname get
#'
#' @examples
#' # that's some catch, that catch-22:
#' get_text("https://www.goodreads.com/book/title",
#'          key="HSkIMuOGlxFIOmfBCGFVA", title='catch-22')
#
get_text <- function(.endpoint, ..., .cache=NULL, .parser=identity, .quiet=FALSE, .encoding=NULL) {
  get_text_(endpoint = .endpoint, params = list(...), cache = .cache, parser = .parser,
            quiet = .quiet, encoding = .encoding)
}

#' @rdname get
#' @export
get_textf <- function(.endpoint, ...) {
  purrr::partial(get_text, .endpoint = .endpoint, ...)
}

#' @rdname get
#' @export
get_raw <- function(.endpoint, ..., .cache=NULL, .parser=identity, .quiet=FALSE) {
  get_raw_(endpoint = .endpoint, params = list(...), cache = .cache, parser = .parser,
           quiet = .quiet)
}

#' @rdname get
#' @export
get_rawf <- function(.endpoint, ...) {
  purrr::partial(get_raw, .endpoint = .endpoint, ...)
}

#' @rdname get
#' @export
get_text_ <- function(endpoint, params=list(), cache=NULL, parser=identity, quiet=FALSE,
                      encoding=NULL, ...) {
  # invoke get_url_
  get_query(endpoint = endpoint, params = params, cache = cache,
            cache_set = set_cached_text, cache_get = get_cached_text, content_as = "text",
            parser = parser, quiet = quiet, encoding = encoding, method = httr::GET, ...)
}

#' @rdname get
#' @export
get_text_f <- function(endpoint, ...) {
  purrr::partial(get_text_, endpoint = endpoint, ...)
}

#' @rdname get
#' @export
get_raw_ <- function(endpoint, params=list(), cache=NULL, parser=identity, quiet=FALSE, ...) {
  # invoke get_url_
  get_query(endpoint = endpoint, params = params, cache = cache,
            cache_set = set_cached_raw, cache_get = get_cached_raw, content_as = "raw",
            parser = parser, quiet = quiet, encoding = NULL, method = httr::GET, ...)
}

#' @rdname get
#' @export
get_raw_f <- function(endpoint, ...) {
  purrr::partial(get_download, endpoint = endpoint, ...)
}

#' @rdname get
#' @export
get_download <- function(.endpoint, ..., .cache=NULL, .parser=identity, .quiet=FALSE) {
  get_download_(endpoint = .endpoint, params = list(...), cache = .cache, parser = .parser,
                quiet = .quiet)
}

#' @rdname get
#' @export
get_downloadf <- function(.endpoint, ...) {
  purrr::partial(get_download, .endpoint = .endpoint, ...)
}

#' @rdname get
#' @export
get_download_ <- function(endpoint, params=list(), cache=NULL, parser=identity,
                         quiet=FALSE, ...) {
  # make sure quiet is true or false
  if(!is.logical(quiet)) stop("'quiet' must be TRUE or FALSE")

  # verify parser is a function
  parser <- match.fun(parser)

  # build URL
  url_string <- create_url(endpoint, params, quiet)

  # create the cache
  cache <- as.cache(cache)

  # file_cache works differently than null_cache, which uses a tempfile
  if(inherits(cache, "file_cache")) {
    # get cached filename
    filename <- cache_filename(cache, hash_raw(url_string))

    # ensure folder exists
    if(!dir.exists(cache$directory)) {
      dir.create(cache$directory)
    }
  } else if(inherits(cache, "null_cache")) {
    # use tempfile
    filename <- tempfile()[1]
  } else {
    stop("Don't know how to get_download_ using cache of type ", class(cache)[1])
  }

  if(!file.exists(filename)) {
    # use curl_download to fetch file
    if(!quiet) message("Retrieving information from ", url_string)
    result <- try(curl::curl_download(url_string, filename, quiet = quiet, ...), silent = TRUE)
  } else {
    if(!quiet) message("Using cached information for ", url_string)
    result <- NULL
  }

  if(class(result) == "try-error") {
    try(unlink(filename))
    stop("Unable to connect to ", url_string, ": ", as.character(result))
  } else {
    # return the result of the parser on the filename
    parser(filename)
  }
}

#' @rdname get
#' @export
get_download_f <- function(endpoint, ...) {
  purrr::partial(get_download_, endpoint = endpoint, ...)
}

# get_query wraps around the URLs that take 'params' as a query string (GET being the main one)

get_query <- function(endpoint, params, cache, parser, cache_set, cache_get, content_as,
                      encoding, quiet=FALSE, method = httr::GET, ...) {

  # make sure quiet is true or false
  if(!is.logical(quiet)) stop("'quiet' must be TRUE or FALSE")

  # build URL
  url_string <- create_url(endpoint, params, quiet)

  # call rest_query
  rest_query(url_string = url_string, cache = cache, parser = parser, cache_set = cache_set,
             cache_get = cache_get, content_as = content_as, encoding = encoding,
             method = method, quiet = quiet, ...)
}

# this function does the URL creation from a named list of parameters
create_url <- function(endpoint, params, quiet = FALSE) {

  # verify search params
  if(length(params) > 0) { # empty list is ok
    if(is.null(names(params))) stop("get_text takes only named arguments")
    if(any(nchar(names(params)) == 0)) stop("get_text takes only named arguments")
  }

  # make sure quiet is true or false
  if(!is.logical(quiet)) stop("'quiet' must be TRUE or FALSE")

  # make sure endpoint is a string of length 1
  if(!is.character(endpoint) || (length(endpoint) > 1)) {
    stop("'endpoint' must be a character vector of length 1")
  }

  # characterify input values, messaging user for NULLs and NAs
  params <- lapply(params, function(x) {
    if(is.null(x)) {
      NULL
    } else if(is.na(x)) {
      message('Coercing an NA query parameter to ""')
      ""
    } else {
      as.character(x)
    }
  })

  # remove NULLs
  params <- params[!vapply(params, is.null, logical(1))]

  # set query params
  purl <- httr::parse_url(endpoint)
  if(length(params) > 0) {
    params <- c(purl$query, params)
    # name sorting ensures consistent hashing
    # duplicate names should still be ok here
    purl$query <- params[order(names(params))]
  }

  # build URL
  httr::build_url(purl)
}
