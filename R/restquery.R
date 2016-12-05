
#' Query an endpoint and cache the data
#'
#' @param ... Key/value pairs to send to the query (will be passed through URLEncode)
#' @param .endpoint The URL endpoint to send the query
#' @param .cache A variable name in .GlobalEnv to use as a cache (NULL to disable)
#' @param .parser A function that the (character) results will be passed through
#' @param .quiet Use .quiet=TRUE to supress messaging
#'
#' @return The query result
#' @export
#'
#' @examples
#' restquery(key="HSkIMuOGlxFIOmfBCGFVA", title='catch-22', .endpoint="https://www.goodreads.com/book/title")
#'
restquery <- function(..., .endpoint, .cache='.api_result', .parser=NULL, .quiet=FALSE) {
  # make URL
  searchparams <- sapply(list(...), as.character, simplify = FALSE)
  # verify search params are all named
  if(any(nchar(names(searchparams)) == 0)) stop("restquery only takes named parameters")
  # sorting ensures consistent url_hash with identical parameters
  params <- sapply(sort(names(searchparams)),
                   function(item) {paste(item, utils::URLencode(searchparams[[item]]), sep="=")})
  url_string <- sprintf("%s?%s", .endpoint, paste(params, collapse="&"))

  lines <- NULL
  # check for cached result
  if(!is.null(.cache)) {
    lines <- get_cached(.cache, url_string)
    if(!is.null(lines)) {
      if(!.quiet) message("Using cached information for ", url_string)
    }
  }
  # if there is no cached result, query the URL and parse
  if(is.null(lines) || is.null(.cache)) {
    if(!.quiet) message("Retreiving information from ", url_string)
    connect <- url(url_string)
    lines <- try(paste(readLines(connect, warn = FALSE), collapse=""), silent = TRUE)
    close(connect)

    # check for fail
    if(class(lines) != "try-error") {
      # store geocoded information in users global environment
      if(!is.null(.cache)) {
        put_cached(.cache, url_string, lines)
      }
    }
  }

  if(class(lines) == "try-error") {
    stop("Unable to connect to URL ", url_string)
  } else {
    if(is.null(.parser)) {
      return(lines)
    } else {
      return(.parser(lines))
    }
  }
}
