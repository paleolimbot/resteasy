
#' Clear cached results
#'
#' Clears the local cache of downloaded files (by default, an
#' environment in the package namespace). Clearing a directory
#' cache will result in all files with the extention ".cached"
#' being deleted from that directory.
#'
#' @param cache,x An environment, a directory name, or NA to clear
#'  the default internal cache
#' @param max_size The maximum number of entries to keep in the cache
#' @param url An object whose digest is identical for identical requests
#' @param directory a directory in which files wil be kept
#' @param env an environment
#' @param data the payload to cache
#' @param ... passed to/from methods
#'
#' @export
#' @rdname caches
#'
#' @examples
#' clear_resteasy_cache()
#'
clear_resteasy_cache <- function(cache = NA) {
  cache <- as.cache(cache)
  clear_cache(cache)
}

#' @rdname caches
#' @export
file_cache <- function(directory, max_size = Inf, ...) {
  if(!is.character(directory) || (length(directory) != 1)) stop("directory must be a directory name")
  structure(list(directory = directory, max_size = max_size, ...),
            class = c("file_cache", "cache"))
}

# max is 10,000, theory being that if the user bothers to create their own
# environment, they are probably pretty serious about caching...
#' @rdname caches
#' @export
environment_cache <- function(env, max_size = 10000, ...) {
  if(!is.environment(env)) stop("env must be an environment")
  structure(list(env = env, max_size = max_size, ...),
            class = c("environment_cache", "cache"))
}

#' @rdname caches
#' @export
null_cache <- function() {
  structure(list(max_size = 0), class = c("null_cache", "cache"))
}

#' @rdname caches
#' @export
is.cache <- function(x) {
  inherits(x, "cache")
}

#' @rdname caches
#' @export
as.cache <- function(x, ...) {
  if(is.cache(x)) {
    x
  } else if(is.environment(x)) {
    environment_cache(x, ...)
  } else if(is.character(x) && (length(x) == 1)) {
    file_cache(x, ...)
  } else if(is.null(x)) {
    null_cache()
  } else if(identical(x, NA)) {
    internal_cache
  } else {
    stop("Don't know how to create cache from ", x)
  }
}

#' @rdname caches
#' @export
cache_full <- function(cache) {
  cache_size(cache) >= cache$max_size
}

# create internal environment to cache responses
internal_cache_environment <- new.env(parent = emptyenv())

# create interal cache (max size 1000 by default)
internal_cache <- environment_cache(internal_cache_environment, max_size = 1000)

# get/set cached info methods
#' @rdname caches
#' @export
set_cached_text <- function(cache, url, data, ...) UseMethod("set_cached_text")
#' @rdname caches
#' @export
get_cached_text <- function(cache, url, ...) UseMethod("get_cached_text")
#' @rdname caches
#' @export
set_cached_raw <- function(cache, url, data, ...) UseMethod("set_cached_raw")
#' @rdname caches
#' @export
get_cached_raw <- function(cache, url, ...) UseMethod("get_cached_raw")

#' @rdname caches
#' @export
clear_cache <- function(cache) UseMethod("clear_cache")
#' @rdname caches
#' @export
cache_size <- function(cache) UseMethod("cache_size")

# consistent hash functions
hash_text <- function(obj) paste0("text_", digest::digest(obj))
hash_raw <- function(obj) paste0("raw_", digest::digest(obj))

# get/set for file caches
#' @rdname caches
#' @export
get_cached_text.file_cache <- function(cache, url, ...) {
  get_cached.file_cache(cache, hash_text(url), function(fname) {
    paste(readLines(fname), collapse="\n")
  })
}

#' @rdname caches
#' @export
get_cached_raw.file_cache <- function(cache, url, ...) {
  get_cached.file_cache(cache, hash_raw(url), function(fname) {
    readBin(fname, what = "raw", n = file.size(fname))
  })
}

#' @rdname caches
#' @export
set_cached_text.file_cache <- function(cache, url, data, ...) {
  set_cached.file_cache(cache, hash_text(url), data, write)
}

#' @rdname caches
#' @export
set_cached_raw.file_cache <- function(cache, url, data, ...) {
  set_cached.file_cache(cache, hash_raw(url), data, writeBin)
}

cache_filename <- function(cache, url_hash) {
  file.path(cache$directory, paste0(url_hash, ".cached"))
}

set_cached.file_cache <- function(cache, url_hash, data, writer) {
  if(!dir.exists(cache$directory) && !is.null(data)) {
    dir.create(cache$directory)
  }
  fname <- cache_filename(cache, url_hash)
  if(is.null(data) && file.exists(fname)) {
    unlink(fname)
  } else if(!is.null(data)) {
    writer(data, fname)
  }
  # check cache fullness if max_size is not Inf
  if(!identical(cache$max_size, Inf) && cache_full(cache)) {
    warning("Cache full: ", cache$directory,
            " (n=", cache_size(cache), ")")
  }
}

get_cached.file_cache <- function(cache, url_hash, reader) {
  fname <- cache_filename(cache, url_hash)
  if(file.exists(fname)) {
    reader(fname)
  } else {
    NULL
  }
}

# clear cache for files
#' @rdname caches
#' @export
clear_cache.file_cache <- function(cache) {
  # removing all *.cached files (safer than remvoing the dir)
  ext <- ".cached"
  unlink(list.files(cache$directory, pattern = paste0("*", ext),
                    recursive = FALSE, full.names = TRUE))
}

# cache size, directory caches
#' @rdname caches
#' @export
cache_size.file_cache <- function(cache) {
  length(list.files(cache$directory))
}

# get/set for environment caches
#' @rdname caches
#' @export
get_cached_text.environment_cache <- function(cache, url, ...) {
  get_cached.environment_cache(cache, hash_text(url))
}

#' @rdname caches
#' @export
get_cached_raw.environment_cache <- function(cache, url, ...) {
  get_cached.environment_cache(cache, hash_raw(url))
}

#' @rdname caches
#' @export
set_cached_text.environment_cache <- function(cache, url, data, ...) {
  set_cached.environment_cache(cache, hash_text(url), data)
}

#' @rdname caches
#' @export
set_cached_raw.environment_cache <- function(cache, url, data, ...) {
  set_cached.environment_cache(cache, hash_raw(url), data)
}

get_cached.environment_cache <- function(cache, url_hash) {
  if(exists(url_hash, where = cache$env)) {
    cache$env[[url_hash]]
  } else {
    NULL
  }
}

set_cached.environment_cache <- function(cache, url_hash, data) {
  cache$env[[url_hash]] <- data
  if(cache_full(cache)) warning("Cache full: (n=", cache_size(cache),
                                "). Use file_cache for larger cache sizes")
}

#' @rdname caches
#' @export
cache_size.environment_cache <- function(cache) {
  length(cache$env)
}

#' @rdname caches
#' @export
clear_cache.environment_cache <- function(cache) {
  items <- as.list(names(cache$env))
  do.call(rm, c(items, list(envir = cache$env)))
}

# the null cache does nothing and is empty
#' @rdname caches
#' @export
set_cached_text.null_cache <- function(cache, url, data, ...) {
  warning("Attempt to set_cache for the null_cache")
  invisible(NULL)
}

#' @rdname caches
#' @export
set_cached_raw.null_cache <- function(cache, url, data, ...) {
  warning("Attempt to set_cache for the null_cache")
  invisible(NULL)
}

#' @rdname caches
#' @export
get_cached_text.null_cache <- function(cache, url, ...) {
  NULL
}

#' @rdname caches
#' @export
get_cached_raw.null_cache <- function(cache, url, ...) {
  NULL
}

#' @rdname caches
#' @export
cache_size.null_cache <- function(cache) {
  0
}
