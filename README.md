
<!-- README.md is generated from README.Rmd. Please edit that file -->
Resteasy: Easy REST Query Function Tools in R
=============================================

Calling HTTP methods is already easy in R thanks to the [curl](https://cran.r-project.org/package=curl) and [httr](https://cran.r-project.org/package=httr) packages. This package provides tools to turn REST APIs into cached R functions with a minimum of readable code. The example used here is the [Google Maps Geocoding API](https://developers.google.com/maps/documentation/geocoding/intro), which is a web service that turns addresses into coordinates on the earth's surface. To geocode a location (say, New York City), JSON data can be downloaded from a URL that looks like this:

    https://maps.googleapis.com/maps/api/geocode/json?address=New+York+City

Using resteasy, you can turn this into an R function with a minimum of effort:

``` r
library(resteasy)
geocode <- get_textf("https://maps.googleapis.com/maps/api/geocode/json", 
                     .parser = jsonlite::fromJSON)
nyc <- geocode(address = "New York City")
#> Retrieving information from https://maps.googleapis.com/maps/api/geocode/json?address=New%20York%20City
nyc$results$geometry$location
#>        lat       lng
#> 1 40.71278 -74.00594
```

Function Overview
-----------------

There are a number of functions in resteasy that make similar calls based on on the HTTP method, content type, and return type. The most common case is the HTTP GET method, which forms the basis for most API calls that I have personally run into (the Google geocoding API happens to be one of these). In order to geocode New York City as shown above, there are actually 12 functions you could theoretically use (`get_text()`, `get_text_()`, `get_textf()`, `get_text_f()`, `get_raw()`, `get_raw_()`, `get_rawf()`, `get_raw_f()`, `get_download()`, `get_download_()`, `get_downloadf()`, and `get_download_f()`). You'll notice that they follow a pattern: they all start with `get_`, have a middle section that is either `text`, `raw`, or `download`, have variants with no suffix, with a `_` suffix, and each of those has a variant with an `f` suffix as well. It may be slightly confusing, but I promise all of these functions have their place in rapidly creating API wrappers using this package.

The easiest case is `get_text()`. The "get" indicates the HTTP method (GET), and the "text" indicates that the method will return a `character` vector with the results.

``` r
get_text("https://maps.googleapis.com/maps/api/geocode/json", 
         address = "New York City")
```

A similar variant is `get_text_()`, which is more verbose but more flexible. In `get_text()`, query parameters are just function arguments. In `get_text_()`, query parameters are passed as a `list` to the `params` argument.

``` r
get_text_("https://maps.googleapis.com/maps/api/geocode/json", 
          params = list(address = "New York City"))
```

It may seem a little redundant to include this method, but I did it because of a specific API that takes multiple values for the query value. It isn't possible to pass these values in a traditional argument list because repeated formal arguments are not allowed in R. This slightly more flexible form is probably better for programmability as well. Note that the arguments to `get_text_` are not preceeded by a `.`.

The final suffix is the `f` ending for each method, which returns a `function` instead of calling the API. These functions use the [purrr](https://cran.r-project.org/package=purrr) package to create a partial function wrapper around the function that has the `f` suffix. This creates a version of the original function with some values "filled in". This is probably the most useful family of functions, since having the URL of the API in more than one place is an error waiting to happen. API keys can be defined here as well, avoiding the need to type them for each call.

``` r
geocode <- get_textf("https://maps.googleapis.com/maps/api/geocode/json")
geocode(address = "New York City")
```

Finally, there are the `get_raw()` and `get_download()` methods. These methods also use the HTTP GET method, but return the raw bytes (vector of type `raw`) of the output and the file to which the request was downloaded, respectively. These are useful when an API does not return a text file, but instead returns a ZIP file or image. Also, `get_download()` will not load the file into memory, which may be desired if an API has the potential to return a large result that you don't necessarily want as an R object.

Parsers
-------

The text result of an API response is rarely useful in R. Usually the result is parsed somehow to produce useful output that might be expected from a similar function were it written in R. In the case of the Google geocoder API, it actually takes a few steps to get to the latitude and longitude of the geocoded location: first the results have to be extracted to a `list` using `jsonlite::fromJSON()`, then the lat/lon can be extracted by looking for a specific node in the output. To integrate this into the function returned from `get_textf()`, it is possible to add a `.parser` argument that will be applied to the text output of the function (or raw output for `_raw` variants, or the filename for `_download` variants).

``` r
parse_output <- function(output) {
  # parse JSON to list using jsonlite
  as_list <- jsonlite::fromJSON(output)
  
  # check for zero results
  if(nrow(as_list$results) == 0) stop("No results found")
  if(nrow(as_list$results) > 1) warning("More than one result found")
  
  # return geometry of first location found
  as_list$results$geometry$location
}

geocode <- get_textf("https://maps.googleapis.com/maps/api/geocode/json",
                     .parser = parse_output)
geocode(address = "New York City")
#> Retrieving information from https://maps.googleapis.com/maps/api/geocode/json?address=New%20York%20City
#>        lat       lng
#> 1 40.71278 -74.00594
```

Caches
------

For this particular example, it makes sense to cache the result so that Google doesn't need to be queried every single time you re-run your code. This package has its own caching system, although you could easily use the [memoise](https://cran.r-project.org/package=memoise) package on the `geocode` function to acheive similar results. The caching system in this package is more basic and less dependent on R data structures, so that files are accessible to you, the user, as well as functions that prefer a filename as input (I've used `read.csv()` like this before, although theoretically you can use `read.csv()` on in-memory text as well). This is specified using the `.cache` argument.

``` r
geocode <- get_textf("https://maps.googleapis.com/maps/api/geocode/json",
                     .parser = parse_output, .cache = "google.cache")
geocode(address = "New York City")
#> Using cached information for https://maps.googleapis.com/maps/api/geocode/json?address=New%20York%20City
#>        lat       lng
#> 1 40.71278 -74.00594
```

``` r
geocode(address = "New York City")
#> Using cached information for https://maps.googleapis.com/maps/api/geocode/json?address=New%20York%20City
#>        lat       lng
#> 1 40.71278 -74.00594
```

Using text for the `.cache` argument will use a folder on disk (relative to the working directory) to cache values. Passing an `environment` will result in the values being kept in that `environment`, and passing `NULL` will result in no cache being used (this is the default). Using an in-memory cache probably makes sense for most text-based APIs when the results are valid for one session; using a file-based cache probably makes sense when the results from the API will never change (climate data is a good example of this).

Effective API Wrappers
----------------------

In practice, you will want to go above and beyond what `get_textf()` has to offer with regard to creating an effective R equivalent to a REST API function. An example of this is the `geocode()` function found in the [prettymapr](https://cran.r-project.org/package=prettymapr) package, which contains argument validation and documentation specific to Google/Pickpoint/Data Science Toolkit geocoding APIs. In may cases you will also want to vectorize over certain arguments using [plyr](https://cran.r-project.org/package=plyr) or the `apply` family of functionals. I find cache management the best reason to use API wrappers based in this package, having written and rewritten several cache management systems for various packages and analysis projects, however it may be just as easy to use [curl](https://cran.r-project.org/package=curl) and [httr](https://cran.r-project.org/package=httr) with some other cache management (like [memoise](https://cran.r-project.org/package=memoise)) instead.
