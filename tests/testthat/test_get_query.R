
context("get_query")

test_that("get* caches results", {
  # start with clean cache
  clear_resteasy_cache("resteasy.cache")

  # test on goodreads / file cache
  expect_message(get_text("https://www.goodreads.com/book/title",
                      key="HSkIMuOGlxFIOmfBCGFVA", title='catch-22',
                      .cache = "resteasy.cache"),
                 "Retrieving information from")

  expect_message(get_text("https://www.goodreads.com/book/title",
                           key="HSkIMuOGlxFIOmfBCGFVA", title='catch-22',
                           .cache = "resteasy.cache"),
                 "Using cached information for")

  # test on goodreads / default cache
  expect_message(get_text("https://www.goodreads.com/book/title",
                           key="HSkIMuOGlxFIOmfBCGFVA", title='catch-22',
                           .cache = NA),
                 "Retrieving information from")

  expect_message(get_text("https://www.goodreads.com/book/title",
                           key="HSkIMuOGlxFIOmfBCGFVA", title='catch-22',
                           .cache = NA),
                 "Using cached information for")

  # test on goodreads / null cache
  expect_message(get_text("https://www.goodreads.com/book/title",
                           key="HSkIMuOGlxFIOmfBCGFVA", title='catch-22',
                           .cache = NULL),
                 "Retrieving information from")

  expect_message(get_text("https://www.goodreads.com/book/title",
                           key="HSkIMuOGlxFIOmfBCGFVA", title='catch-22',
                           .cache = NULL),
                 "Retrieving information from")

  clear_resteasy_cache(NA)
  unlink("resteasy.cache", recursive = TRUE)
})

test_that("get_* uses the correct cache", {
  # start with clean cache
  clear_resteasy_cache("resteasy.cache")

  get_text("https://www.goodreads.com/book/title",
            key="HSkIMuOGlxFIOmfBCGFVA", title='catch-22',
            .cache = "resteasy.cache")

  expect_true(dir.exists("resteasy.cache"))
  expect_equal(cache_size(as.cache("resteasy.cache")), 1)

  unlink("resteasy.cache", recursive = TRUE)
})

test_that("get_text returns text", {
  expect_is(get_text("http://www.google.com/"), "character")
})

test_that("get_raw returns bytes", {
  expect_is(get_raw("http://www.google.com/"), "raw")
})

test_that("get_download returns a filename", {
  fname <- get_download("http://www.google.com")
  expect_true(file.exists(fname))
  unlink(fname)
})

test_that("get_raw and get_download file_caches are in sync", {
  cache <- as.cache("test.cache")
  expect_message(get_download("http://www.google.com/", .cache = cache),
                 "Retrieving information from http://www.google.com/")
  expect_message(get_raw("http://www.google.com/", .cache = cache),
                 "Using cached information for http://www.google.com/")
  unlink("test.cache", recursive = TRUE)
})

test_that("zero arguments will still fetch a result", {
  res <- get_text("http://www.google.com/")
  expect_false(is.null(res))
})

test_that("null parameters are dropped from url", {
  expect_message(get_text("http://www.google.com/", q = NULL),
                 "http://www.google.com/\\s$")
})

test_that("NA parameters are converted to ''", {
  expect_message(get_text("http://www.google.com/", q = NA),
                 "Coercing an NA query parameter to")
})

test_that("invalid addresses result in errors", {
  expect_error(get_text("http://not.an.address.ever"),
               "Unable to connect to http://not.an.address.ever/")
})
