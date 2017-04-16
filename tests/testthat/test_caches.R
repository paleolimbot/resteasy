
context("caches")

test_that("environment caches work properly", {

  cache <- as.cache(new.env(parent = emptyenv()))
  expect_is(cache, "environment_cache")

  # start with clean cache
  clear_resteasy_cache(cache)
  expect_equal(length(cache$env), 0)

  # check adding 1
  set_cached_text(cache, "dummy url", "dummy data")
  expect_equal(length(cache$env), 1)

  # check results
  expect_identical(get_cached_text(cache, "dummy url"), "dummy data")

  # check adding 2
  set_cached_text(cache, "dummy url2", "dummy data2")
  expect_equal(length(cache$env), 2)

  # check resetting of cached item
  set_cached_text(cache, "dummy url2", "dummy data3")
  expect_equal(length(cache$env), 2)

  # check cache_size
  expect_equal(cache_size(cache), 2)

  # check raw setting
  set_cached_raw(cache, "dummy url2", as.raw(1:5))
  expect_equal(cache_size(cache), 3) # should create new entry, not use _text
  expect_identical(get_cached_raw(cache, "dummy url2"), as.raw(1:5))


  # check clearing of cache
  clear_resteasy_cache(cache)
  expect_equal(length(cache$env), 0)
})

test_that("file caches work properly", {

  cache <- as.cache("test.cache")
  expect_is(cache, "file_cache")

  # start with clean cache
  clear_resteasy_cache(cache)
  expect_equal(cache_size(cache), 0)

  # check adding 1
  set_cached_text(cache, "dummy url", "dummy data")
  expect_equal(cache_size(cache), 1)

  # check results
  expect_identical(get_cached_text(cache, "dummy url"), "dummy data")

  # check adding 2
  set_cached_text(cache, "dummy url2", "dummy data2")
  expect_equal(cache_size(cache), 2)

  # check resetting of cached item
  set_cached_text(cache, "dummy url2", "dummy data3")
  expect_equal(cache_size(cache), 2)

  # check cache_size
  expect_equal(cache_size(cache), 2)

  # check raw setting
  set_cached_raw(cache, "dummy url2", as.raw(1:5))
  expect_equal(cache_size(cache), 3) # should create new entry, not use _text
  expect_identical(get_cached_raw(cache, "dummy url2"), as.raw(1:5))


  # check clearing of cache
  clear_resteasy_cache(cache)
  expect_equal(cache_size(cache), 0)

  # clean directory
  unlink("test.cache", recursive = TRUE)
})

test_that("the null cache works properly", {
  cache <- as.cache(NULL)
  expect_is(cache, "null_cache")
  # check that cache is empty
  expect_equal(cache_size(cache), 0)
  # check warning on cache setting
  expect_warning(set_cached_text(cache, "url", "data"),
                 "Attempt to set_cache for the null_cache")
  expect_warning(set_cached_raw(cache, "url", "data"),
                 "Attempt to set_cache for the null_cache")
  # expect silent cache getting
  expect_null(get_cached_text(cache, "url"))
  expect_null(get_cached_raw(cache, "url"))
})

test_that("the internal cache is returned with NA", {
  cache <- as.cache(NA)
  expect_is(cache, "environment_cache")
  expect_identical(cache$env, internal_cache_environment)
})

test_that("a full cache throws a warning", {
  cache <- as.cache(new.env(), max_size = 3)
  expect_is(cache, "environment_cache")

  set_cached_text(cache, "url1", "data1")
  set_cached_text(cache, "url2", "data2")
  expect_warning(set_cached_text(cache, "url3", "data3"), "Cache full")
  expect_true(cache_full(cache))
})



