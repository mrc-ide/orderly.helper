test_that("orderly_version_str constructs good strings", {
  skip_if_not_installed("mockery")
  mock_package_version <- mockery::mock(
    numeric_version("1.7.0"),
    numeric_version("1.99.0"),
    stop("package not installed"))
  mockery::stub(orderly_version_str, "utils::packageVersion",
                mock_package_version)
  expect_equal(
    orderly_version_str(1), "orderly 1 (1.7.0)")
  expect_equal(
    orderly_version_str(2), "orderly 2 (1.99.0)")
  expect_equal(
    orderly_version_str(2), "orderly 2 (???)")
})


test_that("can validate orderly version", {
  expect_equal(validate_orderly_version(1, "arg", FALSE), 1)
  expect_equal(validate_orderly_version(2, "arg", FALSE), 2)
  expect_equal(validate_orderly_version("1", "arg", TRUE), 1)
  expect_equal(validate_orderly_version("2", "arg", TRUE), 2)

  expect_error(validate_orderly_version(1:2, "arg", FALSE),
               "Expected 'version' to be scalar number (from arg)",
               fixed = TRUE)
  expect_error(validate_orderly_version("1", "arg", FALSE),
               "Expected 'version' to be scalar number (from arg)",
               fixed = TRUE)
  expect_error(validate_orderly_version(NA_real_, "arg", FALSE),
               "Expected 'version' to be scalar number (from arg)",
               fixed = TRUE)
  expect_error(validate_orderly_version(3, "arg", FALSE),
               "Invalid version '3', expected '1' or '2' (from arg)",
               fixed = TRUE)

  expect_error(validate_orderly_version("one", "arg", TRUE),
               "Expected 'version' to be a number (from arg)",
               fixed = TRUE)
})


test_that("can guess orderly version", {
  withr::local_envvar(ORDERLY_VERSION = NA)
  withr::local_options(orderly.version = NULL)
  expect_equal(guess_orderly_version(NULL), 2)

  expect_equal(guess_orderly_version(1), 1)
  expect_equal(guess_orderly_version(2), 2)

  withr::with_options(list(orderly.version = 1),
                      expect_equal(guess_orderly_version(NULL), 1))
  withr::with_options(list(orderly.version = 2),
                      expect_equal(guess_orderly_version(NULL), 2))

  withr::with_envvar(c(ORDERLY_VERSION = 1), {
    expect_equal(guess_orderly_version(NULL), 1)
    withr::with_options(list(orderly.version = 1),
                        expect_equal(guess_orderly_version(NULL), 1))
    withr::with_options(list(orderly.version = 2),
                        expect_equal(guess_orderly_version(NULL), 2))
  })
})


test_that("can respond to verbose option", {
  withr::local_options(orderly.helper.verbose = NULL)
  expect_true(orderly_helper_verbose(NULL))
  expect_true(orderly_helper_verbose(TRUE))
  expect_false(orderly_helper_verbose(FALSE))
  withr::with_options(list(orderly.helper.verbose = FALSE), {
    expect_false(orderly_helper_verbose(NULL))
    expect_true(orderly_helper_verbose(TRUE))
    expect_false(orderly_helper_verbose(FALSE))
  })
})


test_that("can detect orderly version", {
  tmp <- withr::local_tempdir()
  expect_error(
    detect_orderly_version(tmp),
    "Did not find orderly root above")
  file.create(file.path(tmp, "orderly_config.yml"))
  expect_error(
    detect_orderly_version(tmp),
    "Failed to read required orderly version from")
  writeLines("minimum_orderly_version: 1.7.0",
             file.path(file.path(tmp, "orderly_config.yml")))
  expect_equal(detect_orderly_version(tmp), 1)
  writeLines("minimum_orderly_version: 1.99.0",
             file.path(file.path(tmp, "orderly_config.yml")))
  expect_equal(detect_orderly_version(tmp), 2)
  writeLines("minimum_orderly_version: 2.0.0",
             file.path(file.path(tmp, "orderly_config.yml")))
  expect_equal(detect_orderly_version(tmp), 2)
})


test_that("can set up orderly1 as orderly", {
  current$name <- NULL
  msg <- testthat::capture_messages(create_orderly_ns(1, TRUE))
  expect_match(msg, "Using orderly 1 \\(1.\\d+.\\d+\\)", all = FALSE)
  expect_identical(getExportedValue("orderly", "orderly_run"),
                   orderly1::orderly_run)

  msg <- testthat::capture_messages(create_orderly_ns(1, TRUE))
  expect_match(msg, "Already using orderly 1 \\(1.\\d+.\\d+\\)", all = FALSE)
  expect_identical(getExportedValue("orderly", "orderly_run"),
                   orderly1::orderly_run)
})


test_that("can set up orderly2 as orderly", {
  expect_silent(create_orderly_ns(2, FALSE))
  expect_identical(getExportedValue("orderly", "orderly_run"),
                   orderly2::orderly_run)

  expect_silent(create_orderly_ns(2, FALSE))
  expect_identical(getExportedValue("orderly", "orderly_run"),
                   orderly2::orderly_run)
})


test_that("sitrep returns useful information", {
  deactivate()
  ans <- sitrep()
  expect_equal(ans$orderly,
               list(version = NULL,
                    is_installed = FALSE,
                    is_loaded = FALSE,
                    is_attached = FALSE))
  expect_equal(ans$orderly1,
               list(version = packageVersion("orderly1"),
                    is_installed = TRUE,
                    is_loaded = TRUE,
                    is_attached = FALSE))
  expect_equal(ans$orderly2,
               list(version = packageVersion("orderly2"),
                    is_installed = TRUE,
                    is_loaded = TRUE,
                    is_attached = FALSE))
  expect_equal(ans$current, list(version = NULL, name = NULL))
})


test_that("sitrep returns useful information after helper", {
  deactivate()
  ans1 <- sitrep()
  create_orderly_ns(2, FALSE)
  ans2 <- sitrep()
  expect_equal(ans2[names(ans2) != "current"], ans1[names(ans1) != "current"])
  expect_equal(ans2$current, list(version = 2, name = "orderly2"))
})


test_that("sitrep check throws when expected", {
  expect_silent(
    check_sitrep(
      list(orderly = list(is_installed = FALSE),
           orderly1 = list(is_attached = FALSE),
           orderly2 = list(is_attached = FALSE))))
  expect_silent(
    check_sitrep(
      list(orderly = list(is_installed = FALSE),
           orderly1 = list(is_attached = TRUE),
           orderly2 = list(is_attached = FALSE))))
  expect_silent(
    check_sitrep(
      list(orderly = list(is_installed = FALSE),
           orderly1 = list(is_attached = FALSE),
           orderly2 = list(is_attached = TRUE))))
  expect_error(
    check_sitrep(
      list(orderly = list(is_installed = TRUE),
           orderly1 = list(is_attached = FALSE),
           orderly2 = list(is_attached = FALSE))),
    "You have 'orderly' installed; please uninstall it")
  expect_error(
    check_sitrep(
      list(orderly = list(is_installed = FALSE),
           orderly1 = list(is_attached = TRUE),
           orderly2 = list(is_attached = TRUE))),
    "You have 'orderly1' and 'orderly2' attached; please restart")
})


test_that("use works with given version", {
  deactivate()
  use(1, FALSE)
  expect_equal(current$version, 1)
})


test_that("activate works with found version", {
  deactivate()
  tmp <- withr::local_tempdir()
  writeLines("minimum_orderly_version: 1.7.0",
             file.path(file.path(tmp, "orderly_config.yml")))
  withr::with_dir(tmp, activate())
  expect_equal(current$version, 1)
})
