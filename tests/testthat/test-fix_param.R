test_that("Simple vectors Work", {
  expect_equal(timefitteR:::fix_param(c(1, 2, 3), n = 3), c(1, 2, 3))
  expect_equal(timefitteR:::fix_param(c(1, 2, 2), n = 3), c(1, 2, 2))
  expect_equal(timefitteR:::fix_param(c(1, 2, 3, 4), n = 3), c(1, 2, 3))
})

test_that("Lists are handled correctly", {
  expect_equal(timefitteR:::fix_param(list(1, 2, 3), n = 3), c(1, 2, 3))
  expect_equal(timefitteR:::fix_param(list(1, 2, 3, 4, 5), n = 3), c(1, 2, 3))
  expect_equal(timefitteR:::fix_param(list(1, 2), n = 3), c(1, 2, 2))
})

test_that("Strings are not broken up", {
  expect_equal(timefitteR:::fix_param("abcde", n = 3), c("abcde", "abcde", "abcde"))
  expect_equal(timefitteR:::fix_param(c("abc", "def", "g", "h"), n = 3), c("abc", "def", "g"))
})

test_that("Data frames and tibbles are handled correctly", {
  expect_equal(timefitteR:::fix_param(data.frame(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10), c(11, 12, 13, 14, 15)), n = 3), c(1, 2, 3))
  expect_equal(timefitteR:::fix_param(data.frame(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10), c(11, 12, 13, 14, 15))[, 2], n = 3), c(6, 7, 8))
  expect_equal(timefitteR:::fix_param(data.frame(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10), c(11, 12, 13, 14, 15))[2], n = 3), c(6, 7, 8))
  expect_equal(timefitteR:::fix_param(data.frame(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10), c(11, 12, 13, 14, 15))[2, ], n = 3), c(2, 7, 12))
  expect_equal(timefitteR:::fix_param(tibble::tibble(x = c(1, 2, 3, 4, 5), y = c(6, 7, 8, 9, 10), z = c(11, 12, 13, 14, 15))["x"], n = 3), c(1, 2, 3))
  expect_equal(timefitteR:::fix_param(data.frame(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10), c(11, 12, 13, 14, 15))[2:3], n = 3), c(6, 7, 8))
})
