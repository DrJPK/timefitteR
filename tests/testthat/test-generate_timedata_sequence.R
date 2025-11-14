test_that("passing parameters works", {
  counter <- 0
  repeat {
    counter <- counter + 1
    df <- generate_timedata_sequence(intercept = runif(1, -5, 5))
    expect_type(df, "list")
    expect_equal(dim(df), c(11, 2))
    expect_equal(sum(is.na(df[2])), 0)
    if (counter > 4) {
      break
    }
  }
  counter <- 0
  repeat {
    counter <- counter + 1
    df <- generate_timedata_sequence(slope = runif(1, -5, 5))
    expect_type(df, "list")
    expect_equal(dim(df), c(11, 2))
    expect_equal(sum(is.na(df[2])), 0)
    if (counter > 4) {
      break
    }
  }
  counter <- 0
  repeat {
    counter <- counter + 1
    df <- generate_timedata_sequence(curvature = runif(1, -5, 5))
    expect_type(df, "list")
    expect_equal(dim(df), c(11, 2))
    expect_equal(sum(is.na(df[2])), 0)
    if (counter > 4) {
      break
    }
  }
  counter <- 0
  repeat {
    counter <- counter + 1
    df <- generate_timedata_sequence(noise = runif(1, 0, 4))
    expect_type(df, "list")
    expect_equal(dim(df), c(11, 2))
    expect_equal(sum(is.na(df[2])), 0)
    if (counter > 4) {
      break
    }
  }
  counter <- 0
  repeat {
    counter <- counter + 1
    df <- generate_timedata_sequence(from = as.integer(runif(1, -4, 4)))
    expect_type(df, "list")
    expect_equal(dim(df), c(11, 2))
    expect_equal(sum(is.na(df[2])), 0)
    if (counter > 4) {
      break
    }
  }
  counter <- 0
  repeat {
    counter <- counter + 1
    x <- as.integer(runif(1, -4, 20))
    if (x < 1) {
      expect_error(generate_timedata_sequence(length = x))
    } else {
      df <- generate_timedata_sequence(length = x)
      expect_type(df, "list")
      expect_equal(dim(df), c((x + 1), 2))
      expect_equal(sum(is.na(df[2])), 0)
      if (counter > 4) {
        break
      }
    }
  }
  expect_error(generate_timedata_sequence(length = "a"))
  expect_error(generate_timedata_sequence(from = "a"))
  expect_error(generate_timedata_sequence(noise_ratio = c(1, 2)))
  expect_error(generate_timedata_sequence(noise_ratio = c(1, 2, 3, 4)))
})
