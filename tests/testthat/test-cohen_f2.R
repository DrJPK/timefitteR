
df <-tibble::tibble(x=seq(1,10,length.out=100),y=x^2 + rnorm(length(x)))
m0 <- lm(y ~ x, data =df)
m1 <- lm(y ~ x^2, data = df)


test_that("Fit Quality Works for 1 model", {
  expect_gte(cohen_f2(m0), 0)
})

test_that("better model give positive f^2", {
  expect_gte(cohen_f2(m0,m1), 0)
})

test_that("non-model parameter 1 causes error", {
  expect_error(cohen_f2(d))
})

test_that("non-model parameter 2 causes error", {
  expect_error(cohen_f2(m0,d))
})
