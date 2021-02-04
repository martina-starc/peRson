test_that("returns data frame ordered by rn and n", {
  shuffle_result <- data.frame(rn = rev(rep(c(1:3), each = 3)), orig_order = rep(c(1:3), times = 3)) %>%
    shuffle_questions()
  expect_equal(shuffle_result$n, 1:9)
  expect_equal(shuffle_result$rn, rep(c(1:3), each = 3))
  # this one could be true on some occasions, how to test this?
  expect_false(isTRUE(all.equal(shuffle_result$orig_order, rep(c(1:3), times = 3))))
})
