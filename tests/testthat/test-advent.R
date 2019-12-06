test_that("day 1 part 1 returns the correct answer", {
  answer <- day_01_part1()
  expect_equal(answer, 3456641)
})

test_that("day 1 part 2 returns the correct answer", {
  answer <- day_01_part2()
  expect_equal(answer, 5182078)
})

test_that("day 2 part 1 returns the correct answer", {
  answer <- day_02_part1()
  expect_equal(answer, 3790645)
})

test_that("day 2 part 2 returns the correct answer", {
  answer <- day_02_part2()
  expect_equal(answer, 6577)
})
