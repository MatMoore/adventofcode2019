context("day 1")
test_that("day 1 part 1 returns the correct answer", {
  answer <- day_01_part1()
  expect_equal(answer, 3456641)
})

test_that("day 1 part 2 returns the correct answer", {
  answer <- day_01_part2()
  expect_equal(answer, 5182078)
})

context("day 2")
test_that("day 2 part 1 returns the correct answer", {
  answer <- day_02_part1()
  expect_equal(answer, 3790645)
})

test_that("day 2 part 2 returns the correct answer", {
  answer <- day_02_part2()
  expect_equal(answer, 6577)
})

context("day 3")
test_that("can parse a wire direction", {
  expect_equal(parse_direction("L1"), "L")
  expect_equal(parse_direction("R0"), "R")
  expect_equal(parse_direction("U123"), "U")
  expect_equal(parse_direction("D4"), "D")
})

test_that("can parse a wire length", {
  expect_equal(parse_length("L1"), 1)
  expect_equal(parse_length("R0"), 0)
  expect_equal(parse_length("U123"), 123)
  expect_equal(parse_length("D4"), 4)
})

test_that("can parse a wire", {
  input <- "R8"

  path <- parse_wire(input)

  expect_equal(path$direction, "R")
  expect_equal(path$wire_length, 8)
})

test_that("can parse a wire path", {
  input <- c("R8", "U5", "L5", "D3")

  path <- parse_wire_path(input)

  expect_equal(path[[1]]$direction, "R")
  expect_equal(path[[1]]$wire_length, 8)

  expect_equal(path[[2]]$direction, "U")
  expect_equal(path[[2]]$wire_length, 5)

  expect_equal(path[[3]]$direction, "L")
  expect_equal(path[[3]]$wire_length, 5)

  expect_equal(path[[4]]$direction, "D")
  expect_equal(path[[4]]$wire_length, 3)
})

test_that("can place central node on a grid", {
  path <- parse_wire_path(c("R8", "U5", "L5", "D3"))
  dims = wire_grid_dimensions(list(path))
  expect_equal(dims$cx, 1)
  expect_equal(dims$cy, 6)
})

test_that("can calculate required grid size", {
  path <- parse_wire_path(c("R8", "U5", "L5", "D3"))
  dims = wire_grid_dimensions(list(path))
  expect_equal(dims$width, 9)
  expect_equal(dims$height, 6)
})

test_that("can put central node in the centre of a grid", {
  # | . . . .
  # | . . . .
  # | . o - +
  # | . . . |
  # + - - - +

  path <- parse_wire_path(c("R2", "D2", "L4", "U4"))
  dims = wire_grid_dimensions(list(path))

  expect_equal(dims$cx, 3)
  expect_equal(dims$cy, 3)
  expect_equal(dims$width, 5)
  expect_equal(dims$height, 5)
})

test_that("can draw a wire path", {
  path <- parse_wire_path(c("R2", "D2", "L4", "U4"))
  dims = wire_grid_dimensions(list(path))
  grid <- matrix(0, nrow=dims$height, ncol=dims$width)

  grid <- draw_wire_path(matrix=grid, row=dims$cy, col=dims$cx, path=path)

  expected <- rbind(
    c(12,0,0,0,0),
    c(11,0,0,0,0),
    c(10,0,0,1,2),
    c(9, 0,0,0,3),
    c(8, 7,6,5,4)
  )

  expect_equal(grid, expected)
})

test_that("can identify wire crossings", {
  path1 <- parse_wire_path(c("R8", "U5", "L5", "D3"))
  path2 <- parse_wire_path(c("U7", "R6", "D4", "L4"))
  dims <- wire_grid_dimensions(list(path1, path2))
  grid <- matrix(0, nrow=dims$height, ncol=dims$width)

  grid2 <- draw_wire_path(matrix=grid, row=dims$cy, col=dims$cx, path=path1)
  grid3 <- draw_wire_path(matrix=grid, row=dims$cy, col=dims$cx, path=path2)
  grid2[grid2!=0] <- 1
  grid3[grid3!=0] <- 1

  expected <- rbind(
    c(1,1,1,1,1,1,1,0,0),
    c(1,0,0,0,0,0,1,0,0),
    c(1,0,0,1,1,1,2,1,1),
    c(1,0,0,1,0,0,1,0,1),
    c(1,0,1,2,1,1,1,0,1),
    c(1,0,0,1,0,0,0,0,1),
    c(1,0,0,0,0,0,0,0,1),
    c(0,1,1,1,1,1,1,1,1)
  )

  expect_equal(grid2 + grid3, expected)
})

test_that("can find the manhattan distance", {
  expect_equal(manhattan_distance(c(1,1), c(2,2)), 2)
  expect_equal(manhattan_distance(c(2,2), c(1,1)), 2)
  expect_equal(manhattan_distance(c(2,2), c(3,1)), 2)
  expect_equal(manhattan_distance(c(2,2), c(2,5)), 3)
})

test_that("can find intersections", {
  path1 <- parse_wire_path(c("R8", "U5", "L5", "D3"))
  path2 <- parse_wire_path(c("U7", "R6", "D4", "L4"))
  dims <- wire_grid_dimensions(list(path1, path2))
  grid <- matrix(0, nrow=dims$height, ncol=dims$width)

  grid2 <- draw_wire_path(matrix=grid, row=dims$cy, col=dims$cx, path=path1)
  grid3 <- draw_wire_path(matrix=grid, row=dims$cy, col=dims$cx, path=path2)
  grid2[grid2!=0] <- 1
  grid3[grid3!=0] <- 1

  intersections <- find_intersections(grid2 + grid3)

  expected <- rbind(
    c(row=5, col=4),
    c(row=3, col=7)
  )

  expect_equal(intersections, expected)
})

test_that("can find the closest intersection", {
  path1 <- parse_wire_path(c("R8", "U5", "L5", "D3"))
  path2 <- parse_wire_path(c("U7", "R6", "D4", "L4"))

  closest <- closest_intersection(path1, path2)

  expect_equal(closest, 6)
})

test_that("example1 has a distance of 159", {
  input1 <- c("R75", "D30", "R83", "U83", "L12", "D49", "R71", "U7", "L72")
  path1 <- parse_wire_path(input1)

  input2 <- c("U62", "R66", "U55", "R34", "D71", "R55", "D58", "R83")
  path2 <- parse_wire_path(input2)

  answer <- closest_intersection(path1, path2)

  expect_equal(answer, 159)
})

test_that("example1 takes 610 steps", {
  input1 <- c("R75", "D30", "R83", "U83", "L12", "D49", "R71", "U7", "L72")
  path1 <- parse_wire_path(input1)

  input2 <- c("U62", "R66", "U55", "R34", "D71", "R55", "D58", "R83")
  path2 <- parse_wire_path(input2)

  answer <- shortest_steps(path1, path2)

  expect_equal(answer, 610)
})

# These are commented because they are slow to run
#test_that("day 3 part 1 returns the incorrect answer", {
#  answer <- day03_part1()
#  print(answer)
#
#  expect_equal(answer, 273)
#})

#test_that("day 3 part 1 returns the incorrect answer", {
#  answer <- day03_part2()
#  print(answer)
#
#  expect_equal(answer, 15622)
#})
