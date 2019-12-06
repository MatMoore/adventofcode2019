# Advent of Code 2019 Day 1
# https://adventofcode.com/2019/day/1

# part 1
day_01_part1 <- function() {
  data('day_01_input')

  return(sum(floor(day_01_input$mass / 3) - 2))
}

# part 2
getFuel <- function (mass) {
  fuelForMass = floor(mass /  3) - 2;
  if(fuelForMass > 0) {
    return(getFuel(fuelForMass) + fuelForMass)
  } else {
    return(0)
  }
}

day_01_part2 <- function() {
  data('day_01_input')

  return(sum(sapply(day_01_input$mass, getFuel)))
}
