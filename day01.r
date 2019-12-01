# Advent of Code 2019 Day 1
# https://adventofcode.com/2019/day/1
#
# TODO: change this to use notebooks?

foo <- read.table('input/day-01-input.txt')

# part 1
sum(floor(foo$V1 / 3) - 2)

# part 2
getFuel <- function (mass) {
  fuelForMass = floor(mass /  3) - 2;
  if(fuelForMass > 0) {
    return(getFuel(fuelForMass) + fuelForMass)
  } else {
    return(0)
  }
}

sum(sapply(foo$V1, getFuel))