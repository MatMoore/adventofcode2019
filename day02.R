run_program <- function (program) {
  add_opcode <- 1
  multiply_opcode <- 2
  halt_opcode <- 99
  
  current_instruction_index <- 1
  while(TRUE) {
    current_instruction <- program[current_instruction_index]
    if(current_instruction == halt_opcode) {
      return(program)
    }
    
    input_1_index = program[current_instruction_index + 1] + 1 
    input_2_index = program[current_instruction_index + 2] + 1
    output_index = program[current_instruction_index + 3] + 1
    
    if(current_instruction == add_opcode) {
      result = program[input_1_index] + program[input_2_index]
    } else if(current_instruction == multiply_opcode) {
      result = program[input_1_index] * program[input_2_index]
    }
    
    program[output_index] = result
    
    current_instruction_index = current_instruction_index + 4
  }
}

initialize_memory <- function(program, input1, input2) {
  memory <- as.numeric(program)
  memory[2] <- input1
  memory[3] <- input2
  return(memory)
}

# part 1
day2_input <- read.table('input/day-02-input.txt', sep=',')
part1_memory <- initialize_memory(day2_input, 12, 2)
print(run_program(part1_memory))

# part 2
find_noun_and_verb <- function() {
  for (noun in 0:99) {
    for (verb in 0:99) {
      memory <- initialize_memory(day2_input, noun, verb);
      output <- run_program(memory)[1]
      if(output == 19690720) {
        return(c(noun, verb))
      }
    }
  }
}

result <- find_noun_and_verb()
print(result)
noun <- result[1]
verb <- result[2]
print(100*noun + verb)
