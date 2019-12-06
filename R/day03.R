parseDirection <- function(instruction) {
  return(substr(instruction, 1, 1))
}

parseLength <- function(instruction) {
  return(as.integer(substr(instruction, 2, 2)))
}

parseWirePath <- function(path) {
  directions <- parseDirection(path)
  lengths <- parseLength(path)
  
  ups <- lengths[directions == "U"]
  downs <- lengths[directions == "D"]
  lefts <- lengths[directions == "L"]
  rights <- lengths[directions == "R"]
  
  
}