# Custom functions for the greenT app


# Generate a random hex code ----------------------------------------------
randomHex <- function(){
  vec <- c(LETTERS, 0:9)
  six <- sample(vec, 6, replace = T) %>% paste(collapse = "")
  hex <- paste0("#", six)
  return(hex)
}
