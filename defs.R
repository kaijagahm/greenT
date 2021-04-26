# Custom functions for the greenT app

# Confusingly, we have three possible ways to name our color inputs
## `inputIds` is the literal names of the input objects. Have to have the numbers spelled out because you can't have in input called "1" #XXX can you?
inputIds <- c(letters, c("zero", "one", "two", "three", "four", "five", 
                         "six", "seven", "eight", "nine"))
## `displayNames` is how I want the labels of the input objects to display: capital letters and digits.
displayNames <- c(LETTERS, 0:9)
## `charactersOut` is how I want the characters to display when they print out to a csv. This isn't critical at all, and maybe I'm being too picky, but I kind of like having uppercase letters for display vs. lowercase letters for collecting data.
charactersOut <- tolower(displayNames)

# Read in my colors, and convert the `character` column from charactersOut (the way it was written out) to `inputIds` (so we can use these colors to set the initial values of the input selectors)
kaijaColors <- read.csv(here("data", "kaijaColors.csv")) %>%
  mutate(character = as.character(fct_recode(character,
                                             !!! setNames(charactersOut, inputIds))))

# Function to reorganize the inputs horizontally --------------------------
horiz <- function(vec = inputIds, nrow = 6, ncol = 6){
  mat <- matrix(vec, nrow = nrow, ncol = ncol, byrow = T)
  horizVec <- c(mat)
  return(horizVec)
}

# Function to create inputs -----------------------------------------------
colorInit <- function(x = .x, y = .y){
  colourpicker::colourInput(x, y, value = randomColor(), showColour = "background")
}

# Mandatory fields for contribution form ----------------------------------
fieldsMandatory <- c("name", "handedness", "gender", "sex", "synesthesia", "consent")
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

fieldsAll <- c("name", "email", "birthYear", "handedness", "gender", "genderSelfDescribe", "sex", "strong", "consistent", "synesthesia", "howLong", "family", "synesthesiaTypes", "otherSynesthesia", "comments", "consent")

# dateTimeFormat ----------------------------------------------------------
dateTimeFormat <- function(){
  dateTime <- str_replace(str_replace_all(as.character(Sys.time()), 
                              ":", "-"), 
              " ", "_")
  
  return(dateTime)
}

# Function to save form data ----------------------------------------------
saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      dateTimeFormat(),
                      digest::digest(data))
  
  write.csv(x = data, file = file.path("responses", fileName),
            row.names = FALSE, quote = TRUE)
}

