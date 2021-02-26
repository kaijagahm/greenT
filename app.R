library(shiny)
library(colourpicker)

ui <- fluidPage(
  tags$head(    
    tags$style("label {display:inline;}") # this isn't working--I'd like to display the labels next to the pickers instead of above them, but I don't really know any css. 
  ),
  titlePanel(title = "greenT"),
  fluidRow(
    column(width = 2,
           colourInput("a", "A"),
           colourInput("g", "G"),
           colourInput("m", "M"),
           colourInput("s", "S"),
           colourInput("y", "Y"),
           colourInput("five", "5")),
    column(width = 2,
           colourInput("b", "B"),
           colourInput("h", "H"),
           colourInput("n", "N"),
           colourInput("t", "T"),
           colourInput("z", "Z"),
           colourInput("six", "6")),
    column(width = 2,
           colourInput("c", "C"),
           colourInput("i", "I"),
           colourInput("o", "O"),
           colourInput("u", "U"),
           colourInput("one", "1"),
           colourInput("seven", "7")),
    column(width = 2,
           colourInput("d", "D"),
           colourInput("j", "J"),
           colourInput("p", "P"),
           colourInput("v", "V"),
           colourInput("two", "2"),
           colourInput("eight", "8")),
    column(width = 2,
           colourInput("e", "E"),
           colourInput("k", "K"),
           colourInput("q", "Q"),
           colourInput("w", "W"),
           colourInput("three", "3"),
           colourInput("nine", "9")),
    column(width = 2,
           colourInput("f", "F"),
           colourInput("l", "L"),
           colourInput("r", "R"),
           colourInput("x", "X"),
           colourInput("four", "4"),
           colourInput("zero", "0"))),
  fluidRow(
    column(width = 12,
           textInput("displayText", "Text to display:")
           )
  ),
  fluidRow(
    
  )
)

server <- function(input, output, session){
  
}
shinyApp(ui, server)