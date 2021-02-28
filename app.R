library(shiny) # duh
library(colourpicker) # for color inputs
library(dplyr) # because i like pipes
library(stringr) # for dealing with text
# Some random change to the code.

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
           textInput("displayText", "Text to display:", value = "Your name")
    )
  ),
  fluidRow(
    plotOutput("colorBlocks")
  )
)

server <- function(input, output, session){
  colors <- reactiveValues()
  observe({
    colors$a <- input$a
    colors$b <- input$a
    colors$c <- input$a
    colors$d <- input$a
    colors$e <- input$a
    colors$f <- input$a
    colors$g <- input$a
    colors$h <- input$a
    colors$i <- input$a
    colors$j <- input$a
    colors$k <- input$a
    colors$l <- input$a
    colors$m <- input$a
    colors$n <- input$a
    colors$o <- input$a
    colors$p <- input$a
    colors$q <- input$a
    colors$r <- input$a
    colors$s <- input$a
    colors$t <- input$a
    colors$u <- input$a
    colors$v <- input$a
    colors$w <- input$a
    colors$x <- input$a
    colors$y <- input$a
    colors$z <- input$a
    colors$one <- input$one
    colors$two <- input$two
    colors$three <- input$three
    colors$four <- input$four
    colors$five <- input$five
    colors$six <- input$six
    colors$seven <- input$seven
    colors$eight <- input$eight
    colors$nine <- input$nine
    colors$zero <- input$zero
  })
  
  # Convert input to lowercase, replace all non-letter characters with a blank space, and split all the characters up into a vector. Later on, we might want to reconsider how the app behaves with non-letter characters, but this seems easiest for now.
  split <- reactive( 
    unlist(strsplit(str_replace_all(tolower(input$displayText), "[^a-z]", " "), split = ""))
  )
  
  # length of the display text string
  len <- reactive(length(split())) 
  
  # Create a rectangle data frame
  rectangleDF <- reactive({
    data.frame(letter = split(),
               y1 = 0,
               y2 = 5,
               x1 = 1:len(),
               x2 = 2:(len()+1))
  }
  )
  observe({
    rvtl <- reactiveValuesToList(colors)
  })
  
}
shinyApp(ui, server)