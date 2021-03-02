library(shiny) # duh
library(colourpicker) # for color inputs
library(dplyr) # because i like pipes
library(stringr) # for dealing with text
# Some random change to the code.
library(data.table) # for binding the list into a data frame
library(forcats) # for dealing with factor recoding

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
           textInput("displayText", "Text to display:", value = "/&@*3q984tguqerlgjq34pt  2p3ot 34kt34t ")
    )
  ),
  fluidRow(
    plotOutput("colorBlocks")
  )
)

server <- function(input, output, session){

# Save the user-entered color values --------------------------------------
  colors <- reactiveValues(space = "#FFFFFF") # initialize an empty reactiveValues object
  # When any of the colors change, change the value in colors()
  # I'm not 100% sure this is right...
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
  # Convert the reactiveValues object into a data frame so we can use it more easily.
  colorsDF <- eventReactive(colors, {
    reactiveValuesToList(colors) %>%
      lapply(., as.data.frame) %>%
      rbindlist(idcol = "grapheme") %>%
      as.data.frame() %>%
      rename("hex" = "X[[i]]") %>%
      mutate(grapheme = forcats::fct_recode(grapheme,
                                            " " = "space",
                                            "1" = "one",
                                            "2" = "two",
                                            "3" = "three",
                                            "4" = "four",
                                            "5" = "five",
                                            "6" = "six",
                                            "7" = "seven",
                                            "8" = "eight",
                                            "9" = "nine",
                                            "0" = "zero"))
  })
  
  # Convert input to lowercase, replace all non-alphanumeric characters with a blank space, and split the string into a vector
  split <- reactive( 
    unlist(strsplit(str_replace_all(tolower(input$displayText),
                                    "[^a-z0-9]", " ") %>% # replace all non-alnum characters with a space
                      str_replace_all(., "\\s{2,}", " "), # replace runs of multiple spaces with a single space
                    split = ""))
  )
  
  # Create a rectangle data frame
  rectangleDF <- reactive({
    data.frame(grapheme = split(),
               y1 = 0,
               y2 = 5) %>%
      mutate(x1 = 1:nrow(.),
             x2 = 2:(nrow(.) + 1)) %>%
      left_join(colorsDF(), by = c("grapheme"))
  }
  )

  observeEvent(rectangleDF, {
    browser()
  })
  
}
shinyApp(ui, server)