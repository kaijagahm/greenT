library(shiny) # duh
library(colourpicker) # for color inputs
library(dplyr) # because i like pipes
library(stringr) # for dealing with text
# Some random change to the code.
library(data.table) # for binding the list into a data frame
library(forcats) # for dealing with factor recoding
library(here) # for file paths
library(randomcoloR) # for randomly-generated colors
library(ggplot2)
library(shinyBS)
source(here("functions.R"))


# UI ----------------------------------------------------------------------
ui <- function(request){ # UI as a function to enable bookmarking
  fluidPage(
    
    # Change the font for the whole app -----------------------------------
    ## white is already the default background color; just leaving this here in case we decide to change it later.
    tags$head(     
      tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Baloo+2&display=swap');
      body {
        background-color: white;
      }
      * {
        font-family: 'Baloo 2', sans-serif;
      }")
      )
    ),
    
    # Title and subtitle, two different formats
    titlePanel(div(HTML("<b>greenT  </b> <em><small>exploring grapheme-color synesthesia</em></small>"))),
    
    # Body
    # Color pickers -------------------------------------------------------
    fluidRow(
      bsCollapse(
        id = "colorSelectors", open = "setColors",
        bsCollapsePanel(
          title = HTML("<em><small>Show/hide selectors</em></small>"),
          value = "setColors",
          column(width = 2,
                 colourInput("a", "A", value = randomColor()),
                 colourInput("g", "G", value = randomColor()),
                 colourInput("m", "M", value = randomColor()),
                 colourInput("s", "S", value = randomColor()),
                 colourInput("y", "Y", value = randomColor()),
                 colourInput("five", "5", value = randomColor())),
          column(width = 2,
                 colourInput("b", "B", value = randomColor()),
                 colourInput("h", "H", value = randomColor()),
                 colourInput("n", "N", value = randomColor()),
                 colourInput("t", "T", value = randomColor()),
                 colourInput("z", "Z", value = randomColor()),
                 colourInput("six", "6", value = randomColor())),
          column(width = 2,
                 colourInput("c", "C", value = randomColor()),
                 colourInput("i", "I", value = randomColor()),
                 colourInput("o", "O", value = randomColor()),
                 colourInput("u", "U", value = randomColor()),
                 colourInput("one", "1", value = randomColor()),
                 colourInput("seven", "7", value = randomColor())),
          column(width = 2,
                 colourInput("d", "D", value = randomColor()),
                 colourInput("j", "J", value = randomColor()),
                 colourInput("p", "P", value = randomColor()),
                 colourInput("v", "V", value = randomColor()),
                 colourInput("two", "2", value = randomColor()),
                 colourInput("eight", "8", value = randomColor())),
          column(width = 2,
                 colourInput("e", "E", value = randomColor()),
                 colourInput("k", "K", value = randomColor()),
                 colourInput("q", "Q", value = randomColor()),
                 colourInput("w", "W", value = randomColor()),
                 colourInput("three", "3", value = randomColor()),
                 colourInput("nine", "9", value = randomColor())),
          column(width = 2,
                 colourInput("f", "F", value = randomColor()),
                 colourInput("l", "L", value = randomColor()),
                 colourInput("r", "R", value = randomColor()),
                 colourInput("x", "X", value = randomColor()),
                 colourInput("four", "4", value = randomColor()),
                 colourInput("zero", "0", value = randomColor()))
        )
      )
    ),
    fluidRow(
      # Text input ---------------------------------------------------------
      column(width = 9,
             textInput("displayText", 
                       "Text to display:", 
                       value = "Type something")
      ),
      
      # Bookmark button ----------------------------------------------------
      column(width = 3,
             br(), # just so the spacing aligns better with the text entry box
             bookmarkButton(label = "Save these colors",
                            icon = shiny::icon("heart-empty", lib = "glyphicon"))
      )
    ),
    
    # Output object -------------------------------------------------------
    fluidRow(
      plotOutput("colorBlocks")
    )
  )
}

server <- function(input, output, session){
  
  # Save the user-entered color values --------------------------------------
  colors <- reactiveValues(space = "#FFFFFF") # initialize an empty reactiveValues object
  # When any of the colors change, change the value in colors()
  # I'm not 100% sure this is right...
  observe({
    colors$a <- input$a
    colors$b <- input$b
    colors$c <- input$c
    colors$d <- input$d
    colors$e <- input$e
    colors$f <- input$f
    colors$g <- input$g
    colors$h <- input$h
    colors$i <- input$i
    colors$j <- input$j
    colors$k <- input$k
    colors$l <- input$l
    colors$m <- input$m
    colors$n <- input$n
    colors$o <- input$o
    colors$p <- input$p
    colors$q <- input$q
    colors$r <- input$r
    colors$s <- input$s
    colors$t <- input$t
    colors$u <- input$u
    colors$v <- input$v
    colors$w <- input$w
    colors$x <- input$x
    colors$y <- input$y
    colors$z <- input$z
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
                                    "[^a-z0-9]", " ") %>% # replace all non-alphanumeric characters with a space
                      str_replace_all(., "\\s{2,}", " "), # replace runs of multiple spaces with a single space
                    split = ""))
  )
  
  # Create a rectangle data frame
  rectangleDF <- reactive({
    req(length(split()) > 0) # will only work when there is text entered in the box
    data.frame(grapheme = split(),
               y = 5) %>%
      mutate(x = 1:nrow(.)) %>%
      left_join(colorsDF(), by = c("grapheme"))
  }
  )
  
  # Plot the color blocks
  output$colorBlocks <- renderPlot({
    rectangleDF() %>%
      ggplot(aes(x, y))+
      geom_tile(aes(fill = hex))+
      scale_fill_identity()+
      theme_void()
  })
  
}
shinyApp(ui, server)