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
library(grDevices)
source(here("functions.R"))
library(showtext) # for fonts
library(sysfonts)
library(shinyWidgets)
font_add_google("Baloo 2")
showtext_auto()

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


# UI ----------------------------------------------------------------------
ui <- function(request){ # UI as a function to enable bookmarking
  fluidPage(
    
    # Change the font for the whole app -----------------------------------
    ## white is already the default background color; leaving this here in case want to change it later.
    tags$head(     
      tags$link(rel = "stylesheet", type = "text/css", href = "tea-style.css"),
      tags$script(src = "custom.js")
    ),
    
    # Title and subtitle, two different formats
    titlePanel(div(HTML("<b>greenT  </b> <em><small>exploring grapheme-color synesthesia</em></small>")),
               windowTitle = "greenT"), # windowTitle controls what shows up on the browser tab
    
    # Body
    # Color pickers -------------------------------------------------------
    ## Note that I've arranged these so that the alphabet goes left to right, then top to bottom, instead of going down the columns. I initially had it going down the columns and it looked weird.
    fluidRow(
      bsCollapse( # creates an environment in which collapsible panels can live
        id = "colorSelectors", open = "setColors",
        # Collapsible panel for the color selectors.
        bsCollapsePanel(
          # This is a bit of a hack: you're supposed to use this for the title of the panel, but I don't want a panel title. I just want the user to know where to click in order to collapse/open the panel. There is probably a better/cleaner way to do this.
          title = HTML("<em><small>Show/hide selectors</em></small>"), 
          value = "setColors", # allows us to control when this opens/closes
          column(width = 2,
                 purrr::map2(.x = c("a", "g", "m", "s", "y", "five"), 
                             .y = c("A", "G", "M", "S", "Y", "5"), 
                             ~colourInput(.x, .y, value = randomColor(), 
                                         showColour = "background"))
                 ), 
          column(width = 2,
                 purrr::map2(.x = c("b", "h", "n", "t", "z", "six"), 
                             .y = c("B", "G", "N", "T", "Z", "6"), 
                             ~colourInput(.x, .y, value = randomColor(), 
                                          showColour = "background"))
                 ),
          column(width = 2,
                 purrr::map2(.x = c("c", "i", "o", "u", "one", "seven"), 
                             .y = c("C", "I", "O", "U", "1", "7"), 
                             ~colourInput(.x, .y, value = randomColor(), 
                                          showColour = "background"))
                 ),
          column(width = 2,
                 purrr::map2(.x = c("d", "j", "p", "v", "two", "eight"), 
                             .y = c("D", "J", "P", "V", "2", "8"), 
                             ~colourInput(.x, .y, value = randomColor(), 
                                          showColour = "background"))
                ),
          column(width = 2,
                 purrr::map2(.x = c("e", "k", "q", "w", "three", "nine"), 
                             .y = c("E", "K", "Q", "W", "3", "9"), 
                             ~colourInput(.x, .y, value = randomColor(), 
                                          showColour = "background"))
                 ),
          column(width = 2,
                 purrr::map2(.x = c("f", "l", "r", "x", "four", "zero"), 
                             .y = c("F", "L", "R", "X", "4", "0"), 
                             ~colourInput(.x, .y, value = randomColor(), 
                                          showColour = "background"))
                 ),
          fluidRow(
            column(width = 12,
                   actionButton("kaijaColors",
                                "Kaija's colors"),
                   actionButton("allWhite",
                                "Set all to white")
            )
          )
        )
      )
    ),
    fluidRow(
      # Text input ---------------------------------------------------------
      column(width = 4,
             textInput("displayText", 
                       "Text to display:", 
                       value = "Type something") # initial text in the box
      ),
      # Save inputs as csv ------------------------------------------------
      column(width = 3, offset = 1,
             br(),
             downloadButton("downloadColors",
                            label = "Download colors")
      ),
      
      # Bookmark button ----------------------------------------------------
      column(width = 3, offset = 1,
             br(), # just so the spacing aligns better with the text entry box
             bookmarkButton(label = "Save app state",
                            icon = shiny::icon("heart-empty", lib = "glyphicon"))
      )
    ),
    
    # Output object -------------------------------------------------------
    tabsetPanel(
      id = "viz",
      type = "tabs",
      # a ggplot object showing colored blocks
      tabPanel("Rectangles", 
               br(),
               div(prettySwitch(
                 inputId = "showLetters",
                 label = "Show letters?",
                 value = TRUE
               ),
               style = "margin-bottom: -10px"),
               plotOutput("colorBlocks"),
               downloadButton("downloadPlot",
                              "Download image")
               ),
      # colored text, rendered with javascript
      tabPanel("Text", 
               textOutput("coloredText")
               )
    )
  )
}

server <- function(input, output, session){
  # Save the user-entered color values --------------------------------------
  colorsList <- reactive({
    list("space" = "#FFFFFF",
         "a" = input$a,
         "b" = input$b,
         "c" = input$c,
         "d" = input$d,
         "e" = input$e,
         "f" = input$f,
         "g" = input$g,
         "h" = input$h,
         "i" = input$i,
         "j" = input$j,
         "k" = input$k,
         "l" = input$l,
         "m" = input$m,
         "n" = input$n,
         "o" = input$o,
         "p" = input$p,
         "q" = input$q,
         "r" = input$r,
         "s" = input$s,
         "t" = input$t,
         "u" = input$u,
         "v" = input$v,
         "w" = input$w,
         "x" = input$x,
         "y" = input$y,
         "z" = input$z,
         "one" = input$one,
         "two" = input$two,
         "three" = input$three,
         "four" = input$four,
         "five" = input$five,
         "six" = input$six,
         "seven" = input$seven,
         "eight" = input$eight,
         "nine" = input$nine,
         "zero" = input$zero)
  })
  
  # Save the user-entered color values --------------------------------------
  # Convert the reactiveValues object into a data frame so we can use it more easily.
  colorsDF <- eventReactive(colorsList(), {
    colorsList() %>%
      lapply(., as.data.frame) %>% # convert each element to a df so we will be able to use rbindlist
      rbindlist(idcol = "grapheme") %>% # store list names in a column called "grapheme"
      as.data.frame() %>% # this is in some weird format, so make it a df
      rename("hex" = "X[[i]]") %>% # change default name to "hex" bc duh.
      mutate(grapheme = forcats::fct_recode(grapheme, # have to change these to allow the join to work
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
  }) %>%
    debounce(25)
  
  # Convert input to lowercase, replace all non-alphanumeric characters with a blank space, and split the string into a vector
  split <- reactive( 
    unlist(strsplit(str_replace_all(tolower(input$displayText),
                                    "[^a-z0-9]", " ") %>% # replace all non-alphanumeric characters with a space
                      str_replace_all(., "\\s{2,}", " "), # replace runs of multiple spaces with a single space
                    split = ""))
  )
  
  
  # Create df input for the ggplot ------------------------------------------
  rectangleDF <- reactive({
    req(length(split()) > 0) # will only work when there is text entered in the box
    data.frame(grapheme = split(),
               ymin = 1,
               ymax = 5) %>%
      mutate(xmin = 1:nrow(.), xmax = 2:(nrow(.)+1)) %>%
      left_join(colorsDF(), by = c("grapheme")) %>%
      mutate(r = col2rgb(hex)[1,],
             g = col2rgb(hex)[2,],
             b = col2rgb(hex)[3,]) %>%
      mutate(contrastColor = case_when((r*0.299 + g*0.587 + b*0.114) > 140 ~ "#000000",
                                       TRUE ~ "#FFFFFF"))
  }
  )
  
  # observeEvent(rectangleDF(), {
  #   browser()
  # })
  
  # Plot color blocks -------------------------------------------------------
  plotVals <- reactiveValues() # initialize a reactiveValues object to store the plot object
  output$colorBlocks <- renderPlot({
    
    p <-  rectangleDF() %>%
      ggplot() +
      geom_rect(aes(xmin = xmin, xmax = xmax,
                    ymin = ymin, ymax = ymax, 
                    fill = hex))+ # fill w hex colors
      scale_fill_identity()+ # take the literal hex values as colors, instead of mapping other colors to them.
      theme_void() + # totally blank background
      {if(input$showLetters)geom_text(aes(x = xmin + 0.5, 
                                          y = ymin + 0.3, 
                                          label = grapheme,
                                          col = contrastColor),
                                      size = 7,
                                      family = "Baloo 2")}+
      scale_color_identity()
    plotVals$rectanglePlot <- p
    print(p)
  })
  
  # Save selected colors as a csv -------------------------------------------
  ## Create the data for export
  colorsForExport <- reactive({
    colorsDF() %>%
      filter(grapheme != " ") %>%
      rename("character" = grapheme) %>%
      mutate(red = col2rgb(hex)[1,],
             green = col2rgb(hex)[2,],
             blue = col2rgb(hex)[3,])
  })
  
  ## Download handler
  output$downloadColors <- downloadHandler(
    filename = function() {
      paste0("colors_", 
             str_replace(str_replace_all(as.character(Sys.time()), ":", "-"), 
                         " ", "_"), ".csv")
    },
    content = function(file) {
      write.csv(colorsForExport(), file, row.names = FALSE)
    }
  )
  
  # Download rectangles plot as image ---------------------------------------
  output$downloadPlot <- downloadHandler(
    filename = function(){
      paste0(str_replace_all(tolower(input$displayText),
                            "[^a-z0-9]", "_") %>% 
              str_replace_all(., "_{2,}", "_"), 
            '.png')
      },
    
    content = function(file){
      req(plotVals$rectanglePlot)
      ggsave(file, plot = plotVals$rectanglePlot, 
             device = 'png', width = length(split()), 
             height = 5)
    })
  
  # Plot colored text -------------------------------------------------------
  output$coloredText <- renderText({
    input$displayText
  })
  
  
  # Set Kaija colors --------------------------------------------------------
  observeEvent(input$kaijaColors, {
    for(i in 1:nrow(kaijaColors)){
      updateColourInput(session,
                        inputId = kaijaColors$character[i],
                        value = kaijaColors$hex[i])
    }
  })
  
  observeEvent(input$allWhite, {
    for(i in 1:nrow(kaijaColors)){
      updateColourInput(session,
                        inputId = kaijaColors$character[i], # XXX there's a better way to refer to this.
                        value = "#FFFFFF")
    }
  })
  
  
}
shinyApp(ui, server, enableBookmarking = "url")