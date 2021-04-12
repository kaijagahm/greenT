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
source(here("defs.R"))
library(showtext) # for fonts
library(sysfonts)
library(shinyWidgets)
font_add_google("Baloo 2")
showtext_auto()

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
          title = HTML("<em><small>Show/hide selectors</em></small>"), 
          value = "setColors", # allows us to control open/close programmatically, if we want
          column(width = 2,
                 # See defs.R for the horiz() function--basically it converts a vector to a matrix and back again, to allow us to arrange the alphabet by rows instead of columns
                 purrr::map2(.x = horiz(inputIds)[1:6], 
                             .y = horiz(displayNames)[1:6], 
                             colorInit)
          ), 
          column(width = 2,
                 purrr::map2(.x = horiz(inputIds)[7:12], 
                             .y = horiz(displayNames)[7:12], 
                             colorInit)
          ),
          column(width = 2,
                 purrr::map2(.x = horiz(inputIds)[13:18], 
                             .y = horiz(displayNames)[13:18], 
                             colorInit)
          ),
          column(width = 2,
                 purrr::map2(.x = horiz(inputIds)[19:24], 
                             .y = horiz(displayNames)[19:24], 
                             colorInit)
          ),
          column(width = 2,
                 purrr::map2(.x = horiz(inputIds)[25:30], 
                             .y = horiz(displayNames)[25:30], 
                             colorInit)
          ),
          column(width = 2,
                 purrr::map2(.x = horiz(inputIds)[31:36], 
                             .y = horiz(displayNames)[31:36], 
                             colorInit)
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
  colorsDF <- reactive({
    lapply(inputIds, function(x) {
      data.frame("grapheme" = x, 
                 "hex" = input[[x]])
    }) %>%
      data.table::rbindlist() %>%
      as.data.frame() %>%
      add_row(grapheme = "space", hex = "#FFFFFF") %>%
      mutate(grapheme = fct_recode(grapheme,
                                   !!! setNames(inputIds, charactersOut)),
             grapheme = fct_recode(grapheme,
                                   " " = "space"))
  }) %>%
    debounce(25)
  
  # observeEvent(colorsDF(),{
  #   browser()
  # })
  
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
      left_join(colorsDF(), by = "grapheme") %>%
      mutate(r = col2rgb(hex)[1,],
             g = col2rgb(hex)[2,],
             b = col2rgb(hex)[3,]) %>%
      mutate(contrastColor = case_when((r*0.299 + g*0.587 + b*0.114) > 140 ~ "#000000",
                                       TRUE ~ "#FFFFFF"))
  })
  
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
    purrr::map2(.x = kaijaColors$character,
                .y = kaijaColors$hex,
                ~updateColourInput(session, inputId = .x, value = .y))
  })
  
  observeEvent(input$allWhite, {
    lapply(inputIds, function(x){
      updateColourInput(session,
                        inputId = x,
                        value = "#FFFFFF")
    })
  })
}
shinyApp(ui, server, enableBookmarking = "url")