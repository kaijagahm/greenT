library(shiny) # duh
library(shinyjs) # for the contribution form
library(colourpicker) # for color inputs
library(tidyverse)
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
source("about.R")
source("contribute.R")
library(googledrive)
library(googlesheets4)
options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE,
  gargle_verbosity = "debug"
)

# UI ----------------------------------------------------------------------
ui <- function(request){ # UI as a function to enable bookmarking
  fluidPage(
    shinyjs::useShinyjs(), # allow shinyjs
    # Change the font for the whole app -----------------------------------
    ## white is already the default background color; leaving this here in case want to change it later.
    tags$head(     
      tags$link(rel = "stylesheet", type = "text/css", href = "tea-style.css"),
      tags$script(src = "custom.js")
    ),
    
    # Title and subtitle, two different formats
    titlePanel(div(HTML("<b style = 'font-size: 30px;'>greenT</b> <em><small>exploring grapheme-color synesthesia</em></small>")),
               windowTitle = "greenT"), # windowTitle controls what shows up on the browser tab
    
    # Body
    # Color pickers -------------------------------------------------------
    ## Note that I've arranged these so that the alphabet goes left to right, then top to bottom, instead of going down the columns. I initially had it going down the columns and it looked weird.
    fluidRow(
      tabsetPanel(
        id = "mainBodyTabset",
        selected = "Explore",
        type = "pills",
        tabPanel(
          title = "Explore",
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
                                    "Kaija's colors",
                                    style = "margin-right:10px"),
                       actionButton("allWhite",
                                    "Set all to white",
                                    style = "margin-right:10px"),
                       downloadButton("downloadColors",
                                      label = "Download colors as .csv")
                )
              )
            )
          ),
          fluidRow(
            column(width = 12,
                   div(style = "display:inline-block;margin-left:10px",
                       textInput("displayText", 
                                 "Text to display:", 
                                 value = "Type something") # initial text in the box
                   ),
                   div(style = "display:inline-block;margin-left:10px",
                       bookmarkButton(label = "Save app state",
                                      icon = shiny::icon("heart-empty", lib = "glyphicon")
                       )
                   )    
            )
          ),
          # Output object -------------------------------------------------------
          # a ggplot object showing colored blocks
          fluidRow(
            column(width = 12,
                   br(),
                   div(style = "margin-left:10px;margin-bottom:-10px",
                       prettySwitch(
                         inputId = "showLetters",
                         label = "Show letters?",
                         value = TRUE
                       )
                   ),
                   plotOutput("colorBlocks"),
                   div(style = "margin-left:10px;margin-bottom:25px",
                       downloadButton("downloadPlot",
                                      "Download rectangles as .png")
                   )
            ),
          )
        ),
        tabPanel(
          title = "About",
          fluidRow(
            column(width = 10,
                   offset = 1,
                   whyThisApp # see about.R
            )
          ),
          fluidRow(
            column(width = 10,
                   offset = 1,
                   moreAboutSynesthesia # see about.R
            )
          ),
          fluidRow(
            column(width = 10,
                   offset = 1,
                   acknowledgments # see about.R
            )
          )
        ),
        tabPanel(
          title = "Contribute your colors",
          fluidRow(
            column(width = 10,
                   offset = 1,
                   howToContribute,
                   
                   # Contribution form --------------------------------------
                   div(
                     id = "contributeForm",
                     textInput("name", 
                               labelMandatory("Name (this will be used to keep track of responses for data analysis only, and will not be included in public datasets or published/posted analyses. You can use an alias if you prefer.)"), 
                               "", width = "100%"),
                     checkboxInput("yesSetColors",
                                   labelMandatory("I confirm that the colors set in the Explore tab are the ones I want to submit. (If not, please go back and set your colors before filling out this form!)"),
                                   value = FALSE,
                                   width = "100%"),
                     textInput("email", 
                               "Email address, if you don't mind being contacted with questions about your experience of synesthesia (optional!)", 
                               "", width = "100%"),
                     numericInput("birthYear", "Birth year", 
                                  min = 1900, max = year(Sys.Date()),
                                  value = "", step = 1),
                     radioButtons("handedness", 
                                  labelMandatory("Are you right-handed or left-handed?"),
                                  choices = c("left", "right", 
                                              "ambidextrous", "prefer not to say"), 
                                  selected = character(0), width = "100%"),
                     radioButtons("gender", labelMandatory("What is your gender identity?"),
                                  choices = c("woman", "non-binary", 
                                              "man", "prefer not to say", 
                                              "prefer to self-describe (click to add text)"), 
                                  selected = character(0), width = "100%"),
                     uiOutput("genderSelfDescribe"),
                     radioButtons("sex", labelMandatory("Sex assigned at birth:"),
                                  choices = c("female", "male", "intersex", 
                                              "prefer not to say"), 
                                  selected = character(0), width = "100%"),
                     radioButtons("strong", "How strong are your color associations?",
                                  choices = c("very strong", "moderately strong", 
                                              "neither strong nor weak", "moderately weak", 
                                              "very weak"), 
                                  selected = character(0), width = "100%"),
                     radioButtons("consistent", "How consistent are your color associations?",
                                  choices = c("very consistent", "mostly consistent", 
                                              "pretty variable", "extremely variable"), 
                                  selected = character(0), width = "100%"),
                     radioButtons("synesthesia", labelMandatory("Do you consider yourself to have grapheme-color synesthesia?"),
                                  choices = c("yes", "no", "not sure"), 
                                  selected = character(0), width = "100%"),
                     radioButtons("howLong", "How long have you had color-grapheme associations?", 
                                  choices = c("as long as I can remember", "a long time, but I can remember not having them", "they developed more recently", "I don't have consistent color-grapheme associations"), 
                                  selected = character(0), width = "100%"),
                     radioButtons("family", "Do any of your family members also have grapheme-color associations?",
                                  choices = c("yes, more than one family member", "yes, one family member", "not that I know of"),
                                  selected = character(0), width = "100%"),
                     checkboxGroupInput("synesthesiaTypes", "Do you experience other types of synesthesia beyond grapheme-color? Select all that apply.", choices = c("ordinal linguistic personification (sequences such as numbers, letters, days, months, etc. have genders/personalities)", "chromesthesia (sounds have associated colors)", "spatial sequence synesthesia (sequences such as numbers, letters, days, months, etc. have particular arrangements in space)", "mirror-touch synesthesia (seeing someone else feel a physical sensation and feeling the same sensation yourself)", "auditory-tactile synesthesia (hearing sounds causes physical sensations)", "number form synesthesia (groups of numbers have a mental map/spatial arrangement)", "lexical-gustatory synesthesia (words have associated tastes, smells, textures, etc.)", "sound-gustatory synesthesia (sounds have associated tastes, smells, textures, etc.)", "no other types of synesthesia", "other (click to add text)"),
                                        selected = character(0), width = "100%"),
                     uiOutput("otherSynesthesia"),
                     textInput("comments", "Any comments?", "", width = "100%"),
                     strong("Consent and submit"),
                     checkboxInput("consent", labelMandatory("I agree to submit the information I have provided, and the colors I selected, to Kaija Gahm (kaija.gahm@gmail.com). I understand that my data may be used in blog posts, informal analyses, etc., but my name and email will not be attached. If I provided an email address, I understand that I may be contacted about my responses, but my email address will not be made public."), width = "100%"),
                     actionButton("submit", "Submit", class = "btn-primary")
                   ),
                   shinyjs::hidden(
                     div(id = "error",
                         div(br(),
                             tags$b("Error: "),
                             span(id = "errorMessage"))
                     )
                   ),
                   shinyjs::hidden(
                     div(
                       id = "thankYou",
                       h3("Thank you for contributing your colors!"),
                       actionLink("submitAnother", "Submit another response")
                     )
                   ),
                   shinyjs::hidden(
                     div(
                       id = "submitMessage",
                       h4("Submitting...")
                     )
                   ),
                   br(),
                   br()
            )
          )
        )
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
                                      size = 10,
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
      paste0("colors_", dateTimeFormat(), ".csv")
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
             "_",
             dateTimeFormat(),
             '.png')
    },
    
    content = function(file){
      req(plotVals$rectanglePlot)
      ggsave(file, plot = plotVals$rectanglePlot, 
             device = 'png', width = length(split())/2, 
             height = 5/2)
    })
  
  # Set Kaija colors --------------------------------------------------------
  # In case people want to see the colors as I see them
  observeEvent(input$kaijaColors, {
    purrr::map2(.x = kaijaColors$character,
                .y = kaijaColors$hex,
                ~colourpicker::updateColourInput(session, inputId = .x, value = .y))
  })
  
  
  # Set all to white --------------------------------------------------------
  # Useful if people find it easier to input their colors when all the selectors start white, instead of replacing existing colors. 
  observeEvent(input$allWhite, {
    lapply(inputIds, function(x){
      colourpicker::updateColourInput(session,
                                      inputId = x,
                                      value = "#FFFFFF")
    })
  })
  
  # Exclude the two buttons from bookmarking so that the observeEvent's won't fire. Could also do this with ignoreInit = T in each observeEvent, but we decided this was cleaner.
  setBookmarkExclude(c("kaijaColors", "allWhite"))
  
  
  # Contribute form -------------------------------------------------------
  # Dynamic options
  ## self-describe gender
  output$genderSelfDescribe <- renderUI({
    req(input$gender)
    if(!input$gender == "prefer to self-describe (click to add text)"){
      return(NULL)
    }else{
      textInput("genderSelfDescribe", 
                label = NULL, "")
    }
  })
  
  ## describe other types of synesthesia
  output$otherSynesthesia <- renderUI({
    req(input$synesthesiaTypes)
    if(!"other (click to add text)" %in% input$synesthesiaTypes){
      return(NULL)
    }else{
      textInput("otherSynesthesia", 
                label = NULL, "")
    }
  })
  
  # Enforce mandatory fields
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(c(mandatoryFilled, input$consent))
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  # Compile all the answers to the contribution questions into a dataset
  contributionData <- reactive({
    # Process the form data
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = as.character(lubridate::ymd_hms(Sys.time()))) %>%
      lapply(., as.data.frame)
    data <- data.table::rbindlist(data, idcol = "field") %>%
      rename("value" = "X[[i]]") %>%
      # Add the color data
      bind_rows(colorsForExport() %>% 
                  mutate(across(c("hex", "red", "green", "blue"), 
                                as.character)) %>% 
                  pivot_longer(cols = c("hex", "red", "green", "blue"), 
                               names_to = "type", # the form data doesn't include a "type" column, but it will just get filled in with NA
                               values_to = "value") %>% 
                  rename("field" = "character"))
  })
  
  # When the submit button is pressed, reset the form and show the thank you message
  observeEvent(input$submit, {
    delay(1000, shinyjs::reset("contributeForm"))
    delay(1000, shinyjs::hide("contributeForm"))
    delay(1000, shinyjs::show("thankYou"))
    shinyjs::show("submitMessage")
  })
  
  # When the reset link is clicked, re-show the form:
  observeEvent(input$submitAnother, {
    shinyjs::show("contributeForm")
    shinyjs::hide("thankYou")
  }) 
  
  # Show text while submitting, and show error message if submit fails
  observeEvent(input$submit, {
    shinyjs::disable("submit")
    shinyjs::hide("error")
    
    tryCatch({
      saveData(contributionData())
      shinyjs::reset("contributeForm")
      shinyjs::hide("contributeForm")
      shinyjs::show("thankYou")
    },
    error = function(err) {
      shinyjs::html("errorMessage", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submitMessage")
    })
  })
  
}
shinyApp(ui, server, enableBookmarking = "url")