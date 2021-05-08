# Text for the About section

whyThisApp <- tagList(
  h2("Why this app?"),
  p("Ever since I can remember, I've had strong mental associations between letters/numbers and colors. I don't literally 'see' colors floating in the air whenever I read words, but I have a strong feeling that, for instance, the letter K is innately magenta, T is bright green, and 6 is bright, sunshiney yellow."),
  p("These associations are a type of synesthesia known as 'grapheme-color'. Synesthesia is a condition where people experience certain senses as linked--hence the term, which comes from the roots 'syn' (together) and 'aesthesis' (sensation)."),
  p("There are lots of other types of synesthesia. Some people associate colors or textures with sounds; others experience links between sound and physical touch. In addition to my grapheme-color synesthesia, I also associate genders and personalities with letters/numbers ('ordinal linguistic personification'), and I visualize number sequences or timelines as three-dimensional arrangements in space ('spatial sequence synesthesia'). But pairing colors with letters and numbers is the type of synesthesia that seems to most pique people's interest."),
  p("I created this app as a way of showing my friends what words, 'look like' in my mind's eye. I was heavily inspired by Bernadette Peters's ", 
    a(href = 'https://synesthesia.me/', "synesthesia.me", 
      .noWS = "outside"), " project. But I wanted to make an interactive app where other people with grapheme-color synesthesia could set their own colors, rendering text as it appears to them, not just to me. At the same time, I've left my own colors as an option, for anyone who wants to see the world through my eyes.", 
    .noWS = c("after-begin", "before-end")) 
)

moreAboutSynesthesia <- tagList(
  h2("More about synesthesia"),
  p("Here are some resources if you're interested in reading more about synesthesia."),
  tags$ul(
    tags$li("Bernadette Peters's website, ", a(href = "https://synesthesia.me/about", "synesthesia.me"), ", which inspired greenT"), 
    tags$li("A ", a(href = "https://www.nytimes.com/2011/11/22/science/mapping-grapheme-color-synesthesia-in-the-brain.html#:~:text=Grapheme%2Dcolor%20synesthesia%20is%20a,blue%20with%20the%20letter%20A.", "2011 New York Times article"), " about grapheme-color synesthesia"), 
    tags$li("A ", a(href = "https://www.frontiersin.org/articles/10.3389/fnhum.2013.00603/full", "scientific paper"), " about the development of grapheme-color synesthesia in children"),
    tags$li("An ", a(href = "https://pubmed.ncbi.nlm.nih.gov/23307940/", " article"), " about so-called 'Fisher-Price synesthesia', a type of learned synesthesia based on a popular children's toy made by Fisher-Price. In case you're curious--my letters match about 5 out of 26 of the Fisher-Price letters, and 1 or 2 of the numbers."),
    tags$li(a(href = "https://www.bu.edu/synesthesia/faq/#q7", "The Synesthesia Project"), " at Boston University (no longer happening, but the site has some good info)"),
    tags$li(a(href = "https://www.sussex.ac.uk/synaesthesia/", "Research on Synesthesia"), " at the University of Sussex. Includes a form where you can fill in your information to potentially be included in future research studies.")
  )
)

acknowledgments <- tagList(
  h2("Acknowledgments"),
  p("I want to acknowledge the following people, who contributed to the development of this app:"),
  tags$ul(
    tags$li("Jonathan Trattner, who helped with the css and discussed ideas with me. He's a shiny developer himself and has written some useful packages. You can find more of his work on", a(href = "https://github.com/jdtrat", " his Github page"), "and on", a(href = "https://www.jdtrat.com/", " his personal website")),
    tags$li("Dean Attali, whose ideas and code made this app possible. His", a(href = "https://deanattali.com/blog/colourpicker-package/", " colourpicker package"), " is obviously integral to this app's interface, and the contribution form is adapted from his wonderful tutorial on", a(href = "https://deanattali.com/2015/06/14/mimicking-google-form-shiny/", " mimicking a Google form with a Shiny app"), ". See more of Dean's work on", a(href = "https://deanattali.com/", " his website"), "."),
    tags$li("Bernadette Peters, whose original", a(href = "https://synesthesia.me/", "synesthesia.me"), " project inspired me to create this app in the first place.")
  )
)
