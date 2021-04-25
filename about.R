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

contributors <- tagList(
  h2("Contributors"),
  h4(strong("Kaija")),
  p("I'm a biologist, soon-to-be PhD student in behavioral ecology, and R programmer by day. Though I've been working in R for years, I started learning Shiny just last summer, and I'm using this app as a way to build skills. You can see more of my work on ", a(href = 'https://github.com/kaijagahm', "GitHub", .noWS = "outside"), " and I write about R and my research projects on ", a(href = 'https://kaijagahm.netlify.app', "my website", .noWS = "outside"), ". When I'm not coding or telling my friends about synesthesia, you can find me hiking, knitting a sweater, or cooking something yummy.",
    #style = "font-size:500px;" # commenting this out for now--not sure what to do about sizes yet.
  ),
  h4(strong("Jonathan")),
  p("I developed this app in partnership with Jonathan Trattner. Jonathan doesn't have synesthesia, but he's a talented programmer and Shiny developer. He's written several Shiny-related packages of his own, and he has deeper experience with Shiny, Javascript, CSS, and HTML than I do. Fittingly, he's also a neuroscience major. We met in the wonderful ", 
    a(href = 'https://www.rfordatasci.com/', "R for Data Science", 
      .noWS = "outside"), " Slack channel, when he helped me out with another app I was working on. You can see more of Jonathan's work at his ", 
    a(href = 'https://www.jdtrat.com/', "website",
      .noWS = "outside"), ".",
    .noWS = c("after-begin", "before-end"))
)
