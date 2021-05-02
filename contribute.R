# Content for the "Contribute Your Colors" tab

howToContribute <- tagList(
  h2("Send me your colors"),
  p("Though I'm not officially conducting research on synesthesia, I'm interested in exploring other people's colors as well as my own. I'd love to run some informal analyses to look at color frequencies among synesthetes."),
  p("If you have grapheme-color synesthesia, you can contribute your colors to my analyses, to be used (without your identifying information attached) in some informal blog posts/writeups. This is completely optional!"),
  p("If you would like to submit your colors, do the following:"),
  tags$ol(
    tags$li("In the 'Explore' tab, set your colors. If it's helpful to start from white selectors, click the 'Set all to white' button before setting a value for each selector."),
    tags$li("Fill out the form below:")
  )
)

# Make the input for the shiny survey -------------------------------------
questions <- data.frame(question = c("Name (this will be used to keep track of responses for data analysis only, and will not be included in public datasets or published/posted analyses. You can use an alias if you prefer.)",
                                     "Email address, if you don't mind being contacted with questions about your experience of synesthesia (optional!)",
                                     "Birth year",
                                     rep("Are you right-handed or left-handed?", 4),
                                     rep("What is your gender identity?", 5),
                                     rep("Sex assigned at birth", 4),
                                     rep("How strong are your color associations", 5),
                                     rep("How consistent are your color associations", 4), 
                                     rep("Do you consider yourself to have grapheme-color synesthesia?", 3),
                                     rep("How long have you had color-grapheme associations?", 4),
                                     rep("Do any of your family members also have grapheme-color associations?", 3),
                                     rep("Do you experience other types of synesthesia beyond grapheme-color? Select all that apply.", 10),
                                     "Any comments?",
                                     "I agree to submit the information I have provided, and the colors I selected, to Kaija Gahm (kaija.gahm@gmail.com). I understand that my data may be used in blog posts, informal analyses, etc., but my name and email will not be attached. If I provided an email address, I understand that I may be contacted about my responses, but my email address will not be made public."),
                        option = c(NA, NA, NA, "left", "right", 
                                   "ambidextrous", "prefer not to say", "woman", 
                                   "non-binary", "man", "prefer not to say", 
                                   "prefer to self-describe", "female", "male", 
                                   "intersex", "prefer not to say", "very strong", 
                                   "moderately strong", "neither strong nor weak", 
                                   "moderately weak", "very weak", 
                                   "very consistent", "mostly consistent", 
                                   "pretty variable", "extremely variable",
                                   "yes", "no", "not sure", 
                                   "as long as I can remember", 
                                   "a long time, but I can remember not having them", 
                                   "they developed more recently", 
                                   "I don't have consistent color-grapheme associations",
                                   "yes, more than one family member", 
                                   "yes, one family member", 
                                   "not that I know of", "ordinal linguistic personification (sequences such as numbers, letters, days, months, etc. have genders/personalities)", "chromesthesia (sounds have associated colors)", "spatial sequence synesthesia (sequences such as numbers, letters, days, months, etc. have particular arrangements in space)", "mirror-touch synesthesia (seeing someone else feel a physical sensation and feeling the same sensation yourself)", "auditory-tactile synesthesia (hearing sounds causes physical sensations)", "number form synesthesia (groups of numbers have a mental map/spatial arrangement)", "lexical-gustatory synesthesia (words have associated tastes, smells, textures, etc.)", "sound-gustatory synesthesia (sounds have associated tastes, smells, textures, etc.)", "no other types of synesthesia", "other", NA,
                                   ""),
                        input_type = c("text", "text", "numeric", rep("mc", 4), 
                                       rep("mc", 5), rep("mc", 4), rep("mc", 5), 
                                       rep("mc", 4), rep("mc", 3), rep("mc", 4),
                                       rep("mc", 3), rep("mc", 10), "text", "mc"),
                        input_id = c("name", "email", "birthYear", 
                                     rep("handedness", 4), rep("gender", 5), 
                                     rep("sex", 4), rep("strong", 5), 
                                     rep("consistent", 4), rep("synesthesia", 3), 
                                     rep("howLong", 4), rep("family", 3), 
                                     rep("otherSynesthesia", 10), "comments", "consent"),
                        dependence = NA,
                        dependence_value = NA,
                        required = c(TRUE, FALSE, FALSE, rep(TRUE, 4), 
                                     rep(TRUE, 5), rep(TRUE, 4), rep(FALSE, 5),
                                     rep(FALSE, 4), rep(TRUE, 3), rep(FALSE, 4),
                                     rep(FALSE, 3), rep(FALSE, 10), FALSE, TRUE)
)


