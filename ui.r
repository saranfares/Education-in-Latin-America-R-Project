shinyUI(fluidPage( 
  titlePanel("Education in Latin America"),
  sidebarLayout( # making the sidebar panel
    sidebarPanel(sliderInput("current_year",label="Select Year",
                             min=2011,max=2016,value=2016, sep="", 
                             animate=animationOptions(loop=F)),
                selectInput("gender", h3("Which gender grouping? (default is combined both genders) "), 
                            choices = list("female" = 1, "male" = 2,
                                            "combined" = 3), selected = 3),
                selectInput("country", h3("Which country? (default is all) "), 
                            choices = list("All" = "All", "Colombia" = "Colombia",
                             "Bolivia" = "Bolivia", "Venezuela"="Venezuela", "Peru"="Peru", "Chile"="Chile"), selected = "All"),
                selectInput("education", h3("Which level of education would you like to look at? (default is primary)"), 
                            choices = list("primary" = 1, "secondary" = 2), selected = 1),
                selectInput("index", h3("Which measurement index would you like to look at?"),
                            choices = list("Completion rate" = 1, "Enrollment rate" = 2), selected = 1),
                selectInput("info_extra", h3("Which additional information would you like to compare? (default is none)"), 
                            choices = list("Literacy rate" = 1, "GDP per Capita" = 2, "none" = 3), selected = 3)
    ),
    mainPanel(
      plotOutput("outputPlot")
    ))
))