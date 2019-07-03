library(shiny)

# This will create a blank UI

# Define UI ----
ui <- fluidPage(
  
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)


ui <- fluidPage(
  titlePanel("title panel"),
  
  sidebarLayout(position = "right",
                sidebarPanel("sidebar panel"),
                mainPanel("main panel")
  )
)
shinyApp(ui = ui, server = server)

## Adding content to the UI

ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      h1("Main header"),
      h2("Second level header"),
      h3("Third level header"),
      h4("Fourth level header"),
      h5("Fifth level header"),
      h6("Sixth level header")
    )
  )
)

shinyApp(ui = ui, server = server)



