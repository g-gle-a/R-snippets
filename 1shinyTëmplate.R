
## Basic Shiny template

library(shiny)

ui <- fluidPage( in.. ,  out.. )

server <- function (input, output)


shinyApp (ui = ui, server = server)


## Example
ui <- fluidPage(
     sliderInput  ( IputId = "num" , label = "Choose  a number", value=25, min =1, max=100) , ## <- comma separating in/out
      plotOutput ("hist")
 )


server <- function (input , output){
        output$hist .- renderPlot ( {       hist( rnorm( input$num ) ) 
	 } ) 
}

shinyApp (ui = ui, server = server)