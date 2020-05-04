
library(shiny)
library(shinythemes)

# Define UI for the application. 

ui <- fluidPage(theme = shinytheme("slate"),

            # All standard shiny code below. 
            
            navbarPage(
                
                title = 'Replication of "Ethnic Riots and Prosocial Behavior: Evidence from Kyrgyzstan"',
                
                tabPanel("Paper",
                         uiOutput("myPaper")
                         ),
                
                tabPanel(
                    
                    title = "About", 
                    
                    fluidRow(
                        
                        column(width = 3),
                        
                        column(width = 6,
                               

                               tags$h1("Original Paper"), 
                               
                               tags$p('The original paper for this replication project is "Ethnic Riots and Prosocial Behavior: Evidence from Kyrgyzstan" written by Anselm Hager, Krzysztof Krakowski, and Max Schaub.'), 
                              
                               tags$p("The original data can be found here :", a(href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/WVBZNE", "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/WVBZNE")),
                               
                               tags$h1("About me"), 
                               
                               tags$p("My name is Cian Stryker and I am a graduate student at Harvard University. I am pursuing a Master’s in Russian, Eastern European, and Central Asian studies with a focus on the Digital Silk Road, or the technological aspects of the Belt and Road Initiative. I am especially interested in the digital expansion of Chinese technology and data management systems in Central Asia. This replication was the final project for my ‘Models’ class at Harvard (GOV 1006). "), 
                               
                               tags$h1("Source Code"), 
                               
                               tags$p("Here is the link to my Github page for this project:", a(href = "https://github.com/CianStryker/Replication_Data", "https://github.com/CianStryker/Replication_Data"))
                               

                        ),
                        
                        column(width= 3)
                    )
                    
                )
                        
               
)
           
        
)    

# Default code
server <- function(input, output) {
  
  output$myPaper <- renderUI({
    PDFfile="Stryker_Prosocial_Behavior.pdf"
    tags$iframe(
      src="Stryker_Prosocial_Behavior.pdf",
      width="100%",
      height="1000px")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
