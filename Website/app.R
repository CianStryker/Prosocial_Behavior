
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("slate"),

            navbarPage(
                
                title = 'Replication of "Ethnic Riots and Prosocial Behavior: Evidence from Kyrgyzstan"',
                
                tabPanel("Paper",
                         tags$iframe(style="height:1000px; width:100%; scrolling=yes",
                                     src = "Paper.pdf")),
                
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
                               
                               tags$p("Here is the link to my Github page for this project:", a(href = "https://github.com/CianStryker/Replication_Data", "https://github.com/CianStryker/Replication_Data")),
                               
                               tags$h1("Note"), 
                               
                               tags$p('Sometimes the pdf will fail to load if you spend too much time on the "About" tab , but just refresh the overall page and it will reload successfully.') 
                               
                               
                        ),
                        
                        column(width= 3)
                    )
                    
                )
                        
               
)
           
        
)    


# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
