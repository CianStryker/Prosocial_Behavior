
library(shiny)
library(bslib)
library(tidyverse)

# Define UI for the application. 

ui <- fluidPage(theme = bs_theme(bootswatch = "sandstone"),

            # All standard shiny code below. 
            
            navbarPage(
                title = 'Replication of "Ethnic Riots and Prosocial Behavior: Evidence from Kyrgyzstan"',
                
                tabPanel(
                  
                  title = "Introduction",
                         
                         fluidRow(
                           
                           column(width = 2),
                           
                           column(width = 8,
                                  
                                  imageOutput(outputId = "Picture", width = "100%", height = "100%")),
                           
                           column(width = 2)
                           
                         ),
                           
                        fluidRow(
                          
                          column(width = 3), 
                          
                          column(width = 6, 
                      
                                  
                                  tags$h1("Replication Paper"), 
                                  
                                  tags$p("This replication and extension is my final project for GOV 1006 'Models' at Harvard University. If you are on a desktop or laptop then you can view the paper under the 'Paper' tab at the top of this page. Note, however, that this shinyapp is not mobile friendly, so if you are using a mobile device then please click on the Mobile Reader tab. If you would like to download the pdf, please click here:", downloadLink("downloadPDF", "Download Link")),
                                  
                                  tags$h1("Original Paper"), 
                                  
                                  tags$p('The original paper for this replication project is "Ethnic Riots and Prosocial Behavior: Evidence from Kyrgyzstan" written by Anselm Hager, Krzysztof Krakowski, and Max Schaub. The map of Kyrgyzstan featured above is also taken from this paper.'), 
                                  
                                  tags$p("The original data can be found", a(href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/WVBZNE", "here.")),
                                  
                                  tags$h1("About me"), 
                                  
                                  tags$p("My name is Cian Stryker and I am a graduate student at Harvard University. I am pursuing a Master’s in Russian, Eastern European, and Central Asian studies with a focus on the Digital Silk Road, or the technological aspects of the Belt and Road Initiative. I am especially interested in the digital expansion of Chinese technology and data management systems in Central Asia. "), 
                                  
                                  tags$h1("Source Code"), 
                                  
                                  tags$p("Here is the link to my Github page for this project:", a(href = "https://github.com/CianStryker/Replication_Data", "https://github.com/CianStryker/Replication_Data"))
                                  
                                  
                           ),
                           
                           column(width= 3)
                         )
                         
                ),
                
                tabPanel(title = HTML("<li>Paper"),
                         
                         
                         uiOutput("myPaper")
                         ),
                
                tabPanel(title = HTML("<li><a href='Stryker_Prosocial_Behavior.pdf'target='_self'>Mobile Reader"))
                

                        
               
)
           
        
)    

# Default code
server <- function(input, output) {
  
  output$Picture <- renderImage({
    
    filename <- normalizePath(file.path("./Images",
                                         paste("Photo_1.png")))
    
    list(src = filename,
         contentType = "image/png", 
         width = "100%",
         height = "100%"
    )
    
  }, deleteFile = FALSE)
  
  
  
  output$downloadPDF <- downloadHandler(
    filename = "Stryker_Prosocial_Behavior.pdf",
    content = function(file) {
      file.copy("www/Stryker_Prosocial_Behavior.pdf", file)
    }
  )
  
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
