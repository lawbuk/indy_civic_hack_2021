#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny) # For shiny app
library(DT)
library(readxl)
if(!require(tidyverse)) {install.packages("tidyverse"); require(tidyverse)}
if(!require(ggmap)) {install.packages("ggmap"); require(ggmap)}
if(!require(RCurl)) {install.packages("RCurl"); require(RCurl)}
if(!require(leaflet)) {install.packages("leaflet"); require(leaflet)} # For interactive map

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)

# Choices for drop-downs
vars <- c(
    "Median income" = "Median_Household_Income_Est",
    "Population" = "Pop_Est",
    "SNAP Benefactor" = "SNAP_Benefactors_Est"
)

Indy_Hack_21 <- read.csv("https://raw.githubusercontent.com/lawbuk/indy_civic_hack_2021/main/Indy_Hackathon_2021.csv", sep = ",", na.strings = "NA",strip.white = TRUE, stringsAsFactors=FALSE)
SAIPE <- read.csv("https://raw.githubusercontent.com/lawbuk/indy_civic_hack_2021/main/Small_Area_Income_and_Poverty_Estimate.csv", sep = ",", na.strings = "NA",strip.white = TRUE, stringsAsFactors=FALSE)%>%
    mutate_if(is.factor, ~ as.numeric(levels(.x))[.x])

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Service Location"),

    # Sidebar with a select input for county and service 
    sidebarLayout(
        sidebarPanel(
            
            helpText("Create demographic map with 
        Indiana's Cummunity Service Provider"),
            
            selectInput(inputId = "county",
                        label = "Select County:",
                        selected = "Marion",
                        choices = c("All",sort(unique(Indy_Hack_21$County))),
                        multiple = TRUE
                        ),
            selectInput(inputId = "service",
                        label = "Select Service:",
                        selected = sort(unique(Indy_Hack_21$Service)),
                        choices = c(sort(unique(Indy_Hack_21$Service))),
                        multiple = TRUE
            ),
            plotOutput(outputId = "scatterPovertySNAP"),
            tableOutput(outputId = "Service_Table")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput(outputId = "ServicePlot", height = 900)
           
        )
    ),
    
    #DT::dataTableOutput("ServiceTable")

    # conditionalPanel("false", icon("crosshair"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$ServicePlot <- renderPlot({

        # Draw a map for a specific location
        
        if (input$county == "All"){
            
            df <- filter(Indy_Hack_21, Service %in% input$service)
            qmplot(x=Longitude, y=Latitude, data = Indy_Hack_21, colour = Service, size = I(5), alpha = I(.5),label = "Provider")+
                    theme(legend.position = "bottom")
            
        } else {
            df <- filter(Indy_Hack_21, County %in% input$county, Service %in% input$service)
                qmplot(x=Longitude, y=Latitude, data = df, colour = Service, size = I(9),label = "Provider")+
            theme(legend.position = "bottom")

        }
    })
    
    output$ServiceTable <- DT::renderDataTable({
        df <- subset(SAIPE,select = County:SNAP_Benefactors_Est)
        
        if (input$county =="All"){
            DT::datatable(df)
        } else {
            DT::datatable(df%>%filter(County %in% input$county))
        }

        })
    output$Service_Table <-  renderTable({
        Indy_Hack_21 %>%
            group_by(County, Service) %>% 
            summarise(n = n()) %>% 
            arrange(desc(n))%>%
            filter(n>1)%>%
            pivot_wider(names_from = Service, values_from = n, values_fill = 0)%>%
            setNames(c("County","Gambling","SNAP","TANF","MR","OTP","VR"))%>%
            na.omit()%>%
            as_tibble()
    })
    output$scatterPovertySNAP <- renderPlot({
        if (input$county == "All"){
            SAIPE%>%
                ggplot(aes(x=Median_Household_Income_Est, y=Poverty_Pop_Est, size = SNAP_Benefactors_Est, color = County)) +
            geom_point( alpha =0.7) +
            scale_size(range = c(1.4, 19)) +
            xlab('Median Household\n Income ($)')+
            ylab('Poverty Population')+
            ggtitle("Poverty estimate by median \nhousehold income (All Counties)")+
            theme(legend.position="none") 

        } else {
            df <- SAIPE%>%
                filter(County %in% input$county)
            
            ggplot(df, aes(x=Median_Household_Income_Est, y=Poverty_Pop_Est, size = SNAP_Benefactors_Est, color = County)) +
                geom_point( alpha =0.7) +
                scale_size(range = c(1.4, 19), name="Snap") +
                xlab('Median Household\n Income ($)')+
                ylab('Poverty Population')+
                ggtitle("Poverty estimate by\n median household income")+
                theme(legend.position="none")
            
        }

    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
