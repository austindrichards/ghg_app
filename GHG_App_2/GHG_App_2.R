library(shiny)
library(tidyverse)
library(readr)
library(ggthemes)
library(readr)





Emissions <- read_csv("CAIT_Country_GHG_Emissions")
maunaloa <- read_csv("Co2_maunaloa")

global_emissions <- filter(Emissions, Country == "World")



maunaloa <- select(maunaloa, "Year", "Conc") %>% 
  filter(Year != "na") %>% 
  filter(Conc != "na")
maunaloa <- mutate(maunaloa, t=(Year-1959))
time_seq <- seq(0,141,1)
bau <- {((0.023)*(time_seq^2))+(0.518*time_seq)+(310.44)}


A <- 500
B <- 2.2
r <- .039

red <- A/(1 + B*exp(-r*time_seq))


bau_df <- data.frame (bau, time_seq)
red_df <- data.frame (red, time_seq)
red_df2 <- filter(red_df, time_seq > 57)


ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country","Country:",
                  choices = Emissions$Country),
      radioButtons("data", "Include:",
                   choices = c ("Total", "`Total Including Landuse`",
                                "`CO2 Only`")
      )
      
    ),
    mainPanel(
      h2("Greenhouse Gas Emissions by Country/Region 1990-2014"),
      plotOutput("emitPlot"),
      h5("Data from World Resource Institute. 'Total including landuse' factors in associated increased greenhouse gas emissions from anthropogenic land alterations. 'Total' includes all greenhouse gas emissions, by CO2 equivellent warming potential"),
      h2("Atmospheric CO2 Concentration", align= 'left'),
      radioButtons("regress", "Future Model:",
                   choices = c ("Business as Usual", "Strict Adherence to Paris Climate Agreements")),
      plotOutput("emitFuture"),
      h5("Data from NOAA. Bars represent direct measurements at Mauna Loa since 1959.  line for 'business as usual' extrapolates the current trend since 1959 into the foreseeable future. Strict Paris agreement trend line assumes leveling off of CO2 emissions so as not to exceed 2 degrees Celsius of warming (climate action plan target). This threshold occurs at an atmospheric concentration of around 500 ppm. ")
    )
  ) )
server <- function(input, output) {
  
  output$emitPlot <- renderPlot({
    ggplot(subset(Emissions, Country == input$country), aes_string(x ='Year', y =  input$data ))+
      geom_col(aes_string(fill = input$data ))+
      xlab("Year")+
      ylab("GHG Emissions (Metric tons, CO2 Equivellent)")+
      theme_classic()+
      theme(legend.position = 'none')
  })
  
  output$emitFuture <- renderPlot({
    
    if (input$regress == "Business as Usual"){
      
      ggplot(maunaloa, aes(x=t, y=Conc))+
        geom_col(aes(fill= Conc))+
        xlab("Years Since 1959")+
        xlim(0, 141)+
        ylim(0, 800)+
        ylab("Atmposheric Concentration of CO2")+
        geom_line(data = bau_df, aes(x=(time_seq), y=bau, colour = 'darkred'))+
        theme(legend.position = 'none')
      
    }
    
    else {
      
      
      ggplot(maunaloa, aes(x=t, y=Conc))+
        geom_col(aes(fill=Year))+
        xlab("Years Since 1959")+
        xlim(0, 141)+
        ylim(0, 800)+
        ylab("Atmposheric Concentration of CO2")+
        geom_line(data=red_df2, aes(x=time_seq, y=red))+
        theme(legend.position = 'none')
      
      
    }
    
  })
} 

shinyApp(ui = ui, server = server)




