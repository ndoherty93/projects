library(shiny)
library(tidyverse)
library(bslib)
library(htmltools)
library(shinyWidgets)
library(ggimage)
library(tidyverse)
library(gt)
library(DT)
library(data.table)

pitches = c("Fastball","Sinker", "Slider", "Sweeper", "Curveball", "ChangeUp", 
            "Splitter", "Cutter", "Undefined")
calls = c("BallCalled", "StrikeCalled", "StrikeSwinging", 
          "FoulBallNotFieldable", "FoulBallFieldable", "InPlay", "HitByPitch")

neworleans1 <- read.csv("20260213-GeorgiaState-1_unverified.csv", header=TRUE)
neworleans2 <- read.csv("20260214-GeorgiaState-1_unverified.csv", header=TRUE)
neworleans3 <- read.csv("20260214-GeorgiaState-2_unverified.csv", header=TRUE)
jaxstate <- read.csv("20260217-RudyAbbottField-1_unverified.csv", header=TRUE)
mercer <- read.csv("20260218-MercerUniversity-1_unverified.csv", header=TRUE)
bellarmine <- read.csv("20260220-GeorgiaState-1_unverified.csv", header=TRUE)

df <- rbind(neworleans1, neworleans2, neworleans3, jaxstate, mercer, bellarmine)

df <- df %>%
  filter(is.na(HorzBreak) == F) %>% 
  mutate(
    TaggedPitchType = factor(TaggedPitchType, levels = pitches),
    PitchCall = factor(PitchCall, levels = calls),
    CustomGameID = paste0(
      Date,
      ":",
      str_sub(AwayTeam,1,3),
      "@",
      str_sub(HomeTeam,1,3),
      ":",
      str_sub(GameID,-1,-1)
    )
  )

#site interface
ui <- page_fluid(page_sidebar(
  title = "Swing Decision",
  sidebar = sidebar(
    title = "Select Batter/Game",
    #this creates a pitcher team input
    selectInput(
      inputId = "BatterTeamInput",
      label = "Select Batter Team",
      choices = sort(unique(df$BatterTeam)),
      selectize = T
    ),
    #this creates a pitcher selection input
    selectInput(
      inputId = "BatterInput",
      label = "Select Batter",
      choices = sort(unique(df$Batter)),
      selectize = T
    ),
    #game selection input
    selectInput(
      inputId = "GameInput",
      label = "Select Game",
      choices = sort(df$CustomGameID),
      selectize = T
    )
  ),
  #setting tabs at top
  navset_tab(
    nav_panel(title = "Chart", fillPage(plotOutput("swingDecision")))
    )
)
) 

server <- function(input, output, session){
  #changes what games are available to select based on pitcher selected
  observeEvent(input$BatterTeamInput,
               updateSelectInput(
                 session,
                 inputId = "BatterInput",
                 choices = sort(unique(df$Batter[df$BatterTeam == input$BatterTeamInput]))
               )
  )
  observeEvent(input$BatterInput,
               updatePickerInput(
                 session,
                 inputId = "GameInput",
                 choices = sort(unique(df$CustomGameID[df$Batter == input$BatterInput])),
                 selected = sort(unique(df$CustomGameID[df$Batter == input$BatterInput]))
               )
  )
  
#code for swing decision plot
output$swingDecision <- renderPlot({
  
  #filter dataset down to choices you've made
  dataFilter <- reactive({
    df %>%
      filter(
        BatterTeam == input$BatterTeamInput,
        Batter == input$BatterInput,
        CustomGameID %in% c(input$GameInput)
      )
  })
    
    #swing decision plot itself
    dataFilter() %>%
      ggplot() +
      aes(PlateLocSide, PlateLocHeight, col=PitchCall, shape =TaggedPitchType,
          label=PitchofPA) +
      geom_point(size=7) +
      scale_shape_manual(values = c("Fastball" = 16, "Sinker" = 16, "Slider" = 17,
                                    "Curveball" = 17, "Cutter" = 18, "ChangeUp" = 15,
                                    "Splitter" = 15)) +
      scale_color_manual(values= c("BallCalled" = "green", "StrikeCalled"="purple",
                                   "FoulBallNotFieldable"="orange",
                                   "FoulBallFieldable"="orange", "InPlay"="yellow",
                                   "StrikeSwinging"="red", "HitByPitch" = "pink")) +
      geom_text(hjust=0.5, vjust=0.5, color="black") +
      ggtitle("Plate Appearances by Inning") +
      geom_rect(mapping = aes(ymax = 3.37750, ymin = 1.5, xmax = -.83083, xmin = .83083)
                , alpha = 0, size=1.2, colour = "black") +
      geom_rect(mapping = aes(ymax = 3.5, ymin = 1.37750, xmax = -.99750, xmin = .99750),
                alpha = 0, size=1.2, colour = "black") +
      geom_rect(mapping = aes(ymax = 5, ymin = 0, xmax = -1.8, xmin = 1.8),
                alpha = 0, size=0.5, colour = "black") +
      geom_rect(mapping = aes(ymax = 3, ymin = 2, xmax = -.41666667, xmin = .41666667),
                alpha=0, size=1.2, colour = "red")+
       facet_wrap(~Inning) + ylab("") + xlab("") +
      theme_bw()
  
})
  
  
}

shinyApp(ui,server)