library(shiny)
library(tidyverse)
library(bslib)
library(htmltools)
library(shinyWidgets)
library(ggimage)

pitches = c("Fastball","Sinker", "Slider", "Sweeper", "Curveball", "ChangeUp", 
  "Splitter", "Cutter", "Undefined")

pitch_colors = c("Fastball" = '#d7191c', "Sinker" = "#fdae61", "Slider" = "#A020F0", 
  "Sweeper" = "magenta", "Curveball" = '#2c7bb6', "ChangeUp" = '#90EE90',
  "Splitter" = '#90EE32', "Cutter" = "pink", "Undefined" = "dodgerblue")

neworleans1 <- read.csv("20260213-GeorgiaState-1_unverified.csv", header=TRUE)
neworleans2 <- read.csv("20260214-GeorgiaState-1_unverified.csv", header=TRUE)
neworleans3 <- read.csv("20260214-GeorgiaState-2_unverified.csv", header=TRUE)
jaxstate <- read.csv("20260217-RudyAbbottField-1_unverified.csv", header=TRUE)
mercer <- read.csv("20260218-MercerUniversity-1_unverified.csv", header=TRUE)
bellarmine <- read.csv("20260220-GeorgiaState-1_unverified.csv", header=TRUE)
tech <- read.csv("20260224-GeorgiaTech-1_unverified.csv", header=TRUE)
belmont <- read.csv("20260227-ESRosePark-1_unverified.csv", header=TRUE)
belmont2 <- read.csv("20260228-ESRosePark-1_unverified.csv", header=TRUE)
belmont3 <- read.csv("20260228-ESRosePark-2_unverified.csv", header=TRUE)
belmont4 <- read.csv("20260301-ESRosePark-1_unverified.csv", header=TRUE)
csu <- read.csv("20260306-GeorgiaState-1_unverified.csv", header=TRUE)
csu2 <- read.csv("20260307-GeorgiaState-2_unverified.csv", header=TRUE)
csu3 <- read.csv("20260307-GeorgiaState-3_unverified.csv", header=TRUE)


# df <- rbind(intrasquad1, intrasquad2, intrasquad3, intrasquad4, intrasquad5, intrasquad6, intrasquad7,
#             intrasquad8, intrasquad9, intrasquad10, intrasquad11, intrasquad12, intrasquad13, intrasquad14,
#             intrasquad15, intrasquad16, intrasquad17, intrasquad18, intrasquad19, intrasquad20, intrasquad21,
#             intrasquad22, intrasquad23, intrasquad24, intrasquad25, intrasquad26, intrasquad27,
#             intrasquad28, intrasquad29, intrasquad30)
df <- rbind(neworleans1, neworleans2, neworleans3, jaxstate, mercer, bellarmine, tech,
belmont, belmont2, belmont3, belmont4, csu, csu2, csu3)

df <- df %>%
  #filter for when trackman didn't catch pitches
  filter(is.na(HorzBreak) == F) %>% 
  mutate(
    #custom tagging would go here
    #PitchType = case_when(
      #Pitcher == "LastName, FirstName" & RelSpeed >= 80 ~ 'Fastball',
      #Pitcher == "LastName, FirstName" & RelSpeed < 80 ~ 'Slider'
    #),
    #use PitchType with custom tagging here
    TaggedPitchType = factor(TaggedPitchType, levels = pitches),
    
    #changes pitcher name from last name, first name to first name last name
    #Pitcher = str_replace_all(Pitcher, "(\\w+), (\\w+)", "\\2 \\1"), 
    
    #dummy variable for average zone
    inZone = case_when(
      between(PlateLocHeight, 1.5, 3.5) & between(PlateLocSide,-0.71,0.71) ~ 1, 
      T ~ 0
    ),
    
    #dummy variable for chase 
    Chase = case_when(
      inZone == 0 & PitchCall %in% c("FoulBall", "StrikeSwinging", "InPlay",
                                     "FoulBallNotFieldable") ~ 1,
      T ~ 0
      
    ),
    
    #create custom GameID
    CustomGameID = paste0(
      UTCDate,
      ":",
      str_sub(AwayTeam,1,3),
      "@",
      str_sub(HomeTeam,1,3),
      ":",
      str_sub(GameID,-1,-1)
    )
    
  )

#add home plate segments
home_plate_segments <- data.frame(
  x = c(0, 0.71, 0.71, 0, -0.71, -0.71),
  y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15),
  xend = c(0.71, 0.71, 0, -0.71, -0.71, 0), 
  yend = c(0.15, 0.3, 0.5, 0.3, 0.15, 0.15)
)

#site interface
ui <- page_fluid(page_sidebar(
  title = "Pitching Reports",
  sidebar = sidebar(
    title = "Select Pitcher/Game",
    #this creates a pitcher team input
    selectInput(
      inputId = "PitcherTeamInput",
      label = "Select Pitcher Team",
      choices = sort(unique(df$PitcherTeam)),
      selectize = T
    ),
    #this creates a pitcher selection input
    selectInput(
      inputId = "PitcherInput",
      label = "Select Pitcher",
      choices = sort(unique(df$Pitcher)),
      selectize = T
    ),
    #game selection input
    pickerInput(
      inputId = "GameInput",
      label = HTML("Select Game<br>(Selects all by default)"),
      choices = unique(df$CustomGameID),
      options = list('actions-box' = TRUE),
      multiple = T
    ),
    #batter hand input can do both or just one
    pickerInput(
      inputId = "BatterHand",
      label = HTML("Select Batter Side<br>(Selects all by defalut)"),
      choices = unique(df$BatterSide),
      selected = unique(df$BatterSide),
      options = list('actions-box'=TRUE),
      multiple = T
    )
  ),
  #setting tabs at top
    navset_tab(
      nav_panel(
        title = "Pitch Breakdown", tableOutput("metrics"),
        fluidRow(
          column(3,tableOutput("hitsruns"),tableOutput("firstpitch")),
          column(6,plotOutput("breakdown"))
        ),
      ),
      nav_panel(title = "Release Point", fluidRow(
                                          column(6,plotOutput("releaseBack")), 
                                          column(6,plotOutput("releaseSide")))
                                          ),
      nav_panel(title = "Pitch Movement", fluidRow(
                                          column(6,plotOutput("movement")),
                                          column(6,plotOutput("spinSpeed")))
                                          ),
      nav_panel(title = "Velo Progression", plotOutput("velos"))
    )
  )
) 

server <- function(input, output, session){
  #changes what games are available to select based on pitcher selected
  observeEvent(input$PitcherTeamInput,
               updateSelectInput(
                 session,
                 inputId = "PitcherInput",
                 choices = sort(unique(df$Pitcher[df$PitcherTeam == input$PitcherTeamInput]))
               )
  )
  observeEvent(input$PitcherInput,
               updatePickerInput(
                 session,
                 inputId = "GameInput",
                 choices = sort(unique(df$CustomGameID[df$Pitcher == input$PitcherInput])),
                 selected = sort(unique(df$CustomGameID[df$Pitcher == input$PitcherInput]))
               )
  )


#code for pitch breakdown plot
output$breakdown <- renderPlot({
  
  #filter dataset down to choices you've made
  dataFilter <- reactive({
    df %>%
      filter(
        PitcherTeam == input$PitcherTeamInput,
        Pitcher == input$PitcherInput,
        CustomGameID %in% c(input$GameInput),
        BatterSide %in% c(input$BatterHand)
      )
  })
  
  #pitch movement plot itself
  dataFilter() %>%
    ggplot(aes(x=-PlateLocSide,y=PlateLocHeight))+geom_point(aes(col=TaggedPitchType),size=2.5)+
      geom_rect(mapping=aes(xmin=-1, xmax=1, ymin=1.5, ymax=3.5, fill=NULL), color="black", alpha=0)+
      geom_rect(mapping=aes(xmin=-1, xmax=-.33, ymin=1.5, ymax=3.5, fill=NULL), color="black", alpha=0)+
      geom_rect(mapping=aes(xmin=.33, xmax=1, ymin=1.5, ymax=3.5, fill=NULL), color="black", alpha=0)+
      geom_rect(mapping=aes(xmin=-1, xmax=1, ymin=2.16, ymax=2.84, fill=NULL), color="black", alpha=0)+
      scale_x_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.2)) + # set limits of the scale and have ticks every 5
      scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 0.2)) +
      geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
          color = "black")+
      scale_color_manual(values = pitch_colors) + # color the points by the pitch colors set earlier
      theme_bw() + # sets the plot theme, base_size should always be used! increases the size of text and other visual aids
      labs( # labels
        title = paste0(input$PitcherInput, ": Pitch Breakdown"), # add the pitchers name into the plot title
        y = "Vertical Plate Location (ft.)", 
        x = "Horizontal Plate Location (ft.): Pitcher's Perspective", 
        color = "Pitch Type" # this is the title of the legend
      ) +
      #theme( # center the title, put the legend on the right of the graph and increase the text size of the legend
      # plot.title = element_text(hjust = 0.5, face = "bold", size = "20"),
      # legend.position = "right",
      #  legend.text = element_text(size = 12)
     # ) +
    coord_fixed() + # this makes it the so the plot will always have the same dimensions and won't shift around by each pitcher
    guides(color = guide_legend(override.aes = list(size = 3))) # this line makes the dots in the legend larger so they're more readable
  
  
})


# this is the metrics table, shows averages by pitch type
output$metrics <- renderTable({
  
  # this filters the dataset down to just the selections you've made
  dataFilter <- reactive({
    df %>%
      filter(
        PitcherTeam == input$PitcherTeamInput,
        Pitcher == input$PitcherInput,
        CustomGameID %in% c(input$GameInput),
        BatterSide %in% c(input$BatterHand)
      ) 
  })
  
  dataFilter() %>%
    # this filter keeps the table updated with what specific game and batter hand have been filtered for
    # for some reason when the table is in the same tab as the movement plot it wasn't updating properly
    # I thought this was supposed to auto update with the function above but it wasn't working so I added the filter below to be safe
    filter(PitcherTeam %in% c(input$PitcherTeamInput), Pitcher %in% c(input$PitcherInput), CustomGameID %in% c(input$GameInput), BatterSide %in% c(input$BatterHand)) %>%
    group_by(TaggedPitchType) %>%
    # this whole section just finds averages for each of these metrics by pitch type
    summarise(
      PitchesperType = n(),
      MaxVelo = round(max(RelSpeed), 1),
      AvgVelo = round(mean(RelSpeed, na.rm = T), 1),
      iVB = round(mean(InducedVertBreak, na.rm = T), 1),
      #VB = round(mean(VertBreak,na.rm = T), 1),
      HB = round(mean(HorzBreak, na.rm = T), 1),
      #VAA = round(mean(VertApprAngle, na.rm = T), 1),
      #HAA = round(mean(HorzApprAngle, na.rm = T), 1),
      Axis = case_when(
        atan2(mean(sin(SpinAxis*pi/180)),mean(cos(SpinAxis*pi/180)))*180/pi < 0 ~ 
          360 + atan2(mean(sin(SpinAxis*pi/180)),mean(cos(SpinAxis*pi/180)))*180/pi,
        TRUE ~ atan2(mean(sin(SpinAxis*pi/180)),mean(cos(SpinAxis*pi/180)))*180/pi
      ), 
      Spin = round(mean(SpinRate, na.rm = T)),
      Balls = sum(ifelse(PitchCall=="BallCalled",1,0)),
      Strikes = sum(ifelse(PitchCall=="StrikeCalled"|PitchCall=="StrikeSwinging"|
          PitchCall=="FoulBallNotFieldable"|PitchCall=="FoulBallFieldbable"|
            PitchCall=="InPlay",1,0)),
      'Strike %' = sum(ifelse(PitchCall=="StrikeCalled"|PitchCall=="StrikeSwinging"|
          PitchCall=="FoulBallNotFieldable"|PitchCall=="FoulBallFieldbable"|
            PitchCall=="InPlay",1,0))/n()*100,
      Outs=sum(OutsOnPlay) + sum(ifelse(KorBB=="Strikeout",1,0))
      #`Height (rel)` = round(mean(RelHeight), 1),
      #`Side (rel)` = round(mean(RelSide), 1),
      #Extension = round(mean(Extension, na.rm = T), 1),
      # whiff% is percent of swings where the batter misses entirely
      # that section at the end is all the ways trackman describes a swing
      #`Whiff%` = paste0(round(sum(PitchCall == "StrikeSwinging") / sum(PitchCall %in% c("StrikeSwinging",
                                                                                        #"InPlay",
                                                                                        #"FoulBallFieldable",
                                                                                        #"FoulBallNotFieldable")) * 100), "%"),
      # CSW stands for called strikes plus whiffs. Invented by PitcherList and they have a great article on it
      # It's the percent of pitches thrown that result in whiffs or called strikes
      # It's strike percentage without fouls or balls in play so I like to think of it as non-competitive strikes
      #`CSW%` = paste0(round(sum(PitchCall %in% c("StrikeSwinging", "StrikeCalled")) / n() * 100), "%")
    ) %>%
    mutate(
      "Usage %" = (PitchesperType/sum(PitchesperType)*100),
      PitchCount = sum(PitchesperType),
      Tilt = case_when(
        Axis <= 3.75 ~ "6:00",
        Axis <= 11.25 ~ "6:15",
        Axis <= 18.75 ~ "6:30",
        Axis <= 26.25 ~ "6:45",
        Axis <= 33.75 ~ "7:00",
        Axis <= 41.25 ~ "7:15",
        Axis <= 48.75 ~ "7:30",
        Axis <= 56.25 ~ "7:45", 
        Axis <= 63.75 ~ "8:00",
        Axis <= 71.25 ~ "8:15",
        Axis <= 78.75 ~ "8:30",
        Axis <= 86.25 ~ "8:45",
        Axis <= 93.75 ~ "9:00",
        Axis <= 101.25 ~ "9:15",
        Axis <= 108.75 ~ "9:30",
        Axis <= 116.25 ~ "9:45",
        Axis <= 123.75 ~ "10:00",
        Axis <= 131.25 ~ "10:15",
        Axis <= 138.75 ~ "10:30",
        Axis <= 146.25 ~ "10:45",
        Axis <= 153.75 ~ "11:00",
        Axis <= 161.25 ~ "11:15",
        Axis <= 168.75 ~ "11:30",
        Axis <= 176.25 ~ "11:45",
        Axis <= 183.75 ~ "12:00",
        Axis <= 191.25 ~ "12:15",
        Axis <= 198.75 ~ "12:30",
        Axis <= 206.25 ~ "12:45",
        Axis <= 213.75 ~ "1:00",
        Axis <= 221.25 ~ "1:15",
        Axis <= 228.75 ~ "1:30",
        Axis <= 236.25 ~ "1:45",
        Axis <= 243.75 ~ "2:00",
        Axis <= 251.25 ~ "2:15",
        Axis <= 258.25 ~ "2:30",
        Axis <= 266.25 ~ "2:45",
        Axis <= 273.25 ~ "3:00",
        Axis <= 281.25 ~ "3:15",
        Axis <= 288.75 ~ "3:30",
        Axis <= 296.25 ~ "3:45",
        Axis <= 303.75 ~ "4:00",
        Axis <= 311.25 ~ "4:15",
        Axis <= 318.75 ~ "4:30",
        Axis <= 326.25 ~ "4:45",
        Axis <= 333.75 ~ "5:00",
        Axis <= 341.25 ~ "5:15",
        Axis <= 348.75 ~ "5:30",
        Axis <= 356.25 ~ "5:45",
        TRUE ~ "6:30"
      )
    ) %>%
    select(
      PitchCount, "Pitch" = TaggedPitchType, PitchesperType, "Usage %", MaxVelo, 
      AvgVelo, iVB, HB, Tilt, Spin, Balls, Strikes, "Strike %", Outs
    )
},digits=0) # this digits bit at the end makes sure that every metric rounds to one decimal point

# code for hitsruns table
output$hitsruns <- renderTable({
  
  dataFilter <- reactive({
    df %>%
      filter(
        PitcherTeam == input$PitcherTeamInput,
        Pitcher == input$PitcherInput,
        CustomGameID %in% c(input$GameInput),
        BatterSide %in% c(input$BatterHand)
      )
  })
  
  dataFilter() %>%
    filter(CustomGameID %in% c(input$GameInput)) %>% #, BatterSide %in% c(input$BatterHand)) %>%
    summarize(
      H = sum(ifelse(PlayResult=="Single"|PlayResult=="Double"|
            PlayResult=="Triple"|PlayResult=="HomeRun",1,0)),
      R = sum(RunsScored),
      BB = sum(ifelse(KorBB=="Walk",1,0)),
      K = sum(ifelse(KorBB=="Strikeout",1,0)),
      Inn = round(sum(ifelse(KorBB=="Strikeout",1,0)) + sum(OutsOnPlay),1),
      "P/PA" = round(n()/(sum(ifelse(PitchCall=="InPlay",1,0))+sum(ifelse(KorBB=="Strikeout",1,0))+
                    sum(ifelse(KorBB=="Walk",1,0))+sum(ifelse(PitchCall=="HitByPitch",1,0))),1)
    )
},digits=0)

#code for first pitch strike table
output$firstpitch <- renderTable({
  
  dataFilter <- reactive({
    df %>%
      filter(
        PitcherTeam == input$PitcherTeamInput,
        Pitcher == input$PitcherInput,
        CustomGameID %in% c(input$GameInput),
        BatterSide %in% c(input$BatterHand)
      )
  })
  
  dataFilter() %>%
    filter(CustomGameID %in% c(input$GameInput)) %>% #, BatterSide %in% c(input$BatterHand)) %>%
    summarize(
      firstPitchK = sum(ifelse(PitchofPA==1 & (PitchCall=="StrikeCalled"|PitchCall=="StrikeSwinging"|
                                                 PitchCall=="FoulBallNotFieldable"|PitchCall=="FoulBallFieldable"|
                                                 PitchCall=="InPlay"),1,0)),
      Batters = sum(ifelse(PitchCall=="InPlay",1,0))+sum(ifelse(KorBB=="Strikeout",1,0))+
                sum(ifelse(KorBB=="Walk",1,0))+sum(ifelse(PitchCall=="HitByPitch",1,0))
    ) %>%
    mutate(
      Batters = Batters,
      "%" = (firstPitchK/Batters)*100
    )
},digits=0)

#code for pitch release back plot
output$releaseBack <- renderPlot({
  
  dataFilter <- reactive({
    
    df %>%
      filter(
        PitcherTeam == input$PitcherTeamInput,
        Pitcher == input$PitcherInput,
        CustomGameID %in% c(input$GameInput),
        BatterSide %in% c(input$BatterHand)
      )
    
  })
  
  dataFilter() %>%
    ggplot(aes(x=RelSide, y=RelHeight))+geom_point(aes(col=TaggedPitchType),size=2.5)+
      scale_x_continuous(limits = c(-4, 4), breaks = seq(-4, 4, 1)) + # set limits of the scale and have ticks every 5
    scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 1)) +
    scale_color_manual(values = pitch_colors) +
    geom_image(
      data = tibble(RelSide = 0, RelHeight = 0.5),
      aes(image = "back mound.png"),
      size = 0.45
    ) +
    geom_image(
      data = tibble(RelSide = 0, RelHeight = 0.75),
      aes(image = "back rubber.png"),
      size = 0.12
    ) +
    labs( # labels
      title = paste0(input$PitcherInput, ": Release Point (Back)"), # add the pitchers name into the plot title
      y = "Release Height", 
      x = "Release Side", 
      color = "Pitch Type" # this is the title of the legend
    ) +
    coord_fixed() + # this makes it the so the plot will always have the same dimensions and won't shift around by each pitcher
    guides(color = guide_legend(override.aes = list(size = 3)))
    
})

#code for pitch release side plot
output$releaseSide <- renderPlot({
  
  dataFilter <- reactive({
    
    df %>%
      filter(
        PitcherTeam == input$PitcherTeamInput,
        Pitcher == input$PitcherInput,
        CustomGameID %in% c(input$GameInput),
        BatterSide %in% c(input$BatterHand)
      )
    
  })
  
  dataFilter() %>%
    ggplot(aes(x=Extension, y=RelHeight))+geom_point(aes(col=TaggedPitchType),size=2.5)+
    scale_x_continuous(limits = c(0, 8), breaks = seq(0, 8, 2)) + # set limits of the scale and have ticks every 5
    scale_y_continuous(limits = c(0, 8), breaks = seq(0, 8, 1)) +
    geom_image(
      data = tibble(Extension = 4, RelHeight = 0.45),
      aes(image = "side mound.png"),
      size = 0.93
      ) +
    geom_image(
      data = tibble(Extension = .25, RelHeight = 0.75),
      aes(image = "side rubber.png"),
      size = 0.07
    ) +
    scale_color_manual(values = pitch_colors) +
    labs( # labels
      title = paste0(input$PitcherInput, ": Release Point (Side)"), # add the pitchers name into the plot title
      y = "Release Height", 
      x = "Extension", 
      color = "Pitch Type" # this is the title of the legend
    ) +
    coord_fixed() + # this makes it the so the plot will always have the same dimensions and won't shift around by each pitcher
    guides(color = guide_legend(override.aes = list(size = 3)))
  
})

#code for movement plot
output$movement <- renderPlot({
  
  dataFilter <- reactive({
    
    df %>%
      filter(
        PitcherTeam == input$PitcherTeamInput,
        Pitcher == input$PitcherInput,
        CustomGameID %in% c(input$GameInput),
        BatterSide %in% c(input$BatterHand)
      )
    
  })
  
  dataFilter() %>%
    ggplot(aes(x = HorzBreak, y = InducedVertBreak)) + geom_point(aes(col=TaggedPitchType),size=2.5) +
    scale_x_continuous(limits = c(-25, 25), breaks = seq(-25, 25, 5)) + # set limits of the scale and have ticks every 5
    scale_y_continuous(limits = c(-25, 25), breaks = seq(-25, 25, 5)) +
    scale_color_manual(values = pitch_colors) +
    labs( # labels
      title = paste0(input$PitcherInput, ": Pitch Movement"), # add the pitchers name into the plot title
      y = "Induced Vertical Break", 
      x = "Horz Break", 
      color = "Pitch Type" # this is the title of the legend
    ) +
    geom_hline(yintercept = 0, linetype = 2) + # these two lines create dashed lines at the intercepts
    geom_vline(xintercept = 0, linetype = 2) + 
    coord_fixed() + # this makes it the so the plot will always have the same dimensions and won't shift around by each pitcher
    guides(color = guide_legend(override.aes = list(size = 3)))
  
})

#code for spinSpeed
output$spinSpeed <- renderPlot({
  
  dataFilter <- reactive({
    
    df %>%
      filter(
        PitcherTeam == input$PitcherTeamInput,
        Pitcher == input$PitcherInput,
        CustomGameID %in% c(input$GameInput),
        BatterSide %in% c(input$BatterHand)
      )
    
  })
  
  dataFilter() %>%
    ggplot(aes(x = RelSpeed, y = SpinRate)) + geom_point(aes(col=TaggedPitchType),size=2.5) +
    #scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + # set limits of the scale and have ticks every 5
    #scale_y_continuous(limits = c(60, 100), breaks = seq(60, 100, 5)) +
    scale_color_manual(values = pitch_colors) +
    labs( # labels
      title = paste0(input$PitcherInput, ": Spin & Speed"), # add the pitchers name into the plot title
      x = "Release Speed", 
      y = "Spin Rate", 
      color = "Pitch Type" # this is the title of the legend
    ) +
    #geom_hline(yintercept = 0, linetype = 2) + # these two lines create dashed lines at the intercepts
    #geom_vline(xintercept = 0, linetype = 2) + 
    #coord_fixed() + # this makes it the so the plot will always have the same dimensions and won't shift around by each pitcher
    guides(color = guide_legend(override.aes = list(size = 3)))
  
})

#code for velos plot
output$velos <- renderPlot({
  
  dataFilter <- reactive({
    
    df %>%
      filter(
        PitcherTeam == input$PitcherTeamInput,
        Pitcher == input$PitcherInput,
        CustomGameID %in% c(input$GameInput),
        BatterSide %in% c(input$BatterHand)
      ) %>%
      group_by(TaggedPitchType, Inning) %>%
      summarise(AvgVelo = round(mean(RelSpeed, na.rm = T), 1))
    
  })
  
  dataFilter() %>%
    ggplot(aes(x = Inning, y = AvgVelo,col=TaggedPitchType))+
    geom_point(size=2.5)+
    geom_path() +
    scale_color_manual(values = pitch_colors) +
    scale_x_continuous(limits = c(1, 11), breaks = seq(1, 11, 1)) + # set limits of the scale and have ticks every 5
    scale_y_continuous(limits = c(65, 100), breaks = seq(65, 100, 5)) 
    
  
})


}

shinyApp(ui,server)