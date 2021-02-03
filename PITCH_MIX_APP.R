## MLB Pitchf/x - pitch mix R Shiny App
## Created by Daniel Luft, February 2021
## Data from Baseball Savant**
## dluft@ithaca.edu
#########################################

# load required packages
library(shiny)
library(tidyverse)
library(baseballr)
# if baseballR is not downloaded, go to https://billpetti.github.io/baseballr/ and download
library(mlbgameday)

# save unique count types to 'count_types' object
count_types <- c("1-1", "1-0", "0-0", "3-2", "2-2", "1-2", "0-2", "0-1", "2-1", "3-0", "2-0", "3-1")


# app design
ui <- fluidPage(
  
  sidebarPanel(titlePanel('Player: '),
               textInput(inputId = 'fname', label = 'Pitcher First Name: '), # first name input (Proper Capitalization and Spelling req.)
               textInput(inputId = 'lname', label = 'Pitcher Last Name: '), # last name input (Proper Capitalization and Spelling req.)
               actionButton(inputId = 'start', label = 'Generate'), # action button to generate the table and plot
               tableOutput('t1')), # table output 
  mainPanel(
    titlePanel(print("Pitch Mix by Count")),
    selectInput('count_tab', 'Pitch mix when count is: ', count_types), # count filter drop down input
    plotOutput('p1')) # plot output
  
)

server <- function(input, output) {
  
  #create reactive data frame, querying baseball savant Pitchf/x for the inputted pitcher
  pfx1 <- eventReactive(input$start, {
    
    first_name <- input$fname
    last_name <- input$lname
    start_date <- '2016-03-25'
    end_date <- '2020-10-30'
    
    # functions to generate pitchf/x data using Bill Petti's baseball R
    return_ID <- function(fName, lName){
      
      playerid_lookup(
        
        last_name = lName,
        first_name = fName
        
      ) -> player_info
      
      playerID = player_info$mlbam_id
      
      return(playerID)
    } # function returns players MLB ID
    ############################
    create_DF <- function(s_date, e_date, ID){
      
      scrape_statcast_savant(
        
        start_date = s_date,
        end_date = e_date,
        playerid = ID,
        player_type = "pitcher"
        
      ) -> pitch_df
      
      return(pitch_df)
      
      
    } # downloads pitchFX data for given player over given time
    ############################
    count1 <- function(data){
      
      data$count <- paste(data$balls, data$strikes, sep = "-")
      
      return(data)
      
    } # adds a count column to the df
    ############################
    sep_date <- function(data){
      
      data <- separate(data, game_date, into = c("Year", "Month", "Day"), sep = "-", remove = FALSE)
      
      return(data)
      
    } # seperates date column into month day and year
    ############################
    BASES_plus <- function(data){
      
      data$runner_1 <- ifelse(data$on_1b != "NA", 1, 0)
      replace_na(data$runner_1, 0) -> data$runner_1
      
      data$runner_2 <- ifelse(data$on_2b != "NA", 1, 0)
      replace_na(data$runner_2, 0) -> data$runner_2
      
      data$runner_3 <- ifelse(data$on_3b != "NA", 1, 0)
      replace_na(data$runner_3, 0) -> data$runner_3
      
      data$BASES <- paste(data$runner_1, data$runner_2, data$runner_3, sep = "")
      data$situation <- paste(data$BASES, data$outs_when_up, sep = " ")
      
      return(data)
    } # creates column for base/out situation
    ############################
    getPitchFX <- function(first_name, last_name, start_date, end_date){
      
      myID = return_ID(fName = first_name, lName = last_name)
      data1 <- create_DF(s_date = start_date, e_date = end_date, ID = myID)
      data2 <- count1(data = data1)
      data3 <- sep_date(data = data2)
      data4 <- BASES_plus(data = data3)
      
      return(data4)
      
    } # master function bringing all tog
    
    # use master function to pull pfx data from 2016-2020 for inputted pitcher
    data <- getPitchFX(first_name, last_name, start_date, end_date) 
    # filter out null and IN (intentional walk) pitches from the data
    data <- data %>% filter(pitch_type != 'null' & pitch_type != 'IN')
    
    return(data)
    
  })
  
  # generate table of pictch type usage for selected count and pitcher
  output$t1 <- renderTable({
    
    dat <- pfx1() # load the reactive data
    dat <- dat %>% filter(count == input$count_tab) # filter where count is inputted count 
    t <- prop.table(table(dat$pitch_type))*100 # generate table, multiply by 100 for % 
    t <- data.frame(t)
    colnames(t) <- c('Pitch','Usage (%)') # rename columns 
    return(t)
    
  })
  
  # generate the plot of the table
  output$p1 <- renderPlot({
    
    dat <- pfx1()
    t <- prop.table(table(dat$pitch_type, dat$count), margin = 2)*100
    t <- data.frame(t)
    colnames(t) <- c('Pitch_Type', 'Count', 'Usage') # rename columns
    t <- t %>% filter(t$Count == input$count_tab) # filter data for only selected count
    
    # make plot of the table 
    p <- ggplot(data = t, aes(Count, Usage, group = Pitch_Type, fill = Pitch_Type)) + 
      geom_bar(stat = 'identity', position = 'dodge') + ylab('Usage (%)') + 
      ggtitle(paste('Pitch mix for', input$fname, input$lname, 'in', input$count_tab, 'counts')) +
      theme(plot.title = element_text(size = 15, face = 'bold'), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10))
    
    return(p)
    
  }) 
  
}

# run app
shinyApp(ui = ui, server = server)



