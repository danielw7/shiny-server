#########################################################################################
##########################   Tennis - Strength Analysis   ###############################
#########################################################################################

###########################
####   packages used   ####
###########################

library(tidyverse)
library(stringr)
library(shiny)
library(shinythemes)
library(DT)
library(RSQLite)
library(scales)

################################
### Import  and clean data  ####
################################

###################################
###     Import and combine     ####
###################################

####  import and create dataframe  ####
ATP2017 <- read.csv('/srv/shiny-server/tennis_app/atp_2017.csv', header = T, dec = '.', sep = ';')
ATP2016 <- read.csv('/srv/shiny-server/tennis_app/atp_2016.csv', header = T, dec = '.', sep = ',')
ATP2015 <- read.csv('/srv/shiny-server/tennis_app/atp_2015.csv', header = T, dec = '.', sep = ',')
ATP2014 <- read.csv('/srv/shiny-server/tennis_app/atp_2014.csv', header = T, dec = '.', sep = ',')
ATP2013 <- read.csv('/srv/shiny-server/tennis_app/atp_2013.csv', header = T, dec = '.', sep = ',')
ATP2012 <- read.csv('/srv/shiny-server/tennis_app/atp_2012.csv', header = T, dec = '.', sep = ',')
ATP2011 <- read.csv('/srv/shiny-server/tennis_app/atp_2011.csv', header = T, dec = '.', sep = ',')
ATP2010 <- read.csv('/srv/shiny-server/tennis_app/atp_2010.csv', header = T, dec = '.', sep = ',')
ATP2009 <- read.csv('/srv/shiny-server/tennis_app/atp_2009.csv', header = T, dec = '.', sep = ',')
ATP2008 <- read.csv('/srv/shiny-server/tennis_app/atp_2008.csv', header = T, dec = '.', sep = ',')
ATP2007 <- read.csv('/srv/shiny-server/tennis_app/atp_2007.csv', header = T, dec = '.', sep = ',')
ATP2006 <- read.csv('/srv/shiny-server/tennis_app/atp_2006.csv', header = T, dec = '.', sep = ',')
ATP2005 <- read.csv('/srv/shiny-server/tennis_app/atp_2005.csv', header = T, dec = '.', sep = ',')

####  WTA: import and create dataframe  ####
WTA2017 <- read.csv('/srv/shiny-server/tennis_app/wta_2017.csv', header = T, dec = '.', sep = ';')
WTA2016 <- read.csv('/srv/shiny-server/tennis_app/wta_2016.csv', header = T, dec = '.', sep = ';')
WTA2015 <- read.csv('/srv/shiny-server/tennis_app/wta_2015.csv', header = T, dec = '.', sep = ',')
WTA2014 <- read.csv('/srv/shiny-server/tennis_app/wta_2014.csv', header = T, dec = '.', sep = ',')
WTA2013 <- read.csv('/srv/shiny-server/tennis_app/wta_2013.csv', header = T, dec = '.', sep = ',')
WTA2012 <- read.csv('/srv/shiny-server/tennis_app/wta_2012.csv', header = T, dec = '.', sep = ',')
WTA2011 <- read.csv('/srv/shiny-server/tennis_app/wta_2011.csv', header = T, dec = '.', sep = ',')
WTA2010 <- read.csv('/srv/shiny-server/tennis_app/wta_2010.csv', header = T, dec = '.', sep = ',')
WTA2009 <- read.csv('/srv/shiny-server/tennis_app/wta_2009.csv', header = T, dec = '.', sep = ',')
WTA2008 <- read.csv('/srv/shiny-server/tennis_app/wta_2008.csv', header = T, dec = '.', sep = ',')
WTA2007 <- read.csv('/srv/shiny-server/tennis_app/wta_2007.csv', header = T, dec = '.', sep = ',')
WTA2006 <- read.csv('/srv/shiny-server/tennis_app/wta_2006.csv', header = T, dec = '.', sep = ',')
WTA2005 <- read.csv('/srv/shiny-server/tennis_app/wta_2005.csv', header = T, dec = '.', sep = ',')

####  bind_rows (instead of rbind which is slower)  ####
all <-  bind_rows(ATP2005, ATP2006, ATP2007, ATP2008, ATP2009, ATP2010, ATP2011,
                  ATP2012, ATP2013, ATP2014, ATP2015, ATP2016, ATP2017, WTA2005, 
                  WTA2006, WTA2007, WTA2008, WTA2009, WTA2010, WTA2011,
                  WTA2012, WTA2013, WTA2014, WTA2015, WTA2016, WTA2017)

####  add year column  ####
all$year <- as.numeric(substr(all$tourney_date, start = 1, stop = 4))

####  manipulate score data in order to make them usable  ####
all$score <- as.character(all$score)
all$firstSet <- sub(' .*', '', all$score)
all$sets2_3 <- sub('^[^ ]*.', '', all$score)
all$secondSet <- sub(' .*', '', all$sets2_3)
all$thirdSet <- sub('^[^ ]*.', '', all$sets2_3)
all$sets4_5 <- sub('^[^ ]*.', '', all$thirdSet)
all$fourthSet <- sub(' .*', '', all$sets4_5)
all$fifthSet <- sub('^[^ ]*.', '', all$sets4_5)

####  extract games won and lost  ####
all$winner_set1_won <- as.numeric(substr(all$firstSet, start = 1, stop = 1))
all$winner_set2_won <- as.numeric(substr(all$secondSet, start = 1, stop = 1))
all$winner_set3_won <- as.numeric(substr(all$thirdSet, start = 1, stop = 1))
all$winner_set4_won <- as.numeric(substr(all$fourthSet, start = 1, stop = 1))
all$winner_set5_won <- as.numeric(substr(all$fifthSet, start = 1, stop = 1))

all$winner_set1_lost <- as.numeric(substr(all$firstSet, start = 3, stop = 3))
all$winner_set2_lost <- as.numeric(substr(all$secondSet, start = 3, stop = 3))
all$winner_set3_lost <- as.numeric(substr(all$thirdSet, start = 3, stop = 3))
all$winner_set4_lost <- as.numeric(substr(all$fourthSet, start = 3, stop = 3))
all$winner_set5_lost <- as.numeric(substr(all$fifthSet, start = 3, stop = 3))

all$loser_set1_won <- as.numeric(substr(all$firstSet, start = 3, stop = 3))
all$loser_set2_won <- as.numeric(substr(all$secondSet, start = 3, stop = 3))
all$loser_set3_won <- as.numeric(substr(all$thirdSet, start = 3, stop = 3))
all$loser_set4_won <- as.numeric(substr(all$fourthSet, start = 3, stop = 3))
all$loser_set5_won <- as.numeric(substr(all$fifthSet, start = 3, stop = 3))

all$loser_set1_lost <- as.numeric(substr(all$firstSet, start = 1, stop = 1))
all$loser_set2_lost <- as.numeric(substr(all$secondSet, start = 1, stop = 1))
all$loser_set3_lost <- as.numeric(substr(all$thirdSet, start = 1, stop = 1))
all$loser_set4_lost <- as.numeric(substr(all$fourthSet, start = 1, stop = 1))
all$loser_set5_lost <- as.numeric(substr(all$fifthSet, start = 1, stop = 1))

####  sum up games won and lost  ####
all$winner_games_won <-  rowSums(all[, c('winner_set1_won', 'winner_set2_won',
                                         'winner_set3_won','winner_set4_won', 'winner_set5_won')], na.rm = T)

all$winner_games_lost <- rowSums(all[, c('winner_set1_lost', 'winner_set2_lost',
                                         'winner_set3_lost', 'winner_set4_lost', 'winner_set5_lost')], na.rm = T)

all$loser_games_won <- rowSums(all[, c('loser_set1_won', 'loser_set2_won',
                                       'loser_set3_won', 'loser_set4_won','loser_set5_won')], na.rm = T)

all$loser_games_lost <- rowSums(all[, c('loser_set1_lost', 'loser_set2_lost', 
                                        'loser_set3_lost', 'loser_set4_lost', 'loser_set5_lost')], na.rm = T)

####  long term analysis  ####
winners_games_won <- aggregate(winner_games_won ~ winner_name + surface, data = all, sum)
winners_games_lost <- aggregate(winner_games_lost ~ winner_name + surface, data = all, sum)
losers_games_won <- aggregate(loser_games_won ~ loser_name + surface, data = all, sum)
losers_games_lost <- aggregate(loser_games_lost ~ loser_name + surface, data = all, sum)


players_statistics_winners <- merge(winners_games_won, winners_games_lost,
                                    by.x = c('winner_name', 'surface'),by.y = c('winner_name', 'surface'))

players_statistics_losers <- merge(losers_games_won,losers_games_lost,
                                   by.x = c('loser_name', 'surface'), by.y = c('loser_name', 'surface'))

players_statistics <- merge(players_statistics_winners, players_statistics_losers,
                            by.x = c('winner_name', 'surface'), by.y = c('loser_name', 'surface'))

players_statistics$games_won <-  rowSums(players_statistics[, c('winner_games_won', 'loser_games_won')], na.rm = T)
players_statistics$games_lost <- rowSums(players_statistics[, c('winner_games_lost', 'loser_games_lost')], na.rm = T)

players_statistics$win_share <- players_statistics$games_won / players_statistics$games_lost

####  short term analysis  ####
all_short <- subset(all, all$year == 2017)

all_short_winners_games_won <- aggregate(winner_games_won ~ winner_name + surface, data = all_short, sum)
all_short_winners_games_lost <- aggregate(winner_games_lost ~ winner_name + surface, data = all_short, sum)
all_short_losers_games_won <- aggregate(loser_games_won ~ loser_name + surface, data = all_short, sum)
all_short_losers_games_lost <- aggregate(loser_games_lost ~ loser_name + surface, data = all_short, sum)

all_short_players_statistics_winners <- merge(all_short_winners_games_won, all_short_winners_games_lost,
                                              by.x = c('winner_name', 'surface'), by.y = c('winner_name', 'surface'))

all_short_players_statistics_losers <- merge(all_short_losers_games_won, all_short_losers_games_lost,
                                             by.x = c('loser_name', 'surface'), by.y = c('loser_name', 'surface'))

all_short_players_statistics <- merge(all_short_players_statistics_winners, all_short_players_statistics_losers,
                                      by.x = c('winner_name', 'surface'), by.y = c('loser_name', 'surface'))

all_short_players_statistics$games_won <- rowSums(all_short_players_statistics[, c('winner_games_won', 
                                                                                   'loser_games_won')], na.rm = T)

all_short_players_statistics$games_lost <- rowSums(all_short_players_statistics[, c('winner_games_lost', 
                                                                                    'loser_games_lost')], na.rm = T)

all_short_players_statistics$win_share <- all_short_players_statistics$games_won / all_short_players_statistics$games_lost

##########################################################
##############   Database setups      ####################
##########################################################

#### Database path and tables  ####
sqlitePath_tennis <- "/srv/shiny-server/tennis_app/tennis_bets.db"
table_tennis <- "table_tracking"

# Define the fields we want to save from the form
fields <- c("player1", "player2", "sex", "surface", "odds", "beton",
            "wager", "result", "strength")

# Define save and load functions
saveData <- function(data) {
  # connect to the database
  db <- dbConnect(SQLite(), sqlitePath_tennis)
  # construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table_tennis,
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  
  # submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}


loadData <- function() {
  # connect to the database
  db <- dbConnect(SQLite(), sqlitePath_tennis)
  
  # construct the fetching query
  query <- sprintf("SELECT * FROM %s", table_tennis)
  
  # submit the fetch query and disconnect
  data <- dbGetQuery(db, query)
  dbDisconnect(db)
  data
}

##############################################################
###########         Shiny Tennis analysis     ################
##############################################################

####  UI    #### 
ui <- fluidPage(
  titlePanel("Tennis Player Analysis"),
  
  theme = shinytheme("slate"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      ("Enter Player Names"),
      br(),
      textInput("player1", "Player 1", ""),
      
      textInput("player2", "Player 2", ""),
      
      radioButtons('surface', label = "Surface", choices = c("Hard", "Clay", "Grass"),
                   selected = c("Hard")),
      
      actionButton("update", "Update analysis", icon("refresh")),
      
      helpText("Click the button in order to update the player analysis"),
      
      radioButtons('sex', label = "Sex", choices = c("female", "male"),
                   selected = c("female")),
      
      sliderInput("odds", "Odds", 1.50, 2.50, 2, ticks = TRUE),
      
      radioButtons("beton", label = "Bet on", choices = c("Player 1", "Player 2"), selected = c("Player 1")),
      
      numericInput("wager", "Wager", 0),
      
      radioButtons("result", "Result", choices = c("game not over yet", "bet won", "bet lost"), selected = c("game not over yet")),
      
      numericInput("strength", "Strength", ""),
      
      actionButton("submit", "Submit")
    ),
    
    mainPanel(tabsetPanel(
      
      tabPanel("Introduction",
               br(),
               includeMarkdown("/srv/shiny-server/tennis_app/intro.rmd")
      ),
      
      tabPanel("Strength Analysis",
        br(),
        textOutput("winner"),
        br(),
        (DT::dataTableOutput("table"))
      ),
      
      tabPanel("Track your bets",
        br(),
        (DT::dataTableOutput("table_tracking"))),
      
      tabPanel("Betting Performance", br(),
        textOutput("total"), br(), br(),
        textOutput("success_rate"), br(),br(),
        DT::dataTableOutput("table_performance"), br(), br(),
        textOutput("gender"), br(), br(),
        DT::dataTableOutput("genderTab"), br(), br(),
        textOutput("resultSurface"), br(), br(),
        DT::dataTableOutput("surfaceTab"))
    ))
  )
)

####  Server  ####
server <- function(input, output, session) {
  
  # Introduction
  output$intro <- renderUI({
    HTML(markdown::markdownToHTML(knit('/srv/shiny-server/tennis_app/intro.rmd', quiet = TRUE)))
    })
  
  # define a reactive expression for the results
  
  all_long1 <- reactive({
    players_statistics %>%
      filter(surface == input$surface,
             grepl(input$player1, winner_name))
  })
  
  all_long2 <- reactive({
    players_statistics %>%
      filter(surface == input$surface,
             grepl(input$player2, winner_name))
  })
  
  all_short1 <- reactive({
    all_short_players_statistics %>%
      filter(surface == input$surface,
             grepl(input$player1, winner_name))
  })
  
  all_short2 <- reactive({
    all_short_players_statistics %>%
      filter(surface == input$surface,
             grepl(input$player2, winner_name))
  })
  
  # reactive values  ####
  v <- reactiveValues()
  
  observeEvent(input$update, {
    v$result <- round((all_short1()$win_share -
                         all_short2()$win_share) +
                        (all_long1()$win_share -
                           all_long2()$win_share), 3)
    
    v$result_display <- abs(round((all_short1()$win_share -
                                     all_short2()$win_share) +
                                    (all_long1()$win_share -
                                       all_long2()$win_share), 3))
  })
  
  # update strength in input in order to submit it to the database
  observe({updateNumericInput(session, "strength", value = v$result_display)})
  
  # output Text ####
  output$winner <- renderText({
    validate(
      need(input$player1 != "", "Please add Player 1"),
      need(input$player2 != "", "Please add Player 2"),
      need(length(v$result) != 0, "No match found"),
      need(length(v$result) < 2, "More than one match found")
    )
    if (v$result > 0.2) {
      print(paste0(
        "Bet on ",
        input$player1,
        ", because the outcome of the analysis is ",
        round(abs(v$result), 3)
      ))
    } else if (v$result < (-0.2)) {
      print(paste0(
        "Bet on " ,
        input$player2,
        ", because the outcome of the analysis is ",
        round(abs(v$result), 3)
      ))
    } else
      print("Do not bet on this game!")
  })
  
  # output table  ####
  output$table <- DT::renderDataTable({
    bind_rows(all_long1()[, c(1, 9)], all_long2()[, c(1, 9)], all_short1()[, c(1, 9)], all_short2()[, c(1, 9)]) %>%
      datatable(
        style = "bootstrap",
        colnames = c('Player', 'Win-Share'),
        options = list(dom = "t",  autoWidth = TRUE, scrollX = TRUE, columnDefs = list(list(width = "150px", targets = "_all"
          ))
        )
      ) %>%
      formatRound(columns = c("win_share"), 3)
  })
  
  # output table for tracking bets  ####
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]], simplify = FALSE)
    data
  })
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(formData())
  })
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$table_tracking <- DT::renderDataTable({
    input$submit
    loadData() %>%
      datatable(style = "bootstrap", rownames = FALSE, colnames = c('Player 1', 'Player 2', 'Sex', 'Surface',
                                               'Odds', 'Bet on', 'Wager', 'Result', 'Strength')) %>%
      formatRound(c("odds", "wager"), 2) %>%
      formatRound(c('strength'), 3)
  })
  
  # output betting performance  ####
  profit_loss <- ifelse(loadData()$result == "bet won", loadData()$wager * loadData()$odds - loadData()$wager,
                  ifelse(loadData()$result == "bet lost",  0 - loadData()$wager, 0))

  total_win <- sum(profit_loss)
  share_wins_total <- sum(loadData()$result == "bet won") / length(loadData()$result)
  share_wins_smaller2 <- sum(loadData()[loadData()$strength < 0.2, ]$result == "bet won") /
                                   length(loadData()[loadData()$strength < 0.2, ]$result)
  share_wins_larger2 <- sum(loadData()[loadData()$strength > 0.2, ]$result == "bet won") /
                                  length(loadData()[loadData()$strength > 0.2, ]$result)
  share_wins_larger25 <- sum(loadData()[loadData()$strength > 0.25, ]$result == "bet won") /
                                   length(loadData()[loadData()$strength > 0.25, ]$result)
  share_wins_larger3 <- sum(loadData()[loadData()$strength > 0.3, ]$result == "bet won") /
                                   length(loadData()[loadData()$strength > 0.3, ]$result)
  share_wins_larger4 <- sum(loadData()[loadData()$strength > 0.4, ]$result == "bet won") /
                                   length(loadData()[loadData()$strength > 0.4, ]$result)
  share_wins_men <- length(loadData()[loadData()$sex == "male" & loadData()$result == "bet won", ]$result) /
                                   length(loadData()[loadData()$sex == "male", ]$result)
  share_wins_women <- length(loadData()[loadData()$sex == "female" & loadData()$result == "bet won", ]$result) /
                                   length(loadData()[loadData()$sex == "female", ]$result)
  share_wins_hard <- length(loadData()[loadData()$surface == "Hard" & loadData()$result == "bet won", ]$result) /
                                   length(loadData()[loadData()$surface == "Hard", ]$result)
  share_wins_clay <- length(loadData()[loadData()$surface == "Clay" & loadData()$result == "bet won", ]$result) /
                                   length(loadData()[loadData()$surface == "Clay", ]$result)
  share_wins_grass <- length(loadData()[loadData()$surface == "Grass" & loadData()$result == "bet won", ]$result) /
                                   length(loadData()[loadData()$surface == "Grass", ]$result)
  
  average_odds_total <- mean(loadData()$odds)
  average_odds_smaller2 <- mean(loadData()[loadData()$strength < 0.2, ]$odds)
  average_odds_larger2 <- mean(loadData()[loadData()$strength > 0.2, ]$odds)
  average_odds_larger25 <- mean(loadData()[loadData()$strength > 0.25, ]$odds)
  average_odds_larger3 <- mean(loadData()[loadData()$strength > 0.3, ]$odds)
  average_odds_larger4 <- mean(loadData()[loadData()$strength > 0.4, ]$odds)
  average_odds_men <- mean(loadData()[loadData()$sex == "male", ]$odds)
  average_odds_women <- mean(loadData()[loadData()$sex == "female", ]$odds)
  average_odds_hard <- mean(loadData()[loadData()$surface == "Hard", ]$odds)
  average_odds_clay <- mean(loadData()[loadData()$surface == "Clay", ]$odds)
  average_odds_grass <- mean(loadData()[loadData()$surface == "Grass", ]$odds)
  
  
  win_probability_total <- (1 / average_odds_total) - 0.02
  win_probability_smaller2 <- (1 / average_odds_smaller2) - 0.02
  win_probability_larger2 <- (1 / average_odds_larger2) - 0.02
  win_probability_larger25 <- (1 / average_odds_larger25 ) - 0.02
  win_probability_larger3 <- (1 / average_odds_larger3) - 0.02
  win_probability_larger4 <- (1 / average_odds_larger4) - 0.02
  win_probability_men <- (1 / average_odds_men) - 0.02
  win_probability_women <- (1 / average_odds_women) - 0.02
  win_probability_hard <- (1 / average_odds_hard) - 0.02
  win_probability_clay <- (1 / average_odds_clay) - 0.02
  win_probability_grass <- (1 / average_odds_grass) - 0.02
  
  # create vectors for strength table
  names_rows_vector <- c("Total", "Strength <0.2", "Strength >0.2", "Strength >0.25",
                         "Strength >0.3", "Strength >0.4")
  share_wins_vector <- c(share_wins_total,  share_wins_smaller2,  share_wins_larger2,
                         share_wins_larger25, share_wins_larger3, share_wins_larger4)
  average_odds_vector <- c(average_odds_total, average_odds_smaller2, average_odds_larger2,
                           average_odds_larger25, average_odds_larger3, average_odds_larger4)
  win_probability_vector <- c(win_probability_total, win_probability_smaller2, win_probability_larger2,
                              win_probability_larger25, win_probability_larger3, win_probability_larger4)
  
  # create vectors for gender table
  names_rows_gender <- c("Total", "Women", "Men")
  share_wins_gender <- c(share_wins_total, share_wins_women, share_wins_men)
  average_odds_gender <- c(average_odds_total, average_odds_women, average_odds_women)
  win_probability_gender <- c(win_probability_total, win_probability_women, win_probability_men)
  
  # create vectors for surface table
  names_rows_surface <- c("Total", "Hard", "Clay", "Grass")
  share_wins_surface <- c(share_wins_total, share_wins_hard, share_wins_clay, share_wins_grass)
  average_odds_surface <- c(average_odds_total, average_odds_hard, average_odds_clay, average_odds_grass)
  win_probability_surface <- c(win_probability_total, win_probability_hard, win_probability_clay, win_probability_grass)
  
  
  # create tables for performance analysis
  table_perf <- data.frame(names_rows_vector, average_odds_vector, share_wins_vector, win_probability_vector)
  table_perf$diff <- table_perf[, 3] - table_perf[, 4]
  
  table_perf_gender <- data.frame(names_rows_gender, average_odds_gender, share_wins_gender, win_probability_gender)
  table_perf_gender$diff <- table_perf_gender[, 3] - table_perf_gender[, 4]
  
  table_perf_surface <- data.frame(names_rows_surface, average_odds_surface, share_wins_surface, win_probability_surface)
  table_perf_surface$diff <- table_perf_surface[, 3] - table_perf_surface[, 4]
  
  output$total <- renderText({
    print(paste0("Your current balance is ", total_win, "."))
    })
  
  output$success_rate <- renderText({
   print(paste0("You have won ", percent(share_wins_total), " of your bets. See table below for a comparison 
                between your actual share of games won and the projected share of games won by the odds."))
  })
  
  output$table_performance <- DT::renderDataTable({
    table_perf %>%
    datatable(style = "bootstrap", rownames = FALSE, options = list(paging = FALSE, dom = "t", autoWidth = TRUE, scrollX = TRUE, 
                                columnDefs = list(list(width = "110px", targets = "_all"))),
              colnames = c("Calculated Strength", "Average odds", "Actual share of games won",
                "Projected share of games won by odds", "Performance Difference")) %>%
      formatRound(c("average_odds_vector"), 2) %>%
      formatPercentage(c("share_wins_vector", "win_probability_vector", "diff"), 2)
  })
  
  output$gender <- renderText({
  print("The following table shows your betting performance by gender of players:")
  
})

  output$genderTab <- DT::renderDataTable({
    table_perf_gender %>%
      datatable(style = "bootstrap", rownames = FALSE, options = list(paging = FALSE, dom = "t", autoWidth = TRUE, scrollX = TRUE,
                                                 columnDefs = list(list(width = "110px", targets = "_all"))),
                colnames = c("Sex", "Average odds", "Actual share of games won",
                             "Projected share of games won by odds", "Performance Difference")) %>%
      formatRound(c("average_odds_gender"), 2) %>%
      formatPercentage(c("share_wins_gender", "win_probability_gender", "diff"), 2)
  })
  
  output$resultSurface <- renderText({
    print(paste0("The following table shows the results by surface:"))
  })
  
  output$surfaceTab <- DT::renderDataTable({
    table_perf_surface %>%
      datatable(style = "bootstrap", rownames = FALSE, options = list(paging = FALSE, dom = "t", autoWidth = TRUE, scrollX = TRUE,
                                                 columnDefs = list(list(width = "110px", targets = "_all"))),
                colnames = c("Surface", "Average odds", "Actual share of games won",
                             "Projected share of games won by odds", "Performance Difference")) %>%
      formatRound(c("average_odds_surface"), 2) %>%
      formatPercentage(c("share_wins_surface", "win_probability_surface", "diff"), 2)
  })
}

shinyApp(ui = ui, server = server)