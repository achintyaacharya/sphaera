# palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
#   "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
#load libraries
require(sqldf)
require(shiny)
require(shinydashboard)
require(shinydashboardPlus)
require(fmsb)
require(lpSolve)
require(stringr)
require(RCurl)
require(jsonlite)
require(plyr)
require(readxl)
require(dplyr)
require(ggplot2)
require(tidyverse)
require(caret)
require(RSQLite)
require(corrplot) 
require(klaR)
require(class)
require(LiblineaR)
require(e1071)
require(shinyjs)
# install.packages('slickR')
require(slickR)
require(shinyjqui)
# install.packages("shinyWidgets")
require(shinyWidgets)
#####Code for correcting the names of players######
# data = read.csv("datafm20.csv", encoding = "UTF-8")

# charmap=read.csv2("charmap.csv",sep = "|", encoding = "UTF-8",header = F)
# charmap$V1[1]
# for (i in 1:nrow(charmap))
# {
#   data$Name <- gsub(charmap$V1[i], charmap$V2[i], data$Name)
#   print(charmap$V1[i])
# }
# 
# write.csv(data, file = "fm20data.csv", fileEncoding = "UTF-8")

# x = c(1,2,3,4,"a","b")

# Get the data
data = read.csv("fm20data.csv", encoding = "UTF-8")
col = colnames(c)


database = dbConnect(RSQLite::SQLite(), dbname = "database.sqlite")
tables <- dbListTables(database)

matches <- as.data.frame(tbl(database, "Match"))
league <- as.data.frame(tbl(database,"League"))
country <- as.data.frame(tbl(database, "Country"))
teams <- as.data.frame(tbl(database, "Team"))
players <- as.data.frame(tbl(database, "Player"))

teamsInLeagues = sqldf("select league.name, team.team_long_name from league as league inner join matches on league.id=matches.league_id inner join teams team on matches.home_team_api_id = team.team_api_id group by league.name, team.team_long_name")
################## Data manipulation ################
# df["Best.Pos"] = factor(df["Best.Pos"])

df = data %>%
  mutate('New_Position' = 
           ifelse((Best.Pos=='AM (R)' | Best.Pos=='AM (L)' | Best.Pos=='AM (C)' 
                   | Best.Pos=='M (R)' | Best.Pos=='M (L)' | Best.Pos=='M (C)'),'Mid',
                  ifelse((Best.Pos=='DM' | Best.Pos=='D (L)' | Best.Pos=='D (C)' 
                          | Best.Pos=='D (R)'| Best.Pos=='WB (L)' | Best.Pos=='WB (R)'),'Def',
                         ifelse(Best.Pos=='GK','GK',ifelse(Best.Pos=='ST (C)','ST','Other')))))

df_mainAttrib = df%>% dplyr::select(c("Name","Club","Division","Height","Weight","Age","Nation","Preferred.Foot","Best.Pos","Value","Wage","Str","Pas","Pac","Jum","Fin","Dri","Agg","Acc","Han","Kic"))

match_result <- matches %>% dplyr::select(id, 
                                          country_id, 
                                          league_id, 
                                          season, 
                                          date, 
                                          match_api_id, 
                                          home_team_api_id, 
                                          away_team_api_id, 
                                          home_team_goal, 
                                          away_team_goal) %>%
  mutate(home_team_win = ifelse(home_team_goal > away_team_goal, 1, 0),
         draw = ifelse(home_team_goal == away_team_goal, 1, 0),
         away_team_win = ifelse(home_team_goal < away_team_goal, 1, 0),
         result = ifelse(home_team_goal > away_team_goal, "H", ifelse(home_team_goal < away_team_goal, "A", "D"))) 

#Summary of the home team
home_team_summary <- match_result %>% 
  group_by(home_team_api_id , season) %>%
  summarize(home_team_wins = sum(home_team_win), 
            draw = sum(draw), 
            away_team_wins = sum(away_team_win),
            total_matches = n(),
            goals = sum(home_team_goal)) %>%
  mutate(perc_home_win = home_team_wins/total_matches, mean_goals_h = goals/total_matches, perc_home_draw = draw/total_matches)
away_team_summary <- match_result %>%
  group_by(away_team_api_id, season) %>%
  summarize(home_team_wins = sum(home_team_win),
            draw= sum(draw),
            away_team_wins = sum(away_team_win),
            total_matches = n(),
            goals = sum(away_team_goal)) %>%
  mutate(perc_away_win = away_team_wins/total_matches, mean_goals_a = goals/total_matches, perc_away_draw = draw/total_matches)

# Creating a database of home teams and another one of the away teams
home_team_summary <- home_team_summary %>% rename(team_api_id = home_team_api_id)
away_team_summary <- away_team_summary %>% rename(team_api_id = away_team_api_id)
perc_home_win <- home_team_summary %>% dplyr::select(team_api_id, season, perc_home_win, perc_home_draw)
perc_away_win <- away_team_summary %>% dplyr::select(team_api_id, season, perc_away_win, perc_away_draw)
mean_goals_home <- home_team_summary %>% dplyr::select(team_api_id, season, mean_goals_h)
mean_goals_away <- away_team_summary %>% dplyr::select(team_api_id, season, mean_goals_a)

team_database_home <- teams %>% dplyr::select(team_api_id, team_long_name, team_short_name) %>%
  left_join(perc_home_win) %>%
  left_join(mean_goals_home) %>%
  rename(home_team_api_id = team_api_id)

# Database of away team
team_database_away <- teams %>% dplyr::select(team_api_id, team_long_name, team_short_name) %>%
  left_join(perc_away_win) %>%
  left_join(mean_goals_away) %>%
  rename(away_team_api_id = team_api_id)

all_match <- match_result

all_match %>% summarize(perc_home = sum(home_team_win/n()),
                        perc_away = sum(away_team_win/n()),
                        perc_draw = sum(draw/n()))

all_home_database <- all_match %>% 
  group_by(home_team_api_id, season) %>% 
  summarize(home_w = sum(home_team_win)/n(),
            home_t = sum(draw)/n(),
            home_l = sum(away_team_win)/n(),
            h_goal = mean(home_team_goal),
            h_r_goal = mean(away_team_goal)) %>%
  mutate(h_rel_goal = h_goal/h_r_goal)

all_away_database <- all_match %>% 
  group_by(away_team_api_id, season) %>% 
  summarize(away_w = sum(away_team_win)/n(),
            away_t = sum(draw)/n(),
            away_l = sum(home_team_win)/n(),
            a_goal = mean(away_team_goal),
            a_r_goal = mean(home_team_goal)) %>%
  mutate(a_rel_goal = a_goal/a_r_goal)

all_home_win <- all_home_database %>% 
  dplyr::select(season, home_team_api_id, home_w, h_goal, h_rel_goal, h_r_goal)
all_away_win <- all_away_database %>% 
  dplyr::select(season, away_team_api_id, away_w, a_goal, a_rel_goal,a_r_goal)

# Create a dataset with the master of all matches and the variables of home_winning
h_all_win <- all_match %>%
  dplyr::select(season, home_team_api_id, away_team_api_id, home_team_win, away_team_win) %>%
  left_join(all_home_win) %>%
  left_join(all_away_win)

h_all_win <- h_all_win %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))
dbDisconnect(database)
#########################################################################################################


load("svm_model_h.rda")
load("svm_model_a.rda")

predictOdds = function(team1,team2)
{
  
  h_team_id = teams %>% filter(team_long_name == team1) %>% dplyr::select(team_api_id)
  a_team_id = teams %>% filter(team_long_name == team2) %>% dplyr::select(team_api_id)
  
  match_details = h_all_win %>% filter(home_team_api_id == h_team_id[1,1] & away_team_api_id == a_team_id[1,1])
  y_hat_svm_h <- predict(fit_svm_h, match_details, probability=T)
  
  # print(match_details)  
  probabilities_h = data.frame(attr(y_hat_svm_h, "probabilities"))
  h_prob_h_lose = mean(as.numeric(probabilities_h$X0))
  h_prob_a_lose = mean(as.numeric(probabilities_h$X1))
  
  y_hat_svm_a <- predict(fit_svm_a, match_details, probability=T)
  
  probabilities_a = data.frame(attr(y_hat_svm_a, "probabilities"))
  a_prob_h_lose = mean(as.numeric(probabilities_a$X0))
  a_prob_a_lose = mean(as.numeric(probabilities_a$X1))
  
  draw_prob = abs(1- round((h_prob_a_lose+a_prob_h_lose),1))
  
  home_win_odds = fractions(round(h_prob_a_lose,2)/(1-round(h_prob_a_lose,2)))
  away_win_odds = fractions(round(a_prob_h_lose,2)/(1-round(a_prob_h_lose,2)))
  draw_odds = fractions(draw_prob/(1-draw_prob))
  
  if(!grepl("/", as.character(home_win_odds)) ){
    home_win_odds = paste0(home_win_odds,"/1")
  }
  
  if(!grepl("/", as.character(away_win_odds)) ){
    away_win_odds = paste0(away_win_odds,"/1")
  }
  
  if(!grepl("/", as.character(draw_odds)) ){
    draw_odds = paste0(draw_odds,"/1")
  }
  returnList = list("home_win_odds" = as.character(home_win_odds),"away_win_odds" = as.character(away_win_odds),"draw_odds" = as.character(draw_odds))
  return(returnList)
}


radar_characteristics = c("Str","Pas","Pac","Jum","Fin","Dri","Agg","Acc","Han","Kic")
teamOptimizer = function(optimization_df = df,
                         num_def = 3,
                         num_mid = 4,
                         num_fwd = 3,
                         max_cost = 3000000){

  num_gk = 1
  # Create vectors to constrain by position
  optimization_df$Goalkeeper = ifelse(optimization_df$New_Position == "GK", 1, 0)
  optimization_df$Defender = ifelse(optimization_df$New_Position == "Def", 1, 0)
  optimization_df$Midfielder = ifelse(optimization_df$New_Position == "Mid", 1, 0)
  optimization_df$Forward = ifelse(optimization_df$New_Position == "ST", 1, 0)
  # Create vector to constrain by max number of players allowed per team

  # next we need the constraint directions
  const_dir <- c("=", "=", "=", "=", "<=")
  
  # The vector to optimize against
  objective = optimization_df$CA
  # objective = optimization_df$pointCostOptimizer
  
  # Put the complete matrix together
  const_mat = matrix(c(optimization_df$Goalkeeper, optimization_df$Defender, optimization_df$Midfielder, optimization_df$Forward,
                       optimization_df$Wage),
                     nrow=(5),
                     byrow=TRUE)
  const_rhs = c(num_gk, num_def, num_mid, num_fwd, max_cost)
  
  # And solve the linear system
  x = lp ("max", objective, const_mat, const_dir, const_rhs, all.bin=TRUE, all.int=TRUE)
  ################Return value #######################
  opt_df = (arrange(optimization_df[which(x$solution==1),], desc(Goalkeeper), desc(Defender), desc(Midfielder), desc(Forward), desc(CA)))
  opt_df = opt_df[c("Name","Club","Height","Weight","Age","Nation","CA","Preferred.Foot","Best.Pos","Value")]
  colnames(opt_df) = c("Name","Club","Height","Weight","Age","Nation","Last Known Ability","Foot","Position","Value")
  return(opt_df)
}

# jscode <- "
# shinyjs.toggleBox = function(boxid) {
# $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
# };
# shinyjs.collapseBox = function(boxid) {
# $('#' + boxid).collapse('hide')};
# shinyjs.expandBox = $function(boxid) {
# $('#' + boxid).collapse('show')};
# "
# jscode <- "
# shinyjs.collapse = function(boxid) {
# $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
# }
# "

jscode <- "
shinyjs.toggle = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"
ui <- dashboardPage(header = dashboardHeader(disable = T),
                    sidebar = dashboardSidebar(disable = TRUE),scrollToTop = T,
      body =  dashboardBody(
        # class = "bodyClass",
        setBackgroundImage(
          src = "images/bg1.svg",
          shinydashboard = TRUE
        ),
        useShinyjs(),
        extendShinyjs(text=jscode, functions = c("toggle")),
        # div(style="font-size:20px",fluidRow(align="center",
        #          headerPanel('sphaera'))),
        navbarPage(title=div(style="align:center",p("sphaera")),theme = "bootstrap.css",collapsible = TRUE, 
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
        ),

           tabPanel("StatThat",
                    
                    fluidRow(align="center",
                             headerPanel('Player Comparison')),
                    div( style = "font-size:12px; color:white",fluidRow(align="left",
                             p('Select the players you wish to compare from the dropdown. 
                               Once selected, click on Compare button to compare the statistics of the players. 
                               This module provides the performance comparison of the players in different areas such as Strength, Pace, Acceleration and various others to give the user a comprehensive evaluation of the two players.'))),
                      # h2("Welcome to ",style="display:inline;color:white"), h1("Soccer Player", style="display:inline;color:white")),
                    fluidRow(align="center",
                             flowLayout(
                      selectizeInput(
                      inputId = "player1",
                      label = "",
                      multiple = FALSE,
                      choices = NULL,
                      options = list(
                        create = FALSE,
                        placeholder = "Search for player",
                        maxItems = '1',
                         onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                         onType = I("function (str) {if (str === '') {this.close();}}")
                      )
                    ),
                    
                    selectizeInput(
                      inputId = "player2",
                      label = "",
                      multiple = FALSE,
                      choices = NULL,
                      options = list(
                        create = FALSE,
                        placeholder = "Search for player",
                        maxItems = '1',
                        onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                        onType = I("function (str) {if (str === '') {this.close();}}")
                      )
                    )),
                    actionButton("compareBtn","Compare!"),#actionButton("wtf","Go"),
                    hr()
                    ),
                      div(id="compBox",
                        class="radarClass",
                        box(
                          id = "compareBox",
                          title = "", 
                          closable = FALSE, 
                          width = 12,
                          # height = "500px",
                          solidHeader = FALSE, 
                          collapsible = TRUE,
                          collapsed = TRUE,
                          alpha=0.5,
                          #################
                          # sidebar = boxSidebar(
                          #   id = "mycardsidebar",
                          #   width = 50,
                          #   flowLayout(
                          #     div(
                          #       # h4("Name",align="center"),
                          #       h4(textOutput("player1op"),align="center"),
                          #       h5("Club",align="center"),
                          #       p(textOutput("p1_club"),align="center"),
                          #       h5("Division",align="center"),
                          #       p(textOutput("p1_division"),align="center"),
                          #       h5("Height",align="center"),
                          #       p(textOutput("p1_height"),align="center"),
                          #       h5("Weight",align="center"),
                          #       p(textOutput("p1_weight"),align="center"),
                          #       h5("Age",align="center"),
                          #       p(textOutput("p1_age"),align="center"),
                          #       h5("Nation",align="center"),
                          #       p(textOutput("p1_nation"),align="center"),
                          #       h5("Foot",align="center"),
                          #       p(textOutput("p1_foot"),align="center"),
                          #       h5("Position",align="center"),
                          #       p(textOutput("p1_pos"),align="center"),
                          #       h5("Value(in $)",align="center"),
                          #       p(textOutput("p1_value"),align="center")
                          #       ,style="display:inline"
                          #     ),
                          #     div(
                          #       # h4("Name",align="center"),
                          #       h4(textOutput("player2op"),align="center"),
                          #       h5("Club",align="center"),
                          #       p(textOutput("p2_club"),align="center"),
                          #       h5("Division",align="center"),
                          #       p(textOutput("p2_division"),align="center"),
                          #       h5("Height",align="center"),
                          #       p(textOutput("p2_height"),align="center"),
                          #       h5("Weight",align="center"),
                          #       p(textOutput("p2_weight"),align="center"),
                          #       h5("Age",align="center"),
                          #       p(textOutput("p2_age"),align="center"),
                          #       h5("Nation",align="center"),
                          #       p(textOutput("p2_nation"),align="center"),
                          #       h5("Foot",align="center"),
                          #       p(textOutput("p2_foot"),align="center"),
                          #       h5("Position",align="center"),
                          #       p(textOutput("p2_pos"),align="center"),
                          #       h5("Value(in $)",align="center"),
                          #       p(textOutput("p2_value"),align="center")
                          #       ,style="display:inline"
                          #     )
                          # )
                          # ),
                          #################
                            tags$div(class = "radarClass",
                                     splitLayout(
                                       tags$div(align="center",tableOutput("player1stats"),style="color:#272a31;font-size: 14px;overflow: hidden;"),#color:#536433
                                       tags$div(style="overflow: hidden;",plotOutput("hist")),
                                       tags$div(align="center",tableOutput("player2stats"),style="color:#272a31;font-size: 14px;overflow: hidden;") #color:#272a31
                            )
                          )
                        )
                    )
                    
                    ),
           tabPanel("Augury",
                    verticalLayout(
                      verticalLayout(fluidRow(align="center",
                                 headerPanel('Augury')), 
                                 div(style="color:#ffffff; font-size:12px",
                                     fluidRow(align="left",p(
                                   "It is fairly tough for an average individual to predict the outcome of a soccer match.
                                   This tool provides user the ability to get an estimate of the odds of any team winning or the odds of the match ending in a draw.
                                   To use this, select the league in which the desired match is played.
                                  Search and select the home and away teams. Click on predict button to generate the odds of the match.
                                   "
                                 ))),
                                 div(style="color:#ffffff; font-size:12px",
                                     fluidRow(align="left",p(
                                       "Note: This prediction is generated on match data for 8 years. 
                                       Predictions provided are an estimate and may not match with the score exactly.
                                       This is due to various factors affecting the outcome of any soccer match.
                                   "
                                     )))
                               ),
                    
                        sidebarLayout(
                          sidebarPanel(
                            class="optClass",
                            fluidRow(align="center",
                                     selectizeInput(
                                       inputId = "league",
                                       label = "League",
                                       multiple = FALSE,
                                       choices = NULL,
                                       options = list(
                                         create = FALSE,
                                         placeholder = "Search for League",
                                         maxItems = '1',
                                         onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                         onType = I("function (str) {if (str === '') {this.close();}}")
                                       )
                                     )
                                     ),
                            selectizeInput(
                              inputId = "team1",
                              label = "Home team",
                              multiple = FALSE,
                              choices = NULL,
                              options = list(
                                create = FALSE,
                                placeholder = "Search for Team",
                                maxItems = '1',
                                onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                onType = I("function (str) {if (str === '') {this.close();}}")
                              )
                            ),
                            selectizeInput(
                              inputId = "team2",
                              label = "Away team",
                              multiple = FALSE,
                              choices = NULL,
                              options = list(
                                create = FALSE,
                                placeholder = "Search for Team",
                                maxItems = '1',
                                onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                                onType = I("function (str) {if (str === '') {this.close();}}")
                              )
                            ),
                            actionButton("predictBtn","Predict!")
                          ),
                          mainPanel(
                            splitLayout(
                              div( style = "overflow: hidden;",slickROutput("slickr", width = 200,height = 500)),
                            div( style = "overflow: hidden;",
                              verticalLayout(
                              hidden(valueBoxOutput("homewin", width = 10),
                                valueBoxOutput("awaywin", width = 10),
                                valueBoxOutput("drawodds", width = 10)
                              )
                              )
                            )
                            ),
                        )
                      ),
                      div(style="color:#ffffff; font-size:12px",
                          fluidRow(align="left",
                                   p("Calculation of odds:"),
                            p("Odds refer to the chances of any team winning versus losing. 
                              They are directly related to the probability of an outcome. 
                              If the home team has 40% chances of winning(2/5), the odds would be calculated as 2:3."),
                          p(
                            "Odds of draw are calculated based on the chances of either team winning. 
                            Based on the probability of either team winning, 
                            probability of match ending in a draw are calculated as absoute value of 
                            sum of probabilities of either team winning subtracted from 1. 
                            This probability is then used to calculated the odds of draw in a 
                            similar fashion as the odds of either team winning.")
                      ))
                      
                    )
                    ),
          tabPanel("Optimus",
                   verticalLayout(
                   fluidRow(align="center",headerPanel('Team Optimization')),
                   div(style="color:#ffffff; font-size:12px",
                       fluidRow(align="left",p(
                         "How do you bet on the best players in a fantasy league to earn maximum points? Optimus gives you the opportunity to create your own dream team. 
                         Just select the league from which the players need to be selected.
                         Select the number of defenders(2-5), midfielders(2-5), attackers(2-5) adding up to a maximum of 10 players.
                         Click on Optimize button to get the best possible combination of players within your budget."
                       ))),
                   sidebarLayout(
                     sidebarPanel(
                       class="optClass",
                       selectInput('leagueOpt', 'Select League', c("All",distinct(df,Based))),
                       numericInput("numDef","Number of Defenders:",3,2,6,1),
                       numericInput("numMid","Number of Midfielders:",3,2,6,1),
                       numericInput("numAtt","Number of Forwards:",3,2,6,1),
                       numericInput("budget","Budget Constraint($):", value = 10000, min = 10000, step = 1000),
                       actionButton("optBtn","Optimize!")
                     ),
                     mainPanel(
                       div(class = "optTable",
                         tableOutput("optTeamOP"))
                    )
                 )#,
                 # 
                 # div(style="color:#ffffff; font-size:12px",
                 #     fluidRow(align="left",
                 #              p(""),
                 #              p(
                 #                "Odds of draw are calculated based on the chances of either team winning. 
                 #            Based on the probability of either team winning, 
                 #            probability of match ending in a draw are calculated as absoute value of 
                 #            sum of probabilities of either team winning subtracted from 1. 
                 #            This probability is then used to calculated the odds of draw in a 
                 #            similar fashion as the odds of either team winning.")
                 #     ))
                 ))#,
        # tabPanel("sphaera", class="title")
             
  ))

)



server <- function(input, output,session) {
  updateSelectizeInput(session, 'player1', choices = df$Name, server = TRUE)
  updateSelectizeInput(session, 'player2', choices = df$Name, server = TRUE)
  updateSelectizeInput(session, 'league', choices = league$name, server = TRUE)
  observeEvent(input$wtf,{ 
    print("in function")
    shinyjs::js$collapse("compareBox")
    })
  
  observeEvent(input$predictBtn,
               {
                 # jqui_hide("#homewin",effect="fade")
                 hide(id="homewin", time = 1,animType = "slide")
                 hide(id="awaywin", time = 1,animType = "slide")
                 hide(id="drawodds",time = 1, animType = "slide")
                 # jqui_hide("#awaywin",effect="fade")
                 # jqui_hide("#drawodds",effect="fade")
                 ls = predictOdds(input$team1,input$team2)
                 Sys.sleep(1)
                 # jqui_show(output$homewin,effect = "fade")
                 output$homewin = renderValueBox({
                   valueBox(
                     ls$home_win_odds, "Home Win Odds", icon = icon("bar-chart-o"),
                     color = "purple"
                   )
                 })
                 # show("homewin")
                 
                 output$awaywin = renderValueBox({
                   valueBox(
                     ls$away_win_odds, "Away Win Odds", icon = icon("bar-chart-o"),
                     color = "olive"
                   )
                 })
                 # show("awaywin")
                 
                 output$drawodds = renderValueBox({
                   valueBox(
                     ls$draw_odds, subtitle = "Draw Odds", icon = icon("bar-chart-o"),
                     color = "red"
                   )
                 })
                 # show("drawodds")
                 print("in function")
                 show(id = "homewin", anim = TRUE, time = 1, animType="slide")
                 show(id = "awaywin", anim = TRUE, time = 1, animType="slide")
                 show(id = "drawodds", anim = TRUE, time = 1, animType="slide")
                 
                 # jqui_show("#homewin",effect = "fade")
                 # jqui_show("#awaywin",effect = "fade")
                 # jqui_show("#drawodds",effect = "fade")
                 print("exiting function")
               })
  
  observeEvent(input$league,
               {
                 league = input$league
                 teamList = teamsInLeagues %>% filter(name == league) %>% dplyr::select(team_long_name)
                 updateSelectizeInput(session, 'team1', choices = teamList$team_long_name, server = TRUE)
                 updateSelectizeInput(session, 'team2', choices = teamList$team_long_name, server = TRUE)
               })
  observeEvent(input$compareBtn,
         {player1name = input$player1
          player2name = input$player2
          if(is.na(player1name) | player1name =="" | is.na(player2name) | player2name ==""){
            showNotification("Select Player!")
          }
          else{
             player1_details = df_mainAttrib %>% 
             filter(Name==player1name) %>%
             # top_n(1) %>%
             dplyr::select (c("Str","Pas","Pac","Jum","Fin","Dri","Agg","Acc","Han","Kic"))
           player2_details = df_mainAttrib %>% 
             filter(Name==player2name)%>% 
             # top_n(1) %>%
             dplyr::select (c("Str","Pas","Pac","Jum","Fin","Dri","Agg","Acc","Han","Kic"))
           radar_df = data.frame(player1_details[1,])
           radar_df = rbind(player2_details[1,],radar_df)
           radar_df <- rbind(rep(20,10) , rep(0,10) , radar_df)
           colnames(radar_df) = c("Strength","Passing","Pace","Jumping","Finishing","Dribbling","Aggression","Acceleration","Handling(GK)","Kicking(GK)")
           # print(radar_df)
           
           output$hist <- renderPlot(
             {
               radarchart( radar_df, axistype=1 ,
                           pcol=c(rgb(0,0,1,0.5),rgb(0,1,0,0.5)),
                           title = paste0(player1name," vs. ",player2name),
                           pfcol=c(rgb(0,0,1,0.35),rgb(0,1,0,0.35)) ,
                           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                           vlcex=1
               )
               legend("topleft",
                      legend = c(player2name,player1name),
                      bty = "n", pch = 20, col = c(rgb(0,0,1,0.35),rgb(0,1,0,0.35)),
                      text.col = "grey25", pt.cex = 2)
             }
           )
           
           player1_sidebar = df_mainAttrib %>% 
             filter(Name==player1name) %>% 
             dplyr::select (c("Name","Club","Division","Height","Weight","Age","Nation","Preferred.Foot","Best.Pos","Value","Wage"))
           player2_sidebar = df_mainAttrib %>% 
             filter(Name==player2name)%>% 
             dplyr::select (c("Name","Club","Division","Height","Weight","Age","Nation","Preferred.Foot","Best.Pos","Value","Wage"))
           player1_sidebar=player1_sidebar[1,]
           player2_sidebar=player2_sidebar[1,]
           output$player1stats = renderTable(t(player1_sidebar), rownames = TRUE, colnames = FALSE, width = "100%")
           output$player2stats = renderTable(t(player2_sidebar), rownames = TRUE, colnames = FALSE, width = "100%")
           
           # shinyjs::show(id="")
           jqui_show("#compareBox",effect = "fade")
           jqui_show("#player1stats",effect = "fade")
           jqui_show("#player2stats",effect = "fade")
           jqui_show("#hist",effect = "fade")
           
           shinyjs::js$toggle("compareBox")
           ######################
           # js$expandBox("Player Comparison")
           # output$player1op = renderText(player1_sidebar$Name)
           # output$p1_club = renderText(player1_sidebar$Club)
           # output$p1_division = renderText(player1_sidebar$Division)
           # output$p1_height = renderText(player1_sidebar$Height)
           # output$p1_weight = renderText(player1_sidebar$Weight)
           # output$p1_age = renderText(player1_sidebar$Age)
           # output$p1_nation = renderText(player1_sidebar$Nation)
           # output$p1_foot = renderText(player1_sidebar$Preferred.Foot)
           # output$p1_pos = renderText(player1_sidebar$Best.Pos)
           # output$p1_value = renderText(paste("$",player1_sidebar$Value))
           # 
           # output$player2op = renderText(player2_sidebar$Name)
           # output$p2_club = renderText(player2_sidebar$Club)
           # output$p2_division = renderText(player2_sidebar$Division)
           # output$p2_height = renderText(player2_sidebar$Height)
           # output$p2_weight = renderText(player2_sidebar$Weight)
           # output$p2_age = renderText(player2_sidebar$Age)
           # output$p2_nation = renderText(player2_sidebar$Nation)
           # output$p2_foot = renderText(player2_sidebar$Preferred.Foot)
           # output$p2_pos = renderText(player2_sidebar$Best.Pos)
           # output$p2_value = renderText(paste("$",player2_sidebar$Value))
           ######################
          }
        }
    )
  output$slickr <- renderSlickR({
    # imgs <- list.files("images/",  full.names = TRUE)#pattern=".jpg",
    # print(imgs)
    # slickR(imgs)+ settings(dots = TRUE, autoplay = TRUE, autoplaySpeed = 1000)
    slickR(list.files("players/", pattern=".jpg", full.names = TRUE))+ settings(autoplay = TRUE, autoplaySpeed = 1000)
  })
  observeEvent(input$numDef,{
    updateNumericInput(inputId = "numAtt",max = 10-(input$numDef + input$numMid))
    updateNumericInput(inputId = "numMid",max = 10-(input$numDef + input$numAtt))
    updateNumericInput(inputId = "numDef",max = 10-(input$numAtt + input$numMid))
  })
  observeEvent(input$numMid,{
    updateNumericInput(inputId = "numAtt",max = 10-(input$numDef + input$numMid))
    updateNumericInput(inputId = "numMid",max = 10-(input$numDef + input$numAtt))
    updateNumericInput(inputId = "numDef",max = 10-(input$numAtt + input$numMid))
  })
  observeEvent(input$numAtt,{
    updateNumericInput(inputId = "numAtt",max = 10-(input$numDef + input$numMid))
    updateNumericInput(inputId = "numMid",max = 10-(input$numDef + input$numAtt))
    updateNumericInput(inputId = "numDef",max = 10-(input$numAtt + input$numMid))
  })
  
  observeEvent(input$optBtn,{
    withProgress(message = 'Calculating...', value = 3,{
      
      lg = input$leagueOpt
      if(lg == "All"){
        optimized_team = teamOptimizer(df,input$numDef,input$numMid,input$numAtt)
        # optimized_team["Division"] = NULL
      }
      else{
        league_DF = df %>% filter(Based == lg)
        optimized_team = teamOptimizer(league_DF,input$numDef,input$numMid,input$numAtt)  
        # optimized_team["Division"] = NULL
      }
      
      output$optTeamOP = renderTable(optimized_team)
    })
    
  })
}

#.table {background-image: url(images/football.jpg)} background: #171717;
shinyApp(ui = ui, server = server)
