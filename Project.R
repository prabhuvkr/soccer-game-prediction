library(lubridate)
library(dplyr)
library(RSQLite)
library(ggplot2)
library(reshape2)
library(ggrepel)
library(dummies)
library(OptimalCutpoints)
library(xgboost)
library(caret)
library(pROC)
sqlite_drv <- dbDriver("SQLite")
sql_db <- dbConnect(sqlite_drv,"C:/Users/prabh/Documents/Assignment/Machine Learning/Project/63_589_bundle_archive/database.sqlite")
match_df <- dbGetQuery(sql_db,"select * from Match")
team_df <- dbGetQuery(sql_db,"select * from Team")
team_attributes <- dbGetQuery(sql_db,"select * from Team_Attributes")
country_df <- dbGetQuery(sql_db,"select * from Country")
league_df <- dbGetQuery(sql_db,"select * from League")
player_df <- dbGetQuery(sql_db,"select * from Player")

#Reorganizing to create a data frame with match stats by home/away
match_df_imp <- match_df[,1:11]
away_team_matches <- merge(team_df,match_df_imp,by.x="team_api_id",by.y="away_team_api_id")
home_team_matches <- merge(team_df,match_df_imp,by.x="team_api_id",by.y="home_team_api_id")
home_team_matches <- subset(home_team_matches,select=-c(id.x,id.y))
away_team_matches <- subset(away_team_matches,select=-c(id.x,id.y))
colnames(home_team_matches)[11:13] <- c("opponent_team_id","goals_scored","goals_conceded")
colnames(away_team_matches)[11:13] <- c("opponent_team_id","goals_conceded","goals_scored")
home_team_matches <- cbind(home_team_matches,side="home")
away_team_matches <- cbind(away_team_matches,side="away")
all_matches <- rbind(home_team_matches,away_team_matches)
all_matches <- all_matches %>% mutate(result=ifelse(goals_scored > goals_conceded,2,ifelse(goals_scored < goals_conceded,0,1))) %>% mutate(year = substring(date,1,4))

#Creating data frame for team stats over the entire period of the dataset
team_stats <- all_matches %>% group_by(team_api_id,team_long_name,league_id) %>% summarise(matches=n(),h_matches=length(result[side=="home"]),a_matches=length(result[side=="away"]),
                                                                                           tot_scored=sum(goals_scored),home_scored=sum(goals_scored[side=="home"]),away_scored=sum(goals_scored[side=="away"]),
                                                                                           tot_conceded = sum(goals_conceded),home_conceded=sum(goals_conceded[side=="home"]),away_conceded = sum(goals_conceded[side=="away"]),
                                                                                           wins=length(result[result=="win"]),losses=length(result[result=="loss"]),draws=length(result[result=="draw"]),
                                                                                           h_wins=length(result[result=="win" & side=="home"]), a_wins=length(result[result=="win" & side=="away"]),
                                                                                           h_loss=length(result[result=="loss" & side=="home"]), a_loss=length(result[result=="loss" & side=="away"]),
                                                                                           mean_goals=mean(goals_scored),var_goals=var(goals_scored), win_pct=wins/matches,loss_pct=losses/matches,
                                                                                           hwin_pct=h_wins/h_matches, awin_pct=a_wins/a_matches)

team_stats <- inner_join(ungroup(team_stats),league_df,by=c("league_id"="id")) %>% select(-one_of("country_id","league_id")) %>% rename(league_name=name)

#Creating data frame for team stats grouped by year
team_stats_byYear <- all_matches %>% group_by(team_api_id,team_long_name,league_id, year) %>% summarise(matches=n(),h_matches=length(result[side=="home"]),a_matches=length(result[side=="away"]),
                                                                                                        tot_scored=sum(goals_scored),home_scored=sum(goals_scored[side=="home"]),away_scored=sum(goals_scored[side=="away"]),
                                                                                                        tot_conceded = sum(goals_conceded),home_conceded=sum(goals_conceded[side=="home"]),away_conceded = sum(goals_conceded[side=="away"]),
                                                                                                        wins=length(result[result=="win"]),losses=length(result[result=="loss"]),draws=length(result[result=="draw"]),
                                                                                                        h_wins=length(result[result=="win" & side=="home"]), a_wins=length(result[result=="win" & side=="away"]),
                                                                                                        h_loss=length(result[result=="loss" & side=="home"]), a_loss=length(result[result=="loss" & side=="away"]),
                                                                                                        mean_goals=mean(goals_scored),var_goals=var(goals_scored), win_pct=wins/matches,loss_pct=losses/matches,
                                                                                                        hwin_pct=h_wins/h_matches, awin_pct=a_wins/a_matches)

team_attributes <- team_attributes %>% mutate(year = substring(date,1,4))
team_attributes_stats <- inner_join(team_stats_byYear,team_attributes, by = c("team_api_id" = "team_api_id", "year" = "year"))
team_attributes_stats <- inner_join(ungroup(team_attributes_stats),league_df,by=c("league_id"="id")) %>% select(-one_of("country_id","league_id")) %>% rename(league_name=name)


#Avg goals scored vs Win percentage
graph1 <- ggplot(data = team_attributes_stats, aes(x = mean_goals, y = win_pct*100)) +
  geom_line(alpha = 0.5, size = 1) + geom_smooth(alpha = 0.5, size = 1) +
  labs(x = "Avg Goals scored", y = "Win Percentage") +
  theme_set(theme_bw(base_size = 22) ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank()) 
graph1


#Frequency distribution of the number of teams based on their winning percentages shows that only 33 teams among the 299 teams, win more than 50% of the time.
plot_wpct_freq <- 
  ggplot(team_attributes_stats,aes(win_pct*100))+geom_area(stat="bin",bins=10,fill="light blue")+xlab("Winning percentage") + facet_grid(~ league_name) + theme_set(theme_bw(base_size = 10) ) 
plot_wpct_freq

#Home win percentage
plot_wpct_freq <- 
  ggplot(team_attributes_stats,aes(hwin_pct*100))+geom_area(stat="bin",bins=10,fill="light blue")+xlab("Winning percentage") + labs(title = "Home Games")
plot_wpct_freq

#Away win percentage
plot_wpct_freq <- 
  ggplot(team_stats,aes(awin_pct*100))+geom_area(stat="bin",bins=10,fill="light blue")+xlab("Winning percentage") + labs(title = "Away Games")
plot_wpct_freq

#Build Up Play Passing vs Win percentage
graph6 <- ggplot(data = team_attributes_stats, aes(x = buildUpPlayPassing, y = win_pct*100)) +
  geom_line(alpha = 0.5, size = 1) + geom_smooth(alpha = 0.5, size = 1) +
  labs(x = "Build Up Play Passing", y = "Win Percentage") +
  theme_set(theme_bw(base_size = 22) ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank()) 
graph6


#Other graphs for finding vizual patterns among variables
graph2 <- ggplot(data = team_attributes_stats, aes(x = buildUpPlaySpeed, y = win_pct*100)) +
  geom_line(alpha = 0.5, size = 1) + geom_smooth(alpha = 0.5, size = 1) +
  labs(x = "Avg Goals scored", y = "Win Percentage") +
  theme_set(theme_bw(base_size = 22) ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank()) 
graph2

graph3 <- ggplot(data = team_attributes_stats, aes(x = defenceAggression, y = win_pct*100)) +
  geom_line(alpha = 0.5, size = 1) + geom_smooth(alpha = 0.5, size = 1) +
  labs(x = "Avg Goals scored", y = "Win Percentage") +
  theme_set(theme_bw(base_size = 22) ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank()) 
graph3

graph4 <- ggplot(data = team_attributes_stats, aes(x = chanceCreationShooting, y = win_pct*100)) +
  geom_line(alpha = 0.5, size = 1) + geom_smooth(alpha = 0.5, size = 1) +
  labs(x = "Avg Goals scored", y = "Win Percentage") +
  theme_set(theme_bw(base_size = 22) ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank()) 
graph4

graph5 <- ggplot(data = team_attributes_stats, aes(x = chanceCreationPassing, y = win_pct*100)) +
  geom_line(alpha = 0.5, size = 1) + geom_smooth(alpha = 0.5, size = 1) +
  labs(x = "Avg Goals scored", y = "Win Percentage") +
  theme_set(theme_bw(base_size = 22) ) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank()) 
graph5

#Merging data frames
match_df_imp <- match_df_imp %>% mutate(year = substring(date,1,4))
match_df_imp <- left_join(match_df_imp, league_df, by=c("league_id"="id")) %>% rename(league_name = name) %>% select(-c("country_id.x","country_id.y")) 
match_df_imp <- match_df_imp %>% mutate(home_result=ifelse(home_team_goal > away_team_goal,2,ifelse(home_team_goal < away_team_goal,0,1))) 


match_teamstats <- left_join(match_df_imp, team_attributes_stats, by=c("home_team_api_id"="team_api_id", "year" = "year")) %>%
                   select(-c("a_matches","away_scored","away_conceded","a_wins","a_loss","awin_pct")) 
                   
match_teamstats <- match_teamstats %>% rename(h_overallmatches = matches,
                                              h_tot_scored = tot_scored,
                          h_team_long_name = team_long_name,
                          h_tot_conceded = tot_conceded,
                          h_overallwins = wins,                           
                          h_overalllosses = losses,                         
                          h_overalldraws = draws,                          
                          h_mean_goals = mean_goals,                     
                          h_var_goals = var_goals,                     
                          h_overallwin_pct = win_pct,                        
                          h_overallloss_pct = loss_pct,                       
                          h_buildUpPlaySpeed = buildUpPlaySpeed,               
                          h_buildUpPlaySpeedClass	= buildUpPlaySpeedClass,          
                          h_buildUpPlayDribbling	= buildUpPlayDribbling,           
                          h_buildUpPlayDribblingClass	= buildUpPlayDribblingClass,      
                          h_buildUpPlayPassing	= buildUpPlayPassing,             
                          h_buildUpPlayPassingClass = buildUpPlayPassingClass,        
                          h_buildUpPlayPositioningClass =	buildUpPlayPositioningClass,    
                          h_chanceCreationPassing	=	chanceCreationPassing,          
                          h_chanceCreationPassingClass =	chanceCreationPassingClass,     
                          h_chanceCreationCrossing = chanceCreationCrossing,         
                          h_chanceCreationCrossingClass =	chanceCreationCrossingClass,    
                          h_chanceCreationShooting =	chanceCreationShooting,         
                          h_chanceCreationShootingClass =	chanceCreationShootingClass,    
                          h_chanceCreationPositioningClass = chanceCreationPositioningClass, 
                          h_defencePressure =	defencePressure,                
                          h_defencePressureClass =	defencePressureClass,           
                          h_defenceAggression =	defenceAggression,              
                          h_defenceAggressionClass =	defenceAggressionClass,         
                          h_defenceTeamWidth =	defenceTeamWidth,               
                          h_defenceTeamWidthClass =	defenceTeamWidthClass,          
                          h_defenceDefenderLineClass =	defenceDefenderLineClass)

match_teamstats <- left_join(match_teamstats, team_attributes_stats, by=c("away_team_api_id"="team_api_id", "year" = "year")) %>%
  select(-c("h_matches.y","home_scored.y","home_conceded.y","h_wins.y","h_loss.y","hwin_pct.y")) 

match_teamstats <- match_teamstats %>% rename(a_overallmatches = matches,
                                              a_tot_scored = tot_scored,
                                              a_team_long_name = team_long_name,
                                              a_tot_conceded = tot_conceded,
                                              a_overallwins = wins,                           
                                              a_overalllosses = losses,                         
                                              a_overalldraws = draws,                          
                                              a_mean_goals = mean_goals,                     
                                              a_var_goals = var_goals,                     
                                              a_overallwin_pct = win_pct,                        
                                              a_overallloss_pct = loss_pct,                       
                                              a_buildUpPlaySpeed = buildUpPlaySpeed,               
                                              a_buildUpPlaySpeedClass	= buildUpPlaySpeedClass,          
                                              a_buildUpPlayDribbling	= buildUpPlayDribbling,           
                                              a_buildUpPlayDribblingClass	= buildUpPlayDribblingClass,      
                                              a_buildUpPlayPassing	= buildUpPlayPassing,             
                                              a_buildUpPlayPassingClass = buildUpPlayPassingClass,        
                                              a_buildUpPlayPositioningClass =	buildUpPlayPositioningClass,    
                                              a_chanceCreationPassing	=	chanceCreationPassing,          
                                              a_chanceCreationPassingClass =	chanceCreationPassingClass,     
                                              a_chanceCreationCrossing = chanceCreationCrossing,         
                                              a_chanceCreationCrossingClass =	chanceCreationCrossingClass,    
                                              a_chanceCreationShooting =	chanceCreationShooting,         
                                              a_chanceCreationShootingClass =	chanceCreationShootingClass,    
                                              a_chanceCreationPositioningClass = chanceCreationPositioningClass, 
                                              a_defencePressure =	defencePressure,                
                                              a_defencePressureClass =	defencePressureClass,           
                                              a_defenceAggression =	defenceAggression,              
                                              a_defenceAggressionClass =	defenceAggressionClass,         
                                              a_defenceTeamWidth =	defenceTeamWidth,               
                                              a_defenceTeamWidthClass =	defenceTeamWidthClass,          
                                              a_defenceDefenderLineClass =	defenceDefenderLineClass)

glimpse(match_teamstats)

match_teamstats <- match_teamstats %>% select(-c(id.y, date.y, team_fifa_api_id.x, league_name.y, id.x, team_fifa_api_id.y, date.x,league_name.x, league_name.y))

match_teamstats <- match_teamstats[!with(match_teamstats,is.na(h_team_long_name) | is.na(a_team_long_name)),]
match_teamstats <- match_teamstats %>% select(1:1, league_name, everything())
match_teamstats <- match_teamstats %>% select(1:12, a_team_long_name, everything())
match_teamstats <- match_teamstats %>% select(-c(home_team_goal, away_team_goal))
match_teamstats <- match_teamstats %>% select(-c(league_name, h_team_long_name, a_team_long_name, season))
match_teamstats <- match_teamstats %>% select(0:0, home_result, everything()) %>% select(-c(match_api_id))
summary(as.factor(match_teamstats$home_result))

#Converting Categoroical variables to factor
match_teamstats <- mutate_if(match_teamstats, is.character, as.factor)
match_teamstats <- mutate_if(match_teamstats, is.factor, as.numeric)
glimpse(match_teamstats)

summary(as.factor(match_teamstats$home_result))
                                                 
#Proceeding wiht match_teamstats as the final dataset

#Creating training and test datasets
set.seed(190694) # Set random number generator seed for reproducability
test_ind_1 <- sample(1:9121, 9121 * 0.2, replace = FALSE) # Select test samples
test_ind_2 <- test_ind_1 + 9121 # Select counter part bouts
test_ind <- c(test_ind_1, test_ind_2) # Join test indices

train_data <- match_teamstats[-test_ind, c(1:82)] # Create training data
test_data <- match_teamstats[test_ind, c(1:82)] # Create test data

summary(as.factor(train_data$home_result))
summary(as.factor(test_data$home_result))

#Balancing Dataset
zero_weight <- 6714/4181 # Calculate proportion of positive samples in data
one_weight <- 6714/3700
#three_weight <- 7881/6714
weight_vec <- rep(1, nrow(train_data)) # Create weight vector
weight_vec[which(train_data$home_result == 0)] <- zero_weight 
weight_vec[which(train_data$home_result == 1)] <- one_weight 
#weight_vec[which(train_data$home_result == 3)] <- three_weight # Assign weight for negative samples
sum(weight_vec[which(train_data$home_result == 0)]) # Check 1 class weight
sum(weight_vec[which(train_data$home_result == 1)])
sum(weight_vec[which(train_data$home_result == 2)])

#Using XGBoost

# Create training matrix
dtrain <- xgb.DMatrix(data = as.matrix(train_data[, 2:82]), label = as.numeric(train_data$home_result) )
# Create test matrix
dtest <- xgb.DMatrix(data = as.matrix(test_data[, 2:82]), label = as.numeric(test_data$home_result) )

set.seed(898765)
bst_1 <- xgboost(data = dtrain, # Set training data
                 
                 eta = 0.05, # Set learning rate
                 max.depth =  5, # Set max depth
                 min_child_weight = 5, # Set minimum number of samples in node to split
                 gamma = 0, # Set minimum loss reduction for split
                 subsample =  0.9, # Set proportion of training data to use in tree
                 colsample_bytree = 0.9, # Set number of variables to use in each tree
                 
                 nrounds = 2000, # Set number of rounds
                 early_stopping_rounds = 100, # Set number of rounds to stop at if there is no improvement
                 
                 verbose = 1, # 1 - Prints out fit
                 #nthread = 1, # Set number of parallel threads
                 print_every_n = 20, # Prints out result every 20th iteration
                 weight = weight_vec,
                 objective = "multi:softmax", 
                 num_class = 3,
                 eval_metric = "mlogloss",
                 eval_metric = "merror") # Set evaluation metric to use

boost_preds <- predict(bst_1, dtrain) # Create predictions for xgboost model


pred_dat <- cbind.data.frame(boost_preds, train_data$home_result)#
names(pred_dat) <- c("predictions", "response")

boost_preds_1 <- predict(bst_1, dtest) # Create predictions for xgboost model

pred_dat <- cbind.data.frame(boost_preds_1 , test_data$home_result)#
# Convert predictions to classes, using optimal cut-off
#boost_pred_class <- rep(0, length(boost_preds_1))
#boost_pred_class[boost_preds_1 >= oc$MaxEfficiency$Global$optimal.cutoff$cutoff[1]] <- 1

boost_pred_class <- pred_dat[,1]

u <- union(boost_pred_class,  test_data$home_result) # Join factor levels
t <- table(factor(boost_pred_class, u), factor(test_data$home_result, u)) # Create table
confusionMatrix(t) # Produce confusion matrix

#Tuning XGBoost
#Optimal solution reached at 62 so setting early_stopping_rounds = 70
# Be Careful - This can take a very long time to run
max_depth_vals <- c(3, 5, 7, 10, 15) # Create vector of max depth values
min_child_weight <- c(1,3,5,7, 10, 15) # Create vector of min child values

# Expand grid of parameter values
cv_params <- expand.grid(max_depth_vals, min_child_weight)
names(cv_params) <- c("max_depth", "min_child_weight")
# Create results vector
auc_vec <- error_vec <- rep(NA, nrow(cv_params)) 
# Loop through results
for(i in 1:nrow(cv_params)){
  set.seed(111111)
  bst_tune <- xgb.cv(data = dtrain, # Set training data
                     
                     nfold = 5, # Use 5 fold cross-validation
                     
                     eta = 0.1, # Set learning rate
                     max.depth = cv_params$max_depth[i], # Set max depth
                     min_child_weight = cv_params$min_child_weight[i], # Set minimum number of samples in node to split
                     
                     
                     nrounds = 100, # Set number of rounds
                     early_stopping_rounds = 70, # Set number of rounds to stop at if there is no improvement
                     
                     verbose = 1, # 1 - Prints out fit
                     nthread = 1, # Set number of parallel threads
                     print_every_n = 20, # Prints out result every 20th iteration
                     
                     weight = weight_vec,
                     objective = "multi:softmax", 
                     num_class = 3,
                     eval_metric = "mlogloss",
                     eval_metric = "merror") # Set evaluation metric to use
  auc_vec[i] <- bst_tune$evaluation_log$test_mlogloss_mean[bst_tune$best_ntreelimit]
  error_vec[i] <- bst_tune$evaluation_log$test_merror_mean[bst_tune$best_ntreelimit]
  
}

# Join results in dataset
res_db <- cbind.data.frame(cv_params, auc_vec, error_vec)
names(res_db)[3:4] <- c("auc", "error") 
res_db$max_depth <- as.factor(res_db$max_depth) # Convert tree number to factor for plotting
res_db$min_child_weight <- as.factor(res_db$min_child_weight) # Convert node size to factor for plotting
# Print AUC heatmap
g_2 <- ggplot(res_db, aes(y = max_depth, x = min_child_weight, fill = auc)) + # set aesthetics
  geom_tile() + # Use geom_tile for heatmap
  theme_bw() + # Set theme
  scale_fill_gradient2(low = "blue", # Choose low color
                       mid = "white", # Choose mid color
                       high = "red", # Choose high color
                       midpoint =mean(res_db$auc), # Choose mid point
                       space = "Lab", 
                       na.value ="grey", # Choose NA value
                       guide = "colourbar", # Set color bar
                       aesthetics = "fill") + # Select aesthetics to apply
  labs(x = "Minimum Child Weight", y = "Max Depth", fill = "Mlogloss") # Set labels
g_2

# print error heatmap
g_3 <- ggplot(res_db, aes(y = max_depth, x = min_child_weight, fill = error)) + # set aesthetics
  geom_tile() + # Use geom_tile for heatmap
  theme_bw() + # Set theme
  scale_fill_gradient2(low = "blue", # Choose low color
                       mid = "white", # Choose mid color
                       high = "red", # Choose high color
                       midpoint =mean(res_db$error), # Choose mid point
                       space = "Lab", 
                       na.value ="grey", # Choose NA value
                       guide = "colourbar", # Set color bar
                       aesthetics = "fill") + # Select aesthetics to apply
  labs(x = "Minimum Child Weight", y = "Max Depth", fill = "Error") # Set labels
g_3 # Generate plot

#Max Depth 5, Minimum Child width 7
gamma_vals <- c(0, 0.05, 0.1, 0.15, 0.2) # Create vector of gamma values

# Be Careful - This can take a very long time to run
set.seed(111111)
auc_vec <- error_vec <- rep(NA, length(gamma_vals))
for(i in 1:length(gamma_vals)){
  bst_tune <- xgb.cv(data = dtrain, # Set training data
                     
                     nfold = 5, # Use 5 fold cross-validation
                     
                     eta = 0.1, # Set learning rate
                     max.depth = 5, # Set max depth
                     min_child_weight = 7, # Set minimum number of samples in node to split
                     gamma = gamma_vals[1], # Set minimum loss reduction for split
                     
                     
                     
                     nrounds = 109, # Set number of rounds
                     early_stopping_rounds = 70, # Set number of rounds to stop at if there is no improvement
                     
                     verbose = 1, # 1 - Prints out fit
                     nthread = 1, # Set number of parallel threads
                     print_every_n = 20, # Prints out result every 20th iteration
                     
                     weight = weight_vec,
                     objective = "multi:softmax", 
                     num_class = 3,
                     eval_metric = "mlogloss",
                     eval_metric = "merror") # Set evaluation metric to use
  auc_vec[i] <- bst_tune$evaluation_log$test_mlogloss_mean[bst_tune$best_ntreelimit]
  error_vec[i] <- bst_tune$evaluation_log$test_merror_mean[bst_tune$best_ntreelimit]
  
}

# Join gamma to values
cbind.data.frame(gamma_vals, auc_vec, error_vec)

#using gamma 0.15
# Use xgb.cv to run cross-validation inside xgboost
set.seed(111111)
bst <- xgb.cv(data = dtrain, # Set training data
              
              nfold = 5, # Use 5 fold cross-validation
              
              eta = 0.1, # Set learning rate
              max.depth = 5, # Set max depth
              min_child_weight = 7, # Set minimum number of samples in node to split
              gamma = 0.15, # Set minimum loss reduction for split
              
              
              nrounds = 1000, # Set number of rounds
              early_stopping_rounds = 50, # Set number of rounds to stop at if there is no improvement
              
              verbose = 1, # 1 - Prints out fit
              nthread = 1, # Set number of parallel threads
              print_every_n = 20, # Prints out result every 20th iteration
              
              weight = weight_vec,
              objective = "multi:softmax", 
              num_class = 3,
              eval_metric = "mlogloss",
              eval_metric = "merror") # Set evaluation metric to use

#using stopping round = 30
# Be Careful - This can take a very long time to run
subsample <- c(0.6, 0.7, 0.8, 0.9, 1) # Create vector of subsample values
colsample_by_tree <- c(0.6, 0.7, 0.8, 0.9, 1) # Create vector of col sample values

# Expand grid of tuning parameters
cv_params <- expand.grid(subsample, colsample_by_tree)
names(cv_params) <- c("subsample", "colsample_by_tree")
# Create vectors to store results
auc_vec <- error_vec <- rep(NA, nrow(cv_params)) 
# Loop through parameter values
for(i in 1:nrow(cv_params)){
  set.seed(111111)
  bst_tune <- xgb.cv(data = dtrain, # Set training data
                     
                     nfold = 5, # Use 5 fold cross-validation
                     
                     eta = 0.1, # Set learning rate
                     max.depth = 5, # Set max depth
                     min_child_weight = 7, # Set minimum number of samples in node to split
                     gamma = 0.15, # Set minimum loss reduction for split
                     subsample = cv_params$subsample[i], # Set proportion of training data to use in tree
                     colsample_bytree = cv_params$colsample_by_tree[i], # Set number of variables to use in each tree
                     
                     nrounds = 150, # Set number of rounds
                     early_stopping_rounds = 30, # Set number of rounds to stop at if there is no improvement
                     
                     verbose = 1, # 1 - Prints out fit
                     nthread = 1, # Set number of parallel threads
                     print_every_n = 20, # Prints out result every 20th iteration
                     
                     weight = weight_vec,
                     objective = "multi:softmax", 
                     num_class = 3,
                     eval_metric = "mlogloss",
                     eval_metric = "merror") # Set evaluation metric to use
  auc_vec[i] <- bst_tune$evaluation_log$test_mlogloss_mean[bst_tune$best_ntreelimit]
  error_vec[i] <- bst_tune$evaluation_log$test_merror_mean[bst_tune$best_ntreelimit]
  
}

res_db <- cbind.data.frame(cv_params, auc_vec, error_vec)
names(res_db)[3:4] <- c("auc", "error") 
res_db$subsample <- as.factor(res_db$subsample) # Convert tree number to factor for plotting
res_db$colsample_by_tree <- as.factor(res_db$colsample_by_tree) # Convert node size to factor for plotting
g_4 <- ggplot(res_db, aes(y = colsample_by_tree, x = subsample, fill = auc)) + # set aesthetics
  geom_tile() + # Use geom_tile for heatmap
  theme_bw() + # Set theme
  scale_fill_gradient2(low = "blue", # Choose low color
                       mid = "white", # Choose mid color
                       high = "red", # Choose high color
                       midpoint =mean(res_db$auc), # Choose mid point
                       space = "Lab", 
                       na.value ="grey", # Choose NA value
                       guide = "colourbar", # Set color bar
                       aesthetics = "fill") + # Select aesthetics to apply
  labs(x = "Subsample", y = "Column Sample by Tree", fill = "Mlogloss") # Set labels
g_4 # Generate plot

g_5 <- ggplot(res_db, aes(y = colsample_by_tree, x = subsample, fill = error)) + # set aesthetics
  geom_tile() + # Use geom_tile for heatmap
  theme_bw() + # Set theme
  scale_fill_gradient2(low = "blue", # Choose low color
                       mid = "white", # Choose mid color
                       high = "red", # Choose high color
                       midpoint =mean(res_db$error), # Choose mid point
                       space = "Lab", 
                       na.value ="grey", # Choose NA value
                       guide = "colourbar", # Set color bar
                       aesthetics = "fill") + # Select aesthetics to apply
  labs(x = "Subsample", y = "Column Sample by Tree", fill = "Error") # Set labels
g_5 # Generate plot

#Subsample = 1, ColSample by tree = 0.8
# Use xgb.cv to run cross-validation inside xgboost
set.seed(111111)
bst_mod_1 <- xgb.cv(data = dtrain, # Set training data
                    
                    nfold = 5, # Use 5 fold cross-validation
                    
                    eta = 0.3, # Set learning rate
                    max.depth = 5, # Set max depth
                    min_child_weight = 5, # Set minimum number of samples in node to split
                    gamma = 0.15, # Set minimum loss reduction for split
                    subsample = 1, # Set proportion of training data to use in tree
                    colsample_bytree =  0.8, # Set number of variables to use in each tree
                    
                    nrounds = 1000, # Set number of rounds
                    early_stopping_rounds = 30, # Set number of rounds to stop at if there is no improvement
                    
                    verbose = 1, # 1 - Prints out fit
                    nthread = 1, # Set number of parallel threads
                    print_every_n = 20, # Prints out result every 20th iteration
                    
                    weight = weight_vec,
                    objective = "multi:softmax", 
                    num_class = 3,
                    eval_metric = "mlogloss",
                    eval_metric = "merror") # Set evaluation metric to use

set.seed(111111)
bst_mod_2 <- xgb.cv(data = dtrain, # Set training data
                    
                    nfold = 5, # Use 5 fold cross-validation
                    
                    eta = 0.1, # Set learning rate
                    max.depth = 5, # Set max depth
                    min_child_weight = 5, # Set minimum number of samples in node to split
                    gamma = 0.15, # Set minimum loss reduction for split
                    subsample = 1, # Set proportion of training data to use in tree
                    colsample_bytree =  0.8, # Set number of variables to use in each tree
                    
                    nrounds = 1000, # Set number of rounds
                    early_stopping_rounds = 30, # Set number of rounds to stop at if there is no improvement
                    
                    verbose = 1, # 1 - Prints out fit
                    nthread = 1, # Set number of parallel threads
                    print_every_n = 20, # Prints out result every 20th iteration
                    
                    weight = weight_vec,
                    objective = "multi:softmax", 
                    num_class = 3,
                    eval_metric = "mlogloss",
                    eval_metric = "merror")

set.seed(111111)
bst_mod_3 <- xgb.cv(data = dtrain, # Set training data
                    
                    nfold = 5, # Use 5 fold cross-validation
                    
                    eta = 0.05, # Set learning rate
                    max.depth = 5, # Set max depth
                    min_child_weight = 5, # Set minimum number of samples in node to split
                    gamma = 0.15, # Set minimum loss reduction for split
                    subsample = 1, # Set proportion of training data to use in tree
                    colsample_bytree =  0.8, # Set number of variables to use in each tree
                    
                    nrounds = 1000, # Set number of rounds
                    early_stopping_rounds = 30, # Set number of rounds to stop at if there is no improvement
                    
                    verbose = 1, # 1 - Prints out fit
                    nthread = 1, # Set number of parallel threads
                    print_every_n = 20, # Prints out result every 20th iteration
                    
                    weight = weight_vec,
                    objective = "multi:softmax", 
                    num_class = 3,
                    eval_metric = "mlogloss",
                    eval_metric = "merror")

set.seed(111111)
bst_mod_4 <- xgb.cv(data = dtrain, # Set training data
                    
                    nfold = 5, # Use 5 fold cross-validation
                    
                    eta = 0.01, # Set learning rate
                    max.depth = 5, # Set max depth
                    min_child_weight = 5, # Set minimum number of samples in node to split
                    gamma = 0.15, # Set minimum loss reduction for split
                    subsample = 1, # Set proportion of training data to use in tree
                    colsample_bytree =  0.8, # Set number of variables to use in each tree
                    
                    nrounds = 1000, # Set number of rounds
                    early_stopping_rounds = 30, # Set number of rounds to stop at if there is no improvement
                    
                    verbose = 1, # 1 - Prints out fit
                    nthread = 1, # Set number of parallel threads
                    print_every_n = 20, # Prints out result every 20th iteration
                    
                    weight = weight_vec,
                    objective = "multi:softmax", 
                    num_class = 3,
                    eval_metric = "mlogloss",
                    eval_metric = "merror")

set.seed(111111)
bst_mod_5 <- xgb.cv(data = dtrain, # Set training data
                    
                    nfold = 5, # Use 5 fold cross-validation
                    
                    eta = 0.005, # Set learning rate
                    max.depth = 5, # Set max depth
                    min_child_weight = 5, # Set minimum number of samples in node to split
                    gamma = 0.15, # Set minimum loss reduction for split
                    subsample = 1, # Set proportion of training data to use in tree
                    colsample_bytree =  0.8, # Set number of variables to use in each tree
                    
                    nrounds = 1000, # Set number of rounds
                    early_stopping_rounds = 30, # Set number of rounds to stop at if there is no improvement
                    
                    verbose = 1, # 1 - Prints out fit
                    nthread = 1, # Set number of parallel threads
                    print_every_n = 20, # Prints out result every 20th iteration
                    
                    weight = weight_vec,
                    objective = "multi:softmax", 
                    num_class = 3,
                    eval_metric = "mlogloss",
                    eval_metric = "merror")

# Extract results for model with eta = 0.3
pd1 <- cbind.data.frame(bst_mod_1$evaluation_log[,c("iter", "test_merror_mean")], rep(0.3, nrow(bst_mod_1$evaluation_log)))
names(pd1)[3] <- "eta"
# Extract results for model with eta = 0.1
pd2 <- cbind.data.frame(bst_mod_2$evaluation_log[,c("iter", "test_merror_mean")], rep(0.1, nrow(bst_mod_2$evaluation_log)))
names(pd2)[3] <- "eta"
# Extract results for model with eta = 0.05
pd3 <- cbind.data.frame(bst_mod_3$evaluation_log[,c("iter", "test_merror_mean")], rep(0.05, nrow(bst_mod_3$evaluation_log)))
names(pd3)[3] <- "eta"
# Extract results for model with eta = 0.01
pd4 <- cbind.data.frame(bst_mod_4$evaluation_log[,c("iter", "test_merror_mean")], rep(0.01, nrow(bst_mod_4$evaluation_log)))
names(pd4)[3] <- "eta"
# Extract results for model with eta = 0.005
pd5 <- cbind.data.frame(bst_mod_5$evaluation_log[,c("iter", "test_merror_mean")], rep(0.005, nrow(bst_mod_5$evaluation_log)))
names(pd5)[3] <- "eta"
# Join datasets
plot_data <- rbind.data.frame(pd1, pd2, pd3, pd4, pd5)
# Converty ETA to factor
plot_data$eta <- as.factor(plot_data$eta)
# Plot points
g_6 <- ggplot(plot_data, aes(x = iter, y = test_merror_mean, color = eta))+
  geom_point(alpha = 0.5) +
  theme_bw() + # Set theme
  theme(panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) + # Remove grid 
  labs(x = "Number of Trees", title = "Error Rate v Number of Trees",
       y = "Error Rate", color = "Learning \n Rate")  # Set labels
g_6

# Plot lines
g_7 <- ggplot(plot_data, aes(x = iter, y = test_merror_mean, color = eta))+
  geom_smooth(alpha = 0.5) +
  theme_bw() + # Set theme
  theme(panel.grid.major = element_blank(), # Remove grid
        panel.grid.minor = element_blank(), # Remove grid
        panel.border = element_blank(), # Remove grid
        panel.background = element_blank()) + # Remove grid 
  labs(x = "Number of Trees", title = "Error Rate v Number of Trees",
       y = "Error Rate", color = "Learning \n Rate")  # Set labels
g_7


#eta 0.1
set.seed(473479)
bst_final <- xgboost(data = dtrain, # Set training data
                    
                    eta = 0.1, # Set learning rate
                    max.depth = 5, # Set max depth
                    min_child_weight = 7, # Set minimum number of samples in node to split
                    gamma = 0.15, # Set minimum loss reduction for split
                    subsample = 1, # Set proportion of training data to use in tree
                    colsample_bytree =  0.8, # Set number of variables to use in each tree
                    
                    nrounds = 100, # Set number of rounds
                    early_stopping_rounds = 40, # Set number of rounds to stop at if there is no improvement
                    
                    verbose = 1, # 1 - Prints out fit
                    nthread = 1, # Set number of parallel threads
                    print_every_n = 20, # Prints out result every 20th iteration
                    
                    weight = weight_vec,
                    objective = "multi:softmax", 
                    num_class = 3,
                    eval_metric = "mlogloss",
                    eval_metric = "merror")


boost_preds <- predict(bst_final, dtrain) # Create predictions for xgboost model


pred_dat <- cbind.data.frame(boost_preds, train_data$home_result)#
names(pred_dat) <- c("predictions", "response")

boost_preds_1 <- predict(bst_final, dtest) # Create predictions for xgboost model

pred_dat <- cbind.data.frame(boost_preds_1 , test_data$home_result)#
# Convert predictions to classes, using optimal cut-off
#boost_pred_class <- rep(0, length(boost_preds_1))
#boost_pred_class[boost_preds_1 >= oc$MaxEfficiency$Global$optimal.cutoff$cutoff[1]] <- 1

boost_pred_class <- pred_dat[,1]

u <- union(boost_pred_class,  test_data$home_result) # Join factor levels
t <- table(factor(boost_pred_class, u), factor(test_data$home_result, u)) # Create table
confusionMatrix(t) # Produce confusion matrix

# Extract importance
imp_mat <- xgb.importance(model = bst_final)
# Plot importance (top 10 variables)
xgb.plot.importance(imp_mat, top_n = 10)


library(DiagrammeR)

xgb.plot.tree(model = bst_final, trees = 0:0)
