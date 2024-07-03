#This is the code to create 
#A) all the possible p values from the cox analysis and 
#B) all the possible HRs  
#C) all possible schoenfeld residuals 


# A - create all possible p values from cox analysis
library(survival)
#Create cox_p function
cox_p <- function(x){
  
  #create a dataframe FOR DR
  time <- c() #insert time
  no_events_group1 <- x
  no_censoring_group1 <- c() #insert number of censored obs in group1
  no_events_group2 <- c() #insert number of events in group2
  no_censoring_group2 <- c() #insert number of censored obs in group2
  
  group1_df <- data.frame(time, no_events_group1, no_censoring_group1)
  group2_df <- data.frame(time, no_events_group2, no_censoring_group2)
  
  #expand dataframe to make one event per row for KM analysis
  #group1
  # Create an empty dataframe to store the transformed data
  transformed_df_group1 <- data.frame()
  
  # Loop through each row of the original dataframe
  for (i in 1:nrow(group1_df)) {
    # Add rows for events
    transformed_df_group1 <- rbind(transformed_df_group1, data.frame(
      time = rep(group1_df$time[i], group1_df$no_events_group1[i]),  # Repeat time for each event
      event = rep(1,group1_df$no_events_group1[i])  # Indicator variable for events (1)
    ))
    
    # Add rows for censoring events
    transformed_df_group1 <- rbind(transformed_df_group1, data.frame(
      time = rep(group1_df$time[i], group1_df$no_censoring_group1[i]),  # Repeat time for each censoring event
      event = rep(0, group1_df$no_censoring_group1[i])  # Indicator variable for censoring (0)
    ))
  }
  
  #group2
  # Create an empty dataframe to store the transformed data
  transformed_df_group2 <- data.frame()
  
  # Loop through each row of the original dataframe
  for (i in 1:nrow(group2_df)) {
    # Add rows for events
    transformed_df_group2 <- rbind(transformed_df_group2, data.frame(
      time = rep(group2_df$time[i], group2_df$no_events_group2[i]),  # Repeat time for each event
      event = rep(1,group2_df$no_events_group2[i])  # Indicator variable for events (1)
    ))
    
    # Add rows for censoring events
    transformed_df_group2 <- rbind(transformed_df_group2, data.frame(
      time = rep(group2_df$time[i], group2_df$no_censoring_group2[i]),  # Repeat time for each censoring event
      event = rep(0, group2_df$no_censoring_group2[i])  # Indicator variable for censoring (0)
    ))
  }
  
  # Add a new variable 'group' to each dataframe to indicate the group
  transformed_df_group1$group <- "Group 1"
  transformed_df_group2$group <- "Group 2"
  
  # Combine the data from both dataframes
  combined_groups <- rbind(transformed_df_group1, transformed_df_group2)
  
  #cox p
  cox <- coxph(Surv(time, event)~group, data = combined_groups)
  summary_cox <- summary(cox)
  p <- summary_cox$coeggocients[, "Pr(>|z|)"]
  print(p)
}

# Creating function to loop logrank function over data frame rows
multi_cox <- function(x){
  x$p <- numeric(nrow(x))
  for (i in 1:nrow(x)) {
    vec <- unname(unlist(x[i,1:11]))
    x$p[i] <- cox_p(vec)
  }
  return(x)
}


# Running the rowwise loop over the data frame, and overwriting the result => create a df called input which shows possible combinations + respective p values
input <- multi_cox() #### insert dataframe that has possible combinations of events on each row for group 1
print(input)

#B create all possible cox HR
library(survival)
#Create cox_HR function
cox_HR <- function(x){
  
  #create a dataframe FOR DR
  time <- c() #insert time
  no_events_group1 <- x
  no_censoring_group1 <- c() #insert number of censored obs in group1
  no_events_group2 <- c() #insert number of events in group2
  no_censoring_group2 <- c() #insert number of censored obs in group2
  
  group1_df <- data.frame(time, no_events_group1, no_censoring_group1)
  group2_df <- data.frame(time, no_events_group2, no_censoring_group2)
  
  #expand dataframe to make one event per row for KM analysis
  #group1
  # Create an empty dataframe to store the transformed data
  transformed_df_group1 <- data.frame()
  
  # Loop through each row of the original dataframe
  for (i in 1:nrow(group1_df)) {
    # Add rows for events
    transformed_df_group1 <- rbind(transformed_df_group1, data.frame(
      time = rep(group1_df$time[i], group1_df$no_events_group1[i]),  # Repeat time for each event
      event = rep(1,group1_df$no_events_group1[i])  # Indicator variable for events (1)
    ))
    
    # Add rows for censoring events
    transformed_df_group1 <- rbind(transformed_df_group1, data.frame(
      time = rep(group1_df$time[i], group1_df$no_censoring_group1[i]),  # Repeat time for each censoring event
      event = rep(0, group1_df$no_censoring_group1[i])  # Indicator variable for censoring (0)
    ))
  }
  
  #group2
  # Create an empty dataframe to store the transformed data
  transformed_df_group2 <- data.frame()
  
  # Loop through each row of the original dataframe
  for (i in 1:nrow(group2_df)) {
    # Add rows for events
    transformed_df_group2 <- rbind(transformed_df_group2, data.frame(
      time = rep(group2_df$time[i], group2_df$no_events_group2[i]),  # Repeat time for each event
      event = rep(1,group2_df$no_events_group2[i])  # Indicator variable for events (1)
    ))
    
    # Add rows for censoring events
    transformed_df_group2 <- rbind(transformed_df_group2, data.frame(
      time = rep(group2_df$time[i], group2_df$no_censoring_group2[i]),  # Repeat time for each censoring event
      event = rep(0, group2_df$no_censoring_group2[i])  # Indicator variable for censoring (0)
    ))
  }
  
  # Add a new variable 'group' to each dataframe to indicate the group
  transformed_df_group1$group <- "Group 1"
  transformed_df_group2$group <- "Group 2"
  
  # Combine the data from both dataframes
  combined_groups <- rbind(transformed_df_group1, transformed_df_group2)
  
  #cox p
  cox <- coxph(Surv(time, event)~group, data = combined_groups)
  summary_cox <- summary(cox)
  HR <- summary_cox$coeggocients[, "exp(coeff)"]
  print(HR)
}

# Creating function to loop logrank function over data frame rows
multi_cox <- function(x){
  x$HR <- numeric(nrow(x))
  for (i in 1:nrow(x)) {
    vec <- unname(unlist(x[i,1:11]))
    x$p[i] <- cox_HR(vec)
  }
  return(x)
}


# Running the rowwise loop over the data frame, and overwriting the result => create a df called input which shows possible combinations + respective p values
input <- multi_cox() #### insert dataframe that has possible combinations of events on each row for group 1
print(input)

#C. create all possible schoenfeld residuals 
library(survival)
#Create cox_HR function
cox_resid <- function(x){
  
  #create a dataframe FOR DR
  time <- c() #insert time
  no_events_group1 <- x
  no_censoring_group1 <- c() #insert number of censored obs in group1
  no_events_group2 <- c() #insert number of events in group2
  no_censoring_group2 <- c() #insert number of censored obs in group2
  
  group1_df <- data.frame(time, no_events_group1, no_censoring_group1)
  group2_df <- data.frame(time, no_events_group2, no_censoring_group2)
  
  #expand dataframe to make one event per row for KM analysis
  #group1
  # Create an empty dataframe to store the transformed data
  transformed_df_group1 <- data.frame()
  
  # Loop through each row of the original dataframe
  for (i in 1:nrow(group1_df)) {
    # Add rows for events
    transformed_df_group1 <- rbind(transformed_df_group1, data.frame(
      time = rep(group1_df$time[i], group1_df$no_events_group1[i]),  # Repeat time for each event
      event = rep(1,group1_df$no_events_group1[i])  # Indicator variable for events (1)
    ))
    
    # Add rows for censoring events
    transformed_df_group1 <- rbind(transformed_df_group1, data.frame(
      time = rep(group1_df$time[i], group1_df$no_censoring_group1[i]),  # Repeat time for each censoring event
      event = rep(0, group1_df$no_censoring_group1[i])  # Indicator variable for censoring (0)
    ))
  }
  
  #group2
  # Create an empty dataframe to store the transformed data
  transformed_df_group2 <- data.frame()
  
  # Loop through each row of the original dataframe
  for (i in 1:nrow(group2_df)) {
    # Add rows for events
    transformed_df_group2 <- rbind(transformed_df_group2, data.frame(
      time = rep(group2_df$time[i], group2_df$no_events_group2[i]),  # Repeat time for each event
      event = rep(1,group2_df$no_events_group2[i])  # Indicator variable for events (1)
    ))
    
    # Add rows for censoring events
    transformed_df_group2 <- rbind(transformed_df_group2, data.frame(
      time = rep(group2_df$time[i], group2_df$no_censoring_group2[i]),  # Repeat time for each censoring event
      event = rep(0, group2_df$no_censoring_group2[i])  # Indicator variable for censoring (0)
    ))
  }
  
  # Add a new variable 'group' to each dataframe to indicate the group
  transformed_df_group1$group <- "Group 1"
  transformed_df_group2$group <- "Group 2"
  
  # Combine the data from both dataframes
  combined_groups <- rbind(transformed_df_group1, transformed_df_group2)
  
  #cox p
  cox <- coxph(Surv(time, event)~group, data = combined_groups)
  resid <- cox.zph(cox)
 p_values <- resid$table[, "p"]
  print(p_values)
}

# Creating function to loop logrank function over data frame rows
multi_cox <- function(x){
  x$resid_p <- numeric(nrow(x))
  for (i in 1:nrow(x)) {
    vec <- unname(unlist(x[i,1:11]))
    x$resid_p[i] <- cox_resid(vec)
  }
  return(x)
}


# Running the rowwise loop over the data frame, and overwriting the result => create a df called input which shows possible combinations + respective p values
input <- multi_cox() #### insert dataframe that has possible combinations of events on each row for group 1
print(input)