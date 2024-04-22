########This is the code for creating a dataframe of all the possible p values with their respective combinations of events in one group (group1 in this case) where the rows in a dataframe created by the method described in combinations.R is used

library(survival)
#Create logrank function
logrank_p <- function(x){
  
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
  
  #log rank
  result <- survdiff(Surv(time, event) ~ group, data = combined_groups)
  print(result$pvalue)
}

# Creating function to loop logrank function over data frame rows
multi_logrank <- function(x){
  x$p <- numeric(nrow(x))
  for (i in 1:nrow(x)) {
    vec <- unname(unlist(x[i,1:11]))
    x$p[i] <- logrank_p(vec)
  }
  return(x)
}


# Running the rowwise loop over the data frame, and overwriting the result => create a df called input which shows possible combinations + respective p values
input <- multi_logrank() #### insert dataframe that has possible combinations of events on each row for group 1
print(input)
