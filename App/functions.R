
  
  
  func_plot_axis_format <-
function(){
  
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"))
}
func_country_profile <-
function(data){
  
  country <- data %>% distinct(country) %>% pull()
  coach   <- data %>% distinct(coach) %>% pull()
  captain <- data %>% 
    filter(captain == "Yes") %>% pull(player) %>% str_remove("captain") %>% str_remove("\\(") %>% str_remove("\\)")
  
  return(
    str_glue("Country: {country} | Coach: {coach} |  Captain: {captain}")
  )
}
func_plot_age_distribution <-
function(data){
  
  min_age    <- data$age %>% min()
  max_age    <- data$age %>% max()
  median_age <- data$age %>% median()
  
  data %>% 
    mutate(position = position %>% fct_relevel("Goalkeepers", "Defenders", "Midfielders", "Forwards") %>% fct_rev()) %>% 
   
     # geoms
    ggplot(aes(age, position))+
    geom_boxplot()+
    geom_jitter(size = 3)+

    # theme
    theme_minimal()+
    
    # format
    theme(axis.text.x = element_text(size = 10, color = "black"),
          axis.text.y = element_text(size = 10, color = "black"))+
    
    # labs
    labs(title = "Age Distribution",
         subtitle = str_glue("Min: {min_age} | Median: {median_age} | Max: {max_age}"))
  
  
  
}
func_plot_axis_format <-
function(){
  
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"))
}
func_country_profile <-
function(data){
  
  country <- data %>% distinct(country) %>% pull()
  coach   <- data %>% distinct(coach) %>% pull()
  captain <- data %>% 
    filter(captain == "Yes") %>% pull(player) %>% str_remove("captain") %>% str_remove("\\(") %>% str_remove("\\)")
  
  return(
    str_glue("Country: {country} | Coach: {coach} |  Captain: {captain}")
  )
}
func_plot_age_distribution <-
function(data){
  
  min_age    <- data$age %>% min()
  max_age    <- data$age %>% max()
  median_age <- data$age %>% median()
  
  data %>% 
    mutate(position = position %>% fct_relevel("Goalkeepers", "Defenders", "Midfielders", "Forwards") %>% fct_rev()) %>% 
   
     # geoms
    ggplot(aes(age, position))+
    geom_boxplot()+
    geom_jitter(size = 3)+

    # theme
    theme_minimal()+
    
    # format
    theme(axis.text.x = element_text(size = 10, color = "black"),
          axis.text.y = element_text(size = 10, color = "black"))+
    
    # labs
    labs(title = "Age Distribution",
         subtitle = str_glue("Min: {min_age} | Median: {median_age} | Max: {max_age}"))
  
  
  
}
func_plot_axis_format <-
function(){
  
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"))
}
func_country_profile <-
function(data){
  
  country <- data %>% distinct(country) %>% pull()
  coach   <- data %>% distinct(coach) %>% pull()
  captain <- data %>% 
    filter(captain == "Yes") %>% pull(player) %>% str_remove("captain") %>% str_remove("\\(") %>% str_remove("\\)")
  
  return(
    str_glue("Country: {country} | Coach: {coach} |  Captain: {captain}")
  )
}
func_plot_age_distribution <-
function(data){
  
  min_age    <- data$age %>% min()
  max_age    <- data$age %>% max()
  median_age <- data$age %>% median()
  
  data %>% 
    mutate(position = position %>% fct_relevel("Goalkeepers", "Defenders", "Midfielders", "Forwards") %>% fct_rev()) %>% 
   
     # geoms
    ggplot(aes(age, position))+
    geom_boxplot()+
    geom_jitter(size = 3)+

    # theme
    theme_minimal()+
    
    # format
    theme(axis.text.x = element_text(size = 10, color = "black"),
          axis.text.y = element_text(size = 10, color = "black"))+
    
    # labs
    labs(title = "Age Distribution",
         subtitle = str_glue("Min: {min_age} | Median: {median_age} | Max: {max_age}"),
         y = "")
  
  
  
}
func_plot_axis_format <-
function(){
  
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"))
}
func_country_profile <-
function(data){
  
  country <- data %>% distinct(country) %>% pull()
  coach   <- data %>% distinct(coach) %>% pull()
  captain <- data %>% 
    filter(captain == "Yes") %>% pull(player) %>% str_remove("captain") %>% str_remove("\\(") %>% str_remove("\\)")
  
  return(
    str_glue("Country: {country} | Coach: {coach} |  Captain: {captain}")
  )
}
func_plot_age_distribution <-
function(data){
  
  min_age    <- data$age %>% min()
  max_age    <- data$age %>% max()
  median_age <- data$age %>% median()
  
  data %>% 
    mutate(position = position %>% fct_relevel("Goalkeepers", "Defenders", "Midfielders", "Forwards") %>% fct_rev()) %>% 
   
     # geoms
    ggplot(aes(age, position))+
    geom_boxplot()+
    geom_jitter(size = 3)+

    # theme
    theme_minimal()+
    
    # format
    theme(axis.text.x = element_text(size = 10, color = "black"),
          axis.text.y = element_text(size = 10, color = "black"))+
    
    # labs
    labs(title = "Age Distribution",
         subtitle = str_glue("Min: {min_age} | Median: {median_age} | Max: {max_age}"),
         y = "")
  
  
  
}
func_default_message <-
function(data){
  
  goals_desc <- data %>% arrange(desc(goals))
  caps_desc  <- data %>% arrange(desc(caps))
  
  top_scorer <- goals_desc[1,]$player
  no_goals   <- goals_desc[1,]$goals
  top_caps   <- caps_desc[1,]$player
  no_caps    <- caps_desc[1,]$caps
  
  default_message <- str_glue("Top Goal Scorer: {top_scorer} with {no_goals}
                              Top Caps: {top_caps} with {no_caps} caps")
  
  return(default_message)
  
}
func_player_profile <-
function(data){
  
  # Get Top Caps
  top_caps <- data %>% 
    arrange(desc(caps))
  
  # Top Caps: Names
  top_cap_1_player <- top_caps[1,]$player
  top_cap_2_player <- top_caps[2,]$player
  top_cap_3_player <- top_caps[3,]$player
  
  # Top Caps: Caps
  top_cap_1 <- top_caps[1,]$caps
  top_cap_2 <- top_caps[2,]$caps
  top_cap_3 <- top_caps[3,]$caps

  if (top_cap_1 == top_cap_2 & top_cap_2 == top_cap_3){
    
    top_cap_output <- str_glue("Top Caps: {top_cap_1_player} & {top_cap_2_player} & {top_cap_3_Player} with {top_cap_1} caps")
    
  } else if(top_cap_1 == top_cap_2){
    
    top_cap_output <- str_glue("Top Caps: {top_cap_1_player} & {top_cap_2_player} with {top_cap_1} caps")
    
  } else {
    
    top_cap_output <- str_glue("Top Caps: {top_cap_1_player} with {top_cap_1} caps")
    
  }
  
  # Get Top Scorers
  top_goals <- data %>% 
    arrange(desc(goals))
  
  # Top Goals: Names
  top_goals_1_player <- top_goals[1,]$player
  top_goals_2_player <- top_goals[2,]$player
  top_goals_3_player <- top_goals[3,]$player
  
  # Top Coals: Goals
  top_goals_1 <- top_goals[1,]$goals
  top_goals_2 <- top_goals[2,]$goals
  top_goals_3 <- top_goals[3,]$goals
  
  if (top_goals_1 == top_goals_2 & top_goals_2 == top_goals_3){
    
    top_goals_output <- str_glue("Top Goal Scorers: {top_goals_1_player} & {top_goals_2_player} & {top_goals_3_player} with {top_goals_1} goals")
    
  } else if(top_goals_1_player == top_goals_2_player){
    
    top_goals_output <- str_glue("Top Goal Scorers: {top_goals_1_player} & {top_goals_2_player} with {top_goals_1} goals")
    
  } else {
    
    top_goals_output <- str_glue("Top Goal Scorers: {top_goals_1_player} with {top_goals_1} goals")
    
  }
  
  return(list(top_cap_output, top_goals_output))
  
}
func_plot_axis_format <-
function(){
  
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"))
}
func_country_profile <-
function(data){
  
  country <- data %>% distinct(country) %>% pull()
  coach   <- data %>% distinct(coach) %>% pull()
  captain <- data %>% 
    filter(captain == "Yes") %>% pull(player) %>% str_remove("captain") %>% str_remove("\\(") %>% str_remove("\\)")
  
  return(
    str_glue("Country: {country} | Coach: {coach} |  Captain: {captain}")
  )
}
func_plot_age_distribution <-
function(data){
  
  min_age    <- data$age %>% min()
  max_age    <- data$age %>% max()
  median_age <- data$age %>% median()
  
  data %>% 
    mutate(position = position %>% fct_relevel("Goalkeepers", "Defenders", "Midfielders", "Forwards") %>% fct_rev()) %>% 
   
     # geoms
    ggplot(aes(age, position))+
    geom_boxplot()+
    geom_jitter(size = 3)+

    # theme
    theme_minimal()+
    
    # format
    theme(axis.text.x = element_text(size = 10, color = "black"),
          axis.text.y = element_text(size = 10, color = "black"))+
    
    # labs
    labs(title = "Age Distribution",
         subtitle = str_glue("Min: {min_age} | Median: {median_age} | Max: {max_age}"),
         y = "")
  
  
  
}
func_default_message <-
function(data){
  
  goals_desc <- data %>% arrange(desc(goals))
  caps_desc  <- data %>% arrange(desc(caps))
  
  top_scorer <- goals_desc[1,]$player
  no_goals   <- goals_desc[1,]$goals
  top_caps   <- caps_desc[1,]$player
  no_caps    <- caps_desc[1,]$caps
  
  default_message <- str_glue("Top Goal Scorer: {top_scorer} with {no_goals}
                              Top Caps: {top_caps} with {no_caps} caps")
  
  return(default_message)
  
}
func_player_profile <-
function(data){
  
  # Get Top Caps
  top_caps <- data %>% 
    arrange(desc(caps))
  
  # Top Caps: Names
  top_cap_1_player <- top_caps[1,]$player
  top_cap_2_player <- top_caps[2,]$player
  top_cap_3_player <- top_caps[3,]$player
  
  # Top Caps: Caps
  top_cap_1 <- top_caps[1,]$caps
  top_cap_2 <- top_caps[2,]$caps
  top_cap_3 <- top_caps[3,]$caps

  if (top_cap_1 == top_cap_2 & top_cap_2 == top_cap_3){
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} & {top_cap_2_player} & {top_cap_3_Player} with {top_cap_1} caps")
    
  } else if(top_cap_1 == top_cap_2){
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} & {top_cap_2_player} with {top_cap_1} caps")
    
  } else {
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} with {top_cap_1} caps")
    
  }
  
  # Get Top Scorers
  top_goals <- data %>% 
    arrange(desc(goals))
  
  # Top Goals: Names
  top_goals_1_player <- top_goals[1,]$player
  top_goals_2_player <- top_goals[2,]$player
  top_goals_3_player <- top_goals[3,]$player
  
  # Top Coals: Goals
  top_goals_1 <- top_goals[1,]$goals
  top_goals_2 <- top_goals[2,]$goals
  top_goals_3 <- top_goals[3,]$goals
  
  if (top_goals_1 == top_goals_2 & top_goals_2 == top_goals_3){
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} & {top_goals_2_player} & {top_goals_3_player} with {top_goals_1} goals")
    
  } else if(top_goals_1_player == top_goals_2_player){
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} & {top_goals_2_player} with {top_goals_1} goals")
    
  } else {
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} with {top_goals_1} goals")
    
  }
  
  return(list(top_cap_output, top_goals_output))
  
}
func_plot_axis_format <-
function(){
  
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"))
}
func_country_profile <-
function(data){
  
  country <- data %>% distinct(country) %>% pull()
  coach   <- data %>% distinct(coach) %>% pull()
  captain <- data %>% 
    filter(captain == "Yes") %>% pull(player) %>% str_remove("captain") %>% str_remove("\\(") %>% str_remove("\\)")
  
  return(
    str_glue("Country: {country} | Coach: {coach} |  Captain: {captain}")
  )
}
func_plot_age_distribution <-
function(data){
  
  min_age    <- data$age %>% min()
  max_age    <- data$age %>% max()
  median_age <- data$age %>% median()
  
  data %>% 
    mutate(position = position %>% fct_relevel("Goalkeepers", "Defenders", "Midfielders", "Forwards") %>% fct_rev()) %>% 
   
     # geoms
    ggplot(aes(age, position))+
    geom_boxplot()+
    geom_jitter(size = 3)+

    # theme
    theme_minimal()+
    
    # format
    theme(axis.text.x = element_text(size = 10, color = "black"),
          axis.text.y = element_text(size = 10, color = "black"))+
    
    # labs
    labs(title = "Age Distribution",
         subtitle = str_glue("Min: {min_age} | Median: {median_age} | Max: {max_age}"),
         y = "")
  
  
  
}
func_default_message <-
function(data){
  
  goals_desc <- data %>% arrange(desc(goals))
  caps_desc  <- data %>% arrange(desc(caps))
  
  top_scorer <- goals_desc[1,]$player
  no_goals   <- goals_desc[1,]$goals
  top_caps   <- caps_desc[1,]$player
  no_caps    <- caps_desc[1,]$caps
  
  default_message <- str_glue("Country: All Countries
                              Top Goal Scorer: {top_scorer} with {no_goals}
                              Top Caps: {top_caps} with {no_caps} caps")
  
  return(default_message)
  
}
func_player_profile <-
function(data){
  
  # Get Top Caps
  top_caps <- data %>% 
    arrange(desc(caps))
  
  # Top Caps: Names
  top_cap_1_player <- top_caps[1,]$player
  top_cap_2_player <- top_caps[2,]$player
  top_cap_3_player <- top_caps[3,]$player
  
  # Top Caps: Caps
  top_cap_1 <- top_caps[1,]$caps
  top_cap_2 <- top_caps[2,]$caps
  top_cap_3 <- top_caps[3,]$caps

  if (top_cap_1 == top_cap_2 & top_cap_2 == top_cap_3){
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} & {top_cap_2_player} & {top_cap_3_Player} with {top_cap_1} caps")
    
  } else if(top_cap_1 == top_cap_2){
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} & {top_cap_2_player} with {top_cap_1} caps")
    
  } else {
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} with {top_cap_1} caps")
    
  }
  
  # Get Top Scorers
  top_goals <- data %>% 
    arrange(desc(goals))
  
  # Top Goals: Names
  top_goals_1_player <- top_goals[1,]$player
  top_goals_2_player <- top_goals[2,]$player
  top_goals_3_player <- top_goals[3,]$player
  
  # Top Coals: Goals
  top_goals_1 <- top_goals[1,]$goals
  top_goals_2 <- top_goals[2,]$goals
  top_goals_3 <- top_goals[3,]$goals
  
  if (top_goals_1 == top_goals_2 & top_goals_2 == top_goals_3){
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} & {top_goals_2_player} & {top_goals_3_player} with {top_goals_1} goals")
    
  } else if(top_goals_1_player == top_goals_2_player){
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} & {top_goals_2_player} with {top_goals_1} goals")
    
  } else {
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} with {top_goals_1} goals")
    
  }
  
  return(list(top_cap_output, top_goals_output))
  
}
func_plot_axis_format <-
function(){
  
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"))
}
func_country_profile <-
function(data){
  
  country <- data %>% distinct(country) %>% pull()
  coach   <- data %>% distinct(coach) %>% pull()
  captain <- data %>% 
    filter(captain == "Yes") %>% pull(player) %>% str_remove("captain") %>% str_remove("\\(") %>% str_remove("\\)")
  
  return(
    str_glue("Country: {country} | Coach: {coach} |  Captain: {captain}")
  )
}
func_plot_age_distribution <-
function(data){
  
  min_age    <- data$age %>% min()
  max_age    <- data$age %>% max()
  median_age <- data$age %>% median()
  
  data %>% 
    mutate(position = position %>% fct_relevel("Goalkeepers", "Defenders", "Midfielders", "Forwards") %>% fct_rev()) %>% 
   
     # geoms
    ggplot(aes(age, position))+
    geom_boxplot()+
    geom_jitter(size = 3)+

    # theme
    theme_minimal()+
    
    # format
    theme(axis.text.x = element_text(size = 10, color = "black"),
          axis.text.y = element_text(size = 10, color = "black"))+
    
    # labs
    labs(title = "Age Distribution",
         subtitle = str_glue("Min: {min_age} | Median: {median_age} | Max: {max_age}"),
         y = "")
  
  
  
}
func_default_message <-
function(data){
  
  goals_desc <- data %>% arrange(desc(goals))
  caps_desc  <- data %>% arrange(desc(caps))
  
  top_scorer <- goals_desc[1,]$player
  no_goals   <- goals_desc[1,]$goals
  top_caps   <- caps_desc[1,]$player
  no_caps    <- caps_desc[1,]$caps
  
  default_message <- str_glue("Country: All Countries
                              Top Goal Scorer: {top_scorer} with {no_goals}
                              Top Caps: {top_caps} with {no_caps} caps")
  
  return(default_message)
  
}
func_player_profile <-
function(data){
  
  # Get Top Caps
  top_caps <- data %>% 
    arrange(desc(caps))
  
  # Top Caps: Names
  top_cap_1_player <- top_caps[1,]$player
  top_cap_2_player <- top_caps[2,]$player
  top_cap_3_player <- top_caps[3,]$player
  
  # Top Caps: Caps
  top_cap_1 <- top_caps[1,]$caps
  top_cap_2 <- top_caps[2,]$caps
  top_cap_3 <- top_caps[3,]$caps

  if (top_cap_1 == top_cap_2 & top_cap_2 == top_cap_3){
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} & {top_cap_2_player} & {top_cap_3_Player} with {top_cap_1} caps")
    
  } else if(top_cap_1 == top_cap_2){
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} & {top_cap_2_player} with {top_cap_1} caps")
    
  } else {
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} with {top_cap_1} caps")
    
  }
  
  # Get Top Scorers
  top_goal <- data %>% 
    arrange(desc(goals))
  
  # Top Goals: Names
  top_goals_1_player <- top_goals[1,]$player
  top_goals_2_player <- top_goals[2,]$player
  top_goals_3_player <- top_goals[3,]$player
  
  # Top Coals: Goals
  top_goals_1 <- top_goals[1,]$goals
  top_goals_2 <- top_goals[2,]$goals
  top_goals_3 <- top_goals[3,]$goals
  
  if (top_goals_1 == top_goals_2 & top_goals_2 == top_goals_3){
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} & {top_goals_2_player} & {top_goals_3_player} with {top_goals_1} goals")
    
  } else if(top_goals_1_player == top_goals_2_player){
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} & {top_goals_2_player} with {top_goals_1} goals")
    
  } else {
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} with {top_goals_1} goals")
    
  }
  
  return(str_glue("{top_cap_output}
                   {top_goals_output}"))
  
}
func_plot_axis_format <-
function(){
  
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"))
}
func_country_profile <-
function(data){
  
  country <- data %>% distinct(country) %>% pull()
  coach   <- data %>% distinct(coach) %>% pull()
  captain <- data %>% 
    filter(captain == "Yes") %>% pull(player) %>% str_remove("captain") %>% str_remove("\\(") %>% str_remove("\\)")
  
  return(
    str_glue("Country: {country} | Coach: {coach} |  Captain: {captain}")
  )
}
func_plot_age_distribution <-
function(data){
  
  min_age    <- data$age %>% min()
  max_age    <- data$age %>% max()
  median_age <- data$age %>% median()
  
  data %>% 
    mutate(position = position %>% fct_relevel("Goalkeepers", "Defenders", "Midfielders", "Forwards") %>% fct_rev()) %>% 
   
     # geoms
    ggplot(aes(age, position))+
    geom_boxplot()+
    geom_jitter(size = 3)+

    # theme
    theme_minimal()+
    
    # format
    theme(axis.text.x = element_text(size = 10, color = "black"),
          axis.text.y = element_text(size = 10, color = "black"))+
    
    # labs
    labs(title = "Age Distribution",
         subtitle = str_glue("Min: {min_age} | Median: {median_age} | Max: {max_age}"),
         y = "")
  
  
  
}
func_default_message <-
function(data){
  
  goals_desc <- data %>% arrange(desc(goals))
  caps_desc  <- data %>% arrange(desc(caps))
  
  top_scorer <- goals_desc[1,]$player
  no_goals   <- goals_desc[1,]$goals
  top_caps   <- caps_desc[1,]$player
  no_caps    <- caps_desc[1,]$caps
  
  default_message <- str_glue("Country: All Countries
                              Top Goal Scorer: {top_scorer} with {no_goals}
                              Top Caps: {top_caps} with {no_caps} caps")
  
  return(default_message)
  
}
func_player_profile <-
function(data){
  
  # Get Top Caps
  top_caps <- data %>% 
    arrange(desc(caps))
  
  # Top Caps: Names
  top_cap_1_player <- top_caps[1,]$player
  top_cap_2_player <- top_caps[2,]$player
  top_cap_3_player <- top_caps[3,]$player
  
  # Top Caps: Caps
  top_cap_1 <- top_caps[1,]$caps
  top_cap_2 <- top_caps[2,]$caps
  top_cap_3 <- top_caps[3,]$caps

  if (top_cap_1 == top_cap_2 & top_cap_2 == top_cap_3){
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} & {top_cap_2_player} & {top_cap_3_Player} with {top_cap_1} caps")
    
  } else if(top_cap_1 == top_cap_2){
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} & {top_cap_2_player} with {top_cap_1} caps")
    
  } else {
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} with {top_cap_1} caps")
    
  }
  
  # Get Top Scorers
  top_goals <- data %>% 
    arrange(desc(goals))
  
  # Top Goals: Names
  top_goals_1_player <- top_goals[1,]$player
  top_goals_2_player <- top_goals[2,]$player
  top_goals_3_player <- top_goals[3,]$player
  
  # Top Coals: Goals
  top_goals_1 <- top_goals[1,]$goals
  top_goals_2 <- top_goals[2,]$goals
  top_goals_3 <- top_goals[3,]$goals
  
  if (top_goals_1 == top_goals_2 & top_goals_2 == top_goals_3){
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} & {top_goals_2_player} & {top_goals_3_player} with {top_goals_1} goals")
    
  } else if(top_goals_1_player == top_goals_2_player){
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} & {top_goals_2_player} with {top_goals_1} goals")
    
  } else {
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} with {top_goals_1} goals")
    
  }
  
  return(str_glue("{top_cap_output}
                   {top_goals_output}"))
  
}
func_plot_axis_format <-
function(){
  
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"))
}
func_country_profile <-
function(data){
  
  country <- data %>% distinct(country) %>% pull()
  coach   <- data %>% distinct(coach) %>% pull()
  captain <- data %>% 
    filter(captain == "Yes") %>% pull(player) %>% str_remove("captain") %>% str_remove("\\(") %>% str_remove("\\)")
  
  return(
    str_glue("Country: {country} | Coach: {coach} |  Captain: {captain}")
  )
}
func_plot_age_distribution <-
function(data){
  
  min_age    <- data$age %>% min()
  max_age    <- data$age %>% max()
  median_age <- data$age %>% median()
  
  p <- data %>% 
    mutate(position = position %>% fct_relevel("Goalkeepers", "Defenders", "Midfielders", "Forwards") %>% fct_rev()) %>% 
    mutate(label_text = str_glue("Player: {player},
                                  Country: {country},
                                  Position: {position},
                                  Age: {age},
                                  Caps: {caps},
                                  Goals: {goals},
                                  Club: {club}")) %>% 
   
     # geoms
    ggplot(aes(position, age))+
    geom_violin()+
    geom_jitter(aes(text = label_text), size = 2.5, alpha = 0.7, width = 0.1)+

    # theme
    theme_minimal()+
    coord_flip()+
    
    # format
    theme(axis.text.x = element_text(size = 11, color = "black"),
          axis.text.y = element_text(size = 11, color = "black"))+
    
    # labs
    # labs(title = "Age Distribution",
    #      subtitle = str_glue("Min: {min_age} | Median: {median_age} | Max: {max_age}"),
    #      y = "")
    xlab("") + ylab("Age")
  
  ggplotly(p, tooltip = "text") %>% 
    layout(title = list(text = paste0("Age Distribution",
                                      "<br>",
                                      "<sup>",
                                      "Min: ", min_age, " | ", "Median: ", median_age, " | ", "Max: ", max_age)))
}
func_default_message <-
function(data){
  
  goals_desc <- data %>% arrange(desc(goals))
  caps_desc  <- data %>% arrange(desc(caps))
  
  top_scorer <- goals_desc[1,]$player
  no_goals   <- goals_desc[1,]$goals
  top_caps   <- caps_desc[1,]$player
  no_caps    <- caps_desc[1,]$caps
  
  default_message <- str_glue("Country: All Countries
                              Top Goal Scorer: {top_scorer} with {no_goals}
                              Top Caps: {top_caps} with {no_caps} caps")
  
  return(default_message)
  
}
func_player_profile <-
function(data){
  
  # Get Top Caps
  top_caps <- data %>% 
    arrange(desc(caps))
  
  # Top Caps: Names
  top_cap_1_player <- top_caps[1,]$player
  top_cap_2_player <- top_caps[2,]$player
  top_cap_3_player <- top_caps[3,]$player
  
  # Top Caps: Caps
  top_cap_1 <- top_caps[1,]$caps
  top_cap_2 <- top_caps[2,]$caps
  top_cap_3 <- top_caps[3,]$caps

  if (top_cap_1 == top_cap_2 & top_cap_2 == top_cap_3){
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} & {top_cap_2_player} & {top_cap_3_Player} with {top_cap_1} caps")
    
  } else if(top_cap_1 == top_cap_2){
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} & {top_cap_2_player} with {top_cap_1} caps")
    
  } else {
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} with {top_cap_1} caps")
    
  }
  
  # Get Top Scorers
  top_goals <- data %>% 
    arrange(desc(goals))
  
  # Top Goals: Names
  top_goals_1_player <- top_goals[1,]$player
  top_goals_2_player <- top_goals[2,]$player
  top_goals_3_player <- top_goals[3,]$player
  
  # Top Coals: Goals
  top_goals_1 <- top_goals[1,]$goals
  top_goals_2 <- top_goals[2,]$goals
  top_goals_3 <- top_goals[3,]$goals
  
  if (top_goals_1 == top_goals_2 & top_goals_2 == top_goals_3){
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} & {top_goals_2_player} & {top_goals_3_player} with {top_goals_1} goals")
    
  } else if(top_goals_1_player == top_goals_2_player){
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} & {top_goals_2_player} with {top_goals_1} goals")
    
  } else {
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} with {top_goals_1} goals")
    
  }
  
  return(str_glue("{top_cap_output}
                   {top_goals_output}"))
  
}
func_plot_axis_format <-
function(){
  
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"))
}
func_country_profile <-
function(data){
  
  country <- data %>% distinct(country) %>% pull()
  coach   <- data %>% distinct(coach) %>% pull()
  captain <- data %>% 
    filter(captain == "Yes") %>% pull(player) %>% str_remove("captain") %>% str_remove("\\(") %>% str_remove("\\)")
  
  return(
    str_glue("Country: {country} | Coach: {coach} |  Captain: {captain}")
  )
}
func_plot_age_distribution <-
function(data){
  
  min_age    <- data$age %>% min()
  max_age    <- data$age %>% max()
  median_age <- data$age %>% median()
  
  p <- data %>% 
    mutate(position = position %>% fct_relevel("Goalkeepers", "Defenders", "Midfielders", "Forwards") %>% fct_rev()) %>% 
    mutate(label_text = str_glue("Player: {player},
                                  Country: {country},
                                  Position: {position},
                                  Age: {age},
                                  Caps: {caps},
                                  Goals: {goals},
                                  Club: {club}")) %>% 
   
     # geoms
    ggplot(aes(position, age))+
    geom_violin()+
    geom_jitter(aes(text = label_text), size = 2.5, alpha = 0.7, width = 0.1)+

    # theme
    theme_minimal()+
    coord_flip()+
    
    # format
    theme(axis.text.x = element_text(size = 11, color = "black"),
          axis.text.y = element_text(size = 11, color = "black"))+
    
    # labs
    # labs(title = "Age Distribution",
    #      subtitle = str_glue("Min: {min_age} | Median: {median_age} | Max: {max_age}"),
    #      y = "")
    xlab("") + ylab("Age")
  
  ggplotly(p, tooltip = "text") %>% 
    layout(title = list(text = paste0("<br>",
                                      "Age Distribution",
                                      "<br>",
                                      "<sup>",
                                      "Min: ", min_age, " | ", "Median: ", median_age, " | ", "Max: ", max_age)))
}
func_default_message <-
function(data){
  
  goals_desc <- data %>% arrange(desc(goals))
  caps_desc  <- data %>% arrange(desc(caps))
  
  top_scorer <- goals_desc[1,]$player
  no_goals   <- goals_desc[1,]$goals
  top_caps   <- caps_desc[1,]$player
  no_caps    <- caps_desc[1,]$caps
  
  default_message <- str_glue("Country: All Countries
                              Top Goal Scorer: {top_scorer} with {no_goals}
                              Top Caps: {top_caps} with {no_caps} caps")
  
  return(default_message)
  
}
func_player_profile <-
function(data){
  
  # Get Top Caps
  top_caps <- data %>% 
    arrange(desc(caps))
  
  # Top Caps: Names
  top_cap_1_player <- top_caps[1,]$player
  top_cap_2_player <- top_caps[2,]$player
  top_cap_3_player <- top_caps[3,]$player
  
  # Top Caps: Caps
  top_cap_1 <- top_caps[1,]$caps
  top_cap_2 <- top_caps[2,]$caps
  top_cap_3 <- top_caps[3,]$caps

  if (top_cap_1 == top_cap_2 & top_cap_2 == top_cap_3){
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} & {top_cap_2_player} & {top_cap_3_Player} with {top_cap_1} caps")
    
  } else if(top_cap_1 == top_cap_2){
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} & {top_cap_2_player} with {top_cap_1} caps")
    
  } else {
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} with {top_cap_1} caps")
    
  }
  
  # Get Top Scorers
  top_goals <- data %>% 
    arrange(desc(goals))
  
  # Top Goals: Names
  top_goals_1_player <- top_goals[1,]$player
  top_goals_2_player <- top_goals[2,]$player
  top_goals_3_player <- top_goals[3,]$player
  
  # Top Coals: Goals
  top_goals_1 <- top_goals[1,]$goals
  top_goals_2 <- top_goals[2,]$goals
  top_goals_3 <- top_goals[3,]$goals
  
  if (top_goals_1 == top_goals_2 & top_goals_2 == top_goals_3){
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} & {top_goals_2_player} & {top_goals_3_player} with {top_goals_1} goals")
    
  } else if(top_goals_1_player == top_goals_2_player){
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} & {top_goals_2_player} with {top_goals_1} goals")
    
  } else {
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} with {top_goals_1} goals")
    
  }
  
  return(str_glue("{top_cap_output}
                   {top_goals_output}"))
  
}
func_plot_axis_format <-
function(){
  
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"))
}
func_country_profile <-
function(data){
  
  country <- data %>% distinct(country) %>% pull()
  coach   <- data %>% distinct(coach) %>% pull()
  captain <- data %>% 
    filter(captain == "Yes") %>% pull(player) %>% str_remove("captain") %>% str_remove("\\(") %>% str_remove("\\)")
  
  return(
    str_glue("Country: {country} | Coach: {coach} |  Captain: {captain}")
  )
}
func_plot_age_distribution <-
function(data){
  
  min_age    <- data$age %>% min()
  max_age    <- data$age %>% max()
  median_age <- data$age %>% median()
  
  p <- data %>% 
    mutate(position = position %>% fct_relevel("Goalkeepers", "Defenders", "Midfielders", "Forwards") %>% fct_rev()) %>% 
    mutate(label_text = str_glue("Player: {player},
                                  Country: {country},
                                  Position: {position},
                                  Age: {age},
                                  Caps: {caps},
                                  Goals: {goals},
                                  Club: {club}")) %>% 
   
     # geoms
    ggplot(aes(position, age))+
    geom_violin()+
    geom_jitter(aes(text = label_text), size = 2.5, alpha = 0.7, width = 0.1)+

    # theme
    theme_minimal()+
    coord_flip()+
    
    # format
    theme(axis.text.x = element_text(size = 11, color = "black"),
          axis.text.y = element_text(size = 11, color = "black"))+
    
    # labs
    # labs(title = "Age Distribution",
    #      subtitle = str_glue("Min: {min_age} | Median: {median_age} | Max: {max_age}"),
    #      y = "")
    xlab("") + ylab("Age")
  
  ggplotly(p, tooltip = "text") %>% 
    layout(title = list(text = paste0("<br>",
                                      "Age Distribution",
                                      "<br>",
                                      "<sup>",
                                      "Min: ", min_age, " | ", "Median: ", median_age, " | ", "Max: ", max_age,
                                      "<br>")))
}
func_default_message <-
function(data){
  
  goals_desc <- data %>% arrange(desc(goals))
  caps_desc  <- data %>% arrange(desc(caps))
  
  top_scorer <- goals_desc[1,]$player
  no_goals   <- goals_desc[1,]$goals
  top_caps   <- caps_desc[1,]$player
  no_caps    <- caps_desc[1,]$caps
  
  default_message <- str_glue("Country: All Countries
                              Top Goal Scorer: {top_scorer} with {no_goals}
                              Top Caps: {top_caps} with {no_caps} caps")
  
  return(default_message)
  
}
func_player_profile <-
function(data){
  
  # Get Top Caps
  top_caps <- data %>% 
    arrange(desc(caps))
  
  # Top Caps: Names
  top_cap_1_player <- top_caps[1,]$player
  top_cap_2_player <- top_caps[2,]$player
  top_cap_3_player <- top_caps[3,]$player
  
  # Top Caps: Caps
  top_cap_1 <- top_caps[1,]$caps
  top_cap_2 <- top_caps[2,]$caps
  top_cap_3 <- top_caps[3,]$caps

  if (top_cap_1 == top_cap_2 & top_cap_2 == top_cap_3){
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} & {top_cap_2_player} & {top_cap_3_Player} with {top_cap_1} caps")
    
  } else if(top_cap_1 == top_cap_2){
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} & {top_cap_2_player} with {top_cap_1} caps")
    
  } else {
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} with {top_cap_1} caps")
    
  }
  
  # Get Top Scorers
  top_goals <- data %>% 
    arrange(desc(goals))
  
  # Top Goals: Names
  top_goals_1_player <- top_goals[1,]$player
  top_goals_2_player <- top_goals[2,]$player
  top_goals_3_player <- top_goals[3,]$player
  
  # Top Coals: Goals
  top_goals_1 <- top_goals[1,]$goals
  top_goals_2 <- top_goals[2,]$goals
  top_goals_3 <- top_goals[3,]$goals
  
  if (top_goals_1 == top_goals_2 & top_goals_2 == top_goals_3){
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} & {top_goals_2_player} & {top_goals_3_player} with {top_goals_1} goals")
    
  } else if(top_goals_1_player == top_goals_2_player){
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} & {top_goals_2_player} with {top_goals_1} goals")
    
  } else {
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} with {top_goals_1} goals")
    
  }
  
  return(str_glue("{top_cap_output}
                   {top_goals_output}"))
  
}
func_plot_axis_format <-
function(){
  
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"))
}
func_country_profile <-
function(data){
  
  country <- data %>% distinct(country) %>% pull()
  coach   <- data %>% distinct(coach) %>% pull()
  captain <- data %>% 
    filter(captain == "Yes") %>% pull(player) %>% str_remove("captain") %>% str_remove("\\(") %>% str_remove("\\)")
  
  return(
    str_glue("Country: {country} | Coach: {coach} |  Captain: {captain}")
  )
}
func_plot_age_distribution <-
function(data){
  
  min_age    <- data$age %>% min()
  max_age    <- data$age %>% max()
  median_age <- data$age %>% median()
  
  data %>% 
    mutate(position = position %>% fct_relevel("Goalkeepers", "Defenders", "Midfielders", "Forwards") %>% fct_rev()) %>% 
    mutate(label_text = str_glue("Player: {player},
                                  Country: {country},
                                  Position: {position},
                                  Age: {age},
                                  Caps: {caps},
                                  Goals: {goals},
                                  Club: {club}")) %>% 
   
     # geoms
    ggplot(aes(age, position))+
    geom_boxplot()+
    geom_jitter(aes(text = label_text), size = 2.5, alpha = 0.7, width = 0.1)+

    # theme
    theme_minimal()+

    
    # format
    theme(axis.text.x = element_text(size = 11, color = "black"),
          axis.text.y = element_text(size = 11, color = "black"))+
    
    #labs
    labs(title = "Age Distribution",
         subtitle = str_glue("Min: {min_age} | Median: {median_age} | Max: {max_age}"),
         y = "")
  #   xlab("") + ylab("Age")
  # 
  # ggplotly(p, tooltip = "text") %>% 
  #   layout(title = list(text = paste0("<br>",
  #                                     "Age Distribution",
  #                                     "<br>",
  #                                     "<sup>",
  #                                     "Min: ", min_age, " | ", "Median: ", median_age, " | ", "Max: ", max_age,
  #                                     "<br>")))
}
func_default_message <-
function(data){
  
  goals_desc <- data %>% arrange(desc(goals))
  caps_desc  <- data %>% arrange(desc(caps))
  
  top_scorer <- goals_desc[1,]$player
  no_goals   <- goals_desc[1,]$goals
  top_caps   <- caps_desc[1,]$player
  no_caps    <- caps_desc[1,]$caps
  
  default_message <- str_glue("Country: All Countries
                              Top Goal Scorer: {top_scorer} with {no_goals}
                              Top Caps: {top_caps} with {no_caps} caps")
  
  return(default_message)
  
}
func_player_profile <-
function(data){
  
  # Get Top Caps
  top_caps <- data %>% 
    arrange(desc(caps))
  
  # Top Caps: Names
  top_cap_1_player <- top_caps[1,]$player
  top_cap_2_player <- top_caps[2,]$player
  top_cap_3_player <- top_caps[3,]$player
  
  # Top Caps: Caps
  top_cap_1 <- top_caps[1,]$caps
  top_cap_2 <- top_caps[2,]$caps
  top_cap_3 <- top_caps[3,]$caps

  if (top_cap_1 == top_cap_2 & top_cap_2 == top_cap_3){
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} & {top_cap_2_player} & {top_cap_3_Player} with {top_cap_1} caps")
    
  } else if(top_cap_1 == top_cap_2){
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} & {top_cap_2_player} with {top_cap_1} caps")
    
  } else {
    
    top_cap_output <- str_glue("Highest Capped Player(s): {top_cap_1_player} with {top_cap_1} caps")
    
  }
  
  # Get Top Scorers
  top_goals <- data %>% 
    arrange(desc(goals))
  
  # Top Goals: Names
  top_goals_1_player <- top_goals[1,]$player
  top_goals_2_player <- top_goals[2,]$player
  top_goals_3_player <- top_goals[3,]$player
  
  # Top Coals: Goals
  top_goals_1 <- top_goals[1,]$goals
  top_goals_2 <- top_goals[2,]$goals
  top_goals_3 <- top_goals[3,]$goals
  
  if (top_goals_1 == top_goals_2 & top_goals_2 == top_goals_3){
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} & {top_goals_2_player} & {top_goals_3_player} with {top_goals_1} goals")
    
  } else if(top_goals_1_player == top_goals_2_player){
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} & {top_goals_2_player} with {top_goals_1} goals")
    
  } else {
    
    top_goals_output <- str_glue("Highest Goal Scorer(s): {top_goals_1_player} with {top_goals_1} goals")
    
  }
  
  return(str_glue("{top_cap_output}
                   {top_goals_output}"))
  
}
