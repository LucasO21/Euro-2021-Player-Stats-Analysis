

# Libraries ----
library(tidyverse)
library(rvest)
library(writexl)

url <- "https://en.wikipedia.org/wiki/UEFA_Euro_2020_squads"
page <- read_html(url)

# get list of participating countries
countries_list <- page %>% 
  html_nodes("h3") %>% 
  html_text()

countries_list <- countries_list[1:24]

# get list of coaches
coach_list <- page %>% 
  html_nodes("p") %>% 
  html_text() %>% 
  str_split("\n") %>% 
  unlist() %>% 
  tibble() %>% 
  rename(coach = ".") %>% 
  filter(grepl("Manager", coach)) %>% 
  mutate(coach = coach %>% str_remove_all("Manager: "),
         coach = coach %>% str_trim(side = "both"))

country_coach_tbl <- tibble(country = countries_list) %>% 
  bind_cols(coach_list)

# get tables
table_list <- html_nodes(page, "table")

table_list <- table_list %>% 
  .[1:24] %>% 
  html_table(fill = TRUE)

# function
func_add_country_coach <- function(table_list_no = 1, coach_country_no = 1){
  
  table_list[[table_list_no]] %>% 
    mutate(country  = country_coach_tbl[coach_country_no,]$country,
           coach    = country_coach_tbl[coach_country_no,]$coach)
}

euros_tbl <- tibble(
  rbind(
  func_add_country_coach(1, 1), func_add_country_coach(2, 2),
  func_add_country_coach(3, 3), func_add_country_coach(4, 4),
  func_add_country_coach(5, 5), func_add_country_coach(6, 6),
  func_add_country_coach(7, 7), func_add_country_coach(8, 8),
  func_add_country_coach(9, 9), func_add_country_coach(10, 10),
  func_add_country_coach(11, 11), func_add_country_coach(12, 12),
  func_add_country_coach(13, 13), func_add_country_coach(14, 14),
  func_add_country_coach(15, 15), func_add_country_coach(16, 16),
  func_add_country_coach(17, 17), func_add_country_coach(18, 18),
  func_add_country_coach(19, 19), func_add_country_coach(20, 20),
  func_add_country_coach(21, 21), func_add_country_coach(22, 22),
  func_add_country_coach(23, 23), func_add_country_coach(24, 24)
  
  )
)

euros_tbl

# extract age
final_euros_tbl <- euros_tbl %>% 
  setNames(names(.) %>% str_to_lower()) %>% 
  rename(dob = `date of birth (age)`) %>% 
  separate(col = dob, into = c("col1", "col2", "col3", "col4", "col5"), sep = " ") %>% 
  select(-c("col1", "col2", "col3", "col4")) %>% 
  rename(age = col5) %>% 
  mutate(age = age %>% str_remove_all("\\)") %>% as.numeric) %>% 
  mutate(position = case_when(
    str_detect(pos., "GK") ~ "Goalkeepers",
    str_detect(pos., "DF") ~ "Defenders",
    str_detect(pos., "MF") ~ "Midfielders",
    str_detect(pos., "FW") ~ "Forwards",
  )) %>% 
  mutate(captain = case_when(
    str_detect(player, "(captain)") ~ "Yes",
    TRUE ~ "No"
  ))

  
final_euros_tbl %>% write_rds(file = "../App/00_Data/euro_2021_data.rds")
final_euros_tbl <- read_rds(file = "../App/00_Data/euro_2021_data.rds")

data <- final_euros_tbl %>% filter(country == "Spain")

# axis format function ----
func_plot_axis_format <- function(){
  
  theme(axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"))
}

# country profile function ----
func_country_profile <- function(data){
  
  country <- data %>% distinct(country) %>% pull()
  coach   <- data %>% distinct(coach) %>% pull()
  captain <- data %>% 
    filter(captain == "Yes") %>% pull(player) %>% str_remove("captain") %>% str_remove("\\(") %>% str_remove("\\)")
  
  return(
    str_glue("Country: {country} | Coach: {coach} |  Captain: {captain}")
  )
}

# Age Distribution Function ----
func_plot_age_distribution <- function(data){
  
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

data <- final_euros_tbl %>% filter(country == "Italy")
final_euros_tbl %>% filter(country == "Germany") %>% func_plot_age_distribution()

# player profile function ----
func_player_profile <- function(data){
  
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


func_player_profile(data)

# default message function ----
func_default_message <- function(data){
  
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

final_euros_tbl %>% func_default_message()
final_euros_tbl %>% func_plot_age_distribution()

c("func_plot_axis_format", "func_country_profile", "func_plot_age_distribution", 
  "func_default_message", "func_player_profile") %>% 
  dump(file = "../App/functions.R", 
       append = TRUE)

final_euros_tbl %>% arrange(desc(goals))






