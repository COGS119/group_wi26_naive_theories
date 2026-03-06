library(here)
library(tidyverse)
library(jsonlite)

processed_data_directory <- here("..","data","processed_data")
file_name <- "naive_theories"

#read experiment data
exp_data <- read_csv(here(processed_data_directory,paste0(file_name,"-alldata.csv")))

#clean up participant ids
exp_data <- exp_data %>%
  #clean up participant ids
  mutate(
    participant_id = case_when(
      participant_id == "9252" ~ "parrot",
      TRUE ~ trimws(tolower(participant_id))
    )
  )

#double check that participant ids are unique
counts_by_random_id <- exp_data %>%
  group_by(random_id,participant_id) %>%
  count()
#output to track participants
write_csv(counts_by_random_id,here(processed_data_directory,paste0(file_name,"-participant-list.csv")))

#extract reward question
demographics <- exp_data %>% 
  filter(trial_type=="survey-multi-choice") %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  select(random_id,participant_id,grade,gender)

final_questions <- exp_data %>% 
  filter(trial_type=="survey") %>%
  ungroup() %>%
  #map response with JSON but make robust to null entries within json
  mutate(
    json = map(response, \(x) {
      y <- fromJSON(x, simplifyVector = FALSE)
      if (is.character(y)) y <- fromJSON(y, simplifyVector = FALSE)
      y <- modify(y, ~ if (is.null(.x)) NA else .x)
      as_tibble_row(y)
    })
  ) %>%
  unnest_wider(json)%>%
  select(random_id,participant_id,age:technical_difficulties)

#join back in
exp_data <- exp_data %>%
  left_join(demographics) %>%
  left_join(final_questions)

#filter dataset
exp_data <- exp_data %>%
  filter(!is.na(category))

#filter participant ids
filter_ids <- c()

#identify participants from the experiment group
group_members <- c("raccoon","gecko","squirrel","sheep","horse","bear")

processed_data <- exp_data %>%
  filter(!(participant_id %in% filter_ids)) %>%
  #flag for group participants
  mutate(participant_is_group_member = case_when(
    participant_id %in% group_members ~ TRUE,
    TRUE ~ FALSE
  
  )) %>%
  #remove unneeded columns
  select(-c(success,plugin_version)) %>%
  #add trial_number
  group_by(participant_id) %>%
  mutate(trial_number = row_number()) %>%
  relocate(trial_number,.after=trial_index)
  
#store processed and prepped data
write_csv(processed_data,here(processed_data_directory,paste0(file_name,"-processed-data.csv")))
