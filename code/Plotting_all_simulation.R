### plotting strategies over generations ####
library(tidyverse)
library(data.table)

#list all folders that contain the word "sim"
lfolders <- list.dirs(path = "data", recursive = FALSE)
folders <- lfolders[ grepl("sim", lfolders) ]

n_agents = 10000
n_time = 400

summary_list <- list()
#generation_dataframe_list <- list()

for(i in seq_along(folders)){
  source(paste(folders[[i]], "sourceMe.R", sep = "/"))
  
  # SUMMARY LIST ---------------------------------------------------------------
  data_extended <- summary()
  data_extended <- data.table(data_extended$agents)
  data_extended[, stealing := (n_agents * n_time) - (foraging+handling)]
  
  data <- data_extended[, list(`pop fitness`, foraging, stealing, handling, conflicts)]
  #data <- as_tibble(data)
  
  data[, gen := seq(nrow(data))]
  
  # remove last gen
  data = data[gen < max(gen), ]
  setnames(data, old = "pop fitness", new = "pop_fitness")
  # melt
  data = melt(data, id.vars = c("gen", "pop_fitness", "conflicts"))
  
  data[, value := value / sum(value), by = "gen"]
  
  summary_list[[i]] <- data
  
  # GENERATION DATAFRAME LIST -----------------------------------------------------
  #[da fare ancora perch? al momento non ci serve]
}


