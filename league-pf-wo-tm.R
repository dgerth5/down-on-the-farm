library(readr)
library(dplyr)
milb_park_effects22 <- read_csv("milb_park_effects22.csv")

lvl <- c("a","b","c")

milb_park_effects22$lvl <- sample(lvl, 120, replace = TRUE)

tm <- "Aberdeen IronBirds"

lg <- milb_park_effects22$lvl[which(milb_park_effects22$team == tm)]

final_Df <- milb_park_effects22 %>%
  filter(team != tm & lvl == lg)

adj_pf <- mean(final_Df$single_pf)
adj_pm <- mean(final_Df$single_mult)

get_pf_wo_tm <- function(df, tm, type){
  
  lg <- as.character(milb_park_effects22[which(milb_park_effects22[,1] == tm), 11])
  spec_df <- df %>%
    filter(team != tm & lvl == lg)

  if (type == "single") {
    avg_pf <- mean(unlist(spec_df[,3]))
    avg_pm <- mean(unlist(spec_df[,4]))
    
  } else if (type == "double") {
    avg_pf <- mean(unlist(spec_df[,5]))
    avg_pm <- mean(unlist(spec_df[,6]))
    
  } else if (type == "triple") {
    avg_pf <- mean(unlist(spec_df[,7]))
    avg_pm <- mean(unlist(spec_df[,8]))
    
  } else if (type == "homerun") {
    avg_pf <- mean(unlist(spec_df[,9]))
    avg_pm <- mean(unlist(spec_df[,10]))
    
  } else {
    print("type is one of single, double, triple, homerun")
  }
  
  lst <- list(avg_pf, avg_pm)
  
  return(lst)
  
}


df2 <- get_pf_wo_tm(milb_park_effects22, tm, "single")
df2
