# Get LPL for a lineup ----------------------------------------------------

get_LPL <- function(my_lineup_code,
                    player_fgp_df,
                    player_fga_df,
                    lineup_data,
                    grid_to_regions,
                    LPL_type = "Rate",
                    full_surface = F){
  
  players_in_lineup <- lineup_data %>% filter(lineup_code == my_lineup_code) %>%
    dplyr::select(matches("player*")) %>% with(unlist(c(.)))
  players_in_lineup_w_lineup_code <- paste(players_in_lineup, my_lineup_code, sep = "_")
  team_code <- lineup_data$team_code[lineup_data$lineup_code == my_lineup_code]
  
  lineup_attempts = player_fga_df[, players_in_lineup_w_lineup_code]
  lineup_percents = player_fgp_df[, players_in_lineup]
  percent_rank = t(apply(-1 * lineup_percents, 1, rank, ties.method = "min"))
  attempt_rank = t(apply(-1 * lineup_attempts, 1, rank, ties.method = "min"))
  
  lineup_estimated_makes <- apply(lineup_percents * lineup_attempts,1,sum)
  lineup_potential_makes <- NA
  location_point_vals <- ifelse(grid_to_regions == "T",3,2)
  old_shooting_order <- colnames(lineup_percents)
  player_lpl <- lineup_attempts
  
  for(i in 1:nrow(percent_rank)){
    new_shooting_order <- names(lineup_percents[i,order(percent_rank[i,])])
    actual_expected_points = lineup_percents[i, ] * lineup_attempts[i, ]
    optimum_expected_points = (lineup_percents[i,order(percent_rank[i,])] *
                                 lineup_attempts[i,order(attempt_rank[i,])])[match(old_shooting_order,new_shooting_order)]
    player_lpl[i,] = (optimum_expected_points - actual_expected_points) * location_point_vals[i]
  }
  
  if(LPL_type == "Rate"){
    LPL_per_shot <- apply(player_lpl,1,sum)/apply(lineup_attempts,1,sum)
  } else {
    LPL_per_shot <- apply(player_lpl,1,sum)
  }
  
  if(full_surface){
    return(LPL_per_shot)
  } else {
    return(list(LPL_rim = sum(LPL_per_shot[grid_to_regions == 'R']),
                LPL_mid = sum(LPL_per_shot[grid_to_regions == 'M']),
                LPL_three = sum(LPL_per_shot[grid_to_regions == 'T']),
                tot_LPL = sum(LPL_per_shot)))
  }
}


# Get LPL player contributions --------------------------------------------


get_PLC <- function(my_lineup_code,
                           player_fgp_df,
                           player_fga_df,
                           lineup_data,
                           grid_to_regions,
                           LPL_type = "Rate"){
  
  players_in_lineup <- lineup_data %>% filter(lineup_code == my_lineup_code) %>%
    dplyr::select(matches("player*")) %>% with(unlist(c(.)))
  players_in_lineup_w_lineup_code <- paste(players_in_lineup, my_lineup_code, sep = "_")
  team_code <- lineup_data$team_code[lineup_data$lineup_code == my_lineup_code]
  
  lineup_attempts = player_fga_df[, players_in_lineup_w_lineup_code]
  lineup_percents = player_fgp_df[, players_in_lineup]
  percent_rank = t(apply(-1 * lineup_percents, 1, rank, ties.method = "min"))
  attempt_rank = t(apply(-1 * lineup_attempts, 1, rank, ties.method = "min"))
  
  location_point_vals <- ifelse(grid_to_regions == "T",3,2)
  old_shooting_order <- colnames(lineup_percents)
  lineup_usage_eff <- lineup_attempts
  player_lpl <- lineup_attempts
  player_lpl2 <- lineup_attempts
  
  for(i in 1:nrow(lineup_usage_eff)){
    new_shooting_order <- names(lineup_percents[i,order(percent_rank[i,])])
    actual_expected_points = lineup_percents[i, ] * lineup_attempts[i, ]
    optimum_expected_points = (lineup_percents[i,order(percent_rank[i,])] *
                                 lineup_attempts[i,order(attempt_rank[i,])])[match(old_shooting_order,new_shooting_order)]
    lineup_usage_eff[i, ] = (optimum_expected_points - actual_expected_points) * location_point_vals[i]
    under_over <- ifelse(lineup_usage_eff[i, ] < 0, -1, 1)
    player_lpl[i,] <- (abs(lineup_usage_eff[i,])/sum(abs(lineup_usage_eff[i,])))*sum(lineup_usage_eff[i,])*under_over
  }
  player_lpl[is.na(player_lpl)] <- 0
  lineup_usage_eff_ps <- lineup_usage_eff
  player_lpl_ps <- player_lpl
  for(i in 1:5){
    player_lpl_ps[,i] <- player_lpl[,i]/(apply(lineup_attempts,1,sum))
  }
  if(LPL_type == "Rate"){
    return(player_lpl_ps)
  } else {
    return(player_lpl)
  }
}

