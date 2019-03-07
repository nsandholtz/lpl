`%>%` = dplyr::`%>%`

# Source functions to assign shots to court regions
source("discrete_court_regions.R")

# Global parameters
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius

teams = c(
  "Atlanta Hawks",
  "Boston Celtics",
  "Brooklyn Nets",
  "Charlotte Hornets",
  "Chicago Bulls",
  "Cleveland Cavaliers",
  "Dallas Mavericks",
  "Denver Nuggets",
  "Detroit Pistons",
  "Golden State Warriors",
  "Houston Rockets",
  "Indiana Pacers",
  "Los Angeles Clippers",
  "Los Angeles Lakers",
  "Memphis Grizzlies",
  "Miami Heat",
  "Milwaukee Bucks",
  "Minnesota Timberwolves",
  "New Orleans Pelicans",
  "New York Knicks",
  "Oklahoma City Thunder",
  "Orlando Magic",
  "Philadelphia 76ers",
  "Phoenix Suns",
  "Portland Trail Blazers", 
  "Sacramento Kings",
  "San Antonio Spurs",
  "Toronto Raptors",
  "Utah Jazz",
  "Washington Wizards"
)

# Get 2016-2017 NBA shot data
shot_df = nbastatR::teams_shots(teams = teams, seasons = 2017)

# Get lineup data for each shot
process_pbp = function(game_id) {
  pbp_url = paste0(
    "https://stats.nba.com/stats/playbyplayv2?GameID=",
    game_id,
    "&StartPeriod=0&EndPeriod=12"
  )
  json = rjson::fromJSON(file = pbp_url, method = "C")

  ncol = length(json$resultSets[[1]][[2]])
  vals = json$resultSets[[1]][[3]]
  for (i in 1:length(vals)) {
    vals[[i]][sapply(vals[[i]], is.null)] = NA
  }
  
  pbp_df = data.frame(matrix(unlist(vals), ncol = ncol, byrow = TRUE), 
                      stringsAsFactors = FALSE)
  colnames(pbp_df) = json$resultSets[[1]][[2]]
  pbp_df = pbp_df %>%
    dplyr::mutate(PERIOD_LAG = dplyr::lag(PERIOD))
  
  teams = sort(na.omit(unique(pbp_df$PLAYER1_TEAM_ABBREVIATION)))
  
  team_one_lineup_mat = team_two_lineup_mat = 
    matrix(NA_character_, nrow = nrow(pbp_df), ncol = 5)
  colnames(team_one_lineup_mat) = paste0("team_one_player_", 1:5)
  colnames(team_two_lineup_mat) = paste0("team_two_player_", 1:5)
  
  for (i in 1:nrow(pbp_df)) {
    # if it's the beginning of a period, reset lineups
    period = pbp_df[i, ] %>%
      dplyr::select(PERIOD, PERIOD_LAG)
    
    if (is.na(period$PERIOD_LAG) || period$PERIOD != period$PERIOD_LAG) {
      team_one_lineup = team_two_lineup = rep(NA, 5)
    }
    
    for (j in 1:3) {
      # check for subs and remove the appropriate player if necessary
      if (j == 1) {
        
        events = pbp_df[i, ] %>%
          dplyr::select(dplyr::matches("DESCRIPTION")) %>%
          as.character()
        subs = any(grepl("SUB", events))
        
        if (subs) {
          player_id = pbp_df[i, ] %>%
            dplyr::select(dplyr::matches(sprintf("PLAYER%s_ID", j))) %>%
            dplyr::pull()
          player_team = pbp_df[i, ] %>%
            dplyr::select(dplyr::matches(sprintf("PLAYER%s_TEAM_ABBREVIATION",j))) %>%
            dplyr::pull()
          if (player_team == teams[1]) {
            if (player_id %in% team_one_lineup) {
              team_one_lineup[team_one_lineup == player_id] = NA
            } else {
              team_one_lineup_mat[i-1, ][is.na(team_one_lineup_mat[i-1, ])] =
player_id
            }
          } else if (player_team == teams[2]) {
            if (player_id %in% team_two_lineup) {
              team_two_lineup[team_two_lineup == player_id] = NA
            } else {
              team_two_lineup_mat[i-1, ][is.na(team_two_lineup_mat[i-1, ])] =
player_id
            }
          } else {
            stop("error")
          }
          # if there are subs increment player index
          j = j + 1
        }
      }
      
      player_id = pbp_df[i, ] %>%
        dplyr::select(dplyr::matches(sprintf("PLAYER%s_ID", j))) %>%
        dplyr::pull()
      player_team = pbp_df[i, ] %>%
        dplyr::select(dplyr::matches(sprintf("Player%s_TEAM_ABBREVIATION", j))) %>%
        dplyr::pull()
      if (!is.na(player_team) && player_id != 0) {
        if (player_team == teams[1] & !(player_id %in% team_one_lineup)) {
          if (sum(is.na(team_one_lineup)) > 0) {
            team_one_lineup[which(is.na(team_one_lineup))[1]] = player_id
          } else {
            # jwm::err_msg(paste0("error for row ", i, " in game ", game_id))
          }
        } else if (player_team == teams[2] & !(player_id %in% team_two_lineup))
{
          if (sum(is.na(team_two_lineup)) > 0) {
            team_two_lineup[is.na(team_two_lineup)][1] = player_id
          } else {
            # jwm::err_msg(paste0("error for row ", i, " in game ", game_id))
          }
        }
      }
    }
    team_one_lineup = sort(team_one_lineup, na.last = TRUE)
    team_two_lineup = sort(team_two_lineup, na.last = TRUE)
    
    team_one_lineup_mat[i, ] = team_one_lineup
    team_two_lineup_mat[i, ] = team_two_lineup
  }
  
  fill_missing_lineup = function(lineup_mat) {
    lineup_sort = apply(lineup_mat, 1, sort, na.last = TRUE) %>%
      unlist() %>%
      matrix(nrow = nrow(pbp_df), byrow = TRUE)
    na_idx = apply(lineup_sort, 1, function(x) { any(is.na(x)) })
    lineup_sort[na_idx, ] = rep(NA, 5)
    lineup_final = zoo::na.locf(lineup_sort, na.rm = FALSE, fromLast = TRUE)
    colnames(lineup_final) = colnames(lineup_mat)
    assertthat::assert_that(all(dim(lineup_mat) == dim(lineup_final)))
    lineup_final
  }
  
  team_one_lineup_final = fill_missing_lineup(team_one_lineup_mat)
  team_two_lineup_final = fill_missing_lineup(team_two_lineup_mat)
  
  pbp_df = cbind(pbp_df, team_one_lineup_final, team_two_lineup_final)
  pbp_df
}

game_ids = sprintf("%010d", unique(shot_df$idGame))
pbp_list = vector("list", length = length(game_ids))

for (i in 1:length(game_ids)) {
  cat(i)
  pbp_list[[i]] = process_pbp(game_ids[i])
  cat("\r")
}


pbp_df = do.call(rbind.data.frame, pbp_list)
pbp_df_numeric = pbp_df %>%
  dplyr::mutate(idGame = as.numeric(GAME_ID),
                idEvent = as.numeric(EVENTNUM))
shot_lineup_df = shot_df %>%
  dplyr::left_join(pbp_df_numeric, by = c("idGame", "idEvent"))

tmp <- data.frame(lapply(shot_lineup_df[,c(62:71)], 
                         function(x) as.numeric(as.character(x)) ),
                  stringsAsFactors = F)
shot_lineup_df[,c(62:71)] <- tmp

shot_lineup_df$is_team_two <- ifelse(shot_lineup_df$idPlayer == shot_lineup_df$team_two_player_1 | 
                shot_lineup_df$idPlayer == shot_lineup_df$team_two_player_2 |
                shot_lineup_df$idPlayer == shot_lineup_df$team_two_player_3 |
                shot_lineup_df$idPlayer == shot_lineup_df$team_two_player_4 |
                shot_lineup_df$idPlayer == shot_lineup_df$team_two_player_5 , T, F)
shot_lineup_df$is_team_one <- ifelse(shot_lineup_df$idPlayer == shot_lineup_df$team_one_player_1 | 
                shot_lineup_df$idPlayer == shot_lineup_df$team_one_player_2 |
                shot_lineup_df$idPlayer == shot_lineup_df$team_one_player_3 |
                shot_lineup_df$idPlayer == shot_lineup_df$team_one_player_4 |
                shot_lineup_df$idPlayer == shot_lineup_df$team_one_player_5 , T, F)

tmp1 <- shot_lineup_df %>%
  dplyr::filter(is_team_one) %>%
  dplyr::select(slugSeason,
                idGame,
                numberPeriod,
                minutesRemaining,
                secondsRemaining,
                idTeam,
                nameTeam,
                idPlayer,
                namePlayer,
                locationX,
                locationY,
                isShotMade,
                team_one_player_1,
                team_one_player_2,
                team_one_player_3,
                team_one_player_4,
                team_one_player_5
                )

tmp2 <- shot_lineup_df %>%
  dplyr::filter(is_team_two) %>%
  dplyr::select(slugSeason,
                idGame,
                numberPeriod,
                minutesRemaining,
                secondsRemaining,
                idTeam,
                nameTeam,
                idPlayer,
                namePlayer,
                locationX,
                locationY,
                isShotMade,
                team_two_player_1,
                team_two_player_2,
                team_two_player_3,
                team_two_player_4,
                team_two_player_5
  )

names(tmp1) <- names(tmp2) <- c("Season",
                                "idGame",
                                "numberPeriod",
                                "minutesRemaining",
                                "secondsRemaining",
                                "idTeam",
                                "nameTeam",
                                "idPlayer",
                                "namePlayer",
                                "locationX",
                                "locationY",
                                "isShotMade",
                                "lineup_player_1",
                                "lineup_player_2",
                                "lineup_player_3",
                                "lineup_player_4",
                                "lineup_player_5")

shot_data_step_2 <- rbind(tmp1, tmp2)
shot_data_step_2[,13:17] <- t(apply(as.matrix(shot_data_step_2[,13:17]), 1, sort))
shot_data_step_2 <- shot_data_step_2 %>%
  dplyr::left_join(df_dict_nba_teams[,c("idTeam", "slugTeam")], by = "idTeam") %>%
  dplyr::mutate(locationX = locationX/10,
                locationY = locationY/10 + hoop_center_y)
shot_data_step_2 <- cbind.data.frame(shot_data_step_2, 
                                    as.data.frame(assign_region(shot_data_step_2$locationX,
                                                                shot_data_step_2$locationY)))

save(shot_data_step_2, file = "shot_data_step_2.rda")
       




