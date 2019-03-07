`%>%` = dplyr::`%>%`

load("shot_data_step_2.rda")

nba_teams <- unique(shot_data_step_2$slugTeam)
full_lineups <- NULL
for(team in nba_teams){
  team_data <- shot_data_step_2 %>% 
    dplyr::filter(slugTeam == team) %>%
    dplyr::select("lineup_player_1",
                  "lineup_player_2",
                  "lineup_player_3",
                  "lineup_player_4",
                  "lineup_player_5") %>%
    dplyr::distinct() 
  full_lineups <- rbind(full_lineups, team_data)
}


# Get lineup minutes ------------------------------------------------------

get.json_data <- function(url, use_read_lines = TRUE, 
                          is_tibble = F, is_flattened = T) {
  if (use_read_lines) {
    data <-
      url %>%
      readr::read_lines() %>%
      jsonlite::fromJSON(flatten = is_flattened, simplifyDataFrame = is_tibble)
    return(data)
  }
  
  url %>%
    jsonlite::fromJSON(flatten = is_flattened, simplifyDataFrame = is_tibble)
}


# The object 'df_dict_nba_teams' is created when nbastatR::teams_shots() is called
nba_team_ids <- df_dict_nba_teams %>% 
  dplyr::filter(slugTeam %in% nba_teams,
                !is.na(df_dict_nba_teams$teamName)) %>%
  dplyr::pull(idTeam)

nba_team_abrev <- df_dict_nba_teams %>% 
  dplyr::filter(slugTeam %in% nba_teams,
                !is.na(df_dict_nba_teams$teamName)) %>%
  dplyr::pull(slugTeam)

full_lineups_w_mins <- NULL
for(i in 25:length(nba_team_ids)) {
  cat(i)
  url <- paste0("https://stats.nba.com/stats/teamdashlineups?TeamID=%20",
                nba_team_ids[i],
                "&GroupQuantity=5&PaceAdjust=N&Rank=N&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PerMode=Totals&Period=0&PlusMinus=N&Position=&Season=2016-17&SeasonSegment=&SeasonType=Regular+Season&VsConference=&VsDivision=&mode=Advanced")
  tmp <- get.json_data(url)
  team_lineup_data <-
    as.data.frame(tmp[[3]][[2]]$rowSet, stringsAsFactors = F)
  names(team_lineup_data) <- tmp[[3]][[2]]$headers
  tmp3 <- team_lineup_data %>%
    dplyr::mutate(
      GP = as.numeric(GP),
      MIN = as.numeric(MIN),
      FGM = as.numeric(FGM),
      FGA = as.numeric(FGA)
    ) %>%
    dplyr::select(GROUP_ID,
                  GP,
                  MIN,
                  FGM,
                  FGA)
  tmp3$lineup_player_1 <- NA
  tmp3$lineup_player_2 <- NA
  tmp3$lineup_player_3 <- NA
  tmp3$lineup_player_4 <- NA
  tmp3$lineup_player_5 <- NA
  tmp3[, 6:10] <- matrix(unlist(lapply(tmp3$GROUP_ID,
                                       function(x) {
                                         sort(as.numeric(unlist(strsplit(x,
                                                                         split = "-"))[-1]))
                                       })),
                         250, 5, byrow = T)
  tmp4 <- tmp3 %>% dplyr::mutate(lineup_code = paste(nba_team_abrev[i],
                                             1:dplyr::n(), sep = "_"))
  full_lineups_w_mins <-
    rbind(full_lineups_w_mins, tmp4[,-1])
  cat("\r")
}

full_lineups_w_mins <- full_lineups %>%
  dplyr::left_join(
    full_lineups_w_mins,
    by = c(
      "lineup_player_1",
      "lineup_player_2",
      "lineup_player_3",
      "lineup_player_4",
      "lineup_player_5"
    )
  )
save(full_lineups_w_mins, file = "full_lineups_w_mins.rda")

shot_data_step_3 <- shot_data_step_2 %>%
  dplyr::left_join(
    full_lineups_w_mins[, c(1:5, 10)],
    by = c(
      "lineup_player_1",
      "lineup_player_2",
      "lineup_player_3",
      "lineup_player_4",
      "lineup_player_5"
    )
  )
save(shot_data_step_3, file = "shot_data_step_3.rda")


