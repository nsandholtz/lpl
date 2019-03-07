# lpl

Our code relies on the r package [nbastatR](https://github.com/abresler/nbastatR) to gather shot data and from the NBA stats api.  
1.  `get_data.R` gets the necessary data from NBA stats.  
2.  `synthesize_lineups.R` merges the play by play data with the shot data to get lineup data for each shot.  
3.  `estimate_FG.R` performes empirical player FG\%'s and player-lineup FGA rate estimation on a coarse discretization of the court.  For more nuanced spatial surfaces, we recommend following Cervone et. al. (2016).    
4.  `calculate_LPL_PLC` calculates LPL and PLC for lineups given their FG\% and FGA rate surfaces.  
