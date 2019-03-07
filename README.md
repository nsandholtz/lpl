# lpl

Our code relies on the r package [nbastatR](https://github.com/abresler/nbastatR) to gather shot data and from the NBA stats api.  
1.  `get_shot_data.R` gets the necessary shot data and play by play data from NBA stats.  
2.  `get_lineup_data.R` gets lineup minutes and merges that data with the shot data to get all necessary data needed to calculate LPL.  
3.  `discrete_court_regions.R` defines a coarse discretization of the court used in the empirical LPL demo.  For more nuanced spatial surfaces, we recommend following the procedure outlined in our paper.    
4.  `empirical_lpl_demo.R` calculates rank surfaces, rank correspondence, LPL, and PLC surfaces empirically using the court regions defined in `dicrete_court_regions.R`.
