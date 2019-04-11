# Empirical Demo of Chuckers

This repository contains code to gather shot data from the NBA stats api and perform an empirical analysis of the allocative efficiency exploratory methods introduced in [Chuckers:	Measuring	Lineup	Shot	Distribution	
Optimality	Using	Spatial	Allocative	Efficiency	Models](http://www.sloansportsconference.com/wp-content/uploads/2019/02/Chuckers-1.pdf).  Our code relies on the r package [nbastatR](https://github.com/abresler/nbastatR) to gather shot data and from the NBA stats api.  The demo can be carried out by running the following scripts in order:
1.  `get_shot_data.R` retrieves the 2016-17 regular season shot data and play by play data from NBA stats.  
2.  `get_lineup_data.R` retrieves 2016-17 regular season lineup minutes for the top 250 lineups for each team and merges that data with the shot data.   
3.  `discrete_court_regions.R` defines a coarse discretization of the court for our empirical LPL demo.  For more nuanced spatial surfaces, we recommend following the modeling procedure outlined in our paper.    
4.  `empirical_lpl_demo.R` calculates and produces plots for a specified lineup's ranks, rank correspondence, LPL, and PLC surfaces empirically using the court regions defined in `dicrete_court_regions.R`.

Additionally, [Jason Baik](http://jsonbaik.rbind.io/about/) has created a [shiny app for this empirical demo](https://jsonbaik.shinyapps.io/nba-chuckers/), which provides an excellent tool to explore our methods for various teams and lineups.  
