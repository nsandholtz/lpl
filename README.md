# Empirical Demo of Lineup Points Lost (LPL)

This repository contains shot and lineup data from the NBA stats api (gathered via [nbastatR](https://github.com/abresler/nbastatR) in conjunction with the lineup construction code from https://github.com/jwmortensen/pbp2lineup) and code to perform an empirical analysis of the allocative efficiency methods introduced in our full paper [Measuring Spatial Allocative Efficiency in Basketball](https://arxiv.org/abs/1912.05129).

<!-- [Chuckers:	Measuring	Lineup	Shot	Distribution	
Optimality	Using	Spatial	Allocative	Efficiency	Models](http://www.sloansportsconference.com/wp-content/uploads/2019/02/Chuckers-1.pdf). -->

The demo can be carried out by running `empirical_lpl_demo.R`, which calculates and produces plots for a specified lineup's ranks, rank correspondence, LPL, and PLC surfaces empirically using the court regions defined in `dicrete_court_regions.R`. 

<!--1.  `get_shot_data.R` retrieves the 2016-17 regular season shot data and play by play data from NBA stats.  
2.  `get_lineup_data.R` retrieves 2016-17 regular season lineup minutes for the top 250 lineups for each team and merges that data with the shot data. 
1.  `discrete_court_regions.R` defines a coarse discretization of the court for our empirical LPL demo.  For more nuanced spatial surfaces, we recommend following the modeling procedure outlined in our paper. -->     


## Description of Plots/Metrics

While we recommend that users read our paper to understand the full details for each metric shown in this demo, we've provided a list of high-level definitions for reference: 

- **Rank Plots:**  Ranks of FG% (Field Goal %) and FGA (Field Goal Attempts) for each region of the court for all 5 players of a given lineup code.  FG% is calculated as the percentage of shots that player X made.  FGA rate is the number of shot attempts per 36 minutes by player X

- **Rank Correspondence:**  Rank Correspondence is defined as <p align="center">(Rank of FGA) - (Rank of FG%)</p> This measures how strongly each player's FG% rank matches their FGA rank. A Rank Correspondence bigger than 0 is labeled as `under-use` because player X is taking *fewer* shots than his FG% warrants. On the other hand, a Rank Correspondence smaller than 0 is labelled as `over-use` because player X is taking *more* shots than his FG% warrants.

- **Lineup Points Lost (LPL):**   LPL is defined as the difference in expected points between the actual distribution of shot attempts from a given lineup and the expected points had those same shots been taken according to the optimal redistribution.  Specifically, for court region i <p align="center"> <img src="https://github.com/nsandholtz/lpl/blob/master/lpl_formula.jpg" width="300"> </p> where j indexes the 5 players in the lineup.  In theory, higher LPL values correspond to greater inefficiencies while lower LPL entail higher efficiency.  The demo shows LPL per 36 minutes and LPL per shot.

- **Player LPL Contribution (PLC):**  PLC is defined as each player's individual contribution to LPL, while maintaining the directionality of their contribution (i.e. whether their contribution is due to over-use or under-use).  These plots in this section show PLC per 36 minutes and PLC per shot for the 5 players in a given lineup.
