# Empirical LPL Demo

`%>%` = dplyr::`%>%`
load("shot_data_step_3.rda")
load("full_lineups_w_mins.rda")
source("discrete_court_regions.R")

# Choose a lineup to explore:
my_lineup_code <- "OKC_1"
lineup_shot_data <- shot_data_step_3 %>%
  dplyr::filter(lineup_code == my_lineup_code)


# Step 1 - calculate FG% for each player in lineup ------------------------
# We calculate FG% irrespective of lineup

player_codes <- c(lineup_shot_data$lineup_player_1[1],
                  lineup_shot_data$lineup_player_2[1],
                  lineup_shot_data$lineup_player_3[1],
                  lineup_shot_data$lineup_player_4[1],
                  lineup_shot_data$lineup_player_5[1])
player_names <- lineup_shot_data$namePlayer[match(player_codes,
                                                  lineup_shot_data$idPlayer)]

lineup_fgp <- matrix(0,10,5)
rownames(lineup_fgp) <- colnames(shot_data_step_3)[19:28]
colnames(lineup_fgp) <- player_codes

for(j in 1:10) {
  for(i in 1:length(player_codes)) {
     player_region_data <- shot_data_step_3[shot_data_step_3$idPlayer == player_codes[i] & 
                                               shot_data_step_3[,18+j] == 1, ]
    lineup_fgp[j,i] <- sum(player_region_data$isShotMade)/nrow(player_region_data)
  }
}
lineup_fgp[is.na(lineup_fgp)] <- 0

# Step 2 - calculate FGA rate for each player in lineup ------------------------
# We calculate FGA rate (per 36 minutes) with respect to lineup
  
lineup_mins <- full_lineups_w_mins$MIN[which(full_lineups_w_mins$lineup_code == my_lineup_code)]

lineup_fga <- matrix(0,10,5)
rownames(lineup_fga) <- colnames(shot_data_step_3)[19:28]
colnames(lineup_fga) <- player_codes

for(j in 1:10) {
  for(i in 1:length(player_codes)) {
    player_region_lineup_data <- shot_data_step_3[shot_data_step_3$idPlayer == player_codes[i] & 
                                              shot_data_step_3[,18+j] == 1 & 
                                                shot_data_step_3$lineup_code == my_lineup_code, ]
    lineup_fga[j,i] <- nrow(player_region_lineup_data) / lineup_mins * 36
  }
}


# Step 3 - Rank players by court region -----------------------------------

fgp_rank = t(apply(-1 * lineup_fgp, 1, rank, ties.method = "random"))
fga_rank = t(apply(-1 * lineup_fga, 1, rank, ties.method = "random"))

# Plot lineup
rank_colors <- RColorBrewer::brewer.pal(5, "RdYlBu")
par(mar = c(0,0,2,0))
par(oma = c(2,2,2,2))
layout(matrix(c(1:10,rep(11,5)), ncol=5, byrow=TRUE), heights=c(4,4,1))

for(i in 1:5){
  plot(court_regions(), 
       col = rank_colors[fgp_rank[,i]],
       main = player_names[i])
}
for(i in 1:5){
  plot(court_regions(), 
       col = rank_colors[fga_rank[,i]],
       main = player_names[i])
}
par(mai=c(0,0,0,0))
plot.new()
legend("center",legend = 1:5, fill = rank_colors, xpd = NA, horiz = T,
       title = "Rank", cex = 2)


# Step 4 - Rank Correspondence --------------------------------------------

rank_correspondence <- fga_rank - fgp_rank
rank_corr_colors <- RColorBrewer::brewer.pal(9, "RdBu")
par(mar = c(0,0,2,0))
par(oma = c(2,2,2,2))
layout(matrix(c(1:5,rep(6,5)), ncol=5, byrow=TRUE), heights=c(4,1))
for(i in 1:5){
  plot(court_regions(), 
       col = rank_corr_colors[rank_correspondence[,i]+5],
       main = player_names[i])
}
par(mai=c(0,0,0,0))
plot.new()
legend("center",legend = -4:4, fill = rank_corr_colors, xpd = NA, horiz = T,
       title = "Rank Correspondence", cex = 2)


# Step 5 - Calculate LPL ---------------------------------------------------

lineup_estimated_makes <- apply(lineup_fgp * lineup_fga,1,sum)
lineup_potential_makes <- NA
location_point_vals <- rep(2:3, each = 5)
old_shooting_order <- colnames(lineup_fgp)
player_lpl <- lineup_fga

for(i in 1:nrow(player_lpl)){
  new_shooting_order <- names(lineup_fgp[i,order(fgp_rank[i,])])
  actual_expected_makes = lineup_fgp[i, ] * lineup_fga[i, ] 
  optimum_expected_makes = (lineup_fgp[i,order(fgp_rank[i,])] *
                               lineup_fga[i,order(fga_rank[i,])])[match(old_shooting_order,new_shooting_order)]
  player_lpl[i,] = (optimum_expected_makes - actual_expected_makes) * location_point_vals[i]
}

LPL_per_shot <- apply(player_lpl,1,sum)/apply(lineup_fga,1,sum)
LPL_per_36 <- apply(player_lpl,1,sum)

# Plot
par(mfrow = c(2,1))
par(mar = c(0,0,4,4))
plot(court_regions(), 
     col = plotrix::color.scale(LPL_per_36,
                                cs1 = c(1,1), 
                                cs2 = c(1,0), 
                                cs3 = c(1,0), 
                                xrange = c(0,max(LPL_per_36))),
     main = paste0("LPL per 36\n",my_lineup_code))
col.labels <- c(0,round(max(LPL_per_36),digits = 2))
plotrix::color.legend(32,5,35,33,
                      col.labels,
                      plotrix::color.scale(seq(0,max(LPL_per_36),length.out = 20),
                                                         cs1 = c(1,1), 
                                                         cs2 = c(1,0), 
                                                         cs3 = c(1,0), 
                                                         xrange = c(0,max(LPL_per_36))),
             gradient="y")

plot(court_regions(), 
     col = plotrix::color.scale(LPL_per_shot,
                                cs1 = c(1,1), 
                                cs2 = c(1,0), 
                                cs3 = c(1,0),
                                xrange = c(0,max(LPL_per_shot))),
     main = paste0("LPL per shot\n",my_lineup_code))
col.labels <- c(0,round(max(LPL_per_shot),digits = 2))
plotrix::color.legend(32,5,35,33,
                      col.labels,
                      plotrix::color.scale(seq(0,max(LPL_per_shot),length.out = 20),
                                           cs1 = c(1,1), 
                                           cs2 = c(1,0), 
                                           cs3 = c(1,0), 
                                           xrange = c(0,max(LPL_per_shot))),
                      gradient="y")


# Step 6 - Calculate Player LPL contribution (PLC) --------------------------

player_lpl <- lineup_fga # Initialize matrix
player_plc <- lineup_fga # Initialize matrix

for(i in 1:nrow(player_plc)) {
  new_shooting_order <- names(lineup_fgp[i,order(fgp_rank[i,])])
  actual_expected_makes = lineup_fgp[i, ] * lineup_fga[i, ] 
  optimum_expected_makes = (lineup_fgp[i,order(fgp_rank[i,])] *
                              lineup_fga[i,order(fga_rank[i,])])[match(old_shooting_order,new_shooting_order)]
  player_lpl[i,] = (optimum_expected_makes - actual_expected_makes) * location_point_vals[i]
  under_over <- ifelse(player_lpl[i, ] < 0, 1, -1)
  player_plc[i,] <- (abs(player_lpl[i,])/sum(abs(player_lpl[i,])))*sum(player_lpl[i,])*under_over
}

player_plc[is.na(player_plc)] <- 0
player_plc_ps <- player_plc
for(i in 1:5){
  player_plc_ps[,i] <- player_plc[,i]/(apply(lineup_fga,1,sum))
}

# Plotting
par(mfrow = c(2,5))
par(oma = c(1,1,1,1))
par(mar = c(0,0,2,4))

# Plot PLC per 36
matrix_of_cols <- matrix(NA,10,5)
matrix_of_cols[player_plc <= 0] <- plotrix::color.scale(player_plc[player_plc <= 0],
                                                        cs1 = c(0,1), 
                                                        cs2 = c(0,1), 
                                                        cs3 = c(1,1), 
                                                        xrange = c(min(player_plc),0))
matrix_of_cols[player_plc >= 0] <- plotrix::color.scale(player_plc[player_plc >= 0],
                                                        cs1 = c(1,1), 
                                                        cs2 = c(1,0), 
                                                        cs3 = c(1,0), 
                                                        xrange = c(0, max(player_plc)))
for(i in 1:5){
  plot(court_regions(), 
       col = matrix_of_cols[,i],
       main = player_names[i])
}

plotrix::color.legend(35,5,40,33,
                      round(c(min(player_plc),
                        max(player_plc)),digits = 2),
                      plotrix::color.scale(c(seq(min(player_plc),0, length.out = 10),
                                             seq(0,max(player_plc), length.out = 10)),
                                           cs1 = c(0,1,1), 
                                           cs2 = c(0,1,0), 
                                           cs3 = c(1,1,0)),
                      cex = .75,
                      gradient="y")

# Plot PLC per shot
matrix_of_cols <- matrix(NA,10,5)
matrix_of_cols[player_plc_ps <= 0] <- plotrix::color.scale(player_plc_ps[player_plc_ps <= 0],
                                                           cs1 = c(0,1), 
                                                           cs2 = c(0,1), 
                                                           cs3 = c(1,1), 
                                                           xrange = c(min(player_plc_ps),0))
matrix_of_cols[player_plc_ps >= 0] <- plotrix::color.scale(player_plc_ps[player_plc_ps >= 0],
                                                           cs1 = c(1,1), 
                                                           cs2 = c(1,0), 
                                                           cs3 = c(1,0), 
                                                           xrange = c(0, max(player_plc_ps)))
for(i in 1:5){
  plot(court_regions(), 
       col = matrix_of_cols[,i],
       main = player_names[i])
}
plotrix::color.legend(35,5,40,33,
                      round(c(min(player_plc_ps),
                              max(player_plc_ps)),digits = 2),
                      plotrix::color.scale(c(seq(min(player_plc_ps),0, length.out = 10),
                                             seq(0,max(player_plc_ps), length.out = 10)),
                                           cs1 = c(0,1,1), 
                                           cs2 = c(0,1,0), 
                                           cs3 = c(1,1,0)),
                      cex = .75,
                      gradient="y")
