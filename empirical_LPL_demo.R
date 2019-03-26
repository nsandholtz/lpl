# Empirical LPL Demo

`%>%` = dplyr::`%>%`
load("shot_data_step_2.rda")
load("full_lineups_w_mins.rda")
source("discrete_court_regions.R")

# Choose a lineup to explore:
my_lineup_code <- "GSW_1"
lineup_shot_data <- shot_data_step_2 %>%
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
rownames(lineup_fgp) <- colnames(shot_data_step_2)[19:28]
colnames(lineup_fgp) <- player_codes

for(j in 1:10) {
  for(i in 1:length(player_codes)) {
     player_region_data <- shot_data_step_2[shot_data_step_2$idPlayer == player_codes[i] & 
                                              shot_data_step_2[,18+j] == 1, ]
    lineup_fgp[j,i] <- sum(player_region_data$isShotMade)/nrow(player_region_data)
  }
}
# Impute a FG% of 0 for regions where no shots were taken:
lineup_fgp[is.na(lineup_fgp)] <- 0

# Step 2 - calculate FGA rate for each player in lineup ------------------------
# We calculate FGA rate (per 36 minutes) with respect to lineup
  
lineup_mins <- full_lineups_w_mins$MIN[which(full_lineups_w_mins$lineup_code == my_lineup_code)]

lineup_fga <- matrix(0,10,5)
rownames(lineup_fga) <- colnames(shot_data_step_2)[19:28]
colnames(lineup_fga) <- player_codes

for(j in 1:10) {
  for(i in 1:length(player_codes)) {
    player_region_lineup_data <- shot_data_step_2[shot_data_step_2$idPlayer == player_codes[i] & 
                                                    shot_data_step_2[,18+j] == 1 & 
                                                    shot_data_step_2$lineup_code == my_lineup_code, ]
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
       main = paste0("FG% Rank\n",player_names[i]))
}
for(i in 1:5){
  plot(court_regions(), 
       col = rank_colors[fga_rank[,i]],
       main = paste0("FGA Rate Rank\n",player_names[i]))
}
par(mai=c(0,0,0,0))
plot.new()
legend("center",legend = 1:5, fill = rank_colors, xpd = NA, horiz = T,
       title = "Rank", cex = 2)

# ggplot version

gg_court <- ggplot2::fortify(court_regions())
gg_lineup_fgp <- NULL
gg_lineup_fga <- NULL
for(i in 1:dim(fgp_rank)[2]){
  gg_player_fgp <- gg_court
  gg_player_fgp$rank <- NA
  gg_player_fga <- gg_court
  gg_player_fga$rank <- NA
  for (j in 1:nrow(fgp_rank)){
    gg_player_fgp$rank[gg_player_fgp$id == row.names(fgp_rank)[j]] <- fgp_rank[j,i]
    gg_player_fga$rank[gg_player_fga$id == row.names(fga_rank)[j]] <- fga_rank[j,i]
  }
  gg_player_fgp$rank <- factor(gg_player_fgp$rank, levels = 1:5)
  gg_player_fgp$player <- player_names[i]
  gg_lineup_fgp <- rbind(gg_lineup_fgp, gg_player_fgp)
  
  gg_player_fga$rank <- factor(gg_player_fga$rank, levels = 1:5)
  gg_player_fga$player <- player_names[i]
  gg_lineup_fga <- rbind(gg_lineup_fga, gg_player_fga)
}

fgp_rank_plot <- ggplot2::ggplot(data = gg_lineup_fgp, ggplot2::aes(x=long, 
                                                                  y=lat, 
                                                                  group = group,
                                                                  fill = rank)) +
  ggplot2::facet_grid(. ~ player) +
  ggplot2::geom_polygon()  +
  ggplot2::geom_path(color = "black") +
  ggplot2::coord_equal() +
  ggplot2::scale_fill_manual(values = rank_colors,
                             limits = 1:5,
                             name = "FG% Rank") +
  ggplot2::theme(axis.line=ggplot2::element_blank(),
                 axis.text.x=ggplot2::element_blank(),
                 axis.text.y=ggplot2::element_blank(),
                 axis.ticks=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 panel.background=ggplot2::element_blank(),
                 panel.border=ggplot2::element_blank(),
                 panel.grid.major=ggplot2::element_blank(),
                 panel.grid.minor=ggplot2::element_blank(),
                 strip.text = ggplot2::element_text(size = 16), 
                 strip.background = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 14),
                 legend.title = ggplot2::element_text(size = 16),
                 legend.justification = "left",
                 legend.position = "right",
                 # legend.margin = ggplot2::margin(0.1, 0.1, 0.1, 0, "npc"),
                 plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
                 panel.spacing.y = ggplot2::unit(0, "cm"))

fga_rank_plot <- ggplot2::ggplot(data = gg_lineup_fga, ggplot2::aes(x=long, 
                                                                y=lat, 
                                                                group = group,
                                                                fill = rank)) +
  ggplot2::facet_grid(. ~ player) +
  ggplot2::geom_polygon()  +
  ggplot2::geom_path(color = "black") +
  ggplot2::coord_equal() +
  ggplot2::scale_fill_manual(values = rank_colors,
                             limits = 1:5,
                             name = "FGA Rank") +
  ggplot2::theme(axis.line=ggplot2::element_blank(),
                 axis.text.x=ggplot2::element_blank(),
                 axis.text.y=ggplot2::element_blank(),
                 axis.ticks=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 panel.background=ggplot2::element_blank(),
                 panel.border=ggplot2::element_blank(),
                 panel.grid.major=ggplot2::element_blank(),
                 panel.grid.minor=ggplot2::element_blank(),
                 strip.text = ggplot2::element_text(size = 16), 
                 strip.background = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 14),
                 legend.title = ggplot2::element_text(size = 16),
                 legend.justification = "left",
                 legend.position = "right",
                 # legend.margin = ggplot2::margin(0.1, 0.1, 0.1, 0, "npc"),
                 plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
                 panel.spacing.y = ggplot2::unit(0, "cm"))

cowplot::plot_grid(fgp_rank_plot, 
                   fga_rank_plot, 
                   nrow = 2, 
                   align = "v")

# Step 4 - Rank Correspondence --------------------------------------------

rank_correspondence <- fga_rank - fgp_rank
rank_corr_colors <- RColorBrewer::brewer.pal(9, "RdBu")
par(mar = c(0,0,3,0))
par(oma = c(2,2,2,2))
par(mfrow = c(1,6))
for(i in 1:5){
  plot(court_regions(), 
       col = rank_corr_colors[rank_correspondence[,i]+5],
       main = bquote(atop(.(player_names[i]),
                          "r"["FGA"] - "r"["FG%"])))
}
plot.new()
legend("center",legend = c("-4 (Over-use)", -3:3, "4 (Under-use)"), 
       fill = rank_corr_colors, 
       xpd = NA, 
       horiz = F,
       title = "Rank Correspondence", cex = 1)

# ggplot version

gg_court <- ggplot2::fortify(court_regions())
gg_lineup_corr <- NULL
for(i in 1:dim(fgp_rank)[2]){
  gg_player_corr <- gg_court
  gg_player_corr$rank <- NA
  for (j in 1:nrow(fgp_rank)){
    gg_player_corr$rank[gg_player_fgp$id == row.names(fgp_rank)[j]] <- fga_rank[j,i] - fgp_rank[j,i]
  }
  gg_player_corr$rank <- factor(gg_player_corr$rank, levels = 4:(-4))
  levels(gg_player_corr$rank)[1] = "4 (Under-Use)"
  levels(gg_player_corr$rank)[9] = "-4 (Over-Use)"
  gg_player_corr$player <- player_names[i]
  gg_lineup_corr <- rbind(gg_lineup_corr, gg_player_corr)
}

rank_corr_plot <- ggplot2::ggplot(data = gg_lineup_corr, ggplot2::aes(x=long, 
                                                                    y=lat, 
                                                                    group = group,
                                                                    fill = rank)) +
  ggplot2::facet_grid(. ~ player) +
  ggplot2::geom_polygon()  +
  ggplot2::geom_path(color = "black") +
  ggplot2::coord_equal() +
  ggplot2::scale_fill_brewer(type = "div",
                             palette = "RdBu",
                             direction = 1,
                             na.value = "grey60",
                             name = "Rank\nCorrespondence",
                             drop = FALSE) +
  ggplot2::theme(axis.line=ggplot2::element_blank(),
                 axis.text.x=ggplot2::element_blank(),
                 axis.text.y=ggplot2::element_blank(),
                 axis.ticks=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 panel.background=ggplot2::element_blank(),
                 panel.border=ggplot2::element_blank(),
                 panel.grid.major=ggplot2::element_blank(),
                 panel.grid.minor=ggplot2::element_blank(),
                 strip.text = ggplot2::element_text(size = 16), 
                 strip.background = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 14),
                 legend.title = ggplot2::element_text(size = 16),
                 legend.justification = "left",
                 legend.position = "right",
                 # legend.margin = ggplot2::margin(0.1, 0.1, 0.1, 0, "npc"),
                 plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
                 panel.spacing.y = ggplot2::unit(0, "cm"))
rank_corr_plot

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
plotrix::color.legend(35,5,40,33,
                      col.labels,
                      plotrix::color.scale(seq(0,max(LPL_per_36),length.out = 20),
                                                         cs1 = c(1,1), 
                                                         cs2 = c(1,0), 
                                                         cs3 = c(1,0), 
                                                         xrange = c(0,max(LPL_per_36))),
             gradient="y")
text(37,36,"LPL p/36", font = 2, cex = .75)

plot(court_regions(), 
     col = plotrix::color.scale(LPL_per_shot,
                                cs1 = c(1,1), 
                                cs2 = c(1,0), 
                                cs3 = c(1,0),
                                xrange = c(0,max(LPL_per_shot))),
     main = paste0("LPL per shot\n",my_lineup_code))
col.labels <- c(0,round(max(LPL_per_shot),digits = 2))
plotrix::color.legend(35,5,40,33,
                      col.labels,
                      plotrix::color.scale(seq(0,max(LPL_per_shot),length.out = 20),
                                           cs1 = c(1,1), 
                                           cs2 = c(1,0), 
                                           cs3 = c(1,0), 
                                           xrange = c(0,max(LPL_per_shot))),
                      gradient="y")
text(37,36,"LPL p/shot", font = 2, cex = .75)

# ggplot version

gg_LPL <- ggplot2::fortify(court_regions())
gg_LPL$lpl_36 <- NA
gg_LPL$lpl_shot <- NA
for (j in 1:length(LPL_per_36)){
  gg_LPL$lpl_36[gg_LPL$id == names(LPL_per_36)[j]] <- LPL_per_36[j]
  gg_LPL$lpl_shot[gg_LPL$id == names(LPL_per_shot)[j]] <- LPL_per_shot[j]
}

gg_LPL_36_plot <- ggplot2::ggplot(data = gg_LPL, ggplot2::aes(x=long, 
                                                           y=lat, 
                                                           group = group,
                                                           fill = lpl_36)) +
  ggplot2::geom_polygon()  +
  ggplot2::geom_path(color = "black") +
  ggplot2::coord_equal() +
  ggplot2::scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(11, "RdYlBu")),
                                name = "LPL per 36") +
  ggplot2::theme(axis.line=ggplot2::element_blank(),
                 axis.text.x=ggplot2::element_blank(),
                 axis.text.y=ggplot2::element_blank(),
                 axis.ticks=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 panel.background=ggplot2::element_blank(),
                 panel.border=ggplot2::element_blank(),
                 panel.grid.major=ggplot2::element_blank(),
                 panel.grid.minor=ggplot2::element_blank(),
                 strip.text = ggplot2::element_text(size = 16), 
                 strip.background = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 14),
                 legend.title = ggplot2::element_text(size = 16),
                 legend.justification = "left",
                 legend.position = "right",
                 plot.title = ggplot2::element_text(hjust = 0.5, 
                                                    size = 16,
                                                    face = "bold"),
                 plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
                 panel.spacing.y = ggplot2::unit(0, "cm")) + 
  ggplot2::ggtitle(label = "LPL per 36")

gg_LPL_shot_plot <- ggplot2::ggplot(data = gg_LPL, ggplot2::aes(x=long, 
                                                              y=lat, 
                                                              group = group,
                                                              fill = lpl_shot)) +
  ggplot2::geom_polygon()  +
  ggplot2::geom_path(color = "black") +
  ggplot2::coord_equal() +
  ggplot2::scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(11, "RdYlBu")),
                                name = "LPL per Shot") +
  ggplot2::theme(axis.line=ggplot2::element_blank(),
                 axis.text.x=ggplot2::element_blank(),
                 axis.text.y=ggplot2::element_blank(),
                 axis.ticks=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 panel.background=ggplot2::element_blank(),
                 panel.border=ggplot2::element_blank(),
                 panel.grid.major=ggplot2::element_blank(),
                 panel.grid.minor=ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(hjust = 0.5, 
                                                    size = 16,
                                                    face = "bold"),
                 strip.text = ggplot2::element_text(size = 16), 
                 strip.background = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 14),
                 legend.title = ggplot2::element_text(size = 16),
                 legend.justification = "left",
                 legend.position = "right",
                 plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
                 panel.spacing.y = ggplot2::unit(0, "cm")) + 
  ggplot2::ggtitle(label = "LPL per Shot")

cowplot::plot_grid(gg_LPL_36_plot, 
                   gg_LPL_shot_plot, 
                   nrow = 1, 
                   align = "h")


# Step 6 - Calculate Player LPL contribution (PLC) --------------------------

player_lpl <- lineup_fga # Initialize matrix
player_plc <- lineup_fga # Initialize matrix

for(i in 1:nrow(player_plc)) {
  new_shooting_order <- names(lineup_fgp[i,order(fgp_rank[i,])])
  actual_expected_makes = lineup_fgp[i, ] * lineup_fga[i, ] 
  optimum_expected_makes = (lineup_fgp[i,order(fgp_rank[i,])] *
                              lineup_fga[i,order(fga_rank[i,])])[match(old_shooting_order,new_shooting_order)]
  player_lpl[i,] = (optimum_expected_makes - actual_expected_makes) * location_point_vals[i]
  under_over <- ifelse(player_lpl[i, ] < 0, -1, 1)
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
       main = paste0("PLC per 36\n",player_names[i]))
}
legend_helper <- 10/abs(min(player_plc))
plotrix::color.legend(35,5,40,33,
                      round(c(min(player_plc),
                              max(player_plc)),digits = 2),
                      c(plotrix::color.scale(seq(min(player_plc),0, length.out = round(abs(min(player_plc))*legend_helper)),
                                             cs1 = c(0,1), 
                                             cs2 = c(0,1), 
                                             cs3 = c(1,1)),
                        plotrix::color.scale(seq(0,max(player_plc), length.out = round(abs(max(player_plc))*legend_helper)),
                                             cs1 = c(1,1), 
                                             cs2 = c(1,0), 
                                             cs3 = c(1,0))),
                      cex = .75,
                      gradient="y")
par(xpd = T)
text(36,36,"Under-use", font = 2, cex = .75)
text(36,0,"Over-use", font = 2, cex = .75)

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
       main = paste0("PLC per Shot\n",player_names[i]))
}
legend_helper <- 10/abs(min(player_plc_ps))
plotrix::color.legend(35,5,40,33,
                      round(c(min(player_plc_ps),
                              max(player_plc_ps)),digits = 2),
                      c(plotrix::color.scale(seq(min(player_plc_ps),0, length.out = round(abs(min(player_plc_ps))*legend_helper)),
                                           cs1 = c(0,1), 
                                           cs2 = c(0,1), 
                                           cs3 = c(1,1)),
                        plotrix::color.scale(seq(0,max(player_plc_ps), length.out = round(abs(max(player_plc_ps))*legend_helper)),
                                             cs1 = c(1,1), 
                                             cs2 = c(1,0), 
                                             cs3 = c(1,0))),
                      cex = .75,
                      gradient="y")
par(xpd = T)
text(36,36,"Under-use", font = 2, cex = .75)
text(36,0,"Over-use", font = 2, cex = .75)

# ggplot version

gg_court <- ggplot2::fortify(court_regions())
gg_lineup_PLC <- NULL
for(i in 1:dim(fgp_rank)[2]){
  gg_player_PLC <- gg_court
  gg_player_PLC$plc_36 <- NA
  gg_player_PLC$plc_shot <- NA
  for (j in 1:nrow(fgp_rank)){
    gg_player_PLC$plc_36[gg_player_PLC$id == row.names(player_plc)[j]] <- player_plc[j,i] 
    gg_player_PLC$plc_shot[gg_player_PLC$id == row.names(player_plc_ps)[j]] <- player_plc_ps[j,i] 
  }
  gg_player_PLC$player <- player_names[i]
  gg_lineup_PLC <- rbind(gg_lineup_PLC, gg_player_PLC)
}

PLC_per_36_plot <- ggplot2::ggplot(data = gg_lineup_PLC, ggplot2::aes(x=long, 
                                                                      y=lat, 
                                                                      group = group,
                                                                      fill = plc_36)) +
  ggplot2::facet_grid(. ~ player) +
  ggplot2::geom_polygon()  +
  ggplot2::geom_path(color = "black") +
  ggplot2::coord_equal() +
  ggplot2::scale_fill_gradient2(low = "blue",
                                high = "red",
                                mid = "white", midpoint = 0,
                                name = "PLC per 36") +
  ggplot2::theme(axis.line=ggplot2::element_blank(),
                 axis.text.x=ggplot2::element_blank(),
                 axis.text.y=ggplot2::element_blank(),
                 axis.ticks=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 panel.background=ggplot2::element_blank(),
                 panel.border=ggplot2::element_blank(),
                 panel.grid.major=ggplot2::element_blank(),
                 panel.grid.minor=ggplot2::element_blank(),
                 strip.text = ggplot2::element_text(size = 16), 
                 strip.background = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 14),
                 legend.title = ggplot2::element_text(size = 16),
                 legend.justification = "left",
                 legend.position = "right",
                 # legend.margin = ggplot2::margin(0.1, 0.1, 0.1, 0, "npc"),
                 plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
                 panel.spacing.y = ggplot2::unit(0, "cm"))

PLC_per_shot_plot <- ggplot2::ggplot(data = gg_lineup_PLC, ggplot2::aes(x=long, 
                                                                      y=lat, 
                                                                      group = group,
                                                                      fill = plc_shot)) +
  ggplot2::facet_grid(. ~ player) +
  ggplot2::geom_polygon()  +
  ggplot2::geom_path(color = "black") +
  ggplot2::coord_equal() +
  ggplot2::scale_fill_gradient2(low = "blue",
                                high = "red",
                                mid = "white", midpoint = 0,
                                name = "PLC per Shot") +
  ggplot2::theme(axis.line=ggplot2::element_blank(),
                 axis.text.x=ggplot2::element_blank(),
                 axis.text.y=ggplot2::element_blank(),
                 axis.ticks=ggplot2::element_blank(),
                 axis.title.x=ggplot2::element_blank(),
                 axis.title.y=ggplot2::element_blank(),
                 panel.background=ggplot2::element_blank(),
                 panel.border=ggplot2::element_blank(),
                 panel.grid.major=ggplot2::element_blank(),
                 panel.grid.minor=ggplot2::element_blank(),
                 strip.text = ggplot2::element_text(size = 16), 
                 strip.background = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 14),
                 legend.title = ggplot2::element_text(size = 16),
                 legend.justification = "left",
                 legend.position = "right",
                 # legend.margin = ggplot2::margin(0.1, 0.1, 0.1, 0, "npc"),
                 plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"),
                 panel.spacing.y = ggplot2::unit(0, "cm"))

cowplot::plot_grid(PLC_per_36_plot, 
                   PLC_per_shot_plot, 
                   nrow = 2, 
                   align = "v")
