library(INLA)
library(NMF)
library(dplyr)

# set up court grid
grid_width = 2
x_court = seq(0 + grid_width / 2, 40 - grid_width / 2, by = grid_width)
y_court = seq(0 + grid_width / 2, 50 - grid_width / 2, by = grid_width)
grid_xy = expand.grid(x = x_court, y = y_court)

# get shot data
shot_df_raw = readRDS("./data/shots_2017.rds")
shot_df = shot_df_raw %>%
  filter(is_made_shot == TRUE)
shot_xy = cbind(shot_df$shot_location_x, shot_df$shot_location_y)

# bin player shot locations
player_codes = sort(unique(shot_df$shooter_player_code))
n_players = length(player_codes)
nn_shot = FNN::knnx.index(grid_xy, shot_xy, k = 1)
player_bin_counts = unclass(table(shot_df$shooter_player_code, 
                                  factor(nn_shot, levels = 1:nrow(grid_xy))))
no_shot_idx = which(colSums(player_bin_counts) == 0)
nn_no_shot = FNN::knnx.index(shot_xy, grid_xy[no_shot_idx, ], k = 1)
closest_shooters = shot_df[nn_no_shot, "shooter_player_code"]
for (i in 1:length(closest_shooters)) {
  player_bin_counts[closest_shooters[i], no_shot_idx[i]] = 1
}
player_bin_counts = sqrt(player_bin_counts)
normalize = function(x) { x / sum(x) }
player_attempt_pct = t(apply(player_bin_counts, 1, normalize))


n_rank = 5
nmf_out = NMF::nmf(player_bin_counts, rank = n_rank, method = "brunet")
U = nmf_out@fit@W
V = nmf_out@fit@H

basis_df = cbind(grid_xy, t(V))
colnames(basis_df) = c("x", "y", paste0("basis", 1:n_rank))

source("./scripts/utils/inla_utils.R")
source("./scripts/utils/geom_court.R")
plotlist = vector("list", length = n_rank)
for (i in 1:n_rank) {
  plotlist[[i]] = ggplot2::ggplot(data=basis_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_raster(ggplot2::aes_string(fill = paste0("basis", i))) +
    ggplot2::scale_fill_gradientn(colors = sp::bpy.colors()) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(paste0("basis", i)) +
    ggplot2::coord_fixed(xlim = c(0, 47), ylim = c(0, 50)) +
    geom_court(center = FALSE)
}

cowplot::plot_grid(plotlist = plotlist)


n_neighbors = 5
similar_players = FNN::knn.index(player_attempt_pct, k = n_neighbors)
melted_similar_players = reshape2::melt(t(similar_players))
melted_similar_players$Var1 = NULL
names(melted_similar_players) = c("i", "j")
H = Matrix::sparseMatrix(i = c(melted_similar_players$i,
melted_similar_players$j, 1:n_players),
                         j = c(melted_similar_players$j,
melted_similar_players$i, 1:n_players),
                         dims = c(n_players, n_players))
colnames(H) = rownames(H) = player_codes

save(H, file = "./data/similarity_matrix_5_neighbors_makes_only.RData", compress
= TRUE)



