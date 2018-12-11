start = proc.time()[3]

library(dplyr)
library(INLA)
library(fields)
library(rgeos)
library(sp)

source("./geom_court.R")
source("./fit_lgcp.R")
source("./inla_utils.R")

# constants ---------------------------------------------------------------

player_codes = sort(unique(shot_df$shooter_lineup))
xlim = 47
ylim = 50

# create mesh
mesh = create_mesh()
spde = inla.spde2.pcmatern(mesh=mesh, alpha=2, ### mesh and smoothness parameter 
                           prior.range=c(1, 0.01), ### P(practic.range<1)=0.01 
                           prior.sigma=c(10, 0.01)) ### P(sigma>10)=0.01
sp_dom = matrix(c(0, 0,
                  xlim, 0,
                  xlim, ylim,
                  0, ylim,
                  0, 0),
                ncol = 2,
                byrow = TRUE)

fit_lgcp = make_fit_lgcp(mesh = mesh, sp_dom = sp_dom, spde = spde)


# define function ---------------------------------------------------------

estimate_player_surface = function(player_code, verbose = FALSE) {
  player_df = shot_df %>%
    filter(shooter_lineup == player_code,
           shot_location_x < xlim) %>%
    select(x = shot_location_x,
           y = shot_location_y,
           make = is_made_shot,
           ndd = closest_defender_distance) %>%
    mutate(make = as.numeric(make))
  
  xy = player_df %>%
    select(x, y) %>%
    as.matrix()
  
  res = fit_lgcp(xy, verbose = verbose)
  
  list(inla_results = res,
       spatial_weights = res$summary.random$i$mean)
}

n_players = length(player_codes)

player_surfaces = matrix(NA, nrow = mesh$n, ncol = n_players)
colnames(player_surfaces) = player_codes[1:n_players]
inla_results = vector("list", n_players)
names(inla_results) = player_codes[1:n_players]

for (i in 1:n_players) {
  out = estimate_player_surface(player_codes[i])
  player_surfaces[, i] = out$spatial_weights
  inla_results[[i]] = out$inla_results
}

save(inla_results, mesh, player_surfaces, 
     file = "./data/player_surfaces_ppp_attempts.RData", 
     compress = TRUE)
end = proc.time()[3]
