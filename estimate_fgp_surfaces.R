start = proc.time()[3]
library(INLA)
library(dplyr)

n_rank = 15
n_threads = 12
shot_df = #
shot_df = shot_df %>%
  filter(!(complex_shot_type %in% c("tip", "putback")))
similarity_matrix_filename =
"./data/similarity_matrix_5_neighbors_makes_only.RData"
load(similarity_matrix_filename)

basis_filename = paste0("./data/U_", n_rank, "_matrix.RData")
load(basis_filename)

catchall_idx = 14
U = U[-catchall_idx, ]

source("./geom_court.R")
source("./inla_utils.R")

softmax = function(x) { exp(x) / (1 + exp(x)) }

# create mesh
xlim = 47
ylim = 50
mesh = create_mesh()

model_df = shot_df %>% 
  mutate(make = as.numeric(is_made_shot)) %>%
  select(shooter_code = shooter_player_code,
         x = shot_location_x,
         y = shot_location_y,
         make)

A = inla.spde.make.A(mesh = mesh, loc = cbind(model_df$x, model_df$y))
basis_weights = A %*% t(U)
n_basis = ncol(basis_weights)
basis_names = paste0("basis_", 1:n_basis)

graph = H
player_codes = colnames(graph)
player_id = factor(model_df$shooter_code, levels = player_codes)
idx = as.numeric(player_id)

data = list(y = model_df$make,
            player_id = idx,
            ndd_idx = idx,
            dribble_idx = idx,
            log_ndd = model_df$log_ndd,
            dribbled = model_df$dribbled)
for (i in 1:n_basis) {
  data[[paste0(basis_names[i], "_idx")]] = idx
  data[[paste0(basis_names[i], "_weight")]] = basis_weights[, i]
}

prior_hp = c(0.5^2, 0.5)
cat("prior mean: ", prior_hp[1] / prior_hp[2], "\n")
cat("prior var: ", prior_hp[1] / prior_hp[2]^2, "\n")

formula_string = "y ~ 1"
for (i in 1:n_basis) {
  basis_function = paste0(" +
                          
                          f(", paste0(paste0(basis_names[i], c("_idx",
"_weight")), collapse = ", "), ",
                          model = 'besag', 
                          graph = graph,
                          hyper = list(prec = list(prior = 'loggamma',
                          param = prior_hp)),
                          constr = FALSE)")
  formula_string = paste0(formula_string, basis_function)
}
inla_formula = as.formula(formula_string)

inla_out = inla(formula = inla_formula, 
                family = "binomial", 
                data = data, 
                num.threads = n_threads,
                verbose = TRUE)

save.image(file = paste0("./data/inla_output_U", n_rank, "_alpha=", prior_hp[1],
                         "beta=", prior_hp[2], "_GLOBAL_INTERCEPT_ONLY.RData"),
compress = TRUE)

end = proc.time()[3]
