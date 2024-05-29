#' Title
#'
#' @param n size of adjacency matrices to be simulated, must be integer greater than or equal to 1
#' @param alphas density of adjacency matrices
#'
#' @return results of simulation, 0 for failure to find clique, 1 for success, and NA for function failure
#' @export simulation
simulation = function(n, alphas) {
  if (n > 30) {
    stop("size of adjacency matrices has exceeded recommendations")
  }
  if (!is.vector(alphas) || !all(is.numeric(alphas)) || !all(0 <= alphas) || !all(alphas <= 1)) {
    stop("alphas must be a vector of numbers between 0 and 1 inclusive")
  }
  # library(UWBiost561)
  num_funcs = 25
  res = matrix(NA, nrow = num_funcs, ncol = length(alphas))
  for (i in 1:length(alphas)) {
    set.seed(561)
    data <- generate_partial_clique(n = 10,
                                                clique_fraction = 0.5,
                                                clique_edge_density = alphas[i])
    for (j in 1:num_funcs) {
      curr_sim = compute_maximal_partial_clique_master(
        adj_mat = data$adj_mat,
        alpha = alphas[i],
        number = j,
        time_limit = 30
      )
      curr_density = curr_sim$edge_density
      # print(curr_density)
      if (!is.na(curr_density)) {
        correct_density = compute_correct_density(data$adj_mat, curr_sim$clique_idx)
        if (curr_density >= correct_density) {
          res[j, i] = 1
        } else {
          res[j, i] = 0
        }
      }
    }
  }
  return(res)
}

# simulation(10, c(0.8, 0.9))
