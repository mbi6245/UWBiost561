#' Title
#'
#' @param adj_mat
#' @param alpha
#'
#' @return
#' @export compute_maximal_partial_clique
#'
#' @examples
check_clique = function(adj_mat, idx, alpha) {
  m = length(idx)
  return((sum(adj_mat[idx, idx]) - m) / 2 >= alpha * m * (m - 1) / 2)
}

calculate_edge_density = function(adj_mat, clique_idx) {
  num_edges <- sum(adj_mat[clique_idx, clique_idx]) / 2
  num_nodes <- length(clique_idx)
  density <- num_edges / (num_nodes * (num_nodes - 1) / 2)
  return(density)
}

compute_maximal_partial_clique <- function(adj_mat, alpha) {
  # Check adj_mat
  if (!is.matrix(adj_mat) || nrow(adj_mat) != ncol(adj_mat) || any(adj_mat != t(adj_mat)) ||
      any(diag(adj_mat) != 1) || any(adj_mat < 0) || any(adj_mat > 1)) {
    stop("adj_mat must be a symmetric matrix with values 0 or 1 and 1s along the diagonal.")
  }
  if (nrow(adj_mat) < 5 || nrow(adj_mat) > 50) {
    stop("adj_mat must have between 5 to 50 rows/columns (inclusive).")
  }

  # Check alpha
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha < 0.5 || alpha > 1) {
    stop("alpha must be a single numeric value between 0.5 and 1 (inclusive).")
  }

  n = dim(adj_mat)[1]
  mpc_idx = seq(1:n)
  curr_clique = adj_mat[mpc_idx, mpc_idx]
  while(!check_clique(curr_clique, mpc_idx, alpha)) {
    remove_idx = sample(length(mpc_idx), 1)
    mpc_idx = mpc_idx[-remove_idx]
    curr_clique = curr_clique[mpc_idx, mpc_idx]
  }

  edge_density = calculate_edge_density(adj_mat, mpc_idx)

  return(list(mpc_idx, edge_density))
}

res <- compute_maximal_partial_clique(adj_mat = temp$adj_mat, alpha = 0.5)
