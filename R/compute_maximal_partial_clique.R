#' Title
#'
#' @param adj_mat
#' @param alpha
#'
#' @return
#' @export compute_maximal_partial_clique
#'
#' @examples
check_clique = function(mat, idx, alpha) {
  m = length(idx)
  print(idx)
  # print(mat[idx, idx])
  return((sum(mat[idx, idx]) - m) / 2 >= alpha * m * (m - 1) / 2)
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
  mpc_idx = c(1)
  for (i in 2:n) {
    adj_mat
  }
  most_sparse = 0
  curr_clique = adj_mat[mpc_idx, mpc_idx]

  while(!check_clique(adj_mat, mpc_idx, alpha)) {
    most_sparse = which.min(rowSums(adj_mat))
    mpc_idx = mpc_idx[mpc_idx == most_sparse]
    # curr_clique =
    # mpc_idx = mpc_idx[-remove_idx]
  }

  edge_density = calculate_edge_density(adj_mat, mpc_idx)

  return(list(mpc_idx, edge_density))
}

find_maximal_partial_clique <- function(adj_mat, alpha) {
  # Get the number of vertices in the graph
  n <- nrow(adj_mat)

  # Initialize an empty list to store all possible partial cliques
  partial_cliques <- list()

  # Generate all possible subsets of vertices
  all_subsets <- lapply(1:n, function(k) combn(n, k))

  # Iterate through each subset of vertices
  for (subset in all_subsets) {
    # Iterate through each subset of vertices
    for (i in 1:ncol(subset)) {
      # Get the subset of vertices
      subset_vertices <- subset[, i]
      m = length(subset_vertices)

      # Calculate the density of the subset
      subset_density <- (sum(adj_mat[subset_vertices, subset_vertices]) - m) / 2

      # Check if the density is greater than the threshold
      # if (subset_density >= threshold) {
        # Check if the subset is a partial clique
        if (subset_density >= alpha * m * (m - 1) / 2) {
          # Add the subset to the list of partial cliques
          partial_cliques[[length(partial_cliques) + 1]] <- subset_vertices
        }
      # }
    }
  }

  # Find the largest partial clique
  largest_partial_clique <- partial_cliques[[which.max(sapply(partial_cliques, length))]]
  # edge_density = calculate_edge_density(adj_mat, largest_partial_clique)

  return(list(largest_partial_clique, edge_density))
}

res <- find_maximal_partial_clique(adj_mat = temp$adj_mat, alpha = 0.1)
