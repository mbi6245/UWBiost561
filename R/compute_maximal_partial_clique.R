#' Title
#'
#' @param adj_mat ajdacency matrix
#' @param alpha minimum density of clique
#'
#' @return indices corresponding to maximum partial clique of given adjacency matrix
#' @export compute_maximal_partial_clique
#'
#' @examples
calculate_edge_density = function(adj_mat, clique_idx) {
  num_nodes <- length(clique_idx)
  num_edges <- (sum(adj_mat[clique_idx, clique_idx]) - num_nodes) / 2
  density <- num_edges / (num_nodes * (num_nodes - 1) / 2)
  return(density)
}

compute_maximal_partial_clique <- function(adj_mat, alpha) {
  if (nrow(adj_mat) < 5 || nrow_adj_mat > 50) {
    stop("adj_mat must have between 5 and 50 rows inclusive")
  }
  if(alpha < 0 || alpha > 1) {
    stop("alpha must be between 0 and 1 inclusive")
  }
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
  edge_density = calculate_edge_density(adj_mat, largest_partial_clique)

  return(list(clique_idx = largest_partial_clique, edge_density = edge_density))
}
