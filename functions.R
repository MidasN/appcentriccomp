library(magrittr)

#' based on input calculate distance matrix;
#' apply k-means given number of clustersf
#' @param pre_dist_input is the dataset.
#' @param method is method for distance calculation.
#' @param labels_col is number of labels to plot per cluster
#' @param seed is number for kmeans replication
#' @param column_exclude_start is used for filtering columns NOT to include
#' @param column_exclude_end is used for filtering columns NOT to include
#' @param additional_exlude is convenient to exclude several variables
#' @return plot.

flexible_clustering <- function(pre_dist_input, method, clust_n, labels_col, column_exclude_start, column_exclude_end, additional_exclude = NULL, seed) {

  set.seed(seed)

  pre_dist = pre_dist_input[,-c(1, column_exclude_start:column_exclude_end)]

  if (!is.null(additional_exclude)){

    pre_dist = pre_dist %>% dplyr::select(-additional_exclude)

  } else {

    print("Nothing to exlude")
}

  d = dist(pre_dist[,-1],
           method)

  fit = cmdscale(d, k = 2)
  fit_tb = fit %>% as_tibble()
  colnames(fit_tb) = c("Dim.1", "Dim.2")


  x = fit[, 1]
  y = fit[, 2]

  kmeans_output = kmeans(fit_tb, clust_n)

  clust = kmeans_output$cluster
  clust_centers = kmeans_output$centers[,-3] %>% as.data.frame()
  colnames(clust_centers)[1:2] = c("Dim.1.c", "Dim.2.c")

  clust_centers %<>%
    mutate(groups = rownames(clust_centers) %>% as.character())

  # clust_centers

  fit_tb = fit_tb %>% # making it global for later use in for_labels df
    mutate(groups = clust %>% as.character(),
           index = 1:nrow(fit_tb))

  fit_tb = inner_join(fit_tb, clust_centers,
                      by = "groups")

  # combining 6 labels per group which based on euclidian dist

  fit_tb %<>%
    group_by(groups) %>%
    mutate(dist = sqrt((Dim.1 - Dim.1.c)^2 + (Dim.2 - Dim.2.c)^2),
           abs_dist = abs(Dim.1 - Dim.1.c) + (Dim.2 - Dim.2.c))

  index_labels_balanced = c(
    fit_tb %>%
      group_by(groups) %>%
      arrange(dist, .by_group = TRUE) %>%
      filter(row_number() %in% (1:labels_col)) %$% index)


  index_labels_balanced = index_labels_balanced[!duplicated(index_labels_balanced)]

  plot = ggscatter(fit_tb %>% ungroup() %>% arrange(index),
          x = "Dim.1", y = "Dim.2",
          label = pre_dist_input$a,
          label.select = pre_dist_input$a[index_labels_balanced],
          color = "groups",
          # star.plot = TRUE,
          palette = "jco",
          size = 1,
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE) +
    geom_point(data = clust_centers, aes(x = Dim.1.c, y = Dim.2.c, size = 13))


  returnList = list("plot" = plot,
                      "labels" = pre_dist_input$a, "selectedLables" = pre_dist_input$a[index_labels_balanced],
                      "fit" = fit_tb %>% ungroup() %>% arrange(index))

  return(returnList)
}



# the main purpose is to got clusters labels before plotting
distance_clustering <- function(pre_dist, method, clust_n, what_to_return){

  pre_dist %<>% ungroup()

  d = dist(pre_dist %>%
             dplyr::select(-a, -groups, -cluster),
           method = method)
  fit = cmdscale(d, k = 2)

  fit_tb = fit %>% as_tibble()

  clust = kmeans(fit_tb, clust_n)$cluster %>% as.factor()
  fit_tb = fit_tb %>% # making it global for later use in for_labels df
    mutate(groups = clust)

  if (what_to_return == "clust") {
    return(clust)
  }
  else {
    return(fit_tb)
  }


}


# main use case is to use when labels are gathered using distance_clustering function
plot_mds <- function(pre_dist, method, labels_to_select, clust_n) {

  fit_tb = distance_clustering(pre_dist, method, clust_n, "fit")

  ggscatter(fit_tb,
            x = "V1", y = "V2",
            label = pre_dist$a,
            label.select = labels_to_select$a,
            color = "groups",
            star.plot = TRUE,
            palette = "jco",
            size = 1,
            ellipse = TRUE,
            ellipse.type = "convex",
            repel = TRUE)
}


explore_model_fit <- function(df, n_profiles_range = 1:6, model_names = c("EII", "VVI", "EEE", "VVV")) {
    x = mclustBIC(df, G = n_profiles_range, modelNames = model_names)
    y = x %>%
        as.data.frame.matrix() %>%
        rownames_to_column("n_profiles") %>%
        dplyr::rename(`Constrained variance, fixed covariance` = EII,
                      `Freed variance, fixed covariance` = VVI,
                      `Constrained variance, constrained covariance` = EEE,
                      `Freed variance, freed covariance` = VVV)
    y
}
