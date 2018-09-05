

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


mds_n_plot <- function(pre_dist, method, labels_to_select, clust_n){

  d = dist(pre_dist[,-1], method = method)
  fit = cmdscale(d, k = 2)

  fit_tb = fit %>% as_tibble()
  colnames(fit_tb) = c("Dim.1", "Dim.2")


  x = fit[, 1]
  y = fit[, 2]

  clust <<- kmeans(fit_tb, clust_n)$cluster %>% as.factor() ## FIXME: delete global declaration
  fit_tb = fit_tb %>% # making it global for later use in for_labels df
    mutate(groups = clust)


  ggscatter(fit_tb,
          x = "Dim.1", y = "Dim.2",
          label = pre_dist$a,
          label.select = labels_to_select,
          color = "groups",
          star.plot = TRUE,
          palette = "jco",
          size = 1,
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)
#  + geom_hline(yintercept = 0, color = "red", size = 2)


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
