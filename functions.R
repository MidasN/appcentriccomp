mds_n_plot <- function(pre_dist, method, labels_to_select, clust_n){

  d = dist(pre_dist[,-1], method = method)
  fit = cmdscale(d, k = 2)

  fit_tb = fit %>% as_tibble()
  colnames(fit_tb) = c("Dim.1", "Dim.2")


  x = fit[, 1]
  y = fit[, 2]

  clust = kmeans(fit_tb, clust_n)$cluster %>% as.factor()
  fit_tb = fit_tb %>%
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
