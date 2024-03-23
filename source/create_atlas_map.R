create_atlas_map <- function(data, species,
                             grid_utm10 = utm10_sf,
                             grid_utm5,
                             old_data,
                             alpha = 0.75,
                             alive_col = "red",
                             empty_col = "green",
                             belgium_map = belgium_sf,
                             waterlopen_map = waterlopen_sf,
                             crs) {
  if (missing(grid_utm5)) {
    plot_data <- data %>%
      filter(spec_name == !!species) %>%
      full_join(grid_utm10, by = join_by(utm_10km == TAG)) %>%
      st_as_sf() %>%
      drop_na()

    pattern <- plot_data %>%
      mutate(pattern = ifelse(state_fill == "levend + leeg",
                              "stripe", "none")) %>%
      pull(pattern)

    if (missing(old_data)) {
      p <- ggplot(plot_data) +
        geom_sf(data = belgium_map, fill = "white",
                linewidth = 0.8) +
        geom_sf(data = waterlopen_map, aes(colour = waterloop),
                linewidth = 0.8) +
        scale_color_manual(values = c("cornflowerblue", "darkblue"),
                           guide = guide_legend(
                             override.aes = list(
                               linewidth = 1.5))) +
        geom_sf_pattern(aes(fill = state_fill),
                        linewidth = 0.6, alpha = alpha,
                        pattern = pattern,
                        pattern_colour = alpha(empty_col, alpha),
                        pattern_fill = alpha(empty_col, alpha),
                        pattern_density = 0.1,
                        pattern_spacing = 0.015
        ) +
        scale_fill_manual(values = c(alive_col, alive_col, empty_col),
                          guide = guide_legend(
                            override.aes = list(pattern = c("none",
                                                            "stripe",
                                                            "none"),
                                                pattern_density = 0.2,
                                                pattern_spacing = 0.03))) +
        geom_sf(data = grid_utm10, fill = alpha("white", 0),
                linewidth = 0.6) +
        labs(x = "", y = "", shape = "Vóór 1995", fill = "Na 1995",
             colour = "Waterloop") +
        coord_sf(datum = crs) +
        theme_void() +
        theme(legend.position = c(0.2, 0.2),
              legend.box = "horizontal")
    } else {
      suppressWarnings({
        centroids_old_data <- old_data %>%
          filter(spec_name == !!species) %>%
          full_join(grid_utm10, by = join_by(utm_10km == TAG)) %>%
          st_as_sf() %>%
          drop_na() %>%
          st_centroid()
      })

      p <- ggplot(plot_data) +
        geom_sf(data = belgium_map, fill = "white",
                linewidth = 0.8) +
        geom_sf(data = waterlopen_map, aes(colour = waterloop),
                linewidth = 0.8) +
        scale_color_manual(values = c("cornflowerblue", "darkblue"),
                           guide = guide_legend(
                             override.aes = list(
                               linewidth = 1.5))) +
        geom_sf_pattern(aes(fill = state_fill),
                        linewidth = 0.6, alpha = alpha,
                        pattern = pattern,
                        pattern_colour = alpha(empty_col, alpha),
                        pattern_fill = alpha(empty_col, alpha),
                        pattern_density = 0.1,
                        pattern_spacing = 0.015
        ) +
        scale_fill_manual(values = c(alive_col, alive_col, empty_col),
                          guide = guide_legend(
                            override.aes = list(pattern = c("none",
                                                            "stripe",
                                                            "none"),
                                                pattern_density = 0.2,
                                                pattern_spacing = 0.03))) +
        geom_sf(data = centroids_old_data, aes(shape = state_fill),
                size = 2) +
        scale_shape_manual(values = c(16, 17, 15),
                           guide = guide_legend(
                             override.aes = list(size = 3))) +
        geom_sf(data = grid_utm10, fill = alpha("white", 0),
                linewidth = 0.6) +
        labs(x = "", y = "", shape = "Vóór 1995", fill = "Na 1995",
             colour = "Waterloop") +
        coord_sf(datum = crs) +
        theme_void() +
        theme(legend.position = c(0.2, 0.2),
              legend.box = "horizontal")
    }

  } else {
    print("nog doen")
  }

  return(p)
}