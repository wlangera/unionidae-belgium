save_figure <- function(plot, file, devices, ...) {
  sapply(devices, function(dev) {
    ggplot2::ggsave(
      filename = paste0(here("output", file), ".", dev),
      plot = plot,
      ...
    )
  })
}