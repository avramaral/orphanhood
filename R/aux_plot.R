plot_maps_isl <- function (data, isl1, isl2, my_var, nm_var = "", tt = "", ll = NULL, my_colors = NA, ...) {
  
  if (is.null(ll)) { stop("One should provide `ll` (i.e., limits for the legend), so that the islands are properly coloured.") }
  
  if (is.na(my_colors)) { my_colors <- c("#00008FFF", "#0000F2FF", "#0063FFFF", "#00D4FFFF", "#46FFB8FF", "#B8FF46FF", "#FFD400FF", "#FF6300FF", "#F00000FF", "#800000FF") }
  
  high_res_islands <- readRDS(file = "DATA/high_res_islands.RDS")
  isl1$geometry <- high_res_islands[high_res_islands$code == 88001, ]$geometry
  isl2$geometry <- high_res_islands[high_res_islands$code == 88564, ]$geometry
  
  i1 <- ggplot(data = isl1, aes(geometry = geometry)) + 
    geom_sf(aes(fill = .data[[my_var]]), color = "black") + 
    scale_fill_gradientn(name = nm_var, colours = my_colors, limits = ll) +
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  
  i2 <- ggplot(data = isl2, aes(geometry = geometry)) + 
    geom_sf(aes(fill = .data[[my_var]]), color = "black") + 
    scale_fill_gradientn(name = nm_var, colours = my_colors, limits = ll) +
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  
  pp <- ggplot(data = data, aes(geometry = geometry)) + 
    geom_sf(aes(fill = .data[[my_var]]), color = "black") + 
    scale_fill_gradientn(name = nm_var, colours = my_colors, limits = ll) +
    labs(title = tt) + 
    theme_bw() +
    theme(legend.position = "right", 
          text = element_text(size = 14, family = "LM Roman 10"), 
          plot.title = element_text(size = 16),
          legend.title = element_text(size = 11),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) 
  
  pp <- pp + annotate("text", x = -80, y = 12.5, label = "SAN ANDRES, PROVIDENCIA AND\nSANTA CATALINA ISLANDS", color = "black", size = 2, family = "LM Roman 10", hjust = 0)
  pp <- pp + inset_element(i1, left = 0.04, bottom = 0.780, right = 0.14, top = 0.940, align_to = "panel")
  pp <- pp + inset_element(i2, left = 0.13, bottom = 0.840, right = 0.23, top = 0.915, align_to = "panel")
  pp <- pp + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))
  
  pp
}

plot_incidence_line <- function (data, my_var = "", ...) {
  
  pp <- ggplot(data, aes(x = year, y = n_orp, color = gender, group = gender)) +
    geom_line(size = 1) +
    geom_point(size = 2) + 
    scale_x_continuous(breaks = 2015:2021, labels = 2015:2021) + 
    scale_y_continuous(labels = scales::comma, limits = c(0, max(data[, my_var]) * 1.05), expand = c(0, 0)) +
    scale_color_manual(values = c("Male" = "#00008FFF", "Female" = "#800000FF", "Total" = "#000000FF")) +
    labs(title = "National Orphanhood Incidence", x = "Year", y = "Number of orphans\n(children aged 0-17 years)", color = "Gender") +
    theme_bw() +
    theme(legend.position = "right", 
          text = element_text(size = 10, family = "LM Roman 10"), 
          plot.title = element_text(size = 12),
          legend.title = element_text(size = 10))
  
  pp
}

plot_orphans_prev <- function (data, nm_y = "", tt = "", ll = NULL, is_count = TRUE, ...) {
  
  pp <- ggplot(data, aes(x = dep_name, y = n_orp, fill = reg_name)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(name = "Region", values = c("Amazonía" = "#00008FFF", "Caribe" = "#004CFFFF", "Central" = "#19FFE5FF", "Eje cafetero y Antioquia" = "#E5FF19FF", "Llanos" = "#FF4C00FF",  "Pacífica" = "#800000FF")) +
    labs(y = nm_y, x = "", title = tt) +
    { if (is_count)  scale_y_continuous(labels = comma,   expand = expansion(mult = c(0, 0.025)), limits = ll) } +
    { if (!is_count) scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.025)), limits = ll) } +
    theme_bw() +
    theme(legend.position = "right", 
          text = element_text(size = 10, family = "LM Roman 10"), 
          plot.title = element_text(size = 12),
          legend.title = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) 
  
  pp
}

plot_cat_map_isl <- function (data, isl1, isl2, my_var, labels, nm_var = "", tt = "", ll = NULL, my_colors = NA, ...) {
  
  if (is.na(my_colors)) { my_colors <- plot3D::jet.col(n = length(labels)) } 
  my_colors <- setNames(my_colors, labels)
  
  high_res_islands <- readRDS(file = "DATA/high_res_islands.RDS")
  isl1$geometry <- high_res_islands[high_res_islands$code == 88001, ]$geometry
  isl2$geometry <- high_res_islands[high_res_islands$code == 88564, ]$geometry
  
  col_88001 <- my_colors[which(isl1[[my_var]] == labels)]
  i1 <- ggplot(data = isl1, aes(geometry = geometry)) + 
    geom_sf(fill = col_88001, color = "black") + 
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
  
  col_88564 <- my_colors[which(isl2[[my_var]] == labels)]
  i2 <- ggplot(data = isl2, aes(geometry = geometry)) + 
    geom_sf(fill = col_88564, color = "black") + 
    theme_bw() +
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), panel.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

  pp <- ggplot(data = data, aes(geometry = geometry)) + 
    geom_sf(aes(fill = factor(.data[[my_var]], levels = labels)), color = "black") + 
    scale_fill_manual(values = my_colors, name = nm_var,  drop = FALSE) +
    labs(title = tt) + 
    theme_bw() +
    theme(legend.position = "right", 
          text = element_text(size = 14, family = "LM Roman 10"), 
          plot.title = element_text(size = 16),
          legend.title = element_text(size = 11),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) 
  
  pp <- pp + annotate("text", x = -80, y = 12.5, label = "SAN ANDRES, PROVIDENCIA AND\nSANTA CATALINA ISLANDS", color = "black", size = 2, family = "LM Roman 10", hjust = 0)
  pp <- pp + inset_element(i1, left = 0.04, bottom = 0.780, right = 0.14, top = 0.940, align_to = "panel")
  pp <- pp + inset_element(i2, left = 0.13, bottom = 0.840, right = 0.23, top = 0.915, align_to = "panel")
  pp <- pp + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))
  
  pp
}
