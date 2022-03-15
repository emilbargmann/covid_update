#--------------------------------------------------------#
# COVID UPDATE                                           #
# Author: Emil Bargmann Madsen                           #
# Date: Jan. 4th, 2022                                   #
# 0. Settings                                            #
#--------------------------------------------------------#

# 1. Packages

list_of_packages <- c("tidyverse", "extrafont", "scales", "broom", "patchwork", "kableExtra", "fixest", "jtools", "margins", "sandwich", "MASS", "patchwork", "rio")
lapply(list_of_packages, require, character.only = TRUE)

# 2. Colours

orange <- "#FF851B" # Hex codes for colours
maroon <- "#85144b"
olive <- "#3D9970"
navy <- "#001f3f"

# 3. Plot themes

font_fam <- "Lato" # Set preferred font

ggplot2::update_geom_defaults(geom = "text", list(family = font_fam))
ggplot2::update_geom_defaults(geom = "label", list(family = font_fam))


# Themes for different plot types
theme_ebm <- function() {
  ggthemes::theme_base(base_size = 12, base_family = font_fam) %+replace% ggplot2::theme(
    plot.background = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_line(lineend = "square"), 
    axis.ticks.length = ggplot2::unit(0.35, "lines"), 
    axis.text = ggplot2::element_text(size = 12)
  )
}

theme_ebm_grid <- function() {
  ggthemes::theme_base(base_size = 12, base_family = font_fam) %+replace% ggplot2::theme(
    plot.background = ggplot2::element_blank(), 
    axis.ticks = ggplot2::element_line(lineend = "square"), 
    axis.ticks.length = ggplot2::unit(0.5, "lines"), 
    axis.text = ggplot2::element_text(size = 12),
    panel.grid.major = element_line(colour = "grey85", linetype = "dotted")
  )
}

theme_ebm_bar <- function() {
  ggthemes::theme_base(base_size = 12, base_family = font_fam) %+replace% ggplot2::theme(
    plot.background = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_line(lineend = "square"), 
    axis.ticks.length = ggplot2::unit(0.5, "lines"), 
    axis.text = ggplot2::element_text(size = 12),
    axis.line.x = ggplot2::element_line(),
    panel.grid.major.y = element_line(colour = "grey85", linetype = "dotted")
  )
}