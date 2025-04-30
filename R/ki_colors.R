#' Retrieves KI Colors
#'
#' Returns a named vector of Karolinska Institutet's palette.
#'
#' @param color_palette Color palette names can be set to primary or functional (default: `"primary"`)
#' @param print_palette To display color for printing or web designs (default: `"FALSE"`)
#' @return A named vector of hex color codes.
#' @examples
#' ki_colors = ki_colors(color_palette = "primary", palette_for_printing = FALSE)
ki_colors <- function(color_palette = "primary", palette_for_printing = FALSE) {
  if(!color_palette %in% c("primary", "functional")){
    stop("Color palette argument must be set to primary or functional")
  }

  if(!is(palette_for_printing, "logical")){
    stop("palette_for_printing must be set to TRUE (for printing) or FALSE (for web colors)")
  }

  # Primary colors
  primary_colors_print <- c("20/100/0/70", "20/100/0/40", "0/60/50/0", "0/7/5/0",
                            "8/0/5/0")

  primary_colors_web <- c("#4F0433", "#870052", "#FF876F", "#FEEEEB",
                          "#EDF4F4")

  # Functional colors
  functional_colors_print <- c("20/100/0/70", "20/100/0/40", "8/15/5/0",
                               "0/90/60/30", "0/60/50/0", "0/15/15/0",
                               "100/70/60/40", "70/0/27/0", " 18/0/10/0",
                               "0/0/0/100", "0/0/0/70", "0/0/0/10",
                               "100/60/90/30", "65/0/60/0", "25/0/23/0",
                               "0/50/100/0", "0/20/60/0", "0/10/25/0")

  functional_colors_web <- c("#4F0433", "#870052", "#eddbe4",
                               "#B84145", "#FF876F", "#FFDDD6",
                               "#002C34", "#4DB5BC", "#CCEBED",
                               "#000000", "#666666", "#F1F1F1",
                               "#094334", "#54B986", "#C7ECDC",
                               "#F59A00", "#FFC66D", "#FFE7C2")


  # Name colors
  names(primary_colors_print) <- c("dark_plum-text", "plum-logo", "orange-highlight",
                                 "light_orange-background","light_blue-background")

  names(primary_colors_web) <- c("dark_plum-text", "plum-logo", "orange-highlight",
                                 "light_orange-background","light_blue-background")

  names(functional_colors_print) <- c("1_dark_plum-text", "1_plum-logo", "1_light_plum",
                                      "2_dark_orange", "2_orange", "2_light_orange",
                                      "3_dark_blue", "3_blue", "3_light_blue",
                                      "4_black", "4_grey", "4_light_grey",
                                      "5_dark_green", "5_green", "5_light_green",
                                      "6_dark_yellow", "6_yellow", "6_light_yellow")

  names(functional_colors_web) <- c("1_dark_plum-text", "1_plum-logo", "1_light_plum",
                                      "2_dark_orange", "2_orange", "2_light_orange",
                                      "3_dark_blue", "3_blue", "3_light_blue",
                                      "4_black", "4_grey", "4_light_grey",
                                      "5_dark_green", "5_green", "5_light_green",
                                      "6_dark_yellow", "6_yellow", "6_light_yellow")

  if (palette_for_printing){
    if (color_palette == "primary"){
      return(primary_colors_print)
    } else {
      return(functional_colors_print)
    }
  } else if (!palette_for_printing) {
    if (color_palette == "primary"){
      return(primary_colors_web)
    } else {
      return(functional_colors_web)
    }
  }
}
