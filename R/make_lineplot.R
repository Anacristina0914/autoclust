


make_aa_lineplot <- function(x, aa_colnames, subgroup_column,
                             subgroup_colors=distinctColorPalette(k = length(unique(x %>% pull(subgroup_column)))),
                             line_width = 1.5, geom_point = 2,
                             plot_theme = autoclust::theme_clustering(text_size = 8, plot_title_size = 18), ...){

  # Prepare data for aa line plot
  long_df <- transform_data_for_line_plot(df = x,
                                     autoantibody_columns = aa_colnames,
                                     subgroup_column = subgroup_column)

  long_df[[subgroup_column]] <- as.factor(long_df[[subgroup_column]])


  frequency_autoantibodies_persubgroup <- ggplot(long_df,
                                                 aes(x = Autoantibody,
                                                     y = Frequency,
                                                     group = !!sym(subgroup_column),
                                                     color = !!sym(subgroup_column))) +
    geom_line(linewidth = line_width) +
    geom_point(size = geom_point) + plot_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(values = subgroup_colors) +
    labs(...)

  return(frequency_autoantibodies_persubgroup)
}
