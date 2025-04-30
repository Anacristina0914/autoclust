make_aa_lineplot <- function(){
  # Subset dataframe to contain only columns in aa
  data.subset.aa <- subset(data.subset, select = autoantibodies_col_num)

  #data.subset.aa$subgroups <- as.factor(cluster_eval_cal$pam_fit$clustering)

  data.subset.aa <- data.subset.aa %>%
    mutate(across(, ~ factor(ifelse(. == "yes", 1, 0), levels = c(0, 1))))

  #Convert binary columns to numeric
  data.subset.aa <- data.subset.aa %>%
    mutate(across(,~ as.numeric(as.character(.))))

  data.subset.aa$subgroups <- as.factor(cluster_eval_cal$pam_fit$clustering)

  # Group by cluster and count how many individuals have positive values for each autoantobody
  grouped_sums <- data.subset.aa %>%
    group_by(subgroups) %>%
    summarise(across(, sum))

  # Count total number of individuals per group
  cluster_sizes <- data.subset.aa %>%
    group_by(subgroups) %>%
    summarise(cluster_size = n())

  # Divide the sum of 1's by the number of individuals per group and multiply by 100 for percentage
  cluster_freq <- grouped_sums %>%
    left_join(cluster_sizes, by = "subgroups") %>%
    mutate(across(autoantibodies_col_names, ~ . / cluster_size *100))

  # Remove cluster information no longer needed
  cluster_freq$cluster_size <- NULL
  # Change format for plotting
  cluster_freq_long <- melt(cluster_freq, id.vars = "subgroups", variable.name = "Autoantibody", value.name = "Frequency")
  cluster_freq_long

  frequency_autoantibodies_percluster <- ggplot(cluster_freq_long, aes(x = Autoantibody, y = Frequency, group = subgroups, color = subgroups)) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 2) +
    theme_bw() +
    labs(title = "Frequency of autoantibody values by Cluster", x = "Autoantibody", y = "Percentage of Positivity") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  frequency_autoantibodies_percluster
}
