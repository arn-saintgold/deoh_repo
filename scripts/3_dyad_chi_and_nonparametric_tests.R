# This file computes the mann whitney U tests on dyads included in the last two 
# tables of the appendix

library(dplyr)


source(file.path('scripts','2_compute_dyad_functions.R'))

features <- usr_dyad_cols
prev_results <- data.frame()
mag_results <- data.frame()

for (f in features) {
  data <- usr_dyads %>% select(all_of(f), is_usr_questionable)
  colnames(data) <- c("value", "group")
  
  # Prevalence table (non-zero count)
  prev_table <- table(data$group, data$value > 0)
  
  # Fisher's Exact Test for prevalence
  fisher_result <- fisher.test(prev_table)
  
  # Summary: prevalence of non-zero values
  prevalence_stats <- data %>%
    group_by(group) %>%
    summarise(
      non_zero_prop = mean(value > 0),
      non_zero_n = sum(value > 0),
      total = n()
    ) %>% as.data.frame()
  
  prev_results <- rbind(prev_results, data.frame(
    Feature = f,
    Prop_NonZero_Group1 = prevalence_stats$non_zero_prop[1],
    Prop_NonZero_Group2 = prevalence_stats$non_zero_prop[2],
    Count_NonZero_Group1 = prevalence_stats$non_zero_n[1],
    Count_NonZero_Group2 = prevalence_stats$non_zero_n[2],
    p_value = fisher_result$p.value
  ))
  
  # Magnitude test among non-zero values
  non_zero_data <- data %>% filter(value > 0)
  
  if (length(unique(non_zero_data$group)) == 2 && nrow(non_zero_data) > 0) {
    wilcox_result <- wilcox.test(value ~ group, data = non_zero_data, exact = FALSE)
    
    # Summary stats for non-zero values
    nonzero_summary <- non_zero_data %>%
      group_by(group) %>%
      summarise(
        median = median(value),
        IQR = IQR(value),
        n = n()
      ) %>% as.data.frame()
    
    mag_results <- rbind(mag_results, data.frame(
      Feature = f,
      Median_Group1 = nonzero_summary$median[1],
      IQR_Group1 = nonzero_summary$IQR[1],
      Median_Group2 = nonzero_summary$median[2],
      IQR_Group2 = nonzero_summary$IQR[2],
      W_statistic = wilcox_result$statistic,
      p_value = wilcox_result$p.value
    ))
  }
}

# Adjust p-values
prev_results$p_adj <- p.adjust(prev_results$p_value, method = "BH")
mag_results$p_adj <- p.adjust(mag_results$p_value, method = "BH")

# Round values
prev_results <- prev_results %>% mutate(across(where(is.numeric), ~ round(., 4)))
mag_results <- mag_results %>% mutate(across(where(is.numeric), ~ round(., 4)))

# LaTeX Tables
prev_latex <- kable(prev_results, format = "latex", booktabs = TRUE,
                    caption = "Prevalence of non-zero feature values across groups (Fisher's exact test).",
                    label = "tab:prevalence_test") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

mag_latex <- kable(mag_results, format = "latex", booktabs = TRUE,
                   caption = "Comparison of non-zero feature values between groups (Mann–Whitney U test).",
                   label = "tab:magnitude_test") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

# Output LaTeX
cat(prev_latex, "\n\n")
cat(mag_latex)
