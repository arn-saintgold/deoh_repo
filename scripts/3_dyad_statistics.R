# This file computes the mann whitney U tests on dyads included in the last two 
# tables of the appendix

source(file.path('scripts','2_compute_dyad_functions.R'))


usr_emo_lean <- fread(usr_emo_lean_path)
emo_csv <- fread(emo_csv_path)
usr_lean = usr_emo_lean[!is.na(is_questionable) & n_comments >= 8 & n_emo>0, .(Nome_Utente, is_questionable)]
names(usr_lean)[which(names(usr_lean) == "is_questionable")] = "is_usr_questionable"

emo_csv_usr_lean <- merge(emo_csv, usr_lean) %>% compute_dyads_comments()

usr_dyads <- emo_csv_usr_lean %>% compute_dyad_usr()

features <- c('frozenness','submission','dominance','shame',
              'ambivalence','aggressiveness','contempt','alarm',
              'unbelief','sentimentality','outrage','pride','hope',
              'envy','guilt','confusion','anxiety','morbidness','cynism',
              'despair','disappointment','bittersweetness','remorse','love',
              'pessimism','curiosity','optimism','delight')
prev_results <- data.frame()
mag_results <- data.frame()

for (f in features) {
  data <- usr_dyads %>% select(all_of(f), is_usr_questionable)
  colnames(data) <- c("value", "group")
  
  # Prevalence test
  prev_table <- table(data$group, data$value > 0)
  chisq_result <- chisq.test(prev_table, correct = TRUE)
  chi_sq_stat <- chisq_result$statistic
  total_n <- sum(prev_table)
  #phi <- sqrt(chi_sq_stat / total_n)
  #cramers_v <- CramerV(prev_table)
  
  prevalence_stats <- data %>%
    group_by(group) %>%
    summarise(
      non_zero_prop = mean(value > 0),
      non_zero_n = sum(value > 0),
      total = n()
    ) %>% as.data.frame()
  
  prev_results <- rbind(prev_results, data.frame(
    Feature = f,
    Prop_NonZero_MAp = prevalence_stats$non_zero_prop[1],
    Prop_NonZero_MIp = prevalence_stats$non_zero_prop[2],
    #Prevalence_Difference = round(prevalence_stats$non_zero_prop[1]-prevalence_stats$non_zero_prop[2],3),
    Count_NonZero_MAp = prevalence_stats$non_zero_n[1],
    Count_NonZero_MIp = prevalence_stats$non_zero_n[2],
    Chi_Sq = round(chi_sq_stat, 3),
    #Phi = round(phi, 3),
    #Cramers_V = cramers_v,
    p_value = chisq_result$p.value
  ))
  
  # Magnitude test
  non_zero_data <- data %>% filter(value > 0)
  
  if (length(unique(non_zero_data$group)) == 2 && nrow(non_zero_data) > 0) {
    wilcox_result <- wilcox.test(value ~ group, data = non_zero_data, exact = FALSE)
    
    group_sizes <- table(non_zero_data$group)
    n1 <- as.numeric(group_sizes[1])
    n2 <- as.numeric(group_sizes[2])
    expected_U <- n1 * n2 / 2
    observed_U <- wilcox_result$statistic
    z_score <- qnorm(wilcox_result$p.value / 2) * -sign(observed_U - expected_U)
    rank_biserial <- z_score / sqrt(n1 + n2)
    
    nonzero_summary <- non_zero_data %>%
      group_by(group) %>%
      summarise(
        median = median(value),
        #IQR = IQR(value),
        n = n()
      ) %>% as.data.frame()
    
    mag_results <- rbind(mag_results, data.frame(
      Feature = f,
      Med_MAp = nonzero_summary$median[1],
      #IQR_MAp = nonzero_summary$IQR[1],
      Med_MIp = nonzero_summary$median[2],
      #IQR_MIp = nonzero_summary$IQR[2],
      W_statistic = wilcox_result$statistic,
      Rank_Biserial = round(rank_biserial, 3),
      p_value = wilcox_result$p.value
    ))
  }
}
# Remove row names from tables
rownames(prev_results)<-NULL
rownames(mag_results)<-NULL

# Adjust p-values
prev_results$p_value <- p.adjust(prev_results$p_value, method = "BH")
mag_results$p_value <- p.adjust(mag_results$p_value, method = "BH")

# Mark significance for bolding
prev_results$sig <- prev_results$p_value <= 0.05
mag_results$sig <- mag_results$p_value <= 0.05

# Format Percentages
#prev_results$Prevalence_Difference <- formattable::percent(prev_results$Prevalence_Difference, digits = 1)
prev_results$Prop_NonZero_MAp <- formattable::percent(prev_results$Prop_NonZero_MAp, digits = 1)
prev_results$Prop_NonZero_MIp <- formattable::percent(prev_results$Prop_NonZero_MIp, digits = 1)

# Round numeric
prev_results <- prev_results %>% mutate(across(where(is.numeric), ~ round(., 3)))
mag_results <- mag_results %>% mutate(across(where(is.numeric), ~ round(., 3)))


# LaTeX table: Prevalence
prev_latex <- kable(prev_results %>% select(-sig), format = "latex", booktabs = TRUE,
                    caption = "\\label{tab:prevalence_test}$\\chi^2$ test results ($df = 1, N = 25153$) with Yates' correction and FDR p-value adjustment, comparing prevalence of non-zero dyad values in users. Dyads are ordered as in figure~\\ref{fig:diffplot}. Rows where p-value $< 0.05$ are in bold. The table shows that dyads with high average values in \\MAp users are employed significantly more often by those users, while there is no significant difference in the other dyads presence.",
                    label = "prevalence_test") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

for (i in which(prev_results$sig)) {
  prev_latex <- prev_latex %>% row_spec(i, bold = TRUE)
}

# LaTeX table: Magnitude
mag_latex <- kable(mag_results %>% select(-sig), format = "latex", booktabs = TRUE,
                   caption = "Mann–Whitney U test results on non-zero user dyad values with rank-biserial effect size and FDR p-value adjustment. Rows where p-value $< 0.05$ are in bold. The test shows that the median dyad values of \\MIp users is higher than the median dyad values of \\MAp users.",
                   label = "magnitude_test") %>%
  kable_styling(latex_options = c("hold_position", "striped"))

for (i in which(mag_results$sig)) {
  mag_latex <- mag_latex %>% row_spec(i, bold = TRUE)
}

# Output LaTeX
#cat(prev_latex, "\n\n")
#cat(mag_latex)

# Output to file
{
  fileConn<-file("output.txt")
  writeLines(c(prev_latex,"\n\n",mag_latex), fileConn)
  close(fileConn)
}
