source(.RProfile)

# exp1.3 ------------------------------------------------------------------
exp1_3 <- read_excel("Exp1.3. Populations_Growth Kinetics_AUC.xlsx", 
                     col_types = c("text", "text", "text", "numeric", "skip", "text")
)

#define 2-way ANOVA
anova_results <- aov(AUC ~ Transfer * Substrate, data = exp1_3)
summary(anova_results, infer = TRUE)
omega_squared(anova_results, ci = 0.95, alternative = "two.sided",)

anova_results <- aov(AUC ~ group, data = exp1_3)
estimated_meandiff <- emmeans(anova_results, ~ group)
estimated_meandiff

# Define comparisons for posthoc tests
comparisons <- list(
  c('glass-10', 'plk-10'),
  c('glass-20', 'plk-20'),
  c('glass-30', 'plk-30'),
  c('pvc-10', 'plk-10'),
  c('pvc-20', 'plk-20'),
  c('pvc-30', 'plk-30'),
  c('steel-10', 'plk-10'),
  c('steel-20', 'plk-20'),
  c('steel-30', 'plk-30'),
  c('plk-10', 'plk-20'),
  c('plk-10', 'plk-30'),
  c('plk-20', 'plk-30'),
  c('glass-10', 'glass-30'),
  c('steel-10', 'steel-30'),
  c('pvc-10', 'pvc-30')
)

group_order = list(
  'glass-10', 'glass-20', 'glass-30', 
  'plk-10', 'plk-20', 'plk-30',
  'pvc-10', 'pvc-20', 'pvc-30', 
  'steel-10', 'steel-20', 'steel-30'
)

# Create contrast matrix
contrast_matrix <- matrix(0, nrow = length(comparisons), ncol = length(group_order), 
                          dimnames = list(comparisons, group_order))

for (i in 1:length(comparisons)) {
  contrast_matrix[i, comparisons[[i]][1]] <- -1
  contrast_matrix[i, comparisons[[i]][2]] <- 1
}

colnames(contrast_matrix) <- NULL
contrast_list <- as.list(as.data.frame(t(contrast_matrix)))

# Run Sidak's posthoc test on contrasts
contrast_results <- contrast(estimated_meandiff, 
                             method = contrast_list, 
                             adjust = "sidak"
)
confint(contrast_results)

# Calculate Hedge's g effect size
hedges_g <- data.frame(contrast = character(length(comparisons)), 
                       estimate = numeric(length(comparisons)), 
                       lower_CI = numeric(length(comparisons)),
                       upper_CI = numeric(length(comparisons)))

for (i in 1:length(comparisons)) { 
  hedges_g_value <- cohen.d(
    exp1_3[exp1_3$group == comparisons[[i]][1], ]$AUC, 
    exp1_3[exp1_3$group == comparisons[[i]][2], ]$AUC, 
    hedges.correction = TRUE)
  
  hedges_g$contrast[i] <- comparisons[i]
  hedges_g$estimate[i] <- round(hedges_g_value$estimate, 3)
  hedges_g$lower_CI[i] <- round(hedges_g_value$conf.int[1], 3)
  hedges_g$upper_CI[i] <- round(hedges_g_value$conf.int[2], 3)
}
