source(.RProfile)

# exp1.1 ------------------------------------------------------------------
exp1_1 <- read_excel("Exp1.1. Populations_Crystal Violet_Data.xlsx", 
  col_types = c("text", "text", "text", "numeric", "text"))

exp1_1$transfer <- relevel(factor(exp1_1$transfer, levels = c(0, 10, 20, 30)), ref = "0")

#define 2-way ANOVA
anova_results <- aov(OD595 ~ transfer * substrate, data = exp1_1)
summary(anova_results, infer = TRUE)
confint(anova_results)
omega_squared(anova_results, ci = 0.95, alternative = "two.sided",)


estimated_meandiff <- emmeans(anova_results, ~ transfer * substrate)
estimated_meandiff

# Define comparisons for posthoc tests
comparisons <- list(
  c('ancestor', 'gl-10'),
  c('ancestor', 'gl-20'),
  c('ancestor', 'gl-30'),
  c('ancestor', 'plk-10'),
  c('ancestor', 'plk-20'),
  c('ancestor', 'plk-30'),
  c('ancestor', 'pvc-10'),
  c('ancestor', 'pvc-20'),
  c('ancestor', 'pvc-30'),
  c('ancestor', 'steel-10'),
  c('ancestor', 'steel-20'),
  c('ancestor', 'steel-30'),
  c('gl-10', 'gl-20'),
  c('gl-10', 'gl-30'),
  c('gl-10', 'plk-10'),
  c('gl-10', 'pvc-10'),
  c('gl-10', 'steel-10'),
  c('gl-20', 'gl-30'),
  c('gl-20', 'plk-20'),
  c('gl-20', 'pvc-20'),
  c('gl-20', 'steel-20'),
  c('gl-30', 'plk-30'),
  c('gl-30', 'pvc-30'),
  c('gl-30', 'steel-30'),
  c('plk-10', 'plk-20'),
  c('plk-10', 'plk-30'),
  c('plk-20', 'plk-30'),
  c('pvc-10', 'plk-10'),
  c('pvc-10', 'pvc-20'),
  c('pvc-10', 'pvc-30'),
  c('pvc-20', 'plk-20'),
  c('pvc-20', 'pvc-30'),
  c('pvc-30', 'plk-30'),
  c('steel-10', 'plk-10'),
  c('steel-10', 'pvc-10'),
  c('steel-10', 'steel-20'),
  c('steel-10', 'steel-30'),
  c('steel-20', 'plk-20'),
  c('steel-20', 'pvc-20'),
  c('steel-20', 'steel-30'),
  c('steel-30', 'plk-30'),
  c('steel-30', 'pvc-30')
)

group_order = list(
  'ancestor', 
  'gl-10', 'gl-20', 'gl-30', 
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

# Calculate Hedge's g effect size
hedges_g <- data.frame(contrast = character(42), estimate = numeric(42), lower_CI = numeric(42), upper_CI = numeric(42))
for (i in 1:length(comparisons)) { 
  hedges_g_value <- cohen.d(
    exp1_1[exp1_1$group == comparisons[[i]][1], ]$OD595, 
    exp1_1[exp1_1$group == comparisons[[i]][2], ]$OD595, 
    hedges.correction = TRUE)
  
  hedges_g$contrast[i] <- comparisons[i]
  hedges_g$estimate[i] <- round(hedges_g_value$estimate, 3)
  hedges_g$lower_CI[i] <- round(hedges_g_value$conf.int[1], 3)
  hedges_g$upper_CI[i] <- round(hedges_g_value$conf.int[2], 3)
}

# Calculate Glass's delta effect size
glass_d <- data.frame(contrast = character(42), estimate = numeric(42), lower_CI = numeric(42), upper_CI = numeric(42))
for (i in 1:length(comparisons)) { 
  glass_d_value <- glass_delta(
    exp1_1[exp1_1$group == comparisons[[i]][1], ]$OD595, 
    exp1_1[exp1_1$group == comparisons[[i]][2], ]$OD595
  )
  
  glass_d$contrast[i] <- comparisons[i]
  glass_d$estimate[i] <- round(glass_d_value$Glass_delta_adjusted, 3)
  glass_d$lower_CI[i] <- round(glass_d_value$CI_low, 3)
  glass_d$upper_CI[i] <- round(glass_d_value$CI_high, 3)
}


