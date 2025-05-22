source(.RProfile)

# rugosity ----------------------------------------------------------------
exp1_7 <- read_excel("Exp1.7. Populations_Morphology_Data.xlsx")

anova_results <- aov(rugosity ~ substrate * transfer, data = exp1_7)
summary(anova_results, infer = TRUE)
confint(anova_results)

estimated_meandiff <- emmeans(anova_results, ~ group)
estimated_meandiff

# Define comparisons for posthoc tests
comparisons <- list(
  c('ancestor', 'glass-10'),
  c('ancestor', 'glass-20'),
  c('ancestor', 'glass-30'),
  c('ancestor', 'planktonic-10'),
  c('ancestor', 'planktonic-20'),
  c('ancestor', 'planktonic-30'),
  c('ancestor', 'pvc-10'),
  c('ancestor', 'pvc-20'),
  c('ancestor', 'pvc-30'),
  c('ancestor', 'steel-10'),
  c('ancestor', 'steel-20'),
  c('ancestor', 'steel-30'),
  c('glass-10', 'glass-20'),
  c('glass-10', 'glass-30'),
  c('glass-10', 'planktonic-10'),
  c('glass-10', 'pvc-10'),
  c('glass-10', 'steel-10'),
  c('glass-20', 'glass-30'),
  c('glass-20', 'planktonic-20'),
  c('glass-20', 'pvc-20'),
  c('glass-20', 'steel-20'),
  c('glass-30', 'planktonic-30'),
  c('glass-30', 'pvc-30'),
  c('glass-30', 'steel-30'),
  c('planktonic-10', 'planktonic-20'),
  c('planktonic-10', 'planktonic-30'),
  c('planktonic-20', 'planktonic-30'),
  c('pvc-10', 'planktonic-10'),
  c('pvc-10', 'pvc-20'),
  c('pvc-10', 'pvc-30'),
  c('pvc-20', 'planktonic-20'),
  c('pvc-20', 'pvc-30'),
  c('pvc-30', 'planktonic-30'),
  c('steel-10', 'planktonic-10'),
  c('steel-10', 'pvc-10'),
  c('steel-10', 'steel-20'),
  c('steel-10', 'steel-30'),
  c('steel-20', 'planktonic-20'),
  c('steel-20', 'pvc-20'),
  c('steel-20', 'steel-30'),
  c('steel-30', 'planktonic-30'),
  c('steel-30', 'pvc-30')
)

group_order = list(
  'ancestor', 
  'glass-10', 'glass-20', 'glass-30', 
  'planktonic-10', 'planktonic-20', 'planktonic-30',
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


anova_results <- aov(rugosity ~ substrate, data = exp1_7)
summary(anova_results, infer = TRUE)
emm <- emmeans(anova_results, ~ substrate)
contrast_results <- contrast(emm, method = "trt.vs.ctrl", ref = "ancestor")
summary(contrast_results, infer = c(TRUE, TRUE))


anova_results <- aov(rugosity ~ group, data = exp1_7)
summary(anova_results, infer = TRUE)
confint(anova_results)

estimated_meandiff <- emmeans(anova_results, ~ group)
estimated_meandiff

# Define comparisons for posthoc tests
comparisons <- list(
  c('ancestor', 'glass-10'),
  c('ancestor', 'glass-20'),
  c('ancestor', 'glass-30'),
  c('ancestor', 'planktonic-10'),
  c('ancestor', 'planktonic-20'),
  c('ancestor', 'planktonic-30'),
  c('ancestor', 'pvc-10'),
  c('ancestor', 'pvc-20'),
  c('ancestor', 'pvc-30'),
  c('ancestor', 'steel-10'),
  c('ancestor', 'steel-20'),
  c('ancestor', 'steel-30'),
  c('glass-10', 'glass-20'),
  c('glass-10', 'glass-30'),
  c('glass-10', 'planktonic-10'),
  c('glass-10', 'pvc-10'),
  c('glass-10', 'steel-10'),
  c('glass-20', 'glass-30'),
  c('glass-20', 'planktonic-20'),
  c('glass-20', 'pvc-20'),
  c('glass-20', 'steel-20'),
  c('glass-30', 'planktonic-30'),
  c('glass-30', 'pvc-30'),
  c('glass-30', 'steel-30'),
  c('planktonic-10', 'planktonic-20'),
  c('planktonic-10', 'planktonic-30'),
  c('planktonic-20', 'planktonic-30'),
  c('pvc-10', 'planktonic-10'),
  c('pvc-10', 'pvc-20'),
  c('pvc-10', 'pvc-30'),
  c('pvc-20', 'planktonic-20'),
  c('pvc-20', 'pvc-30'),
  c('pvc-30', 'planktonic-30'),
  c('steel-10', 'planktonic-10'),
  c('steel-10', 'pvc-10'),
  c('steel-10', 'steel-20'),
  c('steel-10', 'steel-30'),
  c('steel-20', 'planktonic-20'),
  c('steel-20', 'pvc-20'),
  c('steel-20', 'steel-30'),
  c('steel-30', 'planktonic-30'),
  c('steel-30', 'pvc-30')
)

group_order = list(
  'ancestor', 
  'glass-10', 'glass-20', 'glass-30', 
  'planktonic-10', 'planktonic-20', 'planktonic-30',
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


anova_results <- aov(rugosity ~ morphotype, data = exp1_7)
summary(anova_results, infer = TRUE)
emm <- emmeans(anova_results, ~ substrate)
contrast_results <- contrast(emm, method = "trt.vs.ctrl", ref = "ancestor")
summary(contrast_results, infer = c(TRUE, TRUE))

interaction.plot(exp1_7$transfer, exp1_7$morphotype, exp1_7$rugosity)
interaction.plot(exp1_7$transfer, exp1_7$substrate, exp1_7$rugosity)

TukeyHSD(anova_results)

# invasion ----------------------------------------------------------------
exp1_7 <- read_excel("Exp1.7. Populations_Morphology_Data.xlsx")

anova_results <- aov(invasion ~ group, data = exp1_7)
summary(anova_results, infer = TRUE)
confint(anova_results)

estimated_meandiff <- emmeans(anova_results, ~ group)
estimated_meandiff

# Define comparisons for posthoc tests
comparisons <- list(
  c('ancestor', 'glass-10'),
  c('ancestor', 'glass-20'),
  c('ancestor', 'glass-30'),
  c('ancestor', 'planktonic-10'),
  c('ancestor', 'planktonic-20'),
  c('ancestor', 'planktonic-30'),
  c('ancestor', 'pvc-10'),
  c('ancestor', 'pvc-20'),
  c('ancestor', 'pvc-30'),
  c('ancestor', 'steel-10'),
  c('ancestor', 'steel-20'),
  c('ancestor', 'steel-30'),
  c('glass-10', 'glass-20'),
  c('glass-10', 'glass-30'),
  c('glass-10', 'planktonic-10'),
  c('glass-10', 'pvc-10'),
  c('glass-10', 'steel-10'),
  c('glass-20', 'glass-30'),
  c('glass-20', 'planktonic-20'),
  c('glass-20', 'pvc-20'),
  c('glass-20', 'steel-20'),
  c('glass-30', 'planktonic-30'),
  c('glass-30', 'pvc-30'),
  c('glass-30', 'steel-30'),
  c('planktonic-10', 'planktonic-20'),
  c('planktonic-10', 'planktonic-30'),
  c('planktonic-20', 'planktonic-30'),
  c('pvc-10', 'planktonic-10'),
  c('pvc-10', 'pvc-20'),
  c('pvc-10', 'pvc-30'),
  c('pvc-20', 'planktonic-20'),
  c('pvc-20', 'pvc-30'),
  c('pvc-30', 'planktonic-30'),
  c('steel-10', 'planktonic-10'),
  c('steel-10', 'pvc-10'),
  c('steel-10', 'steel-20'),
  c('steel-10', 'steel-30'),
  c('steel-20', 'planktonic-20'),
  c('steel-20', 'pvc-20'),
  c('steel-20', 'steel-30'),
  c('steel-30', 'planktonic-30'),
  c('steel-30', 'pvc-30')
)

group_order = list(
  'ancestor', 
  'glass-10', 'glass-20', 'glass-30', 
  'planktonic-10', 'planktonic-20', 'planktonic-30',
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


anova_results <- aov(invasion ~ substrate, data = exp1_7)
summary(anova_results, infer = TRUE)
emm <- emmeans(anova_results, ~ substrate)
contrast_results <- contrast(emm, method = "trt.vs.ctrl", ref = "ancestor")
summary(contrast_results, infer = c(TRUE, TRUE))


anova_results <- aov(rugosity ~ group, data = exp1_7)
summary(anova_results, infer = TRUE)
confint(anova_results)

estimated_meandiff <- emmeans(anova_results, ~ group)
estimated_meandiff

# Define comparisons for posthoc tests
comparisons <- list(
  c('ancestor', 'glass-10'),
  c('ancestor', 'glass-20'),
  c('ancestor', 'glass-30'),
  c('ancestor', 'planktonic-10'),
  c('ancestor', 'planktonic-20'),
  c('ancestor', 'planktonic-30'),
  c('ancestor', 'pvc-10'),
  c('ancestor', 'pvc-20'),
  c('ancestor', 'pvc-30'),
  c('ancestor', 'steel-10'),
  c('ancestor', 'steel-20'),
  c('ancestor', 'steel-30'),
  c('glass-10', 'glass-20'),
  c('glass-10', 'glass-30'),
  c('glass-10', 'planktonic-10'),
  c('glass-10', 'pvc-10'),
  c('glass-10', 'steel-10'),
  c('glass-20', 'glass-30'),
  c('glass-20', 'planktonic-20'),
  c('glass-20', 'pvc-20'),
  c('glass-20', 'steel-20'),
  c('glass-30', 'planktonic-30'),
  c('glass-30', 'pvc-30'),
  c('glass-30', 'steel-30'),
  c('planktonic-10', 'planktonic-20'),
  c('planktonic-10', 'planktonic-30'),
  c('planktonic-20', 'planktonic-30'),
  c('pvc-10', 'planktonic-10'),
  c('pvc-10', 'pvc-20'),
  c('pvc-10', 'pvc-30'),
  c('pvc-20', 'planktonic-20'),
  c('pvc-20', 'pvc-30'),
  c('pvc-30', 'planktonic-30'),
  c('steel-10', 'planktonic-10'),
  c('steel-10', 'pvc-10'),
  c('steel-10', 'steel-20'),
  c('steel-10', 'steel-30'),
  c('steel-20', 'planktonic-20'),
  c('steel-20', 'pvc-20'),
  c('steel-20', 'steel-30'),
  c('steel-30', 'planktonic-30'),
  c('steel-30', 'pvc-30')
)

group_order = list(
  'ancestor', 
  'glass-10', 'glass-20', 'glass-30', 
  'planktonic-10', 'planktonic-20', 'planktonic-30',
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


anova_results <- aov(invasion ~ morphotype, data = exp1_7)
summary(anova_results, infer = TRUE)
emm <- emmeans(anova_results, ~ morphotype)

TukeyHSD(anova_results)

