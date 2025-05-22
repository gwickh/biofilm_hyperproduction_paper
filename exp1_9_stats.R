source(.RProfile)

# exp1.9 ------------------------------------------------------------------
exp1_9 <- read_excel("Exp1.8. Endpoint Populations_Acid Tolerance_Data.xlsx", 
                     col_types = c("text", "text", "skip", "skip", "numeric", 'text')
)

anova_results <- aov(reduction ~ substrate * condition, data = exp1_9)
summary(anova_results, infer = TRUE)
confint(anova_results)
omega_squared(anova_results, ci = 0.95, alternative = "two.sided",)

# Estimated marginal means
emm <- emmeans(anova_results, ~ substrate | condition)

# Compare each substrate to 'ancestor' within each pH
contrast_results <- contrast(emm, method = "trt.vs.ctrl", ref = "ancestor")
summary(contrast_results, infer = c(TRUE, TRUE))

contrast_results <- contrast(emm, method = "trt.vs.ctrl", ref = "planktonic")
summary(contrast_results, infer = c(TRUE, TRUE))

