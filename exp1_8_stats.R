source(.RProfile)

# exp1.8 ------------------------------------------------------------------
exp1_8 <- read_excel("Exp1.8. Endpoint Populations_Salt Tolerance_Data.xlsx", 
                     col_types = c("text", "text", "skip", "skip", "numeric")
)

#define 1-way ANOVA
anova_results <- aov(reduction ~ substrate, data = exp1_8)
summary(anova_results, infer = TRUE)
confint(anova_results)
omega_squared(anova_results, ci = 0.95, alternative = "two.sided",)
estimated_meandiff <- emmeans(anova_results, ~ substrate)

# Run Tukey's posthoc test on contrasts
TukeyHSD(anova_results)