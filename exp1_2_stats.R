source(.RProfile)

# exp1.2 ------------------------------------------------------------------
exp1_2 <- read_excel("Exp1.2. Populations_Productivity_Data.xlsx", 
                     col_types = c("text", "text", "text", "text", "numeric", "skip", "skip", "text")
)

#define 2-way ANOVA
anova_results <- aov(productivity ~ assay_substrate * selective_substrate, data = exp1_2)
summary(anova_results, infer = TRUE)
omega_squared(anova_results, ci = 0.95, alternative = "two.sided",)

