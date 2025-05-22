source(.RProfile)

# exp1.10 -----------------------------------------------------------------

# Create the substrate matrix
substrate_matrix <- matrix(c(
  20, 12, 4, 8, 4, 0,   # glass
  4,  8, 4, 24, 8, 0,  # pvc
  28,  8, 0, 8, 4, 0,   # steel
  0,  0, 0, 0, 0, 48   # planktonic
), nrow = 4, byrow = TRUE)

rownames(substrate_matrix) <- c("glass", "pvc", "steel", "planktonic")
colnames(substrate_matrix) <- c("hyperrugose", "diffuse", "circumscribed", "filiform", "radial", "ancestor")

substrate_fisher_exact <- fisher.test(substrate_matrix, simulate.p.value = TRUE, B = 500000)

substrate_chi_sq <- chisq.test(substrate_matrix, simulate.p.value = TRUE, B = 500000)
std_resid <- substrate_chi_sq$stdres
round(std_resid, 3)

p_matrix <- 2 * pnorm(-abs(std_resid))
p_flat <- as.vector(p_matrix)
p_fdr <- p.adjust(p_flat, method = "fdr")

dim(p_fdr) <- dim(p_matrix)
dim(p_bonf) <- dim(p_matrix)

rownames(p_fdr) <- rownames(std_resid)
colnames(p_fdr) <- colnames(std_resid)

# Create the timepoint matrix
timepoint_matrix <- matrix(c(
  28, 12, 0, 4, 4, 16,    # 10
  4, 8, 8, 24, 4, 16,     # 20
  20, 8, 0, 12, 8, 16     # 30
), nrow = 3, byrow = TRUE)


rownames(timepoint_matrix) <- c("10", "20", "30")
colnames(timepoint_matrix) <- c("hyperrugose", "diffuse", "circumscribed", "filiform", "radial", "ancestor")

timepoint_fisher_exact <- fisher.test(timepoint_matrix, simulate.p.value = TRUE, B = 500000)

timepoint_chi_sq <- chisq.test(timepoint_matrix, simulate.p.value = TRUE, B = 500000)
std_resid <- timepoint_chi_sq$stdres
round(std_resid, 3)

p_matrix <- 2 * pnorm(-abs(std_resid))
p_flat <- as.vector(p_matrix)
p_fdr <- p.adjust(p_flat, method = "fdr")

dim(p_fdr) <- dim(p_matrix)

rownames(p_fdr) <- rownames(std_resid)
colnames(p_fdr) <- colnames(std_resid)

