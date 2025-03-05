Exp1_11_SNP_Matrix$Substitution <- as.character(Exp1_11_SNP_Matrix$Substitution)
Exp1_11_SNP_Matrix$Substitution <- factor(Exp1_11_SNP_Matrix$Substitution, levels=unique(Exp1_11_SNP_Matrix$Substitution))

tiff("SNPs.tiff", units="cm", width=30, height=25, res=400)
ggplot(Exp1_11_SNP_Matrix) + 
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 0.5, 
           ymax = 5.5, 
           alpha = 0.1)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 5.5, 
           ymax = 9.5, 
           alpha = 0)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 9.5, 
           ymax = 10.5, 
           alpha = 0.1)+  
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 10.5, 
           ymax = 12.5, 
           alpha = 0)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 12.5, 
           ymax = 13.5, 
           alpha = 0.1)+ 
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 13.5, 
           ymax = 16.5, 
           alpha = 0)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 16.5, 
           ymax = 17.5, 
           alpha = 0.1)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 17.5, 
           ymax = 20.5, 
           alpha = 0)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 20.5, 
           ymax = 25.5, 
           alpha = 0.1)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 25.5, 
           ymax = 26.5, 
           alpha = 0)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 26.5, 
           ymax = 29.5, 
           alpha = 0.1)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 29.5, 
           ymax = 32.5, 
           alpha = 0)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 32.5, 
           ymax = 33.5, 
           alpha = 0.1)+
  geom_tile(
    aes(y=Substitution, x=factor(Transfer, level = c('10', '20', '30')), fill = occurences))+
  facet_wrap(
    ~ factor(Substrate, levels=c("Glass-Adapted", "PVC-Adapted", "Stainless Steel-Adapted", "Planktonically-Adapted")),
    nrow=1)+
  scale_y_discrete(
    limits = rev(levels(Exp1_11_SNP_Matrix$Substitution)),
    position = "right")+
  scale_x_discrete(
    expand = c(0,0))+
  scale_fill_manual(
    values =  c("grey", "#dbd40b", "#ff6b00", "#cf0d00"))+
  xlab("Transfer")+
  ylab("Mutations Demonstrating Parallelic Selection")+
  theme_bw() + 
  theme(
    text = element_text(
      size = 12, 
      family = "sans", 
      colour = "black"),
    axis.text.x=element_text(
      size = 12, 
      family = "sans", 
      colour = "black"),
    axis.text.y=element_text(
      size = 12, 
      family = "sans", 
      colour = "black"),
    axis.title.x = element_text(
      size = 12, 
      family = "sans", 
      colour = "black"),
    axis.title.y = element_text(size = 12, 
                                family = "sans", 
                                colour = "black",
                                margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.ticks.length=unit(.2, "cm"),
    axis.line = element_line(colour = 'black', size = 0.5),
    panel.border = element_rect(colour = "black", fill=NA, size=0.6),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    legend.box.margin = margin(-10,-10,-10,-10),
    legend.position = "top",
    legend.justification = c(-0.1,0),
    legend.direction = "horizontal",
    legend.text = element_text(
      size = 12, 
      family = "sans", 
      colour = "black"),
    legend.title = element_blank(),
    legend.spacing.x = unit(0.5, 'cm'),
    legend.background = element_rect(fill="NA"),
    strip.background = element_rect(fill="NA", colour = "NA"),
    strip.text.x = element_text(margin = margin(0.25,0,0.3,0, "cm")),
    strip.text =  element_text(
      size = 12, 
      family = "sans", 
      colour = "black"))
dev.off()

Exp3_17_SNP_Matrix_SNPs_Data$Substitution <- as.character(Exp3_17_SNP_Matrix_SNPs_Data$Substitution)
Exp3_17_SNP_Matrix_SNPs_Data$Substitution <- factor(Exp3_17_SNP_Matrix_SNPs_Data$Substitution, levels=unique(Exp3_17_SNP_Matrix_SNPs_Data$Substitution))

facetlabel <- c( "1" = "Biofilm", 
                 "2" = "Planktonic",
                 "3" = "Biofilm", 
                 "4" = "Planktonic")

tiff("caz.tiff", units="cm", width=23, height=26, res=400)
ggplot(Exp3_17_SNP_Matrix_SNPs_Data) + 
  geom_tile(
    aes(y=Substitution, x=factor(Transfer, level = c('6', '12', '18')), fill = count))+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 0.5, 
           ymax = 1.5, 
           alpha = 0)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 1.5, 
           ymax = 4.5, 
           alpha = 0.1)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 4.5, 
           ymax = 9.5, 
           alpha = 0)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 9.5, 
           ymax = 12.5, 
           alpha = 0.1)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 12.5, 
           ymax = 13.5, 
           alpha = 0)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 13.5, 
           ymax = 14.5, 
           alpha = 0.1)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 14.5, 
           ymax = 16.5, 
           alpha = 0)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 16.5, 
           ymax = 17.5, 
           alpha = 0.1)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 17.5, 
           ymax = 18.5, 
           alpha = 0)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 18.5, 
           ymax = 20.5, 
           alpha = 0.1)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 20.5, 
           ymax = 21.5, 
           alpha = 0)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 21.5, 
           ymax = 22.5, 
           alpha = 0.1)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 22.5, 
           ymax = 24.5, 
           alpha = 0)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 24.5, 
           ymax = 26.5, 
           alpha = 0.1)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 26.5, 
           ymax = 28.5, 
           alpha = 0)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 28.5, 
           ymax = 31.5, 
           alpha = 0.1)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 31.5, 
           ymax = 35.5, 
           alpha = 0)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 35.5, 
           ymax = 37.5, 
           alpha = 0.1)+
  annotate("rect", 
           xmin = 0.5, 
           xmax = 3.5, 
           ymin = 37.5, 
           ymax = 38.5, 
           alpha = 0)+
  facet_wrap(
    ~ panel,
    labeller =  labeller(panel = facetlabel),
    nrow=1)+
  scale_y_discrete(
    limits = rev(levels(Exp3_17_SNP_Matrix_SNPs_Data$Substitution)),
    position = "right")+
  scale_x_discrete(
    expand = c(0,0))+
  scale_fill_manual(
    values =  c("#dbd40b", "#ff6b00", "#cf0d00"))+
  xlab("Transfer")+
  ylab("Mutations Demonstrating Parallelic Selection")+
  theme_bw() + 
  theme(
    text = element_text(
      size = 12, 
      family = "sans", 
      colour = "black"),
    axis.text.x=element_text(
      size = 12, 
      family = "sans", 
      colour = "black"),
    axis.text.y=element_text(
      size = 12, 
      family = "sans", 
      colour = "black"),
    axis.title.x = element_text(
      size = 12, 
      family = "sans", 
      colour = "black"),
    axis.title.y = element_text(size = 12, 
                                family = "sans", 
                                colour = "black",
                                margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.ticks.length=unit(.2, "cm"),
    axis.line = element_line(colour = 'black', size = 0.5),
    panel.border = element_rect(colour = "black", fill=NA, size=0.6),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    legend.box.margin = margin(-10,-10,-10,-10),
    legend.position = "top",
    legend.justification = c(-0.1,0),
    legend.direction = "horizontal",
    legend.text = element_text(
      size = 12, 
      family = "sans", 
      colour = "black"),
    legend.title = element_blank(),
    legend.spacing.x = unit(0.5, 'cm'),
    legend.background = element_rect(fill="NA"),
    strip.background = element_rect(fill="NA", colour = "NA"),
    strip.text.x = element_text(margin = margin(0.25,0,0.3,0, "cm")),
    strip.text =  element_text(
      size = 12, 
      family = "sans", 
      colour = "black"))
dev.off()
