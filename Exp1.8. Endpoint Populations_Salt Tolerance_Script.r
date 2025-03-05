tiff("Exp1.8. Figure.tiff", units="cm", width=20, height=14, res=400)
ggplot(Exp1_8_Endpoint_Populations_Salt_Tolerance_Data) +
  geom_hline(yintercept=0, color = "black", linetype="dashed", size=0.5)+
  geom_boxplot(
    aes(x = factor(substrate, level = c('ancestor', 'glass', 'pvc', 'steel', 'planktonic')), y=-reduction),
    alpha=0.35,
    fatten=1,
    show.legend = F,
    fill=c("white", "#cf0d00","#cf0d00","#cf0d00","#cf0d00"),
    outlier.fill = "white",
    outlier.colour = "black",
    outlier.size = 1,
    outlier.shape = 21,
    outlier.alpha = 0.6,
    position=position_dodge(width=1))+
  stat_summary(
    aes(x= factor(substrate, level = c('ancestor', 'glass', 'pvc', 'steel', 'planktonic')), y=-reduction),
    fun=mean,
    shape=3,
    size=0.25, 
    stroke=0.75,
    show.legend=FALSE, 
    position=position_dodge(width=1))+
  stat_summary(
    aes(x=substrate, y=-reduction, shape=lineage), 
    subset(Exp1_8_Endpoint_Populations_Salt_Tolerance_Data, lineage %in% c("ancestor","1", "2", "3", "4")),
    fun=mean,
    fill="#cf0d00",
    stroke=0.5,
    show.legend=FALSE, 
    position=position_dodge(width=0))+
  coord_cartesian(clip="off", ylim = c(5, 0))+
  theme_bw() + 
  theme(
    text = element_text(
      size = 16, 
      family = "sans", 
      colour = "black"),
    axis.text.x=element_text(
      size = 16, 
      family = "sans", 
      colour = "black"),
    axis.text.y=element_text(
      size = 16, 
      family = "sans", 
      colour = "black"),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 15)),
    axis.ticks.length=unit(.4, "cm"),
    axis.line = element_line(colour = 'black', size = 0.5),
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.6),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(1, "lines"),
    legend.box.margin = margin(-10,-10,-10,-10),
    legend.position = "top",
    legend.justification = c(-0.02,0),
    legend.direction = "horizontal",
    legend.text = element_text(
      size = 16, 
      family = "sans", 
      colour = "black"),
    legend.title = element_text(
      size = 16, 
      family = "sans", 
      colour = "black"),
    legend.spacing.x = unit(0.7, 'cm'),
    legend.background = element_rect(fill="NA"),
    strip.background = element_rect(fill="NA", colour = "NA"),
    strip.text.x = element_text(margin = margin(0.25,0,0.3,0, "cm")),
    strip.text =  element_text(
      size = 16, 
      family = "sans", 
      colour = "black"))+
  scale_shape_manual(
    values = c(21,22,24,23,NA))+
  scale_x_discrete(
    name="Selective Substrate",
    breaks = c("ancestor", "glass", "pvc", "steel", "planktonic"),
    labels = c("Ancestor", "Glass", "PVC", "Stainless Steel","Planktonic"))+
  scale_y_reverse(expression(
    paste("Log"["10 "]*" Reduction in Cell Viability After\nChallenge with 9% w/v Sodium Chloride")))+
  annotation_logticks(
    sides = 'l',
    scaled=T,
    outside = T,
    short = unit(0.2, "cm"),
    mid = unit(0.35, "cm"),
    long = unit(0.35, "cm"))
dev.off()