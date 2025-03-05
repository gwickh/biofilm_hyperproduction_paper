tiff("substrate rugosity.tiff", units="cm", width=22.5, height=25, res=400)
ggplot(Exp1_7_Populations_Morphology_Data) +
  geom_boxplot(
    aes(x=partition_group, y=rugosity,  fill=transfer),
    alpha=0.35,
    fatten=1,
    show.legend = T,
    outlier.colour = "black",
    outlier.size = 1,
    outlier.shape = 21,
    outlier.alpha = 0.6,
    position=position_dodge(width=1))+
  stat_summary(
    aes(x=partition_group, y=rugosity,  fill=transfer),
    fun=mean,
    shape=3,
    size=0.75, 
    stroke=0.75,
    show.legend=FALSE, 
    position=position_dodge(width=1))+
  stat_summary(aes(x=partition_group, y=rugosity,  group=lineage_segment), 
               fun = mean, 
               geom = "path",
               linetype = "dashed",
               show.legend=FALSE,
               size=0.5)+
  stat_summary(
    aes(partition_group, y=rugosity,  fill=transfer, colour=transfer, shape=lineage), 
    subset(Exp1_7_Populations_Morphology_Data, transfer %in% c("10","20", "30", "Ancestor")),
    fun=mean,
    stroke=0.5,
    size=0.75,
    show.legend=FALSE, 
    position=position_dodge(width=0))+
  ylab("Colony Rugosity (Relative Threshold Area)")+
  xlab("Selective Substrate")+
  annotation_logticks(
    sides = 'l',
    outside= TRUE)+
  coord_cartesian(ylim = c(0, 0.6))+
  guides(colour=FALSE, shape=FALSE)+
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
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.ticks.length=unit(.4, "cm"),
    axis.line = element_line(colour = 'black', size = 0.5),
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
    legend.title = element_blank(),
    legend.spacing.x = unit(0.7, 'cm'),
    legend.background = element_rect(fill="NA"),
    strip.background = element_rect(fill="NA", colour = "NA"),
    strip.text.x = element_text(margin = margin(0.25,0,0.3,0, "cm")),
    strip.text =  element_text(
      size = 16, 
      family = "sans", 
      colour = "black"))+
  scale_x_discrete(
    breaks = c("a", "c", "f", "i", "l"),
    labels = c("Ancestor", "Glass-Adapted", "PVC-Adapted", "Stainless Steel-\nAdapted", "Planktonically-\nadapted"))+
  scale_fill_manual(
    breaks = c("10", "20", "30", "Ancestor", "dummy"),
    labels = c("Transfer 10", "Transfer 20", "Transfer 30", "Ancestor", "Ancestor"),
    values = c("#dbd40b", "#ff6b00", "#cf0d00", "NA", "NA"))+
  scale_color_manual(
    breaks = c("10", "20", "30", "Ancestor"),
    values = c("black", "black", "black", "NA"))+
  scale_shape_manual(
    values = c(21,22,24,23,0))
dev.off()



ggplot(Exp1_7_Populations_Morphology_Data) +
  geom_boxplot(
    aes(x=morphotype, y=rugosity,  fill=transfer),
    alpha=0.35,
    fatten=1,
    show.legend = T,
    outlier.colour = "black",
    outlier.size = 1,
    outlier.shape = 21,
    outlier.alpha = 0.6,
    position=position_dodge(width=1))+
  stat_summary(
    aes(x=morphotype, y=rugosity,  fill=transfer),
    fun=mean,
    shape=3,
    size=0.75, 
    stroke=0.75,
    show.legend=FALSE, 
    position=position_dodge(width=1))+
  stat_summary(aes(x=morphotype, y=rugosity,  group=lineage_segment), 
               fun = mean, 
               geom = "path",
               linetype = "dashed",
               show.legend=FALSE,
               size=0.5)+
  stat_summary(
    aes(morphotype, y=rugosity,  fill=transfer, colour=transfer, shape=lineage), 
    subset(Exp1_7_Populations_Morphology_Data, transfer %in% c("10","20", "30", "Ancestor")),
    fun=mean,
    stroke=0.5,
    size=0.75,
    show.legend=FALSE, 
    position=position_dodge(width=0))+
  ylab("Colony Rugosity (Relative Threshold Area)")+
  xlab("Selective Substrate")+
  annotation_logticks(
    sides = 'l',
    outside= TRUE)+
  coord_cartesian(ylim = c(0, 0.75))+
  guides(colour=FALSE, shape=FALSE)+
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
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.ticks.length=unit(.4, "cm"),
    axis.line = element_line(colour = 'black', size = 0.5),
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
    legend.title = element_blank(),
    legend.spacing.x = unit(0.7, 'cm'),
    legend.background = element_rect(fill="NA"),
    strip.background = element_rect(fill="NA", colour = "NA"),
    strip.text.x = element_text(margin = margin(0.25,0,0.3,0, "cm")),
    strip.text =  element_text(
      size = 16, 
      family = "sans", 
      colour = "black"))

tiff("morphotype rugosity.tiff", units="cm", width=10, height=25, res=400)
ggplot(Exp1_7_Populations_Morphology_Data) +
  geom_boxplot(
    aes(x=factor(morphotype, level = c('ancestor', 'filiform', 'circumscribed', 'hyperrugose', 'radial', 'diffuse')), y=rugosity),
    subset(Exp1_7_Populations_Morphology_Data, morphotype %in% 
             c("ancestor","filiform", "circumscribed", "hyperrugose", "radial", "diffuse")),
    alpha=0.7,
    fatten=1,
    fill= c("white", "#E1c300", "#Fa6141", "#C70039", "#900c3f", "#581845"),
    show.legend = F,
    outlier.colour = "black",
    outlier.size = 1,
    outlier.shape = 1,
    outlier.alpha = 0.6)+
  stat_summary(
    aes(x=factor(morphotype, level = c('ancestor', 'filiform', 'circumscribed', 'hyperrugose', 'radial', 'diffuse')), y=rugosity),
    subset(Exp1_7_Populations_Morphology_Data, morphotype %in% c("ancestor","filiform", "circumscribed", "hyperrugose", "radial", "diffuse")),
    fun=mean,
    shape=3,
    size=0.75, 
    stroke=0.75,
    show.legend=FALSE, 
    position=position_dodge(width=1))+
  stat_summary(
    aes(x=factor(morphotype, level = c('ancestor', 'filiform', 'circumscribed', 'hyperrugose', 'radial', 'diffuse')), y=rugosity,  fill = transfer, group=group), 
    subset(Exp1_7_Populations_Morphology_Data, morphotype %in% c("ancestor","filiform", "circumscribed", "hyperrugose", "radial", "diffuse")),
    fun=mean,
    show.legend=F,
    shape=21,
    stroke=0.5,
    size=0.75,
    position=position_dodge(width=0.5))+
  ylab("Agar Invasion (Relative Area of Coverage)")+
  xlab("Morphotype")+
  annotation_logticks(
    sides = 'l',
    outside= TRUE)+
  coord_cartesian(ylim = c(0, 1))+
  theme_bw() + 
  theme(
    text = element_text(
      size = 16, 
      family = "sans", 
      colour = "black"),
    axis.text.x=element_text(
      size = 16, 
      family = "sans", 
      colour = "black",
      angle = 45, vjust = 1, hjust=1),
    axis.text.y=element_text(
      size = 16, 
      family = "sans", 
      colour = "black"),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.ticks.length=unit(.4, "cm"),
    axis.line = element_line(colour = 'black', size = 0.5),
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
    legend.title = element_blank(),
    legend.spacing.x = unit(0.7, 'cm'),
    legend.background = element_rect(fill="NA"),
    strip.background = element_rect(fill="NA", colour = "NA"),
    strip.text.x = element_text(margin = margin(0.25,0,0.3,0, "cm")),
    strip.text =  element_text(
      size = 16, 
      family = "sans", 
      colour = "black"))+
  scale_x_discrete(
    breaks = c("ancestor", "filiform", "circumscribed", "hyperrugose", "radial", "diffuse"),
    labels = c("Ancestor", "Filiform", "Circumscribed", "Hyperrugose", "Radial", "Diffuse"))+
  scale_shape_manual(
    breaks = c("glass", "pvc", "steel", "planktonic"),
    labels = c("Glass-Adapted", "PVC-Adapted", "Stainless Steel-Adapted", "Planktonically-Adapted" ))+
  scale_colour_manual(,
                      breaks=c("Ancestor", "10","20","30"),
                      labels=c("Ancestor", "Transfer 10", "Transfer 20", "Transfer 30"),
                      values =  c("black", "#dbd40b", "#ff6b00", "#cf0d00"))+
  scale_fill_manual(,
                    breaks=c("Ancestor", "10","20","30"),
                    labels=c("Ancestor", "Transfer 10", "Transfer 20", "Transfer 30"),
                    values =  c("black", "#dbd40b", "#ff6b00", "#cf0d00"))
