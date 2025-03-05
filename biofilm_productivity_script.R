tiff("Exp1.2. Figure.tiff", units="cm", width=30, height=18, res=400)
ggplot(Exp1_2_Populations_Productivity_Data) +
  geom_boxplot(
    aes(x=group, y=productivity,  fill=transfer),
    alpha=0.35,
    fatten=1,
    show.legend = F,
    outlier.colour = "black",
    outlier.size = 1,
    outlier.shape = 21,
    outlier.alpha = 0.6,
    position=position_dodge(width=1))+
  stat_summary(
    aes(x=group, y=productivity,  fill=transfer),
    fun=mean,
    shape=3,
    size=0.25, 
    stroke=0.75,
    show.legend=FALSE, 
    position=position_dodge(width=1))+
  stat_summary(aes(x=group, y=productivity, group = segment), 
               fun = mean, 
               geom = "path",
               linetype = "dashed",
               show.legend=FALSE,
               size=0.5)+
  stat_summary(
    aes(x=group, y=productivity, fill=transfer, colour=transfer, shape=lineage), 
    subset(Exp1_2_Populations_Productivity_Data, transfer %in% c("10","20", "30", "Ancestor")),
    fun=mean,
    stroke=0.5,
    show.legend=FALSE, 
    position=position_dodge(width=0))+
  facet_wrap(
    ~ assay_substrate,
    nrow = 1)+
  scale_y_continuous(
    trans ='log10',
    breaks=trans_breaks('log10', function(x) 10^x), labels=scientific)+
  ylab("Biofilm Productivity (c.f.u. / "~mm^2*")")+
  xlab("Lineage")+
  annotation_logticks(
    sides = 'l',
    outside= TRUE)+
  coord_cartesian(clip="off", ylim=c(1000, 10000000))+
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
  scale_x_discrete(
    breaks = c("a", "b", "c", "d", "e", "f", "g"),
    labels = c("Ancestor", " ", "Substrate-Adapted", " ", " ", "Planktonically-adapted", " "))+
  scale_fill_manual(
    breaks = c("10", "20", "30", "Ancestor"),
    values = c("#dbd40b", "#ff6b00", "#cf0d00", "NA"))+
  scale_color_manual(
    breaks = c("10", "20", "30", "Ancestor"),
    values = c("black", "black", "black", "NA"))+
  scale_shape_manual(
    values = c(21,22,24,23,0))
dev.off()
