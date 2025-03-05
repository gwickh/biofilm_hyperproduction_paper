tiff("bf_hetero_intrapop.tiff", units="cm", width=21, height=18, res=400)
ggplot(X210122_Biofilm_Adaptation_Crystal_Violet_Heterogeneity_Data) +
  geom_boxplot(
    aes(x=partition_group, y=meandif,  fill=transfer),
    alpha=0.35,
    fatten=1,
    show.legend = F,
    outlier.colour = "black",
    outlier.size = 1,
    outlier.shape = 21,
    outlier.alpha = 0.6)+
  stat_summary(
    aes(x=partition_group, y=meandif, fill=transfer),
    fun=mean,
    shape=3,
    size=0.25, 
    stroke=0.75,
    show.legend=FALSE, 
    position=position_dodge(width=1))+
  stat_summary(
    aes(x=partition_group, y=meandif, fill = transfer, shape=individual), 
    subset(X210122_Biofilm_Adaptation_Crystal_Violet_Heterogeneity_Data, transfer %in% c("ancestor","10", "20", "30")),
    size = 0.5, 
    fun=mean,
    stroke=0.5,
    colour="black",
    show.legend = F,
    position=position_dodge(width=0)) +
  coord_cartesian(ylim = c(0, 1.5))+
  ggtitle("Intra-Population Heterogeneity")+
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
    plot.title=element_text(
      size = 16, 
      family = "sans", 
      colour = "black",
      hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 12), 
                                size = 16, 
                                family = "sans", 
                                colour = "black"),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 15)),
    axis.ticks.length=unit(.4, "cm"),
    axis.line = element_line(colour = 'black', size = 0.5),
    panel.border = element_rect(colour = "black", fill=NA, size=0.75),
    panel.grid.major.x = element_blank(),
    legend.box.margin = margin(-10,-10, 0,-10),
    legend.box="vertical",
    legend.position = "top",
    legend.justification = c(-0.02,0),
    legend.direction = "horizontal",
    legend.text = element_text(
      size = 16, 
      family = "sans", 
      colour = "black"),
    legend.title = element_blank(),
    legend.spacing.x = unit(0.7, 'cm'),
    legend.background = element_rect(fill="NA"))+
  scale_y_continuous(expression(
    paste("Difference in Mean Biofilm Formation Between \n Population and Individuals (OD "["595"]*")")),
    expand = c(0,0))+
  scale_x_discrete(
    name= "Selective Substrate",
    breaks=c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q"),
    labels=c("Ancestor", " ", " ", "Glass", " ", " ", " ", "PVC", " ", " ", " ", "Stainless Steel", " ", " ", " ", "Planktonic", " "))+
  scale_fill_manual(
    name="transfer",
    breaks=c("ancestor","0","10","20","30"),
    labels=c("Ancestor", "NA", "Transfer 10", "Transfer 20", "Transfer 30"),
    values =  c( "white", "NA", "#f2ec44", "#e35f00", "#cf0d00"))+
  scale_colour_manual(
    name="transfer",
    breaks=c("ancestor","0","10","20","30"),
    labels=c("Ancestor", "NA", "Transfer 10", "Transfer 20", "Transfer 30"),
    values =  c( "black", "NA", "#f2ec44", "#e35f00", "#cf0d00"))+
  scale_shape_manual(
    name="individual",
    values=c(21,21,21,21))
dev.off()


tiff("bf_hetero_relbf.tiff", units="cm", width=21, height=18, res=400)
ggplot(X210122_Biofilm_Adaptation_Crystal_Violet_Heterogeneity_Data) +
  geom_hline(yintercept=1, linetype="dashed", color = "black")+
  stat_summary(
    aes(x=partition_group, y=relbf, colour=transfer, group = individual), 
    fun=mean,
    stroke=0.5,
    show.legend=FALSE, 
    position=position_dodge(width=0.6))+
  stat_summary(
    aes(x=partition_group, y=relbf, colour=transfer, group = individual), 
    geom = "errorbar",
    width=0.5,
    show.legend = F,
    fun.min = function(x) mean(x) + sd(x) / sqrt(length(x)),
    fun.max = function(x) mean(x) - sd(x) / sqrt(length(x)),
    position=position_dodge(width=0.6))+
  coord_cartesian(ylim = c(0, 2))+
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
    panel.border = element_rect(colour = "black", fill=NA, size=0.75),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.box.margin = margin(-10,-10, 0,-10),
    legend.box="vertical",
    legend.position = "top",
    legend.justification = c(-0.02,0),
    legend.direction = "horizontal",
    legend.text = element_text(
      size = 16, 
      family = "sans", 
      colour = "black"),
    legend.title = element_blank(),
    legend.spacing.x = unit(0.7, 'cm'),
    legend.background = element_rect(fill="NA"))+
  scale_y_continuous(expression(
    paste("Biofilm Formation of Isolated Individuals \n Relative to Parent Population")),
    expand = c(0,0))+
  scale_x_discrete(
    name= "Selective Substrate",
    breaks=c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q"),
    labels=c("Ancestor", " ", " ", "Glass", " ", " ", " ", "PVC", " ", " ", " ", "Stainless Steel", " ", " ", " ", "Planktonic", " "))+
  scale_colour_manual(
    name="transfer",
    breaks=c("ancestor", "0", "10","20","30"),
    labels=c("Ancestor", "NA", "Transfer 10", "Transfer 20", "Transfer 30"),
    values =  c("black", "NA", "#dbd40b", "#ff6b00", "#cf0d00"))
dev.off()

tiff("bf_hetero_interind.tiff", units="cm", width=21, height=18, res=400)
ggplot(Exp1_4_Individuals_Biofilm_Formation_Data) +
  geom_col(aes(x=group, y=r2, fill=transfer), 
           alpha=0.8, 
           colour="black", 
           show.legend = F)+
  coord_cartesian(ylim = c(0, 0.6001))+
  ggtitle("Inter-Individual Heterogeneity")+
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
    plot.title=element_text(
      size = 16, 
      family = "sans", 
      colour = "black",
      hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 15)),
    axis.ticks.length=unit(.4, "cm"),
    axis.line = element_line(colour = 'black', size = 0.5),
    panel.border = element_rect(colour = "black", fill=NA, size=0.75),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.box.margin = margin(-10,-10, 0,-10),
    legend.box="vertical",
    legend.position = "top",
    legend.justification = c(-0.02,0),
    legend.direction = "horizontal",
    legend.text = element_text(
      size = 16, 
      family = "sans", 
      colour = "black"),
    legend.title = element_blank(),
    legend.spacing.x = unit(0.7, 'cm'),
    legend.background = element_rect(fill="NA"))+
  scale_y_continuous(expression(
    paste("Coefficient of Determination of Biofilm Formation \n from Co-Isolated Individuals")),
    expand = c(0,0))+
  scale_x_discrete(
    name= "Selective Substrate",
    breaks=c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q"),
    labels=c("Ancestor", " ", " ", "Glass", " ", " ", " ", "PVC", " ", " ", " ", "Stainless Steel", " ", " ", " ", "Planktonic", " "))+
  scale_fill_manual(
    name="transfer",
    breaks=c("Ancestor", "0", "10","20","30"),
    labels=c("Ancestor", "NA", "Transfer 10", "Transfer 20", "Transfer 30"),
    values =  c("white", "NA", "#dbd40b", "#ff6b00", "#cf0d00"))
dev.off()