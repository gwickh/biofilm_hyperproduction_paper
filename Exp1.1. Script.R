tiff("data.tiff", units="cm", width=30, height=18, res=400)
ggplot(data) +
  geom_jitter(
    aes(x=substrate, y=OD595, fill=transfer, colour=grouping, shape = lineage),
    position=(position_dodge(width=1)),
    size =1.4,
    show.legend = FALSE,
    alpha=0.5,
    stroke=1) +
  geom_boxplot(
    aes(x=substrate, y=OD595, fill=transfer),
    outlier.alpha = 1, 
    alpha=0.3, 
    outlier.shape = NA,
    lwd=0.5,
    width=0.6,
    color= c(
      "black", "NA",
      "black", "NA", "black", "NA", "black", "NA", "NA",
      "black", "NA", "black", "NA", "black", "NA", "NA",
      "black", "NA", "black", "NA", "black", "NA", "NA",
      "black", "NA", "black", "NA", "black", "NA", "NA"), 
    position=position_dodge(width=1)) +
  stat_summary(
    aes(x=substrate, y=OD595, fill=errorbar, width = width*0.04),
    geom = "errorbar",
    fun.min = function(x) mean(x) + sd(x) / sqrt(length(x)),
    fun.max = function(x) mean(x) - sd(x) / sqrt(length(x)),
    colour = c(
      "NA", "black", 
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA"), 
    position=position_dodge(width=1))+
  stat_summary(aes(x=substrate, y=OD595, fill=transfer),
               fun=mean, 
               colour= c(
                 "black", "NA",
                 "black", "NA", "black", "NA", "black", "NA", "NA",
                 "black", "NA", "black", "NA", "black", "NA", "NA",
                 "black", "NA", "black", "NA", "black", "NA", "NA",
                 "black", "NA", "black", "NA", "black", "NA", "NA"),
               geom="point", 
               shape=3, 
               size=1.5, 
               stroke=1,
               show.legend=FALSE, 
               position=position_dodge(width=1)) +
  stat_summary(
    aes(x=substrate, y=OD595, fill=errorbar),
    fun=mean, 
    colour = c(
      "NA", "black",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA", "black", "black", "black", "black",
      "NA", "NA", "NA", "NA"),
    shape= c(
      16, 16,
      15,16,17,18,15,16,17,18,15,16,17,18,15,16,17,18,15,16,17,18,15,16,17,18,15,16,17,18,
      15,16,17,18,15,16,17,18,15,16,17,18,15,16,17,18,15,16,17,18,15,16,17,18,15,16,17,18,
      15,16,17,18,15,16,17,18,15,16,17,18,15,16,17,18,15,16,17,18,15,16,17,18,15,16,17,18,
      15,16,17,18,15,16,17,18,15,16,17,18,15,16,17,18,15,16,17,18,15,16,17,18,15,16,17,18),
    geom="point",
    size=1.75,
    stroke=1,
    show.legend=FALSE, 
    position=position_dodge(width=1))+
  coord_cartesian(ylim = c(0, 6))+
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
    panel.border = element_rect(colour = "black", fill=NA, size=0.75),
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
    panel.grid.major.x=element_blank(),
    panel.grid.minor.y = element_blank())+
  scale_x_discrete(
    name="Selective Substrate", 
    limits=c("ancestor","glass","pvc","steel","planktonic"),
    breaks=c("ancestor","glass","pvc","steel","planktonic"), 
    labels=c("Ancestor", "Glass", "PVC", "Stainless Steel", "Planktonic")) +
  scale_y_continuous(expression(
    paste("Biofilm Formation (OD "["595"]*")")),
    expand = c(0,0))+
  scale_fill_manual(
    name="transfer",
    breaks=c("10", "20", "30","Ancestor"),
    labels=c("Transfer 10", "Transfer 20", "Transfer 30", "Ancestor"),
    values = c(
      "#f2ec44", "NA", "#e35f00", "NA","#cf0d00","NA",
      "NA","black","NA","NA","NA","NA",
      "NA","NA","NA","NA","NA","NA", 
      "NA","NA","NA","NA","NA","NA",
      "NA","NA","NA","NA","NA","NA", 
      "NA","NA","NA","NA","NA","NA","NA"))+
  scale_color_manual(
    values =  c("#f2ec44", "NA", "#e35f00","NA", "#cf0d00", "NA", "NA", "NA", "black"))+
  scale_shape_manual(values = c(15, 16, 17, 18))
dev.off()