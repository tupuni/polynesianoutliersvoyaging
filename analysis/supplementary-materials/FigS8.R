require(here)
require(tidyverse)
require(patchwork)

#### plots ####
shapes <- c("E-11-03"=0,"E-11-06"=1,"E-11-07"=8,"E-11-10"=0,"E-11-11"=1,
            "E-11-13"=2,"E-11-16"=5,"E-11-18"=6,"E-11-19"=8,"K-12-28"=3,"K-12-29"=4,
            "E-11-08"=5,"E-11-08dup"=9,"T-12-06"=2,"T-12-06dup"=14,
            "T-12-07"=7,"T-12-08"=6,"T-12-09"=10,"T-12-10"=11,"K-12-24"=12,
            "K-12-25"=13,"K-12-26"=14)
cols <- c("E-11-03"="red","E-11-06"="red","E-11-07"="red","E-11-10"="#7AD04F",
          "E-11-11"="#7AD04F","E-11-13"="#7AD04F","E-11-16"="#7AD04F",
          "E-11-18"="#7AD04F","E-11-19"="#7AD04F","K-12-28"="red","K-12-29"="red",
          "E-11-08"="red","E-11-08dup"="red","T-12-06"="red","T-12-06dup"="red",
          "T-12-07"="red","T-12-08"="red","T-12-09"="red",
          "T-12-10"="red","K-12-24"="red","K-12-25"="red","K-12-26"="red")


## Figure S8
df = data.frame(x = c(43,78), y = c(0,4))
theme_set(theme_bw(base_size=14))
k2o_sio2 <- ggplot(data=df, mapping=aes(x=x, y=y)) + geom_blank() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(limits=c(0,4.7), breaks = c(1,2,3,4)) +
  scale_x_continuous(limits=c(43,78), breaks = c(45,52,56,63,70,78)) +
  labs(y=expression(K[2]*O*~ "(wt%)"), x=expression(SiO[2]*~ "(wt%)"))+
  annotate("segment", x=52, xend=52, y=0, yend=4.7, size=0.2, linetype="dashed", colour="gray70")+
  annotate("segment", x=56, xend=56, y=0, yend=4.7, size=0.2, linetype="dashed", colour="gray70")+
  annotate("segment", x=63, xend=63, y=0, yend=4.7, size=0.2, linetype="dashed", colour="gray70")+
  annotate("segment", x=70, xend=70, y=0, yend=4.7, size=0.2, linetype="dashed", colour="gray70")+
  annotate("segment", x=48, xend=52, y=0.3, yend=0.5, size=0.2, colour="gray70")+
  annotate("segment", x=52, xend=56, y=0.5, yend=0.7, size=0.2, colour="gray70")+
  annotate("segment", x=56, xend=63, y=0.7, yend=1, size=0.2, colour="gray70")+
  annotate("segment", x=63, xend=70, y=1, yend=1.3, size=0.2, colour="gray70")+
  annotate("segment", x=70, xend=78, y=1.3, yend=1.6, size=0.2, colour="gray70")+
  annotate("segment", x=48, xend=52, y=1.2, yend=1.5, size=0.2, colour="gray70")+
  annotate("segment", x=52, xend=56, y=1.5, yend=1.8, size=0.2, colour="gray70")+
  annotate("segment", x=56, xend=63, y=1.8, yend=2.4, size=0.2, colour="gray70")+
  annotate("segment", x=63, xend=70, y=2.4, yend=3, size=0.2, colour="gray70")+
  annotate("segment", x=70, xend=78, y=3, yend=3.7, size=0.2, colour="gray70")+
  annotate("segment", x=48, xend=52, y=1.7, yend=2.4, size=0.2, colour="gray70")+
  annotate("segment", x=52, xend=56, y=2.4, yend=3.2, size=0.2, colour="gray70")+
  annotate("segment", x=56, xend=63, y=3.2, yend=4, size=0.2, colour="gray70")+
  annotate("segment", x=45, xend=48, y=1.4, yend=1.7, size=0.2, colour="gray70")+
  annotate("segment", x=45, xend=48, y=0.9, yend=1.2, size=0.2, colour="gray70")+
  annotate("segment", x=45, xend=48, y=0.2, yend=0.3, size=0.2, colour="gray70")+
  annotate("text", label="Basalt", x=47.5, y=4.5, size=2.5, colour="black")+
  annotate("text", label="Basaltic\n andesite", x=54, y=4.5, size=2.5, colour="black")+
  annotate("text", label="Andesite", x=59.5, y=4.5, size=2.5, colour="black")+
  annotate("text", label="Dacite", x=66.5, y=4.5, size=2.5, colour="black")+
  annotate("text", label="Rhyolite", x=74, y=4.5, size=2.5, colour="black")+
  annotate("text", label="Alkaline series", x=51, y=3.3, size=2.5, colour="gray50", angle = 50)+
  annotate("text", label="High-K calc-alkaline series", x=59.5, y=2.8, size=2.5, colour="gray50", angle = 35)+
  annotate("text", label="Calc-alkaline series", x=63, y=1.7, size=2.5, colour="gray50", angle = 17)+
  annotate("text", label="Low-K series", x=65, y=0.6, size=2.5, colour="gray50", angle = 4)

#filter iab samples
oxides_iab <- oxides %>%
  dplyr::filter(Sample %in% c(
    "E-11-03", "E-11-06", "E-11-07", "E-11-10", "E-11-11", "E-11-13", "E-11-16",
    "E-11-18", "E-11-19", "K-12-28", "K-12-29"))
#plot data
p1 <- k2o_sio2 +
  geom_point(data=oxides_iab,
             aes(x=SiO2, y=K2O, color=factor(Sample), shape=factor(Sample)),
             size=3) +
  scale_shape_manual(values = shapes) + scale_color_manual(values = cols) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 9),
        legend.title = element_text(size = 0), legend.text = element_text(size = 9),
        legend.position="none", aspect.ratio=1)
p1

#make TAS template
df = data.frame(x = c(40,77), y = c(0,14))
theme_set(theme_bw(base_size=14))
tas <- ggplot(data=df, mapping=aes(x=x, y=y)) +
  geom_blank() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_y_continuous(limits=c(0.5,10), breaks = c(2,4,6,8,10), expand = c(0, 0)) +
  scale_x_continuous(limits=c(37,57), breaks = c(41,45,49,53,57), expand = c(0, 0)) +
  labs(y=expression(Na[2]*O + K[2]*O*~ "(wt%)"), x=expression(SiO[2]*~ "(wt%)"))+
  annotate("segment", x=45, xend=45, y=1, yend=5, size = 0.2, colour = "gray50")+
  annotate("segment", x=45, xend=52, y=5, yend=5, size = 0.2, colour = "gray50")+
  annotate("segment", x=52, xend=57, y=5, yend=5.9, size = 0.2, colour = "gray50")+
  annotate("segment", x=45, xend=49.4, y=5, yend= 7.3, size = 0.2, colour = "gray50")+
  annotate("segment", x=49.4, xend=53, y=7.3, yend=9.3, size = 0.2, colour = "gray50")+
  annotate("segment", x=52, xend=69, y=5, yend=8, size = 0.2, colour = "gray50")+
  annotate("segment", x=76.5, xend=69, y=1, yend=8, size = 0.2, colour = "gray50")+
  annotate("segment", x=69, xend=69, y=8, yend=13, size = 0.2, colour = "gray50")+
  annotate("segment", x=45, xend=61.32, y=5, yend=13.7, size = 0.2, colour = "gray50")+
  annotate("segment", x=52, xend=52, y=1, yend=5, size = 0.2, colour = "gray50")+
  annotate("segment", x=57, xend=57, y=1, yend=5.9, size = 0.2, colour = "gray50")+
  annotate("segment", x=63, xend=63, y=1, yend=6.9, size = 0.2, colour = "gray50")+
  annotate("segment", x=52, xend=49.4, y=5, yend=7.3, size = 0.2, colour = "gray50")+
  annotate("segment", x=57, xend=53.05, y=5.9, yend=9.25, size = 0.2, colour = "gray50")+
  annotate("segment", x=63, xend=57.6, y=6.9, yend=11.7, size = 0.2, colour = "gray50")+
  annotate("segment", x=41, xend=45, y=3, yend=3, size = 0.2, colour = "gray50")+
  annotate("segment", x=41, xend=41, y=1, yend=3, size = 0.2, colour = "gray50")+
  annotate("segment", x=41, xend=41, y=3, yend=7, size = 0.2, colour = "gray50")+
  annotate("segment", x=41, xend=45, y=7, yend=9.4, size = 0.2, colour = "gray50")+
  annotate("segment", x=45, xend=52.5, y=9.4, yend=14, size = 0.2, colour = "gray50")+
  annotate("segment", x=49.4, xend=45, y=7.3, yend=9.4, size = 0.2, colour = "gray50")+
  annotate("segment", x=53, xend=48.4, y=9.3, yend=11.5, size = 0.2, colour = "gray50")+
  annotate("segment", x=57.6, xend=52.5, y=11.7, yend=14, size = 0.2, colour = "gray50")+
  annotate("text", label = "Alkali basalt", x = 48.5, y = 3.7, size=2.5, colour = "black")+
  annotate("text", label = "Basaltic\n andesite", x = 54.2, y = 3, size=2.5, colour = "gray80")+
  annotate("text", label = "Andesite", x = 60, y = 3.5, size=2.5, colour = "gray80")+
  annotate("text", label = "Dacite", x = 67.5, y = 4.2, size=2.5, colour = "gray80")+
  annotate("text", label = "Rhyolite", x = 73, y = 9, size=2.5, colour = "black")+
  annotate("text", label = "Trachybasalt", x = 48.8, y = 5.8, size=2.5, colour = "black")+
  annotate("text", label = "Basaltic\n trachyandesite", x = 52.9, y = 7, size=2.5, colour = "gray80")+
  annotate("text", label = "Trachyandesite", x = 57.8, y = 8.2, size=2.5, colour = "gray80")+
  annotate("text", label = "Trachyte", x = 62.5, y = 11.5, size=2.5, colour = "gray80")+
  annotate("text", label = "Trachydacite", x = 65, y = 9, size=2.5, colour = "gray80")+
  annotate("text", label = "Picrobasalt", x = 42.9, y = 2, size=2.5, colour = "gray80")+
  annotate("text", label = "Tephrite\n (Ol < 10%)\n or\n Basanite\n (Ol > 10%)",
           x = 44, y = 6.6, size=2.5, colour = "black")+
  annotate("text", label = "Phonotephrite", x = 48.9, y = 8.5, size=2.5, colour = "gray80")+
  annotate("text", label = "Tephriphonolite", x = 53, y = 11.5, size=2.5, colour = "gray80")+
  annotate("text", label = "Phonolite", x = 57, y = 13.3, size=2.5, colour = "gray80")+
  annotate("text", label = "Foidite", x = 41.5, y = 8.5, size=2.5, colour = "gray80")
# filter oib samples
oxides_oib <- oxides %>%
  filter(Sample %in% c("K-12-24", "K-12-25", "K-12-26","E-11-08", "T-12-06",
                       "T-12-07","T-12-08", "T-12-09", "T-12-10"))
#plot TAS
p2 <- tas + geom_point(data=oxides_oib,
                       aes(x=SiO2, y=Na2O+K2O, color=factor(Sample),
                           shape=factor(Sample)), size=3) +
  scale_shape_manual(values = shapes) + scale_color_manual(values = cols) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 12), axis.text.x = element_text(size = 9),
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 9),
        legend.title = element_text(size = 0), legend.text = element_text(size = 9),
        legend.position="none", aspect.ratio=1)
p2
#save
pdf(here("analysis","supplementary-materials","FigS8.pdf"), width=4, height=8)
(p1 / p2)
dev.off()

