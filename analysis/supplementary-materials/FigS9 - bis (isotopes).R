require(here)
require(tidyverse)
require(patchwork)

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
contour <- c("E-11-03"="red","E-11-06"="red","E-11-07"="red","E-11-10"="#7AD04F",
             "E-11-11"="#7AD04F","E-11-13"="#7AD04F","E-11-16"="#7AD04F",
             "E-11-18"="#7AD04F","E-11-19"="#7AD04F","K-12-28"="red","K-12-29"="red",
             "E-11-08"="red","E-11-08dup"="red","T-12-06"="red","T-12-06dup"="red",
             "T-12-07"="red","T-12-08"="red","T-12-09"="red",
             "T-12-10"="red","K-12-24"="red","K-12-25"="red","K-12-26"="red")

shapes <- c("E-11-03"=22,"E-11-06"=1,"E-11-07"=8,"E-11-10"=23,"E-11-11"=1,
            "E-11-13"=25,"E-11-16"=24,"E-11-18"=6,"E-11-19"=21,"K-12-28"=3,"K-12-29"=4,
            "E-11-08"=21,"E-11-08dup"=9,"T-12-06"=24,"T-12-06dup"=14,
            "T-12-07"=7,"T-12-08"=25,"T-12-09"=10,"T-12-10"=23,"K-12-24"=12,
            "K-12-25"=13,"K-12-26"=14)
cols <- c("E-11-03"="red","E-11-06"="red","E-11-07"="red","E-11-10"="#7AD04F",
          "E-11-11"="#7AD04F","E-11-13"="#7AD04F","E-11-16"="#7AD04F",
          "E-11-18"="#7AD04F","E-11-19"="#7AD04F","K-12-28"="red","K-12-29"="red",
          "E-11-08"="red","E-11-08dup"="red","T-12-06"="red","T-12-06dup"="red",
          "T-12-07"="red","T-12-08"="red","T-12-09"="red",
          "T-12-10"="red","K-12-24"="red","K-12-25"="red","K-12-26"="red")
contour <- c("E-11-03"="black","E-11-06"="red","E-11-07"="red","E-11-10"="black",
             "E-11-11"="#7AD04F","E-11-13"="black","E-11-16"="black",
             "E-11-18"="#7AD04F","E-11-19"="black","K-12-28"="red","K-12-29"="red",
             "E-11-08"="black","E-11-08dup"="red","T-12-06"="black","T-12-06dup"="red",
             "T-12-07"="red","T-12-08"="black","T-12-09"="red",
             "T-12-10"="black","K-12-24"="red","K-12-25"="red","K-12-26"="red")

s <- joined_data

p1 <- s %>%
  ggplot(aes(x=Sr87_Sr86, y=Nd143_Nd144, shape=factor(Sample), fill=factor(Sample),
             color=factor(Sample), group=Sample)) +
  geom_point(size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0.7032, 0.7055), breaks=c(.704,.705)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 12), axis.text = element_text(size = 10),
        axis.title.x = element_blank(), axis.text.x = element_blank(),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^87*"Sr/"*{}^86*"Sr"),
       y=expression({}^143*"Nd/"*{}^144*"Nd"))
p2 <- s %>%
  ggplot(aes(x=Sr87_Sr86, y=Pb206_Pb204, shape=factor(Sample), fill=factor(Sample),
             color=factor(Sample), group=Sample)) +
  geom_point(size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0.7032, 0.7055), breaks=c(.704,.705)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 12), axis.text = element_text(size = 10),
        axis.title.x = element_blank(), axis.text.x = element_blank(),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^87*"Sr/"*{}^86*"Sr"),
       y=expression({}^206*"Pb/"*{}^204*"Pb"))
p3 <- s %>%
  ggplot(aes(x=Sr87_Sr86, y=Pb207_Pb204, shape=factor(Sample), fill=factor(Sample),
             color=factor(Sample), group=Sample)) +
  geom_point(size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0.7032, 0.7055), breaks=c(.704,.705)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 12), axis.text = element_text(size = 10),
        axis.title.x = element_blank(), axis.text.x = element_blank(),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^87*"Sr/"*{}^86*"Sr"),
       y=expression({}^207*"Pb/"*{}^204*"Pb"))
p4 <- s %>%
  ggplot(aes(x=Sr87_Sr86, y=Pb208_Pb204, shape=factor(Sample), fill=factor(Sample),
             color=factor(Sample), group=Sample)) +
  geom_point(size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0.7032, 0.7055), breaks=c(.704,.705)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 12), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^87*"Sr/"*{}^86*"Sr"),
       y=expression({}^208*"Pb/"*{}^204*"Pb"))
p5 <- s %>%
  ggplot(aes(x=Nd143_Nd144, y=Pb206_Pb204, shape=factor(Sample), fill=factor(Sample),
             color=factor(Sample), group=Sample)) +
  geom_point(size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0.51285, .51303), breaks=c(0.5129,0.513)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_blank(), axis.text = element_blank(),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^143*"Nd/"*{}^144*"Nd"),
       y=expression({}^206*"Pb/"*{}^204*"Pb"))
p6 <- s %>%
  ggplot(aes(x=Nd143_Nd144, y=Pb207_Pb204, shape=factor(Sample), fill=factor(Sample),
             color=factor(Sample), group=Sample)) +
  geom_point(size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0.51285, .51303), breaks=c(0.5129,0.513)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_blank(), axis.text = element_blank(),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^143*"Nd/"*{}^144*"Nd"),
       y=expression({}^207*"Pb/"*{}^204*"Pb"))
p7 <- s %>%
  ggplot(aes(x=Nd143_Nd144, y=Pb208_Pb204, shape=factor(Sample), fill=factor(Sample),
             color=factor(Sample), group=Sample)) +
  geom_point(size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0.51285, .51303), breaks=c(0.5129,0.513)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 12), axis.text = element_text(size = 10),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^143*"Nd/"*{}^144*"Nd"),
       y=expression({}^208*"Pb/"*{}^204*"Pb"))
p8 <- s %>%
  ggplot(aes(x=Pb206_Pb204, y=Pb207_Pb204, shape=factor(Sample), fill=factor(Sample),
             color=factor(Sample), group=Sample)) +
  geom_point(size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(18.2, 18.95), breaks=c(18.4,18.8)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_blank(), axis.text = element_blank(),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^206*"Pb/"*{}^204*"Pb"),
       y=expression({}^207*"Pb/"*{}^204*"Pb"))
p9 <- s %>%
  ggplot(aes(x=Pb206_Pb204, y=Pb208_Pb204, shape=factor(Sample), fill=factor(Sample),
             color=factor(Sample), group=Sample)) +
  geom_point(size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(18.2, 18.95), breaks=c(18.4,18.8)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 12), axis.text = element_text(size = 10),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^206*"Pb/"*{}^204*"Pb"),
       y=expression({}^208*"Pb/"*{}^204*"Pb"))
p10 <- s %>%
  ggplot(aes(x=Pb207_Pb204, y=Pb208_Pb204, shape=factor(Sample), fill=factor(Sample),
             color=factor(Sample), group=Sample)) +
  geom_point(size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(15.52, 15.61), breaks=c(15.54,15.58)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 12), axis.text = element_text(size = 10),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^207*"Pb/"*{}^204*"Pb"),
       y=expression({}^208*"Pb/"*{}^204*"Pb"))

pdf(here("analysis","supplementary-materials","FigS9_isotopes.pdf"), width=8, height=8)
((p1/p2/p3/p4) | (plot_spacer()/p5/p6/p7) | (plot_spacer()/plot_spacer()/p8/p9) | (plot_spacer()/plot_spacer()/plot_spacer()/p10)) + guide_area()
dev.off()

