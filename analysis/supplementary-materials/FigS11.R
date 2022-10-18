require(here)
require(tidyverse)
require(patchwork)

shapes <- c("Luzon Arc"=21,"Sulawesi Arc"=22,"Sunda Arc"=23,"Banda Arc"=24,
            "Yap Arc"=25,"Mariana Arc"=21,"Bismarck Arc"=22,"Solomon Arc"=23,
            "Vanuatu Arc"=24,"Tonga-Fiji"=25,"New Zealand"=21,
            "E-11-03"=0,"E-11-06"=1,"E-11-07"=8,"E-11-10"=0,"E-11-11"=1,
            "E-11-13"=2,"E-11-16"=5,"E-11-18"=6,"E-11-19"=8,"K-12-28"=3,"K-12-29"=4)
cols <- c("Luzon Arc"="#440154","Sulawesi Arc"="#345E8C","Sunda Arc"="#404386",
          "Banda Arc"="#472374","Yap Arc"="#29778E","Mariana Arc"="#218F8B",
          "Bismarck Arc"="#25A782","Solomon Arc"="#44BE6F","Vanuatu Arc"="#7AD04F",
          "Tonga-Fiji"="#BADD26","New Zealand"="#FDE725",
          "E-11-03"="red","E-11-06"="red","E-11-07"="red","E-11-10"="#7AD04F",
          "E-11-11"="#7AD04F","E-11-13"="#7AD04F","E-11-16"="#7AD04F",
          "E-11-18"="#7AD04F","E-11-19"="#7AD04F","K-12-28"="red","K-12-29"="red")
contour <- c("Luzon Arc"="black","Sulawesi Arc"="black","Sunda Arc"="black",
             "Banda Arc"="black","Yap Arc"="black","Mariana Arc"="black",
             "Bismarck Arc"="black","Solomon Arc"="black","Vanuatu Arc"="black",
             "Tonga-Fiji"="black","New Zealand"="black",
             "E-11-03"="red","E-11-06"="red","E-11-07"="red","E-11-10"="#7AD04F",
             "E-11-11"="#7AD04F","E-11-13"="#7AD04F","E-11-16"="#7AD04F",
             "E-11-18"="#7AD04F","E-11-19"="#7AD04F","K-12-28"="red","K-12-29"="red")

dir.create(here("analysis","supplementary-materials","FigS11"))

#### Fig S11 A ####
IAB <- q20
s <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18",
  "E-11-03","E-11-06","E-11-07","K-12-28","K-12-29")) %>%
  mutate(Location = Sample) %>% dplyr::select(
      Sample,Location,Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

A <- IAB %>%
  ggplot(aes(x=Sr87_Sr86,y=Nd143_Nd144, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(IAB, Location %in% c("Luzon Arc")), size=3, stroke=.25) +
  geom_point(data=subset(IAB, Location %in% c("Vanuatu Arc")), size=3, stroke=.25) +
  geom_point(data=subset(IAB, Location %in% c("Bismarck Arc")), size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(.703,.709)) + scale_y_continuous(limits=c(.5125,.51313)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 18), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^87*"Sr / "*{}^86*"Sr"),
       y=expression({}^143*"Nd / "*{}^144*"Nd"))
A
pdf(here("analysis","supplementary-materials","FigS11","FigS11-A.pdf"), width=8, height=8)
A
dev.off()

A_detail <- IAB %>%
  ggplot(aes(x=Sr87_Sr86,y=Nd143_Nd144, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(IAB, Location %in% c("Luzon Arc")), size=3, stroke=.25) +
  geom_point(data=subset(IAB, Location %in% c("Vanuatu Arc")), size=3, stroke=.25) +
  geom_point(data=subset(IAB, Location %in% c("Bismarck Arc")), size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(position = "top", limits=c(.7033,.705), expand=c(0,0)) +
  scale_y_continuous(position = "right", limits=c(.51285,.5131), expand=c(0,0)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_blank(), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1)
A_detail
pdf(here("analysis","supplementary-materials","FigS11","FigS11-A_detail.pdf"), width=6, height=6)
A_detail
dev.off()

B <- IAB %>%
  ggplot(aes(x=Pb206_Pb204,y=Pb207_Pb204, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(IAB, Location %in% c("Luzon Arc")), size=3, stroke=.25) +
  geom_point(data=subset(IAB, Location %in% c("Bismarck Arc")), size=3, stroke=.25) +
  geom_point(data=subset(IAB, Location %in% c("Vanuatu Arc")), size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(17.8,19.2)) +
  scale_y_continuous(position = "right", limits=c(15.48,15.8)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 18), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^206*"Pb / "*{}^204*"Pb"),
       y=expression({}^207*"Pb / "*{}^204*"Pb"))
B
pdf(here("analysis","supplementary-materials","FigS11","FigS11-B.pdf"), width=8, height=8)
B
dev.off()

B_detail <- IAB %>%
  ggplot(aes(x=Pb206_Pb204,y=Pb207_Pb204, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(IAB, Location %in% c("Luzon Arc")), size=3, stroke=.25) +
  geom_point(data=subset(IAB, Location %in% c("Vanuatu Arc")), size=3, stroke=.25) +
  geom_point(data=subset(IAB, Location %in% c("Bismarck Arc")), size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(position = "top", limits=c(18.1,18.8), expand=c(0,0)) +
  scale_y_continuous(limits=c(15.48,15.62), expand=c(0,0)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_blank(), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1)
B_detail
pdf(here("analysis","supplementary-materials","FigS11","FigS11-B_detail.pdf"), width=6, height=6)
B_detail
dev.off()
