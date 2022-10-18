require(here)
require(tidyverse)
require(RSQLite)
require(patchwork)

shapes <- c("Luzon Arc"=21,"Sulawesi Arc"=22,"Sunda Arc"=23,"Banda Arc"=24,
            "Yap Arc"=25,"Mariana Arc"=21,"Bismarck Arc"=22,"Solomon Arc"=23,
            "Vanuatu Arc"=24,"Tonga-Fiji"=25,"New Zealand"=21,
            "E-11-03"=0,"E-11-06"=1,"E-11-07"=8,"E-11-10"=24,"E-11-11"=24,
            "E-11-13"=24,"E-11-16"=24,"E-11-18"=24,"E-11-19"=24,
            "K-12-28"=3,"K-12-29"=4)
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
             "E-11-03"="red","E-11-06"="red","E-11-07"="red","E-11-10"="black",
             "E-11-11"="black","E-11-13"="black","E-11-16"="black",
             "E-11-18"="black","E-11-19"="black","K-12-28"="red","K-12-29"="red")

#### Fig S12a ####
IAB <- full_join(q2,q3)
s <- joined_data %>%
  dplyr::filter(Sample %in% c("E-11-03")) %>%
  dplyr::mutate(Location = Sample)

p1 <- IAB %>%
  ggplot(aes(x=Rb,y=Th/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(30,170)) +
  scale_y_continuous(limits=c(0,12)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.title.x = element_blank(),
        axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1)
p2 <- IAB %>%
  ggplot(aes(x=Sr,y=Th/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(0,.35)) +
  scale_y_continuous(limits=c(0,12)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_blank(), axis.text = element_text(size = 8),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)
p3 <- IAB %>%
  ggplot(aes(x=Zr,y=Th/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(80,350)) +
  scale_y_continuous(limits=c(0,12)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_blank(), axis.text = element_text(size = 8),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)
S12a <- p1|p2|p3
S12a

#### Fig S12b ####
IAB <- q5
s <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-06","E-11-07")) %>%
  dplyr::mutate(Location = Sample) %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
IAB <- full_join(IAB,s[3:7,])
s <- s[1:2,]

p4 <- IAB %>%
  ggplot(aes(x=Rb,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0,55)) + scale_y_continuous(limits=c(0,400)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.title.x = element_blank(),
        axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1)
p5 <- IAB %>%
  ggplot(aes(x=Sr,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0,1500)) + scale_y_continuous(limits=c(0,400)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_blank(), axis.text = element_text(size = 8),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)
p6 <- IAB %>%
  ggplot(aes(x=Zr,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0,150)) + scale_y_continuous(limits=c(0,400)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_blank(), axis.text = element_text(size = 8),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)
S12b <- p4|p5|p6
S12b

#### Fig S12c ####
IAB <- q7
s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-28")) %>%
  dplyr::mutate(Location = Sample)

p7 <- IAB %>%
  ggplot(aes(x=Rb,y=U/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(IAB, Location %in% c("Bismarck Arc")), size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0,45)) + scale_y_continuous(limits=c(0,.8)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.title.x = element_blank(),
        axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1)
p8 <- IAB %>%
  ggplot(aes(x=Sr,y=U/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(IAB, Location %in% c("Bismarck Arc")), size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0,1000)) + scale_y_continuous(limits=c(0,.8)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_blank(), axis.text = element_text(size = 8),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)
p9 <- IAB %>%
  ggplot(aes(x=Zr,y=U/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(IAB, Location %in% c("Bismarck Arc")), size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0,150)) + scale_y_continuous(limits=c(0,.8)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_blank(), axis.text = element_text(size = 8),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)
S12c <- p7|p8|p9
S12c

#### Fig S12d ####
IAB <- q9
s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-29")) %>%
  mutate(Location = Sample) %>%
  dplyr::select(Sample,Location,lat,long,SiO2,TiO2,Al2O3,MnO,MgO,CaO,Na2O,K2O,
                Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
                Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

p7 <- IAB %>%
  ggplot(aes(x=Rb,y=U/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(IAB, Location %in% c("Vanuatu Arc")), size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0,60)) + scale_y_continuous(limits=c(0,1)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x="Rb (ppm)")
p8 <- IAB %>%
  ggplot(aes(x=Sr,y=U/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(IAB, Location %in% c("Vanuatu Arc")), size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0,1200)) + scale_y_continuous(limits=c(0,1)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), legend.position = "none", aspect.ratio=1) +
  labs(x="Sr (ppm)")
p9 <- IAB %>%
  ggplot(aes(x=Zr,y=U/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(IAB, Location %in% c("Vanuatu Arc")), size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0,150)) + scale_y_continuous(limits=c(0,1)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.ticks.y = element_blank(), legend.position = "none", aspect.ratio=1) +
  labs(x="Zr (ppm)")
S12d <- p7|p8|p9
S12d

pdf(here("analysis","supplementary-materials","FigS12.pdf"), width=8, height=10)
S12a/S12b/S12c/S12d
dev.off()
