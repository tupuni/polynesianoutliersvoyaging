require(here)
require(tidyverse)
require(patchwork)

shapes <- c("Caroline islands"=21,"Samoan islands"=24,"Austral-Cook chain"=23,
            "Society islands"=22,"Hawai'i islands"=25,"Marquesas islands"=21,
            "Pitcairn-Gambier chain"=21,"North Fiji Basin"=25,"Rotuma"=21,
            "Futuna"=22,"Cikobia"=23,"Uvea"=24,"Caroline plateau"=21,
            "E-11-08"=5,"E-11-08dup"=9,"T-12-06"=2,"T-12-06dup"=14,
            "T-12-07"=7,"T-12-08"=6,"T-12-09"=10,"T-12-10"=11,"K-12-24"=12,
            "K-12-25"=13,"K-12-26"=14)
cols <- c("Caroline islands"="#320A5A","Samoan islands"="#781B6C",
          "Austral-Cook chain"="#BB3654","Society islands"="#EC6824",
          "Marquesas islands"="#FBB41A","Hawai'i islands"="#F4DD53",
          "Pitcairn-Gambier chain"="#C96FB6","North Fiji Basin"="#B4C630",
          "Rotuma"="#6EA002","Futuna"="#6EA002","Cikobia"="#6EA002",
          "Uvea"="#6EA002","Caroline plateau"="#8D50D3",
          "E-11-08"="red","E-11-08dup"="red","T-12-06"="red","T-12-06dup"="red",
          "T-12-07"="red","T-12-08"="red","T-12-09"="red",
          "T-12-10"="red","K-12-24"="red","K-12-25"="red","K-12-26"="red")
contour <- c("Caroline islands"="black","Samoan islands"="black",
             "Austral-Cook chain"="black","Society islands"="black",
             "Hawai'i islands"="black","Marquesas islands"="black",
             "Pitcairn-Gambier chain"="black","North Fiji Basin"="black",
             "Rotuma"="black","Futuna"="black","Cikobia"="black","Uvea"="black",
             "Caroline plateau"="black",
             "E-11-08"="red","E-11-08dup"="red","T-12-06"="red","T-12-06dup"="red",
             "T-12-07"="red","T-12-08"="red","T-12-09"="red",
             "T-12-10"="red","K-12-24"="red","K-12-25"="red","K-12-26"="red")

#### Fig S16a ####
ranges_s_OIB[1:6,1:5]
OIB <- full_join(q30,q31)
OIB[OIB == 0] <- NA # Replace 0 with NA

s <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-08","T-12-06","T-12-07","T-12-08","T-12-09","T-12-10")) %>%
  mutate(Location = Sample)

cols <- c("Savai'i"="#B262A7","Upolu"="#C77FB6","Manua Islands"="#94318E",
          "Tutuila"="#77246C","E-11-08"="red","T-12-06"="red","T-12-07"="red",
          "T-12-08"="red","T-12-09"="red","T-12-10"="red")
shapes <- c("Savai'i"=23,"Upolu"=22,"Manua Islands"=25,"Tutuila"=21,
            "E-11-08"=21,"T-12-06"=24,"T-12-07"=14,"T-12-08"=25,"T-12-09"=9,"T-12-10"=23)
contour <- c("Savai'i"="black","Upolu"="black","Manua Islands"="black",
             "Tutuila"="black","E-11-08"="black","T-12-06"="black","T-12-07"="red",
             "T-12-08"="black","T-12-09"="red","T-12-10"="black")

p1 <- OIB %>%
  ggplot(aes(x=Rb,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(10,70)) +
  scale_y_continuous(limits=c(70,200)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.title.x = element_blank(),
        axis.text = element_text(size = 8), legend.position = "none", aspect.ratio=1)
p2 <- OIB %>%
  ggplot(aes(x=Sr,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(320,950)) +
  scale_y_continuous(limits=c(70,200)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),legend.position = "none", aspect.ratio=1)
p3 <- OIB %>%
  ggplot(aes(x=Zr,y=Ba/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(150,500)) +
  scale_y_continuous(limits=c(70,200)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),legend.position = "none", aspect.ratio=1)
S16a <- p1|p2|p3
S16a

#### Fig S16b ####
ranges_s_OIB[7,]
OIB <- q32
OIB %>% group_by(Location) %>% tally()
OIB[OIB == 0] <- NA # Replace 0 with NA

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-24")) %>%
  dplyr::mutate(Location = Sample)

cols <- c("Chuuk"="#6D58A5","Kosrae"="#8476B6","Ponape"="#341D59","K-12-24"="red")
shapes <- c("Chuuk"=23,"Kosrae"=22,"Ponape"=21,"K-12-24"=12)
contour <- c("Chuuk"="black","Kosrae"="black","Ponape"="black","K-12-24"="red")

p4 <- OIB %>%
  ggplot(aes(x=Rb,y=Ba/Y, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(0,55)) +
  scale_y_continuous(limits=c(1,30)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.x = element_blank(), legend.position = "none", aspect.ratio=1)
p5 <- OIB %>%
  ggplot(aes(x=Sr,y=Ba/Y, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(0,750)) +
  scale_y_continuous(limits=c(1,30)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)
p6 <- OIB %>%
  ggplot(aes(x=Zr,y=Ba/Y, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(0,750)) +
  scale_y_continuous(limits=c(1,30)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(),axis.ticks.y = element_blank(),
        legend.position = "none", aspect.ratio=1)
S16b <- p4|p5|p6
S16b

#### Fig S16c ####
ranges_s_OIB[8,]
OIB <- q33
OIB %>% group_by(Location) %>% tally()

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-25")) %>%
  dplyr::mutate(Location = Sample)

shapes <- c("Chuuk"=23,"Kosrae"=22,"Ponape"=21,"K-12-25"=13)
cols <- c("Chuuk"="#6D58A5","Kosrae"="#8476B6","Ponape"="#341D59","K-12-25"="red")
contour <- c("Chuuk"="black","Kosrae"="black","Ponape"="black","K-12-25"="red")

p7 <- OIB %>%
  ggplot(aes(x=Rb,y=La/Y, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(0,50)) +
  #scale_y_continuous(limits=c(0,40)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.x = element_blank(),legend.position = "none", aspect.ratio=1) +
  labs(x="Rb (ppm)")
p8 <- OIB %>%
  ggplot(aes(x=Sr,y=La/Y, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(350,900)) +
  #scale_y_continuous(limits=c(0,40)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), legend.position = "none",
        axis.title.x = element_blank(),aspect.ratio=1) +
  labs(x="Sr (ppm)")
p9 <- OIB %>%
  ggplot(aes(x=Zr,y=La/Y, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(0,350)) +
  #scale_y_continuous(limits=c(0,40)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), legend.position = "none",
        axis.title.x = element_blank(),aspect.ratio=1) +
  labs(x="Zr (ppm)")

S16c <- p7|p8|p9
S16c

#### Fig S16d ####
ranges_s_OIB[9,]
OIB <- q34
OIB %>% group_by(Location) %>% tally()

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-26")) %>%
  dplyr::mutate(Location = Sample)

shapes <- c("Chuuk"=23,"Kosrae"=22,"Ponape"=21,"K-12-26"=14)
cols <- c("Chuuk"="#6D58A5","Kosrae"="#8476B6","Ponape"="#341D59","K-12-26"="red")
contour <- c("Chuuk"="black","Kosrae"="black","Ponape"="black","K-12-26"="red")

p10 <- OIB %>%
  ggplot(aes(x=Rb,y=La/Y, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(0,55)) +
  #scale_y_continuous(limits=c(0,300)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x="Rb (ppm)")
p11 <- OIB %>%
  ggplot(aes(x=Sr,y=La/Y, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  #scale_x_continuous(limits=c(0,750)) +
  #scale_y_continuous(limits=c(0,300)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),legend.position = "none", aspect.ratio=1) +
  labs(x="Sr (ppm)")
p12 <- OIB %>%
  ggplot(aes(x=Zr,y=La/Y, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  # scale_x_continuous(limits=c(0,750)) +
  #scale_y_continuous(limits=c(0,300)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),legend.position = "none", aspect.ratio=1) +
  labs(x="Zr (ppm)")
S16d <- p10|p11|p12
S16d

pdf(here("analysis","supplementary-materials","FigS16.pdf"), width=8, height=10)
S16a/S16b/S16c/S16d
dev.off()
