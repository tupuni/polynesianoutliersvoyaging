require(here)
require(tidyverse)
require(RSQLite)
require(patchwork)
require(stats)
require(FactoMineR)
require(factoextra)
library(ggridges)

georoc <- dbConnect(RSQLite::SQLite(), path_to_georoc)
pofatu <- dbConnect(RSQLite::SQLite(), path_to_pofatu)

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

#### Fig S12a ####
IAB <- full_join(q2,q3)
s <- joined_data %>%
  filter(Sample %in% c("E-11-03")) %>%
  mutate(Location = case_when(grepl("E-11-03", Sample) ~ "E-11-03"))

p1 <- IAB %>%
  ggplot(aes(x=Nb/La,y=Sr/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0,1.5)) + scale_y_continuous(limits=c(0,500)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.title.x = element_blank(),
        axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1)
p2 <- IAB %>%
  ggplot(aes(x=Ba/Yb,y=Sr/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0,1000)) + scale_y_continuous(limits=c(0,500)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),legend.position = "none", aspect.ratio=1) #+
S12a <- p1|p2

#### Fig S12b ####
IAB <- q5
s <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-06","E-11-07")) %>%
  mutate(Location = case_when(
    grepl("E-11-10", Sample) ~ "E-11-10",
    grepl("E-11-11", Sample) ~ "E-11-11",
    grepl("E-11-13", Sample) ~ "E-11-13",
    grepl("E-11-16", Sample) ~ "E-11-16",
    grepl("E-11-18", Sample) ~ "E-11-18",
    grepl("E-11-06", Sample) ~ "E-11-06",
    grepl("E-11-07", Sample) ~ "E-11-07"))
p3 <- IAB %>%
  ggplot(aes(x=Nb/La,y=Sr/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0,1)) + scale_y_continuous(limits=c(0,1000)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.x = element_blank(),
        legend.position = "none", aspect.ratio=1)
p4 <- IAB %>%
  ggplot(aes(x=Ba/Yb,y=Sr/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0,500)) + scale_y_continuous(limits=c(0,1000)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),legend.position = "none", aspect.ratio=1) #+
S12b <- p3|p4

#### Fig S12c ####
IAB <- q7
s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-28")) %>%
  mutate(Location = case_when(grepl("K-12-28", Sample) ~ "K-12-28"))
p5 <- IAB %>%
  ggplot(aes(x=Nb/La,y=Sr/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0,1)) + scale_y_continuous(limits=c(0,500)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.x = element_blank(),
        legend.position = "none", aspect.ratio=1)
p6 <- IAB %>%
  ggplot(aes(x=Ba/Yb,y=Sr/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0,300)) + scale_y_continuous(limits=c(0,500)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),legend.position = "none", aspect.ratio=1) #+
S12c <- p5|p6
S12c

#### Fig S12d ####
IAB <- q9
s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-29")) %>%
  mutate(Location = case_when(grepl("K-12-29", Sample) ~ "K-12-29"))
p7 <- IAB %>%
  ggplot(aes(x=Nb/La,y=Sr/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0,1)) + scale_y_continuous(limits=c(100,500)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1)
p8 <- IAB %>%
  ggplot(aes(x=Ba/Yb,y=Sr/Yb, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) + geom_point(data=s, size=3) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(0,300)) + scale_y_continuous(limits=c(100,500)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 14), axis.text = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),legend.position = "none", aspect.ratio=1) #+
S12d <- p7|p8
S12d

pdf(here("analysis","supplementary-materials","FigS12.pdf"), width=8, height=10)
S12a/S12b/S12c/S12d
dev.off()
