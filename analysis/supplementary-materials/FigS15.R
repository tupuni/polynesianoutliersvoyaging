require(here)
require(tidyverse)
require(patchwork)

shapes <- c("Caroline islands"=21,"Samoan islands"=24,"Austral-Cook chain"=23,
            "Society islands"=22,"Hawai'i islands"=25,"Marquesas islands"=21,
            "Pitcairn-Gambier chain"=21,
            "E-11-08"=5,"E-11-08dup"=9,"T-12-06"=2,"T-12-06dup"=14,
            "T-12-07"=7,"T-12-08"=6,"T-12-09"=10,"T-12-10"=11,"K-12-24"=12,
            "K-12-25"=13,"K-12-26"=14)
cols <- c("Caroline islands"="#320A5A","Samoan islands"="#781B6C",
          "Austral-Cook chain"="#BB3654","Society islands"="#EC6824",
          "Marquesas islands"="#FBB41A","Hawai'i islands"="#F4DD53",
          "Pitcairn-Gambier chain"="#C96FB6",
          "E-11-08"="red","E-11-08dup"="red","T-12-06"="red","T-12-06dup"="red",
          "T-12-07"="red","T-12-08"="red","T-12-09"="red",
          "T-12-10"="red","K-12-24"="red","K-12-25"="red","K-12-26"="red")
contour <- c("Caroline islands"="black","Samoan islands"="black",
             "Austral-Cook chain"="black","Society islands"="black",
             "Hawai'i islands"="black","Marquesas islands"="black",
             "Pitcairn-Gambier chain"="black",
             "E-11-08"="red","E-11-08dup"="red","T-12-06"="red","T-12-06dup"="red",
             "T-12-07"="red","T-12-08"="red","T-12-09"="red",
             "T-12-10"="red","K-12-24"="red","K-12-25"="red","K-12-26"="red")

dir.create(here("analysis","supplementary-materials","FigS15"))

OIB <- full_join(q26, q27)

s <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-08","T-12-06","T-12-07","T-12-08","T-12-09","T-12-10",
  "K-12-24","K-12-25","K-12-26")) %>%
  dplyr::mutate(Location = Sample) %>%
  dplyr::select(
    Sample,Location,Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

#### Fig S15 A ####
A <- OIB %>%
  ggplot(aes(x=Pb208_Pb204,y=Pb206_Pb204, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Caroline islands")), size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Samoan islands")), size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(37.7, 40)) +
  scale_y_continuous(position="right",
                     limits=c(17.8,20.6), breaks = c(18,18.5,19,19.5,20,20.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 18), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^208*"Pb / "*{}^204*"Pb"),
       y=expression({}^206*"Pb / "*{}^204*"Pb"))
A
pdf(here("analysis","supplementary-materials","FigS15","FigS15-A.pdf"), width=8, height=8)
A
dev.off()

A_detail <- OIB %>%
  ggplot(aes(x=Pb208_Pb204,y=Pb206_Pb204, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Caroline islands")), size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Samoan islands")), size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(position = "top", limits=c(37.5, 39.8), expand=c(0,0)) +
  scale_y_continuous(limits=c(17.7,19.5), expand=c(0,0)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_blank(), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1)
A_detail
pdf(here("analysis","supplementary-materials","FigS15","FigS15-A_detail.pdf"), width=5, height=5)
A_detail
dev.off()

#### Fig S15 B ####
B <- OIB %>%
  ggplot(aes(x=Sr87_Sr86,y=Nd143_Nd144, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Caroline islands")), size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Samoan islands")), size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(.7026,.7072)) +
  scale_y_continuous(limits=c(.5125, .5131)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 18), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^87*"Sr / "*{}^86*"Sr"),
       y=expression({}^143*"Nd / "*{}^144*"Nd"))
B
pdf(here("analysis","supplementary-materials","FigS15","FigS15-B.pdf"), width=8, height=8)
B
dev.off()

B_detail <- OIB %>%
  ggplot(aes(x=Sr87_Sr86,y=Nd143_Nd144, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Caroline islands")), size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Samoan islands")), size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(position="top", limits=c(.7026,.7065)) +
  scale_y_continuous(position="right", limits=c(.5126, .5131)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_blank(), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1)
B_detail
pdf(here("analysis","supplementary-materials","FigS15","FigS15-B_detail.pdf"), width=5, height=5)
B_detail
dev.off()


#### Fig S15 C ####
C <- OIB %>%
  ggplot(aes(x=Pb208_Pb204, y=Pb207_Pb204, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Caroline islands")), size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Samoan islands")), size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(37.7, 40)) +
  scale_y_continuous(position="right",limits=c(15.4,15.75)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 18), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1) +
  labs(x=expression({}^208*"Pb / "*{}^204*"Pb"),
       y=expression({}^207*"Pb / "*{}^204*"Pb"))
C
pdf(here("analysis","supplementary-materials","FigS15","FigS15-C.pdf"), width=8, height=8)
C
dev.off()

C_detail <- OIB %>%
  ggplot(aes(x=Pb208_Pb204,y=Pb207_Pb204, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Caroline islands")), size=3, stroke=.25) +
  geom_point(data=subset(OIB, Location %in% c("Samoan islands")), size=3, stroke=.25) +
  geom_point(data=s, size=3) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(38, 39.25), expand=c(0,0)) +
  scale_y_continuous(position = "right", limits=c(15.47, 15.63), expand=c(0,0)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_blank(), axis.text = element_text(size = 10),
        legend.position = "none", aspect.ratio=1)
C_detail
pdf(here("analysis","supplementary-materials","FigS15","FigS15-C_detail.pdf"), width=5, height=5)
C_detail
dev.off()
