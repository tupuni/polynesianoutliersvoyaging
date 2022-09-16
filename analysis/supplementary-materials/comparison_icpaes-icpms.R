require(here)
require(tidyverse)
require(ggpmisc)

icpaes2 <- icpaes %>%
  rename(Rb1=Rb, Sr1=Sr, Ba1=Ba, Sc1=Sc, V1=V, Cr1=Cr, Co1=Co, Ni1=Ni, Y1=Y,
         Zr1=Zr, Nb1=Nb, La1=La, Ce1=Ce, Nd1=Nd, Sm1=Sm, Eu1=Eu, Gd1=Gd, Dy1=Dy,
         Er1=Er, Yb1=Yb, Th1=Th) %>%
  dplyr::select(Sample, Rb1, Sr1, Ba1, Sc1, V1, Cr1, Co1, Ni1, Y1, Zr1, Nb1, La1, Ce1,
         Nd1, Sm1, Eu1, Gd1, Dy1, Er1, Yb1, Th1)
icpms2 <- icpms %>%
  rename(Rb2=Rb, Sr2=Sr, Ba2=Ba, Sc2=Sc, V2=V, Cr2=Cr, Co2=Co, Ni2=Ni, Y2=Y,
         Zr2=Zr, Nb2=Nb, La2=La, Ce2=Ce, Nd2=Nd, Sm2=Sm, Eu2=Eu, Gd2=Gd, Dy2=Dy,
         Er2=Er, Yb2=Yb, Th2=Th) %>%
  dplyr::select(Sample, Rb2, Sr2, Ba2, Sc2, V2, Cr2, Co2, Ni2, Y2, Zr2, Nb2, La2, Ce2,
         Nd2, Sm2, Eu2, Gd2, Dy2, Er2, Yb2, Th2)

lreg <- full_join(icpaes2, icpms2) %>% slice(1:23)

shapes <- c("E-11-03"=15,"E-11-06"=17,"E-11-07"=18,"E-11-08"=19,"E-11-08dup"=20,
            "E-11-10"=0,"E-11-11"=1,"E-11-13"=5,"E-11-16"=6,"E-11-18"=2,
            "E-11-19"=7,"T-12-06"=19,"T-12-06dup"=20,"T-12-07"=15,
            "T-12-08"=17,"T-12-09"=18,"T-12-10"=8,"K-12-24"=15,"K-12-25"=16,
            "K-12-26"=17,"K-12-28"=18,"K-12-29"=8,"OIB"=1,"IAB"=5)
cols <- c("E-11-03"="darkgoldenrod1","E-11-06"="chocolate1","E-11-07"="darkorange2",
          "E-11-08"="orangered3","E-11-08dup"="#8B0000","E-11-10"="lightskyblue1",
          "E-11-11"="lightskyblue","E-11-13"="dodgerblue1","E-11-16"="dodgerblue2",
          "E-11-18"="dodgerblue3","E-11-19"="dodgerblue4","T-12-06"="thistle2",
          "T-12-06dup"="pink1","T-12-07"="plum1","T-12-08"="plum","T-12-09"="violet",
          "T-12-10"="violetred","K-12-24"="darkolivegreen1","K-12-25"="chartreuse2",
          "K-12-26"="palegreen3","K-12-28"="darkolivegreen","K-12-29"="darkslategray",
          "OIB"="black","IAB"="black")

lreg %>% ggplot(aes(x = Sr1, y = Sr2, shape=factor(Sample), color=factor(Sample))) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_smooth( size = .8, col = "black", method = "lm", se = FALSE) +
  geom_point(size = 3) +
  scale_shape_manual(values = shapes) + scale_color_manual(values = cols) +
  #stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
  #             parse = TRUE, label.x.npc = "left", size = 4) +
  #scale_x_continuous(limits=c(0, 120)) + scale_y_continuous(limits=c(0, 120)) +
  labs(title="Sr (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="right", aspect.ratio=1)
lreg %>% ggplot(aes(x = Ba1, y = Ba2, shape=factor(Sample), color=factor(Sample))) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_smooth( size = .8, col = "black", method = "lm", se = FALSE) +
  geom_point(size = 3) +
  scale_shape_manual(values = shapes) + scale_color_manual(values = cols) +
  #stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
  #             parse = TRUE, label.x.npc = "left", size = 4) +
  #scale_x_continuous(limits=c(0, 120)) + scale_y_continuous(limits=c(0, 120)) +
  labs(title="Ba (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="right", aspect.ratio=1)

Rb <- lreg %>% ggplot(aes(x = Rb1, y = Rb2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  scale_x_continuous(limits=c(0, 120)) + scale_y_continuous(limits=c(0, 120)) +
  labs(title="Rb (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
Rb
Sr <- lreg %>% ggplot(aes(x = Sr1, y = Sr2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  scale_x_continuous(limits=c(150, 1250)) + scale_y_continuous(limits=c(150, 1250)) +
  labs(title="Sr (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
Sr
Ba <- lreg %>% ggplot(aes(x = Ba1, y = Ba2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  scale_x_continuous(limits=c(0, 1200)) + scale_y_continuous(limits=c(0, 1200)) +
  labs(title="Ba (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
Ba
Sc <- lreg %>% ggplot(aes(x = Sc1, y = Sc2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  scale_x_continuous(limits=c(10, 50)) + scale_y_continuous(limits=c(10, 50)) +
  labs(title="Sc (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
Sc
V <- lreg %>% ggplot(aes(x = V1, y = V2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  scale_x_continuous(limits=c(0, 400)) + scale_y_continuous(limits=c(0, 400)) +
  labs(title="V (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
V
Cr <- lreg %>% ggplot(aes(x = Cr1, y = Cr2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  #scale_x_continuous(limits=c(150, 1250)) + scale_y_continuous(limits=c(150, 1250)) +
  labs(title="Cr (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
Cr
Co <- lreg %>% ggplot(aes(x = Co1, y = Co2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  scale_x_continuous(limits=c(0, 80)) + scale_y_continuous(limits=c(0, 80)) +
  labs(title="Co (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
Co
Ni <- lreg %>% ggplot(aes(x = Ni1, y = Ni2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  #scale_x_continuous(limits=c(150, 1250)) + scale_y_continuous(limits=c(150, 1250)) +
  labs(title="Ni (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
Ni
Y <- lreg %>% ggplot(aes(x = Y1, y = Y2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  #scale_x_continuous(limits=c(150, 1250)) + scale_y_continuous(limits=c(150, 1250)) +
  labs(title="Y (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
Y
Zr <- lreg %>% ggplot(aes(x = Zr1, y = Zr2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  scale_x_continuous(limits=c(0, 650)) + scale_y_continuous(limits=c(0, 650)) +
  labs(title="Zr (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
Zr
Nb <- lreg %>% ggplot(aes(x = Nb1, y = Nb2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  scale_x_continuous(limits=c(0, 115)) + scale_y_continuous(limits=c(0, 115)) +
  labs(title="Nb (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
Nb
La <- lreg %>% ggplot(aes(x = La1, y = La2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  scale_x_continuous(limits=c(5, 100)) + scale_y_continuous(limits=c(5, 100)) +
  labs(title="La (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
La
Ce <- lreg %>% ggplot(aes(x = Ce1, y = Ce2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  scale_x_continuous(limits=c(20, 210)) + scale_y_continuous(limits=c(20, 210)) +
  labs(title="Ce (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
Ce
Nd <- lreg %>% ggplot(aes(x = Nd1, y = Nd2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  scale_x_continuous(limits=c(5, 115)) + scale_y_continuous(limits=c(5, 115)) +
  labs(title="Nd (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
Nd
Sm <- lreg %>% ggplot(aes(x = Sm1, y = Sm2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  scale_x_continuous(limits=c(2, 24)) + scale_y_continuous(limits=c(2, 24)) +
  labs(title="Sm (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
Sm
Eu <- lreg %>% ggplot(aes(x = Eu1, y = Eu2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  scale_x_continuous(limits=c(0, 7)) + scale_y_continuous(limits=c(0, 7)) +
  labs(title="Eu (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
Eu
Gd <- lreg %>% ggplot(aes(x = Gd1, y = Gd2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  scale_x_continuous(limits=c(2, 19)) + scale_y_continuous(limits=c(2, 19)) +
  labs(title="Gd (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
Gd
Dy <- lreg %>% ggplot(aes(x = Dy1, y = Dy2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  scale_x_continuous(limits=c(2, 13)) + scale_y_continuous(limits=c(2, 13)) +
  labs(title="Dy (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
Dy
Er <- lreg %>% ggplot(aes(x = Er1, y = Er2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  scale_x_continuous(limits=c(1, 6)) + scale_y_continuous(limits=c(1, 6)) +
  labs(title="Er (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
Er
Yb <- lreg %>% ggplot(aes(x = Yb1, y = Yb2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  scale_x_continuous(limits=c(1, 5.5)) + scale_y_continuous(limits=c(1, 5.5)) +
  labs(title="Yb (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
Yb
Th <- lreg %>% ggplot(aes(x = Th1, y = Th2)) +
  geom_abline(size = .5, linetype = "dashed", col = "black") +
  geom_point(shape = 20, size = 4, stroke = .75, col = "blue", fill = "white") +
  geom_smooth( size = .7, col = "blue", method = "lm", se = FALSE) +
  stat_poly_eq(formula = y ~ x, aes(label = paste(..rr.label..)),
               parse = TRUE, label.x.npc = "left", size = 4) +
  scale_x_continuous(limits=c(0, 10)) + scale_y_continuous(limits=c(0, 10)) +
  labs(title="Th (ppm)", x = expression("ICP-AES"), y = expression("ICP-MS")) +
  theme_classic() + theme(axis.line=element_blank()) +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 11), axis.text = element_text(size = 11),
        panel.border = element_rect(colour="black", fill = NA, size = 1),
        legend.position="none", aspect.ratio=1)
Th

plots <- ((Rb | Sr | Ba | Sc | V | Cr) /
            (Co | Ni | Y | Zr | Nb | La) /
            (Ce | Nd | Sm | Eu | Gd | Dy) /
            (Er | Yb | Th | plot_spacer() | plot_spacer() | plot_spacer()))

#save
pdf(here("analysis","supplementary-materials","linear-regression.pdf"),
    width=20, height=14)
plots
dev.off()



## multielement plots
shapes <- c("E-11-03"=15,"E-11-06"=17,"E-11-07"=18,"E-11-08"=19,"E-11-08dup"=20,
            "E-11-10"=0,"E-11-11"=1,"E-11-13"=5,"E-11-16"=6,"E-11-18"=2,
            "E-11-19"=7,"T-12-06"=19,"T-12-06dup"=20,"T-12-07"=15,
            "T-12-08"=17,"T-12-09"=18,"T-12-10"=8,"K-12-24"=15,"K-12-25"=16,
            "K-12-26"=17,"K-12-28"=18,"K-12-29"=8,"OIB"=1,"IAB"=5)
cols <- c("E-11-03"="darkgoldenrod1","E-11-06"="chocolate1","E-11-07"="darkorange2",
          "E-11-08"="orangered3","E-11-08dup"="#8B0000","E-11-10"="lightskyblue1",
          "E-11-11"="lightskyblue","E-11-13"="dodgerblue1","E-11-16"="dodgerblue2",
          "E-11-18"="dodgerblue3","E-11-19"="dodgerblue4","T-12-06"="thistle2",
          "T-12-06dup"="pink1","T-12-07"="plum1","T-12-08"="plum","T-12-09"="violet",
          "T-12-10"="violetred","K-12-24"="darkolivegreen1","K-12-25"="chartreuse2",
          "K-12-26"="palegreen3","K-12-28"="darkolivegreen","K-12-29"="darkslategray",
          "OIB"="black","IAB"="black")

icpaes_spider <- icpaes %>%
  dplyr::select(Sample, La, Ce, Nd, Sm, Eu, Gd, Dy, Er, Yb) %>%
  dplyr::mutate(Sample=Sample, La=La/0.687, Ce=Ce/1.775, Nd=Nd/1.354,
                Sm=Sm/0.444, Eu=Eu/0.168, Gd=Gd/0.596, Dy=Dy/0.737,
                Er=Er/0.48,Yb=Yb/0.493) %>% gather("var","conc", La:Yb)
icpms_spider <- icpms %>%
  dplyr::select(Sample, La, Ce, Nd, Sm, Eu, Gd, Dy, Er, Yb) %>%
  dplyr::mutate(Sample=Sample, La=La/0.687, Ce=Ce/1.775, Nd=Nd/1.354,
                Sm=Sm/0.444, Eu=Eu/0.168, Gd=Gd/0.596, Dy=Dy/0.737,
                Er=Er/0.48,Yb=Yb/0.493) %>% gather("var","conc", La:Yb)
sunmcdonough <- read.csv(here("analysis", "data", "raw_data", "sun_mcdonough.csv"),
                         header=TRUE, sep=",", stringsAsFactors=FALSE) %>%
  dplyr::select(Sample, La, Ce, Nd, Sm, Eu, Gd, Dy, Er, Yb) %>%
  dplyr::mutate(Sample=Sample, La=La/0.687, Ce=Ce/1.775, Nd=Nd/1.354,
                Sm=Sm/0.444, Eu=Eu/0.168, Gd=Gd/0.596, Dy=Dy/0.737,
                Er=Er/0.48,Yb=Yb/0.493)
oib <- sunmcdonough %>% filter(Sample=="OIB") %>% gather("var","conc", La:Yb)
iab <- sunmcdonough %>% filter(Sample=="IAB") %>% gather("var","conc", La:Yb)

p1 <- icpaes_spider %>% filter(Sample %in% c("E-11-10", "E-11-11", "E-11-13",
                                             "E-11-16", "E-11-18", "E-11-19")) %>%
  mutate(var = fct_relevel(var,"La","Ce","Nd","Sm","Eu","Gd","Dy","Er","Yb")) %>%
  ggplot(aes(x=var, y=conc, color=factor(Sample), shape=factor(Sample),
             group=Sample)) + geom_point(size=3) + geom_line() +
  geom_point(data=iab, size=3) + geom_line(data=iab) +
  scale_shape_manual(values = shapes) + scale_color_manual(values = cols) +
  scale_y_log10(breaks=c(1,10,100), limits=c(1,100), labels = scales::comma) +
  ylab("Sample / Primitive mantle") + annotation_logticks(sides="l") +
  theme(panel.border=element_rect(fill=NA,size=.8),panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_text(size=12),
        axis.text=element_text(size=12),axis.ticks.y=element_blank(),
        legend.title=element_blank(),legend.text=element_text(size=9),
        legend.position = "top", legend.direction = "horizontal") +
  annotate("text", x=8, y=70, size=5, label="ICP-AES")
p1
p2 <- icpms_spider %>% filter(Sample %in% c("E-11-10", "E-11-11", "E-11-13",
                                            "E-11-16", "E-11-18", "E-11-19")) %>%
  mutate(var = fct_relevel(var,"La","Ce","Nd","Sm","Eu","Gd","Dy","Er","Yb")) %>%
  ggplot(aes(x=var, y=conc, color=factor(Sample), shape=factor(Sample),
             group=Sample)) + geom_point(size=3) + geom_line() +
  geom_point(data=iab, size=3) + geom_line(data=iab) +
  scale_shape_manual(values = shapes) + scale_color_manual(values = cols) +
  scale_y_log10(breaks=c(1,10,100), limits=c(1,100), labels = scales::comma) +
  ylab("Sample / Primitive mantle") + annotation_logticks(sides="l") +
  theme(panel.border=element_rect(fill=NA,size=.8),panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=12),axis.ticks.y=element_blank(),
        legend.title=element_blank(),legend.text=element_text(size=9),
        legend.position = "top", legend.direction = "horizontal") +
  annotate("text", x=8, y=70, size=5, label="ICP-MS")
p2

p3 <- icpaes_spider %>% filter(Sample %in% c("E-11-03", "E-11-06", "E-11-07",
                                             "K-12-28", "K-12-29")) %>%
  mutate(var = fct_relevel(var,"La","Ce","Nd","Sm","Eu","Gd","Dy","Er","Yb")) %>%
  ggplot(aes(x=var, y=conc, color=factor(Sample), shape=factor(Sample),
             group=Sample)) + geom_point(size=3) + geom_line() +
  geom_point(data=iab, size=3) + geom_line(data=iab) +
  scale_shape_manual(values = shapes) + scale_color_manual(values = cols) +
  scale_y_log10(breaks=c(1,10,100), limits=c(1,100), labels = scales::comma) +
  ylab("Sample / Primitive mantle") + annotation_logticks(sides="l") +
  theme(panel.border=element_rect(fill=NA,size=.8),panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_text(size=12),
        axis.text=element_text(size=12),axis.ticks.y=element_blank(),
        legend.title=element_blank(),legend.text=element_text(size=9),
        legend.position = "none") +
  annotate("text", x=8, y=70, size=5, label="ICP-AES")
p3
p4 <- icpms_spider %>% filter(Sample %in% c("E-11-03", "E-11-06", "E-11-07",
                                            "K-12-28", "K-12-29")) %>%
  mutate(var = fct_relevel(var,"La","Ce","Nd","Sm","Eu","Gd","Dy","Er","Yb")) %>%
  ggplot(aes(x=var, y=conc, color=factor(Sample), shape=factor(Sample),
             group=Sample)) + geom_point(size=3) + geom_line() +
  geom_point(data=iab, size=3) + geom_line(data=iab) +
  scale_shape_manual(values = shapes) + scale_color_manual(values = cols) +
  scale_y_log10(breaks=c(1,10,100), limits=c(1,100), labels = scales::comma) +
  ylab("Sample / Primitive mantle") + annotation_logticks(sides="l") +
  theme(panel.border=element_rect(fill=NA,size=.8),panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=12),axis.ticks.y=element_blank(),
        legend.title=element_blank(),legend.text=element_text(size=9),
        legend.position = "none") +
  annotate("text", x=8, y=70, size=5, label="ICP-MS")
p4

p5 <- icpaes_spider %>%
  filter(Sample %in% c("E-11-08","K-12-24","K-12-25","K-12-26","T-12-06","T-12-06dup",
                       "T-12-07","T-12-08","T-12-09","T-12-10")) %>%
  mutate(var = fct_relevel(var,"La","Ce","Nd","Sm","Eu","Gd","Dy","Er","Yb")) %>%
  ggplot(aes(x=var, y=conc, color=factor(Sample), shape=factor(Sample),
             group=Sample)) + geom_point(size=3) + geom_line() +
  geom_point(data=oib, size=3) + geom_line(data=oib) +
  scale_shape_manual(values = shapes) + scale_color_manual(values = cols) +
  scale_y_log10(breaks=c(1,10,100), limits=c(1,150), labels = scales::comma) +
  ylab("Sample / Primitive mantle") + annotation_logticks(sides="l") +
  theme(panel.border=element_rect(fill=NA,size=.8),panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_text(size=12),
        axis.text=element_text(size=12),axis.ticks.y=element_blank(),
        legend.title=element_blank(),legend.text=element_text(size=9),
        legend.position = "none") +
  annotate("text", x=8, y=100, size=5, label="ICP-AES")
p5
p6 <- icpms_spider %>%
  filter(Sample %in% c("E-11-08","K-12-24","K-12-25","K-12-26","T-12-06","T-12-06dup",
                       "T-12-07","T-12-08","T-12-09","T-12-10")) %>%
  mutate(var = fct_relevel(var,"La","Ce","Nd","Sm","Eu","Gd","Dy","Er","Yb")) %>%
  ggplot(aes(x=var, y=conc, color=factor(Sample), shape=factor(Sample),
             group=Sample)) + geom_point(size=3) + geom_line() +
  geom_point(data=oib, size=3) + geom_line(data=oib) +
  scale_shape_manual(values = shapes) + scale_color_manual(values = cols) +
  scale_y_log10(breaks=c(1,10,100), limits=c(1,150), labels = scales::comma) +
  ylab("Sample / Primitive mantle") + annotation_logticks(sides="l") +
  theme(panel.border=element_rect(fill=NA,size=.8),panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.title=element_blank(),
        axis.text=element_text(size=12),axis.ticks.y=element_blank(),
        legend.title=element_blank(),legend.text=element_text(size=9),
        legend.position = "none") +
  annotate("text", x=8, y=100, size=5, label="ICP-MS")
p6

spiders <- (p1 | p2) / (p3 | p4) / (p5 | p6) + guide_area() + plot_layout(guides = 'collect')
#save
pdf(here("analysis","supplementary-materials","multielement-plots.pdf"),
    width=10, height=10)
spiders
dev.off()


