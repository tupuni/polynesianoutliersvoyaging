require(here)
require(tidyverse)
require(patchwork)
require(stats)
require(FactoMineR)
require(factoextra)
library(ggridges)

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

dir.create(here("analysis","supplementary-materials","FigS14"))

#### Fig 14a ####
## Emae_Taumako PCA 1
OIB <- full_join(q10,q11) %>%
  dplyr::select(
    Sample,Location,Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)

OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-08","T-12-06","T-12-07","T-12-08","T-12-09","T-12-10")) %>%
  dplyr::mutate(Location = case_when(
    grepl("E-11-08", Sample) ~ "E-11-08",
    grepl("T-12-06", Sample) ~ "T-12-06",
    grepl("T-12-07", Sample) ~ "T-12-07",
    grepl("T-12-08", Sample) ~ "T-12-08",
    grepl("T-12-09", Sample) ~ "T-12-09",
    grepl("T-12-10", Sample) ~ "T-12-10")) %>% dplyr::select(
      Sample,Location,Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

res.pca <- prcomp(OIB[,3:7],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
E_T_PCA_1a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = OIB$Location, fill.ind = OIB$Location,
  pointsize = 2, invisible = "quali", labelsize = 2, repel = T) +
  scale_x_continuous(limits=c(-3.5, 3.5)) + scale_y_continuous(limits=c(-2.5, 3)) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
E_T_PCA_1a
pdf(here("analysis","supplementary-materials","FigS14","FigS14-a-PCA.pdf"), width=3.5, height=3.5)
E_T_PCA_1a
dev.off()

res.pca.df <- cbind(OIB[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, s[,3:7])
pred <- cbind(s[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

E_T_PCA_1b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) +
  geom_point(aes(x=median(d_pca[142:147,"PC1"]),
                 y=median(d_pca[142:147,"PC2"])), shape=3, color="red") +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-3.5, 3.5)) + scale_y_continuous(limits=c(-2.5, 3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
E_T_PCA_1b

## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[142:147,"PC1"], xend=d_pca[142:147,"PC1"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC1","\n","(",round(eig["Dim.1","variance.percent"], digits = 1),"%)"))
PC2 <- ggplot(d_pca, aes(x = PC2, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[142:147,"PC2"], xend=d_pca[142:147,"PC2"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC2","\n","(",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
PC3 <- ggplot(d_pca, aes(x = PC3, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[142:147,"PC3"], xend=d_pca[142:147,"PC3"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC3","\n","(",round(eig["Dim.3","variance.percent"], digits = 1),"%)"))
PC4 <- ggplot(d_pca, aes(x = PC4, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[142:147,"PC4"], xend=d_pca[142:147,"PC4"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC4","\n","(",round(eig["Dim.4","variance.percent"], digits = 1),"%)"))
PC5 <- ggplot(d_pca, aes(x = PC5, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[142:147,"PC5"], xend=d_pca[142:147,"PC5"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC5","\n","(",round(eig["Dim.5","variance.percent"], digits = 1),"%)"))

PCs <- PC1/PC2/PC3/PC4/PC5 & theme(plot.margin = unit(c(5,0,5,10), "pt"))

pdf(here("analysis","supplementary-materials","FigS14","FigS14-a.pdf"), width=6, height=3.5)
(E_T_PCA_1b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()

#### Fig 14b ####
## Emae_Taumako PCA 2
OIB <- full_join(q12,q13) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-08","E-11-08dup","T-12-06","T-12-06dup","T-12-07","T-12-08",
  "T-12-09","T-12-10")) %>%
  dplyr::mutate(Location=Sample) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB[,3:21], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
E_T_PCA_2a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = OIB$Location, fill.ind = OIB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-5, 7.5)) + scale_y_continuous(limits=c(-5.5, 5.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
E_T_PCA_2a
pdf(here("analysis","supplementary-materials","FigS14","FigS14-b-PCA.pdf"), width=3.5, height=3.5)
E_T_PCA_2a
dev.off()

pred <- stats::predict(res.pca, s[,3:21])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(OIB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

E_T_PCA_2b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(aes(x=median(d_pca[67:72,"PC1"]),
                 y=median(d_pca[67:72,"PC2"])), shape=3, color="red") +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-5, 7.5)) + scale_y_continuous(limits=c(-5.5, 5.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
E_T_PCA_2b

## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[67:72,"PC1"], xend=d_pca[67:72,"PC1"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC1","\n","(",round(eig["Dim.1","variance.percent"], digits = 1),"%)"))
PC2 <- ggplot(d_pca, aes(x = PC2, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[67:72,"PC2"], xend=d_pca[67:72,"PC2"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC2","\n","(",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
PC3 <- ggplot(d_pca, aes(x = PC3, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[67:72,"PC3"], xend=d_pca[67:72,"PC3"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC3","\n","(",round(eig["Dim.3","variance.percent"], digits = 1),"%)"))
PC4 <- ggplot(d_pca, aes(x = PC4, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[67:72,"PC4"], xend=d_pca[67:72,"PC4"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC4","\n","(",round(eig["Dim.4","variance.percent"], digits = 1),"%)"))
PC5 <- ggplot(d_pca, aes(x = PC5, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[67:72,"PC5"], xend=d_pca[67:72,"PC5"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC5","\n","(",round(eig["Dim.5","variance.percent"], digits = 1),"%)"))

PCs <- PC1/PC2/PC3/PC4/PC5 & theme(plot.margin = unit(c(5,0,5,10), "pt"))

pdf(here("analysis","supplementary-materials","FigS14","FigS14-b.pdf"), width=6, height=3.5)
(E_T_PCA_2b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()


#### Fig 14c ####
## K1224 PCA 1
OIB <- q14 %>%
  dplyr::select(
    Sample,Location,Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>%
  dplyr::filter(Sample %in% c("K-12-24")) %>%
  dplyr::mutate(Location=Sample) %>%
  dplyr::select(Sample,Location,
                Nd143_Nd144,Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB[,3:7], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
K1224_PCA_1a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = OIB$Location, fill.ind = OIB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-4, 4)) + scale_y_continuous(limits=c(-3.5, 3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
K1224_PCA_1a
pdf(here("analysis","supplementary-materials","FigS14","FigS14-c-PCA.pdf"), width=3.5, height=3.5)
K1224_PCA_1a
dev.off()

pred <- stats::predict(res.pca, s[,3:7])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(OIB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

K1224_PCA_1b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-4, 4)) + scale_y_continuous(limits=c(-3.5, 3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K1224_PCA_1b

## PCs
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[338,"PC1"], xend=d_pca[338,"PC1"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC1","\n","(",round(eig["Dim.1","variance.percent"], digits = 1),"%)"))
PC2 <- ggplot(d_pca, aes(x = PC2, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[338,"PC2"], xend=d_pca[338,"PC2"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC2","\n","(",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
PC3 <- ggplot(d_pca, aes(x = PC3, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[338,"PC3"], xend=d_pca[338,"PC3"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC3","\n","(",round(eig["Dim.3","variance.percent"], digits = 1),"%)"))
PC4 <- ggplot(d_pca, aes(x = PC4, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[338,"PC4"], xend=d_pca[338,"PC4"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC4","\n","(",round(eig["Dim.4","variance.percent"], digits = 1),"%)"))
PC5 <- ggplot(d_pca, aes(x = PC5, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[338,"PC5"], xend=d_pca[338,"PC5"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC5","\n","(",round(eig["Dim.5","variance.percent"], digits = 1),"%)"))

PCs <- PC1/PC2/PC3/PC4/PC5 & theme(plot.margin = unit(c(5,0,5,10), "pt"))

pdf(here("analysis","supplementary-materials","FigS14","FigS14-c.pdf"), width=6, height=3.5)
(K1224_PCA_1b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()


#### Fig 14d ####
## K1224 PCA 2
OIB <- q15 %>% dplyr::select(
  Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA
OIB %>% group_by(Location) %>% tally()

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-24")) %>%
  mutate(Location = Sample) %>% dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB[,3:21], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
K1224_PCA_2a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = OIB$Location, fill.ind = OIB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-18, 8)) + scale_y_continuous(limits=c(-8, 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
K1224_PCA_2a
pdf(here("analysis","supplementary-materials","FigS14","FigS14-d-PCA.pdf"), width=3.5, height=3.5)
K1224_PCA_2a
dev.off()

pred <- stats::predict(res.pca, s[,3:21])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(OIB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

K1224_PCA_2b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-18, 8)) + scale_y_continuous(limits=c(-8, 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K1224_PCA_2b

## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[500,"PC1"], xend=d_pca[500,"PC1"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC1","\n","(",round(eig["Dim.1","variance.percent"], digits = 1),"%)"))
PC2 <- ggplot(d_pca, aes(x = PC2, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[500,"PC2"], xend=d_pca[500,"PC2"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC2","\n","(",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
PC3 <- ggplot(d_pca, aes(x = PC3, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[500,"PC3"], xend=d_pca[500,"PC3"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC3","\n","(",round(eig["Dim.3","variance.percent"], digits = 1),"%)"))
PC4 <- ggplot(d_pca, aes(x = PC4, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[500,"PC4"], xend=d_pca[500,"PC4"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC4","\n","(",round(eig["Dim.4","variance.percent"], digits = 1),"%)"))
PC5 <- ggplot(d_pca, aes(x = PC5, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[500,"PC5"], xend=d_pca[500,"PC5"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC5","\n","(",round(eig["Dim.5","variance.percent"], digits = 1),"%)"))

PCs <- PC1/PC2/PC3/PC4/PC5 & theme(plot.margin = unit(c(5,0,5,10), "pt"))

pdf(here("analysis","supplementary-materials","FigS14","FigS14-d.pdf"), width=6, height=3.5)
(K1224_PCA_2b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()


#### FigS14e ####
## K1225 PCA 1
OIB <- q16 %>% dplyr::select(
  Sample,Location,Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>%
  dplyr::filter(Sample %in% c("K-12-25")) %>%
  dplyr::mutate(Location = Sample) %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB[,3:7], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
K1225_PCA_1a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = OIB$Location, fill.ind = OIB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-3, 3.5)) + scale_y_continuous(limits=c(-2, 2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
K1225_PCA_1a
pdf(here("analysis","supplementary-materials","FigS14","FigS14-e-PCA.pdf"), width=3.5, height=3.5)
K1225_PCA_1a
dev.off()

pred <- stats::predict(res.pca, s[,3:7])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(OIB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

K1225_PCA_1b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-3, 3.5)) + scale_y_continuous(limits=c(-2, 2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K1225_PCA_1b

## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[31,"PC1"], xend=d_pca[31,"PC1"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC1","\n","(",round(eig["Dim.1","variance.percent"], digits = 1),"%)"))
PC2 <- ggplot(d_pca, aes(x = PC2, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[31,"PC2"], xend=d_pca[31,"PC2"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC2","\n","(",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
PC3 <- ggplot(d_pca, aes(x = PC3, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[31,"PC3"], xend=d_pca[31,"PC3"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC3","\n","(",round(eig["Dim.3","variance.percent"], digits = 1),"%)"))
PC4 <- ggplot(d_pca, aes(x = PC4, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[31,"PC4"], xend=d_pca[31,"PC4"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC4","\n","(",round(eig["Dim.4","variance.percent"], digits = 1),"%)"))
PC5 <- ggplot(d_pca, aes(x = PC5, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[31,"PC5"], xend=d_pca[31,"PC5"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC5","\n","(",round(eig["Dim.5","variance.percent"], digits = 1),"%)"))

PCs <- PC1/PC2/PC3/PC4/PC5 & theme(plot.margin = unit(c(5,0,5,10), "pt"))

pdf(here("analysis","supplementary-materials","FigS14","FigS14-e.pdf"), width=6, height=3.5)
(K1225_PCA_1b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()

#### Fig 14f ####
## K1225 PCA 2
OIB <- q17 %>% dplyr::select(
  Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA
OIB %>% group_by(Location) %>% tally()

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-25")) %>%
  mutate(Location = Sample) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB[,3:21], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
K1225_PCA_2a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = OIB$Location, fill.ind = OIB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-6, 9)) + scale_y_continuous(limits=c(-3.5, 3.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
K1225_PCA_2a
pdf(here("analysis","supplementary-materials","FigS14","FigS14-f-PCA.pdf"), width=3.5, height=3.5)
K1225_PCA_2a
dev.off()

pred <- stats::predict(res.pca, s[,3:21])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(OIB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

K1225_PCA_2b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-6, 9)) + scale_y_continuous(limits=c(-3.5, 3.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K1225_PCA_2b

## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[17,"PC1"], xend=d_pca[17,"PC1"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC1","\n","(",round(eig["Dim.1","variance.percent"], digits = 1),"%)"))
PC2 <- ggplot(d_pca, aes(x = PC2, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[17,"PC2"], xend=d_pca[17,"PC2"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC2","\n","(",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
PC3 <- ggplot(d_pca, aes(x = PC3, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[17,"PC3"], xend=d_pca[17,"PC3"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC3","\n","(",round(eig["Dim.3","variance.percent"], digits = 1),"%)"))
PC4 <- ggplot(d_pca, aes(x = PC4, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[17,"PC4"], xend=d_pca[17,"PC4"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC4","\n","(",round(eig["Dim.4","variance.percent"], digits = 1),"%)"))
PC5 <- ggplot(d_pca, aes(x = PC5, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[17,"PC5"], xend=d_pca[17,"PC5"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC5","\n","(",round(eig["Dim.5","variance.percent"], digits = 1),"%)"))

PCs <- PC1/PC2/PC3/PC4/PC5 & theme(plot.margin = unit(c(5,0,5,10), "pt"))

pdf(here("analysis","supplementary-materials","FigS14","FigS14-f.pdf"), width=6, height=3.5)
(K1225_PCA_2b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()


#### Fig 14g ####
## K1226 PCA 1
OIB <- q18 %>% dplyr::select(
  Sample,Location,Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>%
  dplyr::filter(Sample %in% c("K-12-26")) %>%
  dplyr::mutate(Location=Sample) %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB[,3:7], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
K1226_PCA_1a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = OIB$Location, fill.ind = OIB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-7, 3.5)) + scale_y_continuous(limits=c(-4, 3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
K1226_PCA_1a
pdf(here("analysis","supplementary-materials","FigS14","FigS14-g-PCA.pdf"), width=3.5, height=3.5)
K1226_PCA_1a
dev.off()

pred <- stats::predict(res.pca, s[,3:7])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(OIB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

K1226_PCA_1b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-7, 3.5)) + scale_y_continuous(limits=c(-4, 3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K1226_PCA_1b

## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[85,"PC1"], xend=d_pca[85,"PC1"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC1","\n","(",round(eig["Dim.1","variance.percent"], digits = 1),"%)"))
PC2 <- ggplot(d_pca, aes(x = PC2, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[85,"PC2"], xend=d_pca[85,"PC2"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC2","\n","(",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
PC3 <- ggplot(d_pca, aes(x = PC3, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[85,"PC3"], xend=d_pca[85,"PC3"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC3","\n","(",round(eig["Dim.3","variance.percent"], digits = 1),"%)"))
PC4 <- ggplot(d_pca, aes(x = PC4, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[85,"PC4"], xend=d_pca[85,"PC4"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC4","\n","(",round(eig["Dim.4","variance.percent"], digits = 1),"%)"))
PC5 <- ggplot(d_pca, aes(x = PC5, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[85,"PC5"], xend=d_pca[85,"PC5"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC5","\n","(",round(eig["Dim.5","variance.percent"], digits = 1),"%)"))

PCs <- PC1/PC2/PC3/PC4/PC5 & theme(plot.margin = unit(c(5,0,5,10), "pt"))

pdf(here("analysis","supplementary-materials","FigS14","FigS14-g.pdf"), width=6, height=3.5)
(K1226_PCA_1b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()


#### Fig 14h ####
## K1226 PCA 2
OIB <- q19 %>% dplyr::select(
  Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA
OIB %>% group_by(Location) %>% tally()

s <- joined_data %>%
  dplyr::filter(Sample %in% c("K-12-26")) %>%
  dplyr::mutate(Location=Sample) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB[,3:21], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
K1226_PCA_2a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = OIB$Location, fill.ind = OIB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-4, 8)) + scale_y_continuous(limits=c(-1, 2.8)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
K1226_PCA_2a
pdf(here("analysis","supplementary-materials","FigS14","FigS14-h-PCA.pdf"), width=3.5, height=3.5)
K1226_PCA_2a
dev.off()

pred <- stats::predict(res.pca, s[,3:21])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(OIB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

K1226_PCA_2b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-4, 8)) + scale_y_continuous(limits=c(-1, 2.8)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K1226_PCA_2b

## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[18,"PC1"], xend=d_pca[18,"PC1"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC1","\n","(",round(eig["Dim.1","variance.percent"], digits = 1),"%)"))
PC2 <- ggplot(d_pca, aes(x = PC2, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[18,"PC2"], xend=d_pca[18,"PC2"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC2","\n","(",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
PC3 <- ggplot(d_pca, aes(x = PC3, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[18,"PC3"], xend=d_pca[18,"PC3"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC3","\n","(",round(eig["Dim.3","variance.percent"], digits = 1),"%)"))
PC4 <- ggplot(d_pca, aes(x = PC4, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[18,"PC4"], xend=d_pca[18,"PC4"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC4","\n","(",round(eig["Dim.4","variance.percent"], digits = 1),"%)"))
PC5 <- ggplot(d_pca, aes(x = PC5, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[18,"PC5"], xend=d_pca[18,"PC5"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.title.y=element_text(size=7, margin=margin(r=0), hjust = 0),
    axis.text.x=element_text(size=6), axis.line.x=element_line(size=.25),
    axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(y=paste0("PC5","\n","(",round(eig["Dim.5","variance.percent"], digits = 1),"%)"))

PCs <- PC1/PC2/PC3/PC4/PC5 & theme(plot.margin = unit(c(5,0,5,10), "pt"))

pdf(here("analysis","supplementary-materials","FigS14","FigS14-h.pdf"), width=6, height=3.5)
(K1226_PCA_2b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()

