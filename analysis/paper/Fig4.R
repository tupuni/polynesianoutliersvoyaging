require(here)
require(tidyverse)
require(patchwork)
require(stats)
require(FactoMineR)
require(factoextra)
library(ggridges)

georoc <- dbConnect(RSQLite::SQLite(), path_to_georoc)
pofatu <- dbConnect(RSQLite::SQLite(), path_to_pofatu)

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
dir.create(here("analysis","figures","Figure_4"))

#### Fig 4a ####
## Emae_Taumako PCA 1
OIB <- full_join(q10,q11) %>% dplyr::select(
    Sample,Location,Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)
is.na(OIB) <- sapply(OIB, is.infinite) #replace Inf by NA
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-08","T-12-06","T-12-07","T-12-08","T-12-09","T-12-10")) %>%
  mutate(Location = case_when(
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
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
pdf(here("analysis","figures","Figure_4","Fig4-a-PCA.pdf"), width=3.5, height=3.5)
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
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
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

# PC values > distance to artefacts (individual or median of group)
# distance within the 5 most significant PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:141,"Sample"]),
  Location = c(d_pca[1:141,"Location"]),
  PC1 = c(sqrt(((median(d_pca[142:147,"PC1"]))-d_pca[1:141,"PC1"])^2)),
  PC2 = c(sqrt(((median(d_pca[142:147,"PC2"]))-d_pca[1:141,"PC2"])^2)),
  PC3 = c(sqrt(((median(d_pca[142:147,"PC3"]))-d_pca[1:141,"PC3"])^2)),
  PC4 = c(sqrt(((median(d_pca[142:147,"PC4"]))-d_pca[1:141,"PC4"])^2)),
  PC5 = c(sqrt(((median(d_pca[142:147,"PC5"]))-d_pca[1:141,"PC5"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig["Dim.1","variance.percent"])+(PC2*eig["Dim.2","variance.percent"])+
      (PC3*eig["Dim.3","variance.percent"])+(PC4*eig["Dim.4","variance.percent"])+
      (PC4*eig["Dim.5","variance.percent"])) /
      (eig["Dim.1","variance.percent"]+eig["Dim.2","variance.percent"]+
         eig["Dim.3","variance.percent"]+eig["Dim.4","variance.percent"]+
         eig["Dim.5","variance.percent"]))

# distances > distance index, from 1 (closer to artefact) to 5 (more distant)
# 1= min > (1st quartile/2)
# 2= (1st quartile/2) > 1st quartile
# 3= 1st quartile > median
# 4= median > 3rd quartile
# 5= 3rd quartile > max
summary(dist$PC1)
summary_stat <- data.frame(unclass(summary(dist$weight_mean)), check.names = F)
dist <- dist %>% mutate(
  PC1_dist = cut(PC1,
                 breaks=c(0,.2,.3,.66,1.2,4),
                 labels=c('1','2','3','4','5')),
  dist_cat = cut(weight_mean,
                 breaks=c(0,(summary_stat["1st Qu.",]/2),
                          summary_stat["1st Qu.",],
                          summary_stat["Median",],
                          summary_stat["3rd Qu.",],
                          summary_stat["Max.",]),
                 labels=c('1','2','3','4','5')))

pc1_dist <- ggplot(dist, aes(x = factor(PC1_dist), fill=Location)) +
  geom_bar(position="fill") + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  theme_void() + theme(
    axis.title.x=element_blank(), axis.text.x=element_text(size=6),
    axis.title.y=element_text(size=5, angle = 90, vjust = 0.5),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x="Distance index from artefacts",
       y=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"))
pc1_dist

#p_dist <- pc1_dist / pc2_dist / pc3_dist / pc4_dist / pc5_dist &
#  theme(plot.margin = unit(c(5,0,5,0), "pt"))

p_dist <- ggplot(dist, aes(x = factor(dist_cat), fill=Location)) +
  geom_bar(position="fill", alpha=.75) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  theme_void() + theme(
    axis.title.x=element_text(size=9, vjust = -1.5),
    axis.text.x=element_text(size=8, vjust = -.5),
    #axis.title.y=element_text(size=8, angle = 90, vjust = 0.5),
    axis.title.y=element_blank(),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.1,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x="Distance index to artefacts")
p_dist

distance_index <- plot_spacer()/p_dist/plot_spacer() +
  plot_layout(heights = c(.1,3.1,.1))
distance_index

pdf(here("analysis","figures","Figure_4","Fig4-a.pdf"), width=5.5, height=3.5)
(E_T_PCA_1b | distance_index) +
  plot_layout(widths = c(4, 2))
dev.off()


## PCs
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[142:147,"PC1"], xend=d_pca[142:147,"PC1"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"))
PC2 <- ggplot(d_pca, aes(x = PC2, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[142:147,"PC2"], xend=d_pca[142:147,"PC2"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
PC3 <- ggplot(d_pca, aes(x = PC3, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[142:147,"PC3"], xend=d_pca[142:147,"PC3"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC3 (",round(eig["Dim.3","variance.percent"], digits = 1),"%)"))
PC4 <- ggplot(d_pca, aes(x = PC4, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[142:147,"PC4"], xend=d_pca[142:147,"PC4"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC4 (",round(eig["Dim.4","variance.percent"], digits = 1),"%)"))
PC5 <- ggplot(d_pca, aes(x = PC5, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[142:147,"PC5"], xend=d_pca[142:147,"PC5"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC5 (",round(eig["Dim.5","variance.percent"], digits = 1),"%)"))

PCs <- PC1/PC2/PC3/PC4/PC5 & theme(plot.margin = unit(c(5,5,5,5), "pt"))

pdf(here("analysis","figures","Figure_4","Fig4-a-PCs.pdf"), width=3, height=4)
PCs
dev.off()

#### Fig 4b ####
## Emae_Taumako PCA 2
OIB <- full_join(q12,q13) %>% dplyr::select(
  Sample,Location,SiO2,TiO2,MgO,CaO,Na2O,K2O,
  Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Hf,Eu,Y,Yb)
is.na(OIB) <- sapply(OIB, is.infinite) #replace Inf by NA
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-08","E-11-08dup","T-12-06","T-12-06dup","T-12-07","T-12-08",
  "T-12-09","T-12-10")) %>% mutate(Location = case_when(
    grepl("E-11-08", Sample) ~ "E-11-08",
    grepl("E-11-08dup", Sample) ~ "E-11-08dup",
    grepl("T-12-06", Sample) ~ "T-12-06",
    grepl("T-12-06dup", Sample) ~ "T-12-06dup",
    grepl("T-12-07", Sample) ~ "T-12-07",
    grepl("T-12-08", Sample) ~ "T-12-08",
    grepl("T-12-09", Sample) ~ "T-12-09",
    grepl("T-12-10", Sample) ~ "T-12-10")) %>% dplyr::select(
      Sample,Location,SiO2,TiO2,MgO,CaO,Na2O,K2O,
      Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Hf,Eu,Y,Yb)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB[,5:23], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
E_T_PCA_2a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = OIB$Location, fill.ind = OIB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-6, 9)) + scale_y_continuous(limits=c(-4.5, 3.7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
E_T_PCA_2a
pdf(here("analysis","figures","Figure_4","Fig4-b-PCA.pdf"), width=3.5, height=3.5)
E_T_PCA_2a
dev.off()

pred <- stats::predict(res.pca, s[,5:23])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(OIB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

E_T_PCA_2b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-6, 9)) + scale_y_continuous(limits=c(-4.5, 3.7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
E_T_PCA_2b

# PC values > distance to artefacts (individual or median of group)
# distance within the 5 most significant PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:81,"Sample"]),
  Location = c(d_pca[1:81,"Location"]),
  PC1 = c(sqrt(((median(d_pca[82:87,"PC1"]))-d_pca[1:81,"PC1"])^2)),
  PC2 = c(sqrt(((median(d_pca[82:87,"PC2"]))-d_pca[1:81,"PC2"])^2)),
  PC3 = c(sqrt(((median(d_pca[82:87,"PC3"]))-d_pca[1:81,"PC3"])^2)),
  PC4 = c(sqrt(((median(d_pca[82:87,"PC4"]))-d_pca[1:81,"PC4"])^2)),
  PC5 = c(sqrt(((median(d_pca[82:87,"PC5"]))-d_pca[1:81,"PC5"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig["Dim.1","variance.percent"])+(PC2*eig["Dim.2","variance.percent"])+
      (PC3*eig["Dim.3","variance.percent"])+(PC4*eig["Dim.4","variance.percent"])+
      (PC4*eig["Dim.5","variance.percent"])) /
      (eig["Dim.1","variance.percent"]+eig["Dim.2","variance.percent"]+
         eig["Dim.3","variance.percent"]+eig["Dim.4","variance.percent"]+
         eig["Dim.5","variance.percent"]))

# distances > distance index, from 1 (closer to artefact) to 5 (more distant)
# 1= min > (1st quartile/2)
# 2= (1st quartile/2) > 1st quartile
# 3= 1st quartile > median
# 4= median > 3rd quartile
# 5= 3rd quartile > max
summary_stat <- data.frame(unclass(summary(dist$weight_mean)), check.names = F)
dist <- dist %>% mutate(
  dist_cat = cut(weight_mean,
                 breaks=c(0,(summary_stat["1st Qu.",]/2),
                          summary_stat["1st Qu.",],
                          summary_stat["Median",],
                          summary_stat["3rd Qu.",],
                          summary_stat["Max.",]),
                 labels=c('1','2','3','4','5')))

p_dist <- ggplot(dist, aes(x = factor(dist_cat), fill=Location)) +
  geom_bar(position="fill", alpha=.75) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  theme_void() + theme(
    axis.title.x=element_text(size=9, vjust = -1.5),
    axis.text.x=element_text(size=8, vjust = -.5),
    axis.title.y=element_blank(),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.1,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x="Distance index to artefacts")

distance_index <- plot_spacer()/p_dist/plot_spacer() +
  plot_layout(heights = c(.1,3.1,.1))
distance_index

pdf(here("analysis","figures","Figure_4","Fig4-b.pdf"), width=5.5, height=3.5)
(E_T_PCA_2b | distance_index) +
  plot_layout(widths = c(4, 2))
dev.off()

## PCs
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[82:87,"PC1"], xend=d_pca[82:87,"PC1"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"))
PC2 <- ggplot(d_pca, aes(x = PC2, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[82:87,"PC2"], xend=d_pca[82:87,"PC2"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
PC3 <- ggplot(d_pca, aes(x = PC3, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[82:87,"PC3"], xend=d_pca[82:87,"PC3"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC3 (",round(eig["Dim.3","variance.percent"], digits = 1),"%)"))
PC4 <- ggplot(d_pca, aes(x = PC4, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[82:87,"PC4"], xend=d_pca[82:87,"PC4"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC4 (",round(eig["Dim.4","variance.percent"], digits = 1),"%)"))
PCs <- PC1/PC2/PC3/PC4/PC5 & theme(plot.margin = unit(c(5,5,5,5), "pt"))

pdf(here("analysis","figures","Figure_4","Fig4-b-PCs.pdf"), width=3, height=4)
PCs
dev.off()

#### Fig 4c ####
## K1224 PCA 1
OIB <- q13 %>% dplyr::select(
  Sample,Location,Nd143_Nd144,Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
is.na(OIB) <- sapply(OIB, is.infinite) #replace Inf by NA
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>%
  dplyr::filter(Sample %in% c("K-12-24")) %>%
  mutate(Location = case_when(grepl("K-12-24", Sample) ~ "K-12-24")) %>%
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
  scale_x_continuous(limits=c(-3, 2.2)) + scale_y_continuous(limits=c(-2, 4.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
pdf(here("analysis","figures","Figure_4","Fig4-c-PCA.pdf"), width=3.5, height=3.5)
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
  scale_x_continuous(limits=c(-3, 2.2)) + scale_y_continuous(limits=c(-2, 4.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))

## PCs
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[29,"PC1"], xend=d_pca[29,"PC1"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"))
PC2 <- ggplot(d_pca, aes(x = PC2, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[29,"PC2"], xend=d_pca[29,"PC2"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
PC3 <- ggplot(d_pca, aes(x = PC3, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[29,"PC3"], xend=d_pca[29,"PC3"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC3 (",round(eig["Dim.3","variance.percent"], digits = 1),"%)"))
PC4 <- ggplot(d_pca, aes(x = PC4, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[29,"PC4"], xend=d_pca[29,"PC4"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC4 (",round(eig["Dim.4","variance.percent"], digits = 1),"%)"))
PCs <- PC1/PC2/PC3/PC4 & theme(plot.margin = unit(c(5,0,5,0), "pt"))

pdf(here("analysis","figures","Figure_4","Fig4-c.pdf"), width=6, height=3.5)
K1224_PCA_1b | PCs
dev.off()


#### Fig 4d ####
## K1224 PCA 2
OIB <- q14 %>% dplyr::select(
  Sample,Location,TiO2,Al2O3,MgO,CaO,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Hf,Eu,Y,Yb)
is.na(OIB) <- sapply(OIB, is.infinite) #replace Inf by NA
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>%
  dplyr::filter(Sample %in% c("K-12-24")) %>%
  mutate(Location = case_when(grepl("K-12-24", Sample) ~ "K-12-24")) %>%
  dplyr::select(Sample,Location,
                TiO2,Al2O3,MgO,CaO,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Hf,Eu,Y,Yb)
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
  scale_x_continuous(limits=c(-5, 8)) + scale_y_continuous(limits=c(-6, 6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
pdf(here("analysis","figures","Figure_4","Fig4-d-PCA.pdf"), width=3.5, height=3.5)
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
  scale_x_continuous(limits=c(-5, 8)) + scale_y_continuous(limits=c(-6, 6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))


## PCs
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[56,"PC1"], xend=d_pca[56,"PC1"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"))
PC2 <- ggplot(d_pca, aes(x = PC2, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[56,"PC2"], xend=d_pca[56,"PC2"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
PC3 <- ggplot(d_pca, aes(x = PC3, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[56,"PC3"], xend=d_pca[56,"PC3"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC3 (",round(eig["Dim.3","variance.percent"], digits = 1),"%)"))
PC4 <- ggplot(d_pca, aes(x = PC4, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[56,"PC4"], xend=d_pca[56,"PC4"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC4 (",round(eig["Dim.4","variance.percent"], digits = 1),"%)"))
PCs <- PC1/PC2/PC3/PC4 & theme(plot.margin = unit(c(5,0,5,0), "pt"))

pdf(here("analysis","figures","Figure_4","Fig4-d.pdf"), width=6, height=3.5)
K1224_PCA_2b | PCs
dev.off()


#### Fig 4e ####
## K1225 PCA 1
OIB <- q15 %>% dplyr::select(
  Sample,Location,Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
is.na(OIB) <- sapply(OIB, is.infinite) #replace Inf by NA
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>%
  filter(Sample %in% c("K-12-25")) %>%
  mutate(Location = case_when(grepl("K-12-25", Sample) ~ "K-12-25")) %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)
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
  scale_x_continuous(limits=c(-7.5, 3)) + scale_y_continuous(limits=c(-2.2, 3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
pdf(here("analysis","figures","Figure_4","Fig4-e-PCA.pdf"), width=3.5, height=3.5)
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
  scale_x_continuous(limits=c(-7.5, 3)) + scale_y_continuous(limits=c(-2.2, 3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
## PCs
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[76,"PC1"], xend=d_pca[76,"PC1"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"))
PC2 <- ggplot(d_pca, aes(x = PC2, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[76,"PC2"], xend=d_pca[76,"PC2"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
PC3 <- ggplot(d_pca, aes(x = PC3, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[76,"PC3"], xend=d_pca[76,"PC3"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC3 (",round(eig["Dim.3","variance.percent"], digits = 1),"%)"))
PC4 <- ggplot(d_pca, aes(x = PC4, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[76,"PC4"], xend=d_pca[76,"PC4"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC4 (",round(eig["Dim.4","variance.percent"], digits = 1),"%)"))
PCs <- PC1/PC2/PC3/PC4 & theme(plot.margin = unit(c(5,0,5,0), "pt"))

pdf(here("analysis","figures","Figure_4","Fig4-e.pdf"), width=6, height=3.5)
K1225_PCA_1b | PCs
dev.off()


#### Fig 4f ####
## K1225 PCA 2
OIB <- q16 %>% dplyr::select(
  Sample,Location,TiO2,Al2O3,MgO,CaO,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Hf,Eu,Y,Yb)
#  Sample,Location,TiO2,MgO,CaO,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Hf,Eu,Y,Yb)
is.na(OIB) <- sapply(OIB, is.infinite) #replace Inf by NA
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>%
  dplyr::filter(Sample %in% c("K-12-25")) %>%
  mutate(Location = case_when(grepl("K-12-25", Sample) ~ "K-12-25")) %>%
  dplyr::select(Sample,Location,
                #TiO2,MgO,CaO,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Hf,Eu,Y,Yb)
                TiO2,Al2O3,MgO,CaO,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Hf,Eu,Y,Yb)
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
  scale_x_continuous(limits=c(-8, 8)) + scale_y_continuous(limits=c(-2.5, 2.7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
pdf(here("analysis","figures","Figure_4","Fig4-f-PCA.pdf"), width=3.5, height=3.5)
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
  scale_x_continuous(limits=c(-8, 8)) + scale_y_continuous(limits=c(-2.5, 2.7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K1225_PCA_2b

## PCs
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[107,"PC1"], xend=d_pca[107,"PC1"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"))
PC2 <- ggplot(d_pca, aes(x = PC2, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[107,"PC2"], xend=d_pca[107,"PC2"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
PC3 <- ggplot(d_pca, aes(x = PC3, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[107,"PC3"], xend=d_pca[107,"PC3"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC3 (",round(eig["Dim.3","variance.percent"], digits = 1),"%)"))
PC4 <- ggplot(d_pca, aes(x = PC4, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[107,"PC4"], xend=d_pca[107,"PC4"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC4 (",round(eig["Dim.4","variance.percent"], digits = 1),"%)"))
PCs <- PC1/PC2/PC3/PC4 & theme(plot.margin = unit(c(5,0,5,0), "pt"))

pdf(here("analysis","figures","Figure_4","Fig4-f.pdf"), width=6, height=3.5)
K1225_PCA_2b | PCs
dev.off()


#### Fig 4g ####
## K1226 PCA 1
OIB <- q17 %>% dplyr::select(
  Sample,Location,Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
is.na(OIB) <- sapply(OIB, is.infinite) #replace Inf by NA
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>%
  filter(Sample %in% c("K-12-26")) %>%
  mutate(Location = case_when(grepl("K-12-26", Sample) ~ "K-12-26")) %>%
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
  scale_x_continuous(limits=c(-3.2, 3)) + scale_y_continuous(limits=c(-1.5, 2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
pdf(here("analysis","figures","Figure_4","Fig4-g-PCA.pdf"), width=3.5, height=3.5)
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
  scale_x_continuous(limits=c(-3.2, 3)) + scale_y_continuous(limits=c(-1.5, 2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
## PCs
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[85,"PC1"], xend=d_pca[85,"PC1"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"))
PC2 <- ggplot(d_pca, aes(x = PC2, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[85,"PC2"], xend=d_pca[85,"PC2"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
PC3 <- ggplot(d_pca, aes(x = PC3, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[85,"PC3"], xend=d_pca[85,"PC3"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC3 (",round(eig["Dim.3","variance.percent"], digits = 1),"%)"))
PC4 <- ggplot(d_pca, aes(x = PC4, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[85,"PC4"], xend=d_pca[85,"PC4"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC4 (",round(eig["Dim.4","variance.percent"], digits = 1),"%)"))
PCs <- PC1/PC2/PC3/PC4 & theme(plot.margin = unit(c(5,0,5,0), "pt"))

pdf(here("analysis","figures","Figure_4","Fig4-g.pdf"), width=6, height=3.5)
K1226_PCA_1b | PCs
dev.off()


#### Fig 4h ####
## K1226 PCA 2
OIB <- q18 %>% dplyr::select(
  Sample,Location,
  SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Hf,Eu,Yb)
is.na(OIB) <- sapply(OIB, is.infinite) #replace Inf by NA
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>%
  dplyr::filter(Sample %in% c("K-12-26")) %>%
  mutate(Location = case_when(grepl("K-12-26", Sample) ~ "K-12-26")) %>%
  dplyr::select(
    Sample,Location,
    SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Hf,Eu,Yb)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(OIB[,3:19], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
K1226_PCA_2a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = OIB$Location, fill.ind = OIB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-8, 7.5)) + scale_y_continuous(limits=c(-4.6, 3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
K1226_PCA_2a
pdf(here("analysis","figures","Figure_4","Fig4-h-PCA.pdf"), width=3.5, height=3.5)
K1226_PCA_2a
dev.off()

pred <- stats::predict(res.pca, s[,3:19])
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
  scale_x_continuous(limits=c(-8, 7.5)) + scale_y_continuous(limits=c(-4.6, 3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K1226_PCA_2b

## PCs
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[76,"PC1"], xend=d_pca[76,"PC1"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"))
PC2 <- ggplot(d_pca, aes(x = PC2, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[76,"PC2"], xend=d_pca[76,"PC2"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
PC3 <- ggplot(d_pca, aes(x = PC3, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[76,"PC3"], xend=d_pca[76,"PC3"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC3 (",round(eig["Dim.3","variance.percent"], digits = 1),"%)"))
PC4 <- ggplot(d_pca, aes(x = PC4, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[76,"PC4"], xend=d_pca[76,"PC4"],
           y=0, yend=Inf, col="red", size=.75) +
  scale_color_manual(values = cols) + scale_fill_manual(values = cols) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_void() + theme(
    axis.text.x=element_text(size=6), axis.title.x=element_text(size=6),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.05,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("PC4 (",round(eig["Dim.4","variance.percent"], digits = 1),"%)"))
PCs <- PC1/PC2/PC3/PC4 & theme(plot.margin = unit(c(5,0,5,0), "pt"))

pdf(here("analysis","figures","Figure_4","Fig4-h.pdf"), width=6, height=3.5)
K1226_PCA_2b | PCs
dev.off()


