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

median(d_pca[142:147,"PC1"])
median(d_pca[142:147,"PC2"])

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

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:141,"Sample"]),
  Location = c(d_pca[1:141,"Location"]),
  PC1 = c(sqrt(((median(d_pca[142:147,"PC1"]))-d_pca[1:141,"PC1"])^2)),
  PC2 = c(sqrt(((median(d_pca[142:147,"PC2"]))-d_pca[1:141,"PC2"])^2)),
  PC3 = c(sqrt(((median(d_pca[142:147,"PC3"]))-d_pca[1:141,"PC3"])^2)),
  PC4 = c(sqrt(((median(d_pca[142:147,"PC4"]))-d_pca[1:141,"PC4"])^2)),
  PC5 = c(sqrt(((median(d_pca[142:147,"PC5"]))-d_pca[1:141,"PC5"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+
      (PC4*eig[4,2])+(PC5*eig[5,2])) / (sum(eig[1:5,2])))

# distances > distance index, from 1 (closer to artefact) to 5 (more distant)
# 1= min > (1st quartile/2)
# 2= (1st quartile/2) > 1st quartile
# 3= 1st quartile > median
# 4= median > 3rd quartile
# 5= 3rd quartile > max
summary(dist$PC1)
summary_stat <- data.frame(unclass(summary(dist$weight_mean)), check.names = F)
dist <- dist %>% mutate(
  dist_cat = cut(
    weight_mean,
    breaks=c(0,
             (summary_stat["1st Qu.",]-((summary_stat["1st Qu.",]-summary_stat["Min.",])/2)),
             summary_stat["1st Qu.",],
             summary_stat["Median",],
             summary_stat["3rd Qu.",],
             summary_stat["Max.",]),
    labels=c('1','2','3','4','5')))

dist <- dist %>% mutate(
  PC1_dist = cut(PC1,
                 breaks=c(0,.2,.3,.66,1.2,4),
                 labels=c('1','2','3','4','5')),
  dist_cat = cut(weight_mean,
                 breaks=c(0,
                          (summary_stat["1st Qu.",]/2),
                          summary_stat["1st Qu.",],
                          summary_stat["Median",],
                          summary_stat["3rd Qu.",],
                          summary_stat["Max.",]+.5),
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

distance_index <- ggplot(dist, aes(x = factor(dist_cat), fill=Location)) +
  geom_bar(position="fill", alpha=.75) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  theme_void() + theme(
    axis.title.x=element_text(size=9, vjust = -1.5),
    axis.text.x=element_text(size=8, vjust = -.5),
    axis.title.y=element_blank(),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.1,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("Distance index","\n","(Total variance)"))
distance_index

distance_index <- distance_index/plot_spacer() + plot_layout(heights = c(4,.1))

pdf(here("analysis","figures","Figure_4","Fig4-a.pdf"), width=5.5, height=3.5)
(E_T_PCA_1b | distance_index) + plot_layout(widths = c(4, 2))
dev.off()

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

pdf(here("analysis","figures","Figure_4","Fig4-a-SI.pdf"), width=6, height=3.5)
(E_T_PCA_1b | PCs) + plot_layout(widths = c(3.5, 2.5))
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
  scale_color_manual(values=cols) + scale_x_continuous(limits=c(-6, 9)) +
  scale_y_continuous(limits=c(-4.5, 3.7), breaks=c(-3,0,3)) +
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
  geom_point(aes(x=median(d_pca[82:87,"PC1"]),
                 y=median(d_pca[82:87,"PC2"])), shape=3, color="red") +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-6, 9)) +
  scale_y_continuous(limits=c(-4.5, 3.7), breaks=c(-3,0,3)) +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
E_T_PCA_2b

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:81,"Sample"]),
  Location = c(d_pca[1:81,"Location"]),
  PC1 = c(sqrt(((median(d_pca[82:87,"PC1"]))-d_pca[1:81,"PC1"])^2)),
  PC2 = c(sqrt(((median(d_pca[82:87,"PC2"]))-d_pca[1:81,"PC2"])^2)),
  PC3 = c(sqrt(((median(d_pca[82:87,"PC3"]))-d_pca[1:81,"PC3"])^2)),
  PC4 = c(sqrt(((median(d_pca[82:87,"PC4"]))-d_pca[1:81,"PC4"])^2)),
  PC5 = c(sqrt(((median(d_pca[82:87,"PC5"]))-d_pca[1:81,"PC5"])^2)),
  PC6 = c(sqrt(((median(d_pca[82:87,"PC6"]))-d_pca[1:81,"PC6"])^2)),
  PC7 = c(sqrt(((median(d_pca[82:87,"PC7"]))-d_pca[1:81,"PC7"])^2)),
  PC8 = c(sqrt(((median(d_pca[82:87,"PC8"]))-d_pca[1:81,"PC8"])^2)),
  PC9 = c(sqrt(((median(d_pca[82:87,"PC9"]))-d_pca[1:81,"PC9"])^2)),
  PC10 = c(sqrt(((median(d_pca[82:87,"PC10"]))-d_pca[1:81,"PC10"])^2)),
  PC11 = c(sqrt(((median(d_pca[82:87,"PC11"]))-d_pca[1:81,"PC11"])^2)),
  PC12 = c(sqrt(((median(d_pca[82:87,"PC12"]))-d_pca[1:81,"PC12"])^2)),
  PC13 = c(sqrt(((median(d_pca[82:87,"PC13"]))-d_pca[1:81,"PC13"])^2)),
  PC14 = c(sqrt(((median(d_pca[82:87,"PC14"]))-d_pca[1:81,"PC14"])^2)),
  PC15 = c(sqrt(((median(d_pca[82:87,"PC15"]))-d_pca[1:81,"PC15"])^2)),
  PC16 = c(sqrt(((median(d_pca[82:87,"PC16"]))-d_pca[1:81,"PC16"])^2)),
  PC17 = c(sqrt(((median(d_pca[82:87,"PC17"]))-d_pca[1:81,"PC17"])^2)),
  PC18 = c(sqrt(((median(d_pca[82:87,"PC18"]))-d_pca[1:81,"PC18"])^2)),
  PC19 = c(sqrt(((median(d_pca[82:87,"PC19"]))-d_pca[1:81,"PC19"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])+(PC8*eig[8,2])+(PC9*eig[9,2])+
      (PC10*eig[10,2])+(PC11*eig[11,2])+(PC12*eig[12,2])+(PC13*eig[13,2])+
      (PC14*eig[14,2])+(PC15*eig[15,2])+(PC16*eig[16,2])+(PC17*eig[17,2])+
      (PC18*eig[18,2])+(PC19*eig[19,2])) / (sum(eig[1:19,2])))

# distances > distance index, from 1 (closer to artefact) to 5 (more distant)
# 1= min > (1st quartile/2)
# 2= (1st quartile/2) > 1st quartile
# 3= 1st quartile > median
# 4= median > 3rd quartile
# 5= 3rd quartile > max
summary_stat <- data.frame(unclass(summary(dist$weight_mean)), check.names = F)
dist <- dist %>% mutate(
  dist_cat = cut(
    weight_mean,
    breaks=c(0,
             (summary_stat["1st Qu.",]-((summary_stat["1st Qu.",]-summary_stat["Min.",])/2)),
             summary_stat["1st Qu.",],
             summary_stat["Median",],
             summary_stat["3rd Qu.",],
             summary_stat["Max.",]),
    labels=c('1','2','3','4','5')))

distance_index <- ggplot(dist, aes(x = factor(dist_cat), fill=Location)) +
  geom_bar(position="fill", alpha=.75) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  theme_void() + theme(
    axis.title.x=element_text(size=9, vjust = -1.5),
    axis.text.x=element_text(size=8, vjust = -.5),
    axis.title.y=element_blank(),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.1,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("Distance index","\n","(Total variance)"))
distance_index

distance_index <- distance_index/plot_spacer() + plot_layout(heights = c(4,.1))

pdf(here("analysis","figures","Figure_4","Fig4-b.pdf"), width=5.5, height=3.5)
(E_T_PCA_2b | distance_index) + plot_layout(widths = c(4, 2))
dev.off()

## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[82:87,"PC1"], xend=d_pca[82:87,"PC1"],
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
  annotate("segment", x=d_pca[82:87,"PC2"], xend=d_pca[82:87,"PC2"],
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
  annotate("segment", x=d_pca[82:87,"PC3"], xend=d_pca[82:87,"PC3"],
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
  annotate("segment", x=d_pca[82:87,"PC4"], xend=d_pca[82:87,"PC4"],
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
  annotate("segment", x=d_pca[82:87,"PC5"], xend=d_pca[82:87,"PC5"],
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

pdf(here("analysis","figures","Figure_4","Fig4-b-SI.pdf"), width=6, height=3.5)
(E_T_PCA_2b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()


#### Fig 4c ####
## K1224 PCA 1
OIB <- q14 %>% dplyr::select(
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
  scale_x_continuous(limits=c(-3.5, 4.5)) + scale_y_continuous(limits=c(-2.5, 3.5)) +
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
  scale_x_continuous(limits=c(-3.5, 4.5)) + scale_y_continuous(limits=c(-2.5, 3.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K1224_PCA_1b

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:337,"Sample"]),
  Location = c(d_pca[1:337,"Location"]),
  PC1 = c(sqrt(((median(d_pca[338,"PC1"]))-d_pca[1:337,"PC1"])^2)),
  PC2 = c(sqrt(((median(d_pca[338,"PC2"]))-d_pca[1:337,"PC2"])^2)),
  PC3 = c(sqrt(((median(d_pca[338,"PC3"]))-d_pca[1:337,"PC3"])^2)),
  PC4 = c(sqrt(((median(d_pca[338,"PC4"]))-d_pca[1:337,"PC4"])^2)),
  PC5 = c(sqrt(((median(d_pca[338,"PC5"]))-d_pca[1:337,"PC5"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+
      (PC4*eig[4,2])+(PC5*eig[5,2])) / (sum(eig[1:5,2])))

# distances > distance index, from 1 (closer to artefact) to 5 (more distant)
# 1= min > (1st quartile/2)
# 2= (1st quartile/2) > 1st quartile
# 3= 1st quartile > median
# 4= median > 3rd quartile
# 5= 3rd quartile > max
summary_stat <- data.frame(unclass(summary(dist$weight_mean)), check.names = F)
dist <- dist %>% mutate(
  dist_cat = cut(
    weight_mean,
    breaks=c(0,
             (summary_stat["1st Qu.",]-((summary_stat["1st Qu.",]-summary_stat["Min.",])/2)),
             summary_stat["1st Qu.",],
             summary_stat["Median",],
             summary_stat["3rd Qu.",],
             summary_stat["Max.",]),
    labels=c('1','2','3','4','5')))

distance_index <- ggplot(dist, aes(x = factor(dist_cat), fill=Location)) +
  geom_bar(position="fill", alpha=.75) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  theme_void() + theme(
    axis.title.x=element_text(size=9, vjust = -1.5),
    axis.text.x=element_text(size=8, vjust = -.5),
    axis.title.y=element_blank(),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.1,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("Distance index","\n","(Total variance)"))
distance_index

distance_index <- distance_index/plot_spacer() + plot_layout(heights = c(4,.1))

pdf(here("analysis","figures","Figure_4","Fig4-c.pdf"), width=5.5, height=3.5)
(K1224_PCA_1b | distance_index) + plot_layout(widths = c(4, 2))
dev.off()


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

pdf(here("analysis","figures","Figure_4","Fig4-c-SI.pdf"), width=6, height=3.5)
(K1224_PCA_1b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()


#### Fig 4d ####
## K1224 PCA 2
OIB <- q15 %>% dplyr::select(
  Sample,Location,TiO2,Al2O3,MgO,CaO,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Hf,Eu,Y,Yb)
is.na(OIB) <- sapply(OIB, is.infinite) #replace Inf by NA
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-24")) %>%
  mutate(Location = case_when(grepl("K-12-24", Sample) ~ "K-12-24")) %>%
  dplyr::select(
    Sample,Location,TiO2,Al2O3,MgO,CaO,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Hf,Eu,Y,Yb)
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
  scale_x_continuous(limits=c(-8, 5)) + scale_y_continuous(limits=c(-6, 4.2)) +
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
  scale_x_continuous(limits=c(-8, 5)) + scale_y_continuous(limits=c(-6, 4.2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K1224_PCA_2b

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:111,"Sample"]),
  Location = c(d_pca[1:111,"Location"]),
  PC1 = c(sqrt(((d_pca[112,"PC1"])-d_pca[1:111,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[112,"PC2"])-d_pca[1:111,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[112,"PC3"])-d_pca[1:111,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[112,"PC4"])-d_pca[1:111,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[112,"PC5"])-d_pca[1:111,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[112,"PC6"])-d_pca[1:111,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[112,"PC7"])-d_pca[1:111,"PC7"])^2)),
  PC8 = c(sqrt(((d_pca[112,"PC8"])-d_pca[1:111,"PC8"])^2)),
  PC9 = c(sqrt(((d_pca[112,"PC9"])-d_pca[1:111,"PC9"])^2)),
  PC10 = c(sqrt(((d_pca[112,"PC10"])-d_pca[1:111,"PC10"])^2)),
  PC11 = c(sqrt(((d_pca[112,"PC11"])-d_pca[1:111,"PC11"])^2)),
  PC12 = c(sqrt(((d_pca[112,"PC12"])-d_pca[1:111,"PC12"])^2)),
  PC13 = c(sqrt(((d_pca[112,"PC13"])-d_pca[1:111,"PC13"])^2)),
  PC14 = c(sqrt(((d_pca[112,"PC14"])-d_pca[1:111,"PC14"])^2)),
  PC15 = c(sqrt(((d_pca[112,"PC15"])-d_pca[1:111,"PC15"])^2)),
  PC16 = c(sqrt(((d_pca[112,"PC16"])-d_pca[1:111,"PC16"])^2)),
  PC17 = c(sqrt(((d_pca[112,"PC17"])-d_pca[1:111,"PC17"])^2)),
  PC18 = c(sqrt(((d_pca[112,"PC18"])-d_pca[1:111,"PC18"])^2)),
  PC19 = c(sqrt(((d_pca[112,"PC19"])-d_pca[1:111,"PC19"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])+(PC8*eig[8,2])+(PC9*eig[9,2])+
      (PC10*eig[10,2])+(PC11*eig[11,2])+(PC12*eig[12,2])+(PC13*eig[13,2])+
      (PC14*eig[14,2])+(PC15*eig[15,2])+(PC16*eig[16,2])+(PC17*eig[17,2])+
      (PC18*eig[18,2])+(PC19*eig[19,2])) / (sum(eig[1:19,2])))

# distances > distance index, from 1 (closer to artefact) to 5 (more distant)
# 1= min > (1st quartile/2)
# 2= (1st quartile/2) > 1st quartile
# 3= 1st quartile > median
# 4= median > 3rd quartile
# 5= 3rd quartile > max
summary_stat <- data.frame(unclass(summary(dist$weight_mean)), check.names = F)
dist <- dist %>% mutate(
  dist_cat = cut(
    weight_mean,
    breaks=c(0,
             (summary_stat["1st Qu.",]-((summary_stat["1st Qu.",]-summary_stat["Min.",])/2)),
             summary_stat["1st Qu.",],
             summary_stat["Median",],
             summary_stat["3rd Qu.",],
             summary_stat["Max.",]),
    labels=c('1','2','3','4','5')))

distance_index <- ggplot(dist, aes(x = factor(dist_cat), fill=Location)) +
  geom_bar(position="fill", alpha=.75) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  theme_void() + theme(
    axis.title.x=element_text(size=9, vjust = -1.5),
    axis.text.x=element_text(size=8, vjust = -.5),
    axis.title.y=element_blank(),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.1,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("Distance index","\n","(Total variance)"))
distance_index

distance_index <- distance_index/plot_spacer() + plot_layout(heights = c(4,.1))

pdf(here("analysis","figures","Figure_4","Fig4-d.pdf"), width=5.5, height=3.5)
(K1224_PCA_2b | distance_index) + plot_layout(widths = c(4, 2))
dev.off()


## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[112,"PC1"], xend=d_pca[112,"PC1"],
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
  annotate("segment", x=d_pca[112,"PC2"], xend=d_pca[112,"PC2"],
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
  annotate("segment", x=d_pca[112,"PC3"], xend=d_pca[112,"PC3"],
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
  annotate("segment", x=d_pca[112,"PC4"], xend=d_pca[112,"PC4"],
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
  annotate("segment", x=d_pca[112,"PC5"], xend=d_pca[112,"PC5"],
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

pdf(here("analysis","figures","Figure_4","Fig4-d-SI.pdf"), width=6, height=3.5)
(K1224_PCA_2b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()


#### Fig 4e ####
## K1225 PCA 1
OIB <- q16 %>% dplyr::select(
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
  scale_x_continuous(limits=c(-3.5, 3.5)) + scale_y_continuous(limits=c(-2, 2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
K1225_PCA_1a
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
  scale_x_continuous(limits=c(-3.5, 3.5)) + scale_y_continuous(limits=c(-2, 2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K1225_PCA_1b

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:94,"Sample"]),
  Location = c(d_pca[1:94,"Location"]),
  PC1 = c(sqrt(((median(d_pca[95,"PC1"]))-d_pca[1:94,"PC1"])^2)),
  PC2 = c(sqrt(((median(d_pca[95,"PC2"]))-d_pca[1:94,"PC2"])^2)),
  PC3 = c(sqrt(((median(d_pca[95,"PC3"]))-d_pca[1:94,"PC3"])^2)),
  PC4 = c(sqrt(((median(d_pca[95,"PC4"]))-d_pca[1:94,"PC4"])^2)),
  PC5 = c(sqrt(((median(d_pca[95,"PC5"]))-d_pca[1:94,"PC5"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+
      (PC4*eig[4,2])+(PC5*eig[5,2])) / (sum(eig[1:5,2])))

# distances > distance index, from 1 (closer to artefact) to 5 (more distant)
# 1= min > (1st quartile/2)
# 2= (1st quartile/2) > 1st quartile
# 3= 1st quartile > median
# 4= median > 3rd quartile
# 5= 3rd quartile > max
summary_stat <- data.frame(unclass(summary(dist$weight_mean)), check.names = F)
dist <- dist %>% mutate(
  dist_cat = cut(
    weight_mean,
    breaks=c(0,
             (summary_stat["1st Qu.",]-((summary_stat["1st Qu.",]-summary_stat["Min.",])/2)),
             summary_stat["1st Qu.",],
             summary_stat["Median",],
             summary_stat["3rd Qu.",],
             summary_stat["Max.",]),
    labels=c('1','2','3','4','5')))

distance_index <- ggplot(dist, aes(x = factor(dist_cat), fill=Location)) +
  geom_bar(position="fill", alpha=.75) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  theme_void() + theme(
    axis.title.x=element_text(size=9, vjust = -1.5),
    axis.text.x=element_text(size=8, vjust = -.5),
    axis.title.y=element_blank(),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.1,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("Distance index","\n","(Total variance)"))
distance_index

distance_index <- distance_index/plot_spacer() + plot_layout(heights = c(4,.1))

pdf(here("analysis","figures","Figure_4","Fig4-e.pdf"), width=5.5, height=3.5)
(K1225_PCA_1b | distance_index) + plot_layout(widths = c(4, 2))
dev.off()

## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[95,"PC1"], xend=d_pca[95,"PC1"],
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
  annotate("segment", x=d_pca[95,"PC2"], xend=d_pca[95,"PC2"],
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
  annotate("segment", x=d_pca[95,"PC3"], xend=d_pca[95,"PC3"],
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
  annotate("segment", x=d_pca[95,"PC4"], xend=d_pca[95,"PC4"],
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

pdf(here("analysis","figures","Figure_4","Fig4-e-SI.pdf"), width=6, height=3.5)
(K1225_PCA_1b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()


#### Fig 4f ####
## K1225 PCA 2
OIB <- q17 %>% dplyr::select(
  Sample,Location,SiO2,Al2O3,MgO,CaO,V,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Y,Yb,Pb)
is.na(OIB) <- sapply(OIB, is.infinite) #replace Inf by NA
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA
OIB %>% group_by(Location) %>% tally()

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-25")) %>%
  mutate(Location = case_when(grepl("K-12-25", Sample) ~ "K-12-25")) %>%
  dplyr::select(
    Sample,Location,SiO2,Al2O3,MgO,CaO,V,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Y,Yb,Pb)
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
  scale_x_continuous(limits=c(-7, 9)) + scale_y_continuous(limits=c(-4, 4)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
K1225_PCA_2a
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
  scale_x_continuous(limits=c(-7, 9)) + scale_y_continuous(limits=c(-4, 4)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K1225_PCA_2b

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:29,"Sample"]),
  Location = c(d_pca[1:29,"Location"]),
  PC1 = c(sqrt(((d_pca[30,"PC1"])-d_pca[1:29,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[30,"PC2"])-d_pca[1:29,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[30,"PC3"])-d_pca[1:29,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[30,"PC4"])-d_pca[1:29,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[30,"PC5"])-d_pca[1:29,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[30,"PC6"])-d_pca[1:29,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[30,"PC7"])-d_pca[1:29,"PC7"])^2)),
  PC8 = c(sqrt(((d_pca[30,"PC8"])-d_pca[1:29,"PC8"])^2)),
  PC9 = c(sqrt(((d_pca[30,"PC9"])-d_pca[1:29,"PC9"])^2)),
  PC10 = c(sqrt(((d_pca[30,"PC10"])-d_pca[1:29,"PC10"])^2)),
  PC11 = c(sqrt(((d_pca[30,"PC11"])-d_pca[1:29,"PC11"])^2)),
  PC12 = c(sqrt(((d_pca[30,"PC12"])-d_pca[1:29,"PC12"])^2)),
  PC13 = c(sqrt(((d_pca[30,"PC13"])-d_pca[1:29,"PC13"])^2)),
  PC14 = c(sqrt(((d_pca[30,"PC14"])-d_pca[1:29,"PC14"])^2)),
  PC15 = c(sqrt(((d_pca[30,"PC15"])-d_pca[1:29,"PC15"])^2)),
  PC16 = c(sqrt(((d_pca[30,"PC16"])-d_pca[1:29,"PC16"])^2)),
  PC17 = c(sqrt(((d_pca[30,"PC17"])-d_pca[1:29,"PC17"])^2)),
  PC18 = c(sqrt(((d_pca[30,"PC18"])-d_pca[1:29,"PC18"])^2)),
  PC19 = c(sqrt(((d_pca[30,"PC19"])-d_pca[1:29,"PC19"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])+(PC8*eig[8,2])+(PC9*eig[9,2])+
      (PC10*eig[10,2])+(PC11*eig[11,2])+(PC12*eig[12,2])+(PC13*eig[13,2])+
      (PC14*eig[14,2])+(PC15*eig[15,2])+(PC16*eig[16,2])+(PC17*eig[17,2])+
      (PC18*eig[18,2])+(PC19*eig[19,2])) / (sum(eig[1:19,2])))

# distances > distance index, from 1 (closer to artefact) to 5 (more distant)
# 1= min > (1st quartile/2)
# 2= (1st quartile/2) > 1st quartile
# 3= 1st quartile > median
# 4= median > 3rd quartile
# 5= 3rd quartile > max
summary_stat <- data.frame(unclass(summary(dist$weight_mean)), check.names = F)
dist <- dist %>% mutate(
  dist_cat = cut(
    weight_mean,
    breaks=c(0,
             (summary_stat["1st Qu.",]-((summary_stat["1st Qu.",]-summary_stat["Min.",])/2)),
             summary_stat["1st Qu.",],
             summary_stat["Median",],
             summary_stat["3rd Qu.",],
             summary_stat["Max.",]),
    labels=c('1','2','3','4','5')))

distance_index <- ggplot(dist, aes(x = factor(dist_cat), fill=Location)) +
  geom_bar(position="fill", alpha=.75) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  theme_void() + theme(
    axis.title.x=element_text(size=9, vjust = -1.5),
    axis.text.x=element_text(size=8, vjust = -.5),
    axis.title.y=element_blank(),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.1,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("Distance index","\n","(Total variance)"))
distance_index

distance_index <- distance_index/plot_spacer() + plot_layout(heights = c(4,.1))

pdf(here("analysis","figures","Figure_4","Fig4-f.pdf"), width=5.5, height=3.5)
(K1225_PCA_2b | distance_index) + plot_layout(widths = c(4, 2))
dev.off()

## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[30,"PC1"], xend=d_pca[30,"PC1"],
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
  annotate("segment", x=d_pca[30,"PC2"], xend=d_pca[30,"PC2"],
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
  annotate("segment", x=d_pca[30,"PC3"], xend=d_pca[30,"PC3"],
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
  annotate("segment", x=d_pca[30,"PC4"], xend=d_pca[30,"PC4"],
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
  annotate("segment", x=d_pca[30,"PC5"], xend=d_pca[30,"PC5"],
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

pdf(here("analysis","figures","Figure_4","Fig4-f-SI.pdf"), width=6, height=3.5)
(K1225_PCA_2b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()


#### Fig 4g ####
## K1226 PCA 1
OIB <- q18 %>% dplyr::select(
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
  scale_x_continuous(limits=c(-7.5, 3.5)) + scale_y_continuous(limits=c(-3.5, 3.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
K1226_PCA_1a
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
  scale_x_continuous(limits=c(-7.5, 3.5)) + scale_y_continuous(limits=c(-3.5, 3.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K1226_PCA_1b

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:84,"Sample"]),
  Location = c(d_pca[1:84,"Location"]),
  PC1 = c(sqrt(((median(d_pca[85,"PC1"]))-d_pca[1:84,"PC1"])^2)),
  PC2 = c(sqrt(((median(d_pca[85,"PC2"]))-d_pca[1:84,"PC2"])^2)),
  PC3 = c(sqrt(((median(d_pca[85,"PC3"]))-d_pca[1:84,"PC3"])^2)),
  PC4 = c(sqrt(((median(d_pca[85,"PC4"]))-d_pca[1:84,"PC4"])^2)),
  PC5 = c(sqrt(((median(d_pca[85,"PC5"]))-d_pca[1:84,"PC5"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+
      (PC4*eig[4,2])+(PC5*eig[5,2])) / (sum(eig[1:5,2])))

# distances > distance index, from 1 (closer to artefact) to 5 (more distant)
# 1= min > (1st quartile/2)
# 2= (1st quartile/2) > 1st quartile
# 3= 1st quartile > median
# 4= median > 3rd quartile
# 5= 3rd quartile > max
summary_stat <- data.frame(unclass(summary(dist$weight_mean)), check.names = F)
dist <- dist %>% mutate(
  dist_cat = cut(
    weight_mean,
    breaks=c(0,
             (summary_stat["1st Qu.",]-((summary_stat["1st Qu.",]-summary_stat["Min.",])/2)),
             summary_stat["1st Qu.",],
             summary_stat["Median",],
             summary_stat["3rd Qu.",],
             summary_stat["Max.",]),
    labels=c('1','2','3','4','5')))

distance_index <- ggplot(dist, aes(x = factor(dist_cat), fill=Location)) +
  geom_bar(position="fill", alpha=.75) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  theme_void() + theme(
    axis.title.x=element_text(size=9, vjust = -1.5),
    axis.text.x=element_text(size=8, vjust = -.5),
    axis.title.y=element_blank(),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.1,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("Distance index","\n","(Total variance)"))
distance_index

distance_index <- distance_index/plot_spacer() + plot_layout(heights = c(4,.2))

pdf(here("analysis","figures","Figure_4","Fig4-g.pdf"), width=5.5, height=3.5)
(K1226_PCA_1b | distance_index) + plot_layout(widths = c(4, 2))
dev.off()

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

pdf(here("analysis","figures","Figure_4","Fig4-g-SI.pdf"), width=6, height=3.5)
(K1226_PCA_1b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()


#### Fig 4h ####
## K1226 PCA 2
OIB <- q19 %>% dplyr::select(
  Sample,Location,TiO2,Na2O,MgO,V,Cs,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Y,Yb,Pb)

is.na(OIB) <- sapply(OIB, is.infinite) #replace Inf by NA
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>%
  dplyr::filter(Sample %in% c("K-12-26")) %>%
  mutate(Location = case_when(grepl("K-12-26", Sample) ~ "K-12-26")) %>%
  dplyr::select(
    #Sample,Location,TiO2,Al2O3,MgO,CaO,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Hf,Eu,Y,Yb)
    Sample,Location,TiO2,Na2O,MgO,V,Cs,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Y,Yb,Pb)
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
  #scale_x_continuous(limits=c(-8, 5.5)) + scale_y_continuous(limits=c(-2, 2.5)) +
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
  scale_x_continuous(limits=c(-8, 5.5)) + scale_y_continuous(limits=c(-2, 2.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K1226_PCA_2b

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:34,"Sample"]),
  Location = c(d_pca[1:34,"Location"]),
  PC1 = c(sqrt(((d_pca[35,"PC1"])-d_pca[1:34,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[35,"PC2"])-d_pca[1:34,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[35,"PC3"])-d_pca[1:34,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[35,"PC4"])-d_pca[1:34,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[35,"PC5"])-d_pca[1:34,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[35,"PC6"])-d_pca[1:34,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[35,"PC7"])-d_pca[1:34,"PC7"])^2)),
  PC8 = c(sqrt(((d_pca[35,"PC8"])-d_pca[1:34,"PC8"])^2)),
  PC9 = c(sqrt(((d_pca[35,"PC9"])-d_pca[1:34,"PC9"])^2)),
  PC10 = c(sqrt(((d_pca[35,"PC10"])-d_pca[1:34,"PC10"])^2)),
  PC11 = c(sqrt(((d_pca[35,"PC11"])-d_pca[1:34,"PC11"])^2)),
  PC12 = c(sqrt(((d_pca[35,"PC12"])-d_pca[1:34,"PC12"])^2)),
  PC13 = c(sqrt(((d_pca[35,"PC13"])-d_pca[1:34,"PC13"])^2)),
  PC14 = c(sqrt(((d_pca[35,"PC14"])-d_pca[1:34,"PC14"])^2)),
  PC15 = c(sqrt(((d_pca[35,"PC15"])-d_pca[1:34,"PC15"])^2)),
  PC16 = c(sqrt(((d_pca[35,"PC16"])-d_pca[1:34,"PC16"])^2)),
  PC17 = c(sqrt(((d_pca[35,"PC17"])-d_pca[1:34,"PC17"])^2)),
  PC18 = c(sqrt(((d_pca[35,"PC18"])-d_pca[1:34,"PC18"])^2)),
  PC19 = c(sqrt(((d_pca[35,"PC19"])-d_pca[1:34,"PC19"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])+(PC8*eig[8,2])+(PC9*eig[9,2])+
      (PC10*eig[10,2])+(PC11*eig[11,2])+(PC12*eig[12,2])+(PC13*eig[13,2])+
      (PC14*eig[14,2])+(PC15*eig[15,2])+(PC16*eig[16,2])+(PC17*eig[17,2])+
      (PC18*eig[18,2])+(PC19*eig[19,2])) / (sum(eig[1:19,2])))

# distances > distance index, from 1 (closer to artefact) to 5 (more distant)
# 1= min > (1st quartile/2)
# 2= (1st quartile/2) > 1st quartile
# 3= 1st quartile > median
# 4= median > 3rd quartile
# 5= 3rd quartile > max
summary_stat <- data.frame(unclass(summary(dist$weight_mean)), check.names = F)
dist <- dist %>% mutate(
  dist_cat = cut(
    weight_mean,
    breaks=c(0,
             (summary_stat["1st Qu.",]-((summary_stat["1st Qu.",]-summary_stat["Min.",])/2)),
             summary_stat["1st Qu.",],
             summary_stat["Median",],
             summary_stat["3rd Qu.",],
             summary_stat["Max.",]),
    labels=c('1','2','3','4','5')))

distance_index <- ggplot(dist, aes(x = factor(dist_cat), fill=Location)) +
  geom_bar(position="fill", alpha=.75) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  theme_void() + theme(
    axis.title.x=element_text(size=9, vjust = -1.5),
    axis.text.x=element_text(size=8, vjust = -.5),
    axis.title.y=element_blank(),
    axis.line.x=element_line(size=.25), axis.ticks.length.x=unit(.1,"cm"),
    axis.ticks.x=element_line(size=.25, color="black"), legend.position="none") +
  labs(x=paste0("Distance index","\n","(Total variance)"))
distance_index

distance_index <- distance_index/plot_spacer() + plot_layout(heights = c(4,.1))

pdf(here("analysis","figures","Figure_4","Fig4-h.pdf"), width=5.5, height=3.5)
(K1226_PCA_2b | distance_index) + plot_layout(widths = c(4, 2))
dev.off()

## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[35,"PC1"], xend=d_pca[35,"PC1"],
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
  annotate("segment", x=d_pca[35,"PC2"], xend=d_pca[35,"PC2"],
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
  annotate("segment", x=d_pca[35,"PC3"], xend=d_pca[35,"PC3"],
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
  annotate("segment", x=d_pca[35,"PC4"], xend=d_pca[35,"PC4"],
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
  annotate("segment", x=d_pca[35,"PC5"], xend=d_pca[35,"PC5"],
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

pdf(here("analysis","figures","Figure_4","Fig4-h-SI.pdf"), width=6, height=3.5)
K1226_PCA_2b | PCs
dev.off()


