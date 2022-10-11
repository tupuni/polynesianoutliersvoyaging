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
            "Pitcairn-Gambier chain"=21,"North Fiji Basin"=25,"Rotuma"=21,
            "Futuna"=22,"Cikobia"=23,"Uvea"=24,
            "E-11-08"=5,"E-11-08dup"=9,"T-12-06"=2,"T-12-06dup"=14,
            "T-12-07"=7,"T-12-08"=6,"T-12-09"=10,"T-12-10"=11,"K-12-24"=12,
            "K-12-25"=13,"K-12-26"=14)
cols <- c("Caroline islands"="#320A5A","Samoan islands"="#781B6C",
          "Austral-Cook chain"="#BB3654","Society islands"="#EC6824",
          "Marquesas islands"="#FBB41A","Hawai'i islands"="#F4DD53",
          "Pitcairn-Gambier chain"="#C96FB6","North Fiji Basin"="#B4C630",
          "Rotuma"="#6EA002","Futuna"="#6EA002","Cikobia"="#6EA002",
          "Uvea"="#6EA002",
          "E-11-08"="red","E-11-08dup"="red","T-12-06"="red","T-12-06dup"="red",
          "T-12-07"="red","T-12-08"="red","T-12-09"="red",
          "T-12-10"="red","K-12-24"="red","K-12-25"="red","K-12-26"="red")
contour <- c("Caroline islands"="black","Samoan islands"="black",
             "Austral-Cook chain"="black","Society islands"="black",
             "Hawai'i islands"="black","Marquesas islands"="black",
             "Pitcairn-Gambier chain"="black","North Fiji Basin"="black",
             "Rotuma"="black","Futuna"="black","Cikobia"="black","Uvea"="black",
             "E-11-08"="red","E-11-08dup"="red","T-12-06"="red","T-12-06dup"="red",
             "T-12-07"="red","T-12-08"="red","T-12-09"="red",
             "T-12-10"="red","K-12-24"="red","K-12-25"="red","K-12-26"="red")

dir.create(here("analysis","figures","Figure_4"))

#### Fig 4a ####
## Emae_Taumako PCA 1
OIB <- full_join(q10,q11) %>%
  mutate(Location=recode(Location,"Tonga-Fiji"="North Fiji Basin")) %>%
  dplyr::select(
    Sample,Location,Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)

d_price2014 <- price2014 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(
    Nd143_Nd144 > min(ranges_s_OIB[1:6,"Nd143_Nd144 min"]) &
      Nd143_Nd144 < max(ranges_s_OIB[1:6,"Nd143_Nd144 max"]) &
      Sr87_Sr86 > min(ranges_s_OIB[1:6,"Sr87_Sr86 min"]) &
      Sr87_Sr86 < max(ranges_s_OIB[1:6,"Sr87_Sr86 max"]) &
      Pb206_Pb204 > min(ranges_s_OIB[1:6,"Pb206_Pb204 min"]) &
      Pb206_Pb204 < max(ranges_s_OIB[1:6,"Pb206_Pb204 max"]) &
      Pb207_Pb204 > min(ranges_s_OIB[1:6,"Pb207_Pb204 min"]) &
      Pb207_Pb204 < max(ranges_s_OIB[1:6,"Pb207_Pb204 max"]) &
      Pb208_Pb204 > min(ranges_s_OIB[1:6,"Pb208_Pb204 min"]) &
      Pb208_Pb204 < max(ranges_s_OIB[1:6,"Pb208_Pb204 max"])) %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB <- full_join(OIB,d_price2014)

d_price2017 <- price2017 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(
    Nd143_Nd144 > min(ranges_s_OIB[1:6,"Nd143_Nd144 min"]) &
      Nd143_Nd144 < max(ranges_s_OIB[1:6,"Nd143_Nd144 max"]) &
      Sr87_Sr86 > min(ranges_s_OIB[1:6,"Sr87_Sr86 min"]) &
      Sr87_Sr86 < max(ranges_s_OIB[1:6,"Sr87_Sr86 max"]) &
      Pb206_Pb204 > min(ranges_s_OIB[1:6,"Pb206_Pb204 min"]) &
      Pb206_Pb204 < max(ranges_s_OIB[1:6,"Pb206_Pb204 max"]) &
      Pb207_Pb204 > min(ranges_s_OIB[1:6,"Pb207_Pb204 min"]) &
      Pb207_Pb204 < max(ranges_s_OIB[1:6,"Pb207_Pb204 max"]) &
      Pb208_Pb204 > min(ranges_s_OIB[1:6,"Pb208_Pb204 min"]) &
      Pb208_Pb204 < max(ranges_s_OIB[1:6,"Pb208_Pb204 max"])) %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB <- full_join(OIB,d_price2017)
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
  scale_x_continuous(limits=c(-3.5, 3.5)) + scale_y_continuous(limits=c(-3, 2.5)) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
E_T_PCA_1a
pdf(here("analysis","figures","Figure_4","Fig4-a-PCA.pdf"), width=3.5, height=3.5)
E_T_PCA_1a
dev.off()

res.pca.df <- cbind(OIB[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, s[,3:7])
pred <- cbind(s[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

median(d_pca[179:184,"PC1"])
median(d_pca[179:184,"PC2"])

E_T_PCA_1b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) +
  geom_point(aes(x=median(d_pca[179:184,"PC1"]),
                 y=median(d_pca[179:184,"PC2"])), shape=3, color="red") +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-3.5, 3.5)) + scale_y_continuous(limits=c(-3, 2.5)) +
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
  Sample = c(d_pca[1:178,"Sample"]),
  Location = c(d_pca[1:178,"Location"]),
  PC1 = c(sqrt(((median(d_pca[179:184,"PC1"]))-d_pca[1:178,"PC1"])^2)),
  PC2 = c(sqrt(((median(d_pca[179:184,"PC2"]))-d_pca[1:178,"PC2"])^2)),
  PC3 = c(sqrt(((median(d_pca[179:184,"PC3"]))-d_pca[1:178,"PC3"])^2)),
  PC4 = c(sqrt(((median(d_pca[179:184,"PC4"]))-d_pca[1:178,"PC4"])^2)),
  PC5 = c(sqrt(((median(d_pca[179:184,"PC5"]))-d_pca[1:178,"PC5"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+
      (PC4*eig[4,2])+(PC5*eig[5,2])) / (sum(eig[1:5,2])))

# distances > distance index, from 1 (closer to artefact) to 5 (more distant)
# 1= min > middle point between Min and 1st quartile
# 2= middle point between Min and 1st quartile > 1st quartile
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

#### Fig 4b ####
## Emae_Taumako PCA 2
OIB <- full_join(q12,q13) %>%
  mutate(Location=recode(Location,"Tonga-Fiji"="North Fiji Basin")) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)

d_price2014 <- price2014 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(
    SiO2 > min(ranges_s_OIB[1:6,"SiO2 min"]) &
      SiO2 < max(ranges_s_OIB[1:6,"SiO2 max"]) &
      MgO > min(ranges_s_OIB[1:6,"MgO min"]) &
      MgO < max(ranges_s_OIB[1:6,"MgO max"]) &
      Na2O > min(ranges_s_OIB[1:6,"Na2O min"]) &
      Na2O < max(ranges_s_OIB[1:6,"Na2O max"]) &
      K2O > min(ranges_s_OIB[1:6,"K2O min"]) &
      K2O < max(ranges_s_OIB[1:6,"K2O max"]) &
      Yb > min(ranges_s_OIB[1:6,"Yb min"]) &
      Yb < max(ranges_s_OIB[1:6,"Yb max"])) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
OIB <- full_join(OIB,d_price2014)

d_price2017 <- price2017 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(
    SiO2 > min(ranges_s_OIB[1:6,"SiO2 min"]) &
      SiO2 < max(ranges_s_OIB[1:6,"SiO2 max"]) &
      MgO > min(ranges_s_OIB[1:6,"MgO min"]) &
      MgO < max(ranges_s_OIB[1:6,"MgO max"]) &
      Na2O > min(ranges_s_OIB[1:6,"Na2O min"]) &
      Na2O < max(ranges_s_OIB[1:6,"Na2O max"]) &
      K2O > min(ranges_s_OIB[1:6,"K2O min"]) &
      K2O < max(ranges_s_OIB[1:6,"K2O max"]) &
      Yb > min(ranges_s_OIB[1:6,"Yb min"]) &
      Yb < max(ranges_s_OIB[1:6,"Yb max"])) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
OIB <- full_join(OIB,d_price2017)
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
  scale_x_continuous(limits=c(-6, 6)) + scale_y_continuous(limits=c(-4.5, 4.5)) +
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

pred <- stats::predict(res.pca, s[,3:21])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(OIB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

E_T_PCA_2b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_point(aes(x=median(d_pca[120:125,"PC1"]),
                 y=median(d_pca[120:125,"PC2"])), shape=3, color="red") +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-6, 6)) + scale_y_continuous(limits=c(-4.5, 4.5)) +
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
  Sample = c(d_pca[1:119,"Sample"]),
  Location = c(d_pca[1:119,"Location"]),
  PC1 = c(sqrt(((median(d_pca[120:125,"PC1"]))-d_pca[1:119,"PC1"])^2)),
  PC2 = c(sqrt(((median(d_pca[120:125,"PC2"]))-d_pca[1:119,"PC2"])^2)),
  PC3 = c(sqrt(((median(d_pca[120:125,"PC3"]))-d_pca[1:119,"PC3"])^2)),
  PC4 = c(sqrt(((median(d_pca[120:125,"PC4"]))-d_pca[1:119,"PC4"])^2)),
  PC5 = c(sqrt(((median(d_pca[120:125,"PC5"]))-d_pca[1:119,"PC5"])^2)),
  PC6 = c(sqrt(((median(d_pca[120:125,"PC6"]))-d_pca[1:119,"PC6"])^2)),
  PC7 = c(sqrt(((median(d_pca[120:125,"PC7"]))-d_pca[1:119,"PC7"])^2)),
  PC8 = c(sqrt(((median(d_pca[120:125,"PC8"]))-d_pca[1:119,"PC8"])^2)),
  PC9 = c(sqrt(((median(d_pca[120:125,"PC9"]))-d_pca[1:119,"PC9"])^2)),
  PC10 = c(sqrt(((median(d_pca[120:125,"PC10"]))-d_pca[1:119,"PC10"])^2)),
  PC11 = c(sqrt(((median(d_pca[120:125,"PC11"]))-d_pca[1:119,"PC11"])^2)),
  PC12 = c(sqrt(((median(d_pca[120:125,"PC12"]))-d_pca[1:119,"PC12"])^2)),
  PC13 = c(sqrt(((median(d_pca[120:125,"PC13"]))-d_pca[1:119,"PC13"])^2)),
  PC14 = c(sqrt(((median(d_pca[120:125,"PC14"]))-d_pca[1:119,"PC14"])^2)),
  PC15 = c(sqrt(((median(d_pca[120:125,"PC15"]))-d_pca[1:119,"PC15"])^2)),
  PC16 = c(sqrt(((median(d_pca[120:125,"PC16"]))-d_pca[1:119,"PC16"])^2)),
  PC17 = c(sqrt(((median(d_pca[120:125,"PC17"]))-d_pca[1:119,"PC17"])^2)),
  PC18 = c(sqrt(((median(d_pca[120:125,"PC18"]))-d_pca[1:119,"PC18"])^2)),
  PC19 = c(sqrt(((median(d_pca[120:125,"PC19"]))-d_pca[1:119,"PC19"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])+(PC8*eig[8,2])+(PC9*eig[9,2])+
      (PC10*eig[10,2])+(PC11*eig[11,2])+(PC12*eig[12,2])+(PC13*eig[13,2])+
      (PC14*eig[14,2])+(PC15*eig[15,2])+(PC16*eig[16,2])+(PC17*eig[17,2])+
      (PC18*eig[18,2])+(PC19*eig[19,2])) / (sum(eig[1:19,2])))

# distances > distance index, from 1 (closer to artefact) to 5 (more distant)
# 1= min > middle point between Min and 1st quartile
# 2= middle point between Min and 1st quartile > 1st quartile
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


#### Fig 4c ####
## K1224 PCA 1
OIB <- q14 %>%
  mutate(Location=recode(Location,"Tonga-Fiji"="North Fiji Basin")) %>%
  dplyr::select(
    Sample,Location,Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)

d_price2014 <- price2014 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(
    Nd143_Nd144 > min(ranges_s_OIB[7,"Nd143_Nd144 min"]) &
      Nd143_Nd144 < max(ranges_s_OIB[7,"Nd143_Nd144 max"]) &
      Sr87_Sr86 > min(ranges_s_OIB[7,"Sr87_Sr86 min"]) &
      Sr87_Sr86 < max(ranges_s_OIB[7,"Sr87_Sr86 max"]) &
      Pb206_Pb204 > min(ranges_s_OIB[7,"Pb206_Pb204 min"]) &
      Pb206_Pb204 < max(ranges_s_OIB[7,"Pb206_Pb204 max"]) &
      Pb207_Pb204 > min(ranges_s_OIB[7,"Pb207_Pb204 min"]) &
      Pb207_Pb204 < max(ranges_s_OIB[7,"Pb207_Pb204 max"]) &
      Pb208_Pb204 > min(ranges_s_OIB[7,"Pb208_Pb204 min"]) &
      Pb208_Pb204 < max(ranges_s_OIB[7,"Pb208_Pb204 max"])) %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB <- full_join(OIB,d_price2014)

d_price2017 <- price2017 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(
    Nd143_Nd144 > min(ranges_s_OIB[7,"Nd143_Nd144 min"]) &
      Nd143_Nd144 < max(ranges_s_OIB[7,"Nd143_Nd144 max"]) &
      Sr87_Sr86 > min(ranges_s_OIB[7,"Sr87_Sr86 min"]) &
      Sr87_Sr86 < max(ranges_s_OIB[7,"Sr87_Sr86 max"]) &
      Pb206_Pb204 > min(ranges_s_OIB[7,"Pb206_Pb204 min"]) &
      Pb206_Pb204 < max(ranges_s_OIB[7,"Pb206_Pb204 max"]) &
      Pb207_Pb204 > min(ranges_s_OIB[7,"Pb207_Pb204 min"]) &
      Pb207_Pb204 < max(ranges_s_OIB[7,"Pb207_Pb204 max"]) &
      Pb208_Pb204 > min(ranges_s_OIB[7,"Pb208_Pb204 min"]) &
      Pb208_Pb204 < max(ranges_s_OIB[7,"Pb208_Pb204 max"])) %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB <- full_join(OIB,d_price2017)
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
  scale_x_continuous(limits=c(-3.5, 4.5)) + scale_y_continuous(limits=c(-4.5, 3.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
K1224_PCA_1a
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
  scale_x_continuous(limits=c(-3.5, 4.5)) + scale_y_continuous(limits=c(-4.5, 3.5)) +
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
  Sample = c(d_pca[1:346,"Sample"]),
  Location = c(d_pca[1:346,"Location"]),
  PC1 = c(sqrt(((median(d_pca[347,"PC1"]))-d_pca[1:346,"PC1"])^2)),
  PC2 = c(sqrt(((median(d_pca[347,"PC2"]))-d_pca[1:346,"PC2"])^2)),
  PC3 = c(sqrt(((median(d_pca[347,"PC3"]))-d_pca[1:346,"PC3"])^2)),
  PC4 = c(sqrt(((median(d_pca[347,"PC4"]))-d_pca[1:346,"PC4"])^2)),
  PC5 = c(sqrt(((median(d_pca[347,"PC5"]))-d_pca[1:346,"PC5"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+
      (PC4*eig[4,2])+(PC5*eig[5,2])) / (sum(eig[1:5,2])))

# distances > distance index, from 1 (closer to artefact) to 5 (more distant)
# 1= min > middle point between Min and 1st quartile
# 2= middle point between Min and 1st quartile > 1st quartile
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

#### Fig 4d ####
## K1224 PCA 2
OIB <- q15 %>%
  mutate(Location=recode(Location,"Tonga-Fiji"="North Fiji Basin")) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)

d_price2014 <- price2014 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(
    #SiO2 > min(ranges_s_OIB[7,"SiO2 min"]) &
    #SiO2 < max(ranges_s_OIB[7,"SiO2 max"]) &
    Na2O > min(ranges_s_OIB[7,"Na2O min"]) &
      Na2O < max(ranges_s_OIB[7,"Na2O max"]) &
      K2O > min(ranges_s_OIB[7,"K2O min"]) &
      K2O < max(ranges_s_OIB[7,"K2O max"]) &
      MgO > min(ranges_s_OIB[7,"MgO min"]) &
      MgO < max(ranges_s_OIB[7,"MgO max"]) &
      Yb > min(ranges_s_OIB[7,"Yb min"]) &
      Yb < max(ranges_s_OIB[7,"Yb max"])) %>%
  dplyr::select(Sample,Location,
                SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
OIB <- full_join(OIB,d_price2014)

d_price2017 <- price2017 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(
    #SiO2 > min(ranges_s_OIB[7,"SiO2 min"]) &
    #SiO2 < max(ranges_s_OIB[7,"SiO2 max"]) &
    Na2O > min(ranges_s_OIB[7,"Na2O min"]) &
      Na2O < max(ranges_s_OIB[7,"Na2O max"]) &
      K2O > min(ranges_s_OIB[7,"K2O min"]) &
      K2O < max(ranges_s_OIB[7,"K2O max"]) &
      MgO > min(ranges_s_OIB[7,"MgO min"]) &
      MgO < max(ranges_s_OIB[7,"MgO max"]) &
      Yb > min(ranges_s_OIB[7,"Yb min"]) &
      Yb < max(ranges_s_OIB[7,"Yb max"])) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
OIB <- full_join(OIB,d_price2017)
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

OIB %>% group_by(Location) %>% tally()

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-24")) %>%
  mutate(Location = case_when(grepl("K-12-24", Sample) ~ "K-12-24")) %>%
  dplyr::select(
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
  scale_x_continuous(limits=c(-18, 7)) + scale_y_continuous(limits=c(-6, 7)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
K1224_PCA_2a
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
  scale_x_continuous(limits=c(-18, 7)) + scale_y_continuous(limits=c(-6, 7)) +
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
  Sample = c(d_pca[1:591,"Sample"]),
  Location = c(d_pca[1:591,"Location"]),
  PC1 = c(sqrt(((d_pca[592,"PC1"])-d_pca[1:591,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[592,"PC2"])-d_pca[1:591,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[592,"PC3"])-d_pca[1:591,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[592,"PC4"])-d_pca[1:591,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[592,"PC5"])-d_pca[1:591,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[592,"PC6"])-d_pca[1:591,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[592,"PC7"])-d_pca[1:591,"PC7"])^2)),
  PC8 = c(sqrt(((d_pca[592,"PC8"])-d_pca[1:591,"PC8"])^2)),
  PC9 = c(sqrt(((d_pca[592,"PC9"])-d_pca[1:591,"PC9"])^2)),
  PC10 = c(sqrt(((d_pca[592,"PC10"])-d_pca[1:591,"PC10"])^2)),
  PC11 = c(sqrt(((d_pca[592,"PC11"])-d_pca[1:591,"PC11"])^2)),
  PC12 = c(sqrt(((d_pca[592,"PC12"])-d_pca[1:591,"PC12"])^2)),
  PC13 = c(sqrt(((d_pca[592,"PC13"])-d_pca[1:591,"PC13"])^2)),
  PC14 = c(sqrt(((d_pca[592,"PC14"])-d_pca[1:591,"PC14"])^2)),
  PC15 = c(sqrt(((d_pca[592,"PC15"])-d_pca[1:591,"PC15"])^2)),
  PC16 = c(sqrt(((d_pca[592,"PC16"])-d_pca[1:591,"PC16"])^2)),
  PC17 = c(sqrt(((d_pca[592,"PC17"])-d_pca[1:591,"PC17"])^2)),
  PC18 = c(sqrt(((d_pca[592,"PC18"])-d_pca[1:591,"PC18"])^2)),
  PC19 = c(sqrt(((d_pca[592,"PC19"])-d_pca[1:591,"PC19"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])+(PC8*eig[8,2])+(PC9*eig[9,2])+
      (PC10*eig[10,2])+(PC11*eig[11,2])+(PC12*eig[12,2])+(PC13*eig[13,2])+
      (PC14*eig[14,2])+(PC15*eig[15,2])+(PC16*eig[16,2])+(PC17*eig[17,2])+
      (PC18*eig[18,2])+(PC19*eig[19,2])) / (sum(eig[1:19,2])))

# distances > distance index, from 1 (closer to artefact) to 5 (more distant)
# 1= min > middle point between Min and 1st quartile
# 2= middle point between Min and 1st quartile > 1st quartile
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


#### Fig 4e ####
## K1225 PCA 1
OIB <- q16 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  mutate(Location=recode(Location,"Tonga-Fiji"="North Fiji Basin")) %>%
    dplyr::select(
      Sample,Location,Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

ranges_s_OIB[8,]

d_price2014 <- price2014 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(
    Nd143_Nd144 > min(ranges_s_OIB[8,"Nd143_Nd144 min"]) &
      Nd143_Nd144 < max(ranges_s_OIB[8,"Nd143_Nd144 max"]) &
      Sr87_Sr86 > min(ranges_s_OIB[8,"Sr87_Sr86 min"]) &
      Sr87_Sr86 < max(ranges_s_OIB[8,"Sr87_Sr86 max"]) &
      Pb206_Pb204 > min(ranges_s_OIB[8,"Pb206_Pb204 min"]) &
      Pb206_Pb204 < max(ranges_s_OIB[8,"Pb206_Pb204 max"]) &
      Pb207_Pb204 > min(ranges_s_OIB[8,"Pb207_Pb204 min"]) &
      Pb207_Pb204 < max(ranges_s_OIB[8,"Pb207_Pb204 max"]) &
      Pb208_Pb204 > min(ranges_s_OIB[8,"Pb208_Pb204 min"]) &
      Pb208_Pb204 < max(ranges_s_OIB[8,"Pb208_Pb204 max"])) %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB <- full_join(OIB,d_price2014)

d_price2017 <- price2017 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(
    Nd143_Nd144 > min(ranges_s_OIB[8,"Nd143_Nd144 min"]) &
      Nd143_Nd144 < max(ranges_s_OIB[8,"Nd143_Nd144 max"]) &
      Sr87_Sr86 > min(ranges_s_OIB[8,"Sr87_Sr86 min"]) &
      Sr87_Sr86 < max(ranges_s_OIB[8,"Sr87_Sr86 max"]) &
      Pb206_Pb204 > min(ranges_s_OIB[8,"Pb206_Pb204 min"]) &
      Pb206_Pb204 < max(ranges_s_OIB[8,"Pb206_Pb204 max"]) &
      Pb207_Pb204 > min(ranges_s_OIB[8,"Pb207_Pb204 min"]) &
      Pb207_Pb204 < max(ranges_s_OIB[8,"Pb207_Pb204 max"]) &
      Pb208_Pb204 > min(ranges_s_OIB[8,"Pb208_Pb204 min"]) &
      Pb208_Pb204 < max(ranges_s_OIB[8,"Pb208_Pb204 max"])) %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB <- full_join(OIB,d_price2017)
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>%
  filter(Sample %in% c("K-12-25")) %>%
  mutate(Location = case_when(grepl("K-12-25", Sample) ~ "K-12-25")) %>%
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
  scale_x_continuous(limits=c(-3, 3)) + scale_y_continuous(limits=c(-3, 2.2)) +
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
  scale_x_continuous(limits=c(-3, 3)) + scale_y_continuous(limits=c(-3, 2.2)) +
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
  Sample = c(d_pca[1:26,"Sample"]),
  Location = c(d_pca[1:26,"Location"]),
  PC1 = c(sqrt(((median(d_pca[27,"PC1"]))-d_pca[1:26,"PC1"])^2)),
  PC2 = c(sqrt(((median(d_pca[27,"PC2"]))-d_pca[1:26,"PC2"])^2)),
  PC3 = c(sqrt(((median(d_pca[27,"PC3"]))-d_pca[1:26,"PC3"])^2)),
  PC4 = c(sqrt(((median(d_pca[27,"PC4"]))-d_pca[1:26,"PC4"])^2)),
  PC5 = c(sqrt(((median(d_pca[27,"PC5"]))-d_pca[1:26,"PC5"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+
      (PC4*eig[4,2])+(PC5*eig[5,2])) / (sum(eig[1:5,2])))

# distances > distance index, from 1 (closer to artefact) to 5 (more distant)
# 1= min > middle point between Min and 1st quartile
# 2= middle point between Min and 1st quartile > 1st quartile
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

#### Fig 4f ####
## K1225 PCA 2
OIB <- q17 %>%
  mutate(Location=recode(Location,"Tonga-Fiji"="North Fiji Basin")) %>%
  dplyr::select(
  Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)

d_price2014 <- price2014 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(
    #SiO2 > min(ranges_s_OIB[8,"SiO2 min"]) &
    #SiO2 < max(ranges_s_OIB[8,"SiO2 max"]) &
    Na2O > min(ranges_s_OIB[8,"Na2O min"]) &
      Na2O < max(ranges_s_OIB[8,"Na2O max"]) &
      K2O > min(ranges_s_OIB[8,"K2O min"]) &
      K2O < max(ranges_s_OIB[8,"K2O max"]) &
      MgO > min(ranges_s_OIB[8,"MgO min"]) &
      MgO < max(ranges_s_OIB[8,"MgO max"]) &
      Yb > min(ranges_s_OIB[8,"Yb min"]) &
      Yb < max(ranges_s_OIB[8,"Yb max"])) %>%
  dplyr::select(Sample,Location,
                SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
OIB <- full_join(OIB,d_price2014)

d_price2017 <- price2017 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(
    #SiO2 > min(ranges_s_OIB[8,"SiO2 min"]) &
    #SiO2 < max(ranges_s_OIB[8,"SiO2 max"]) &
    Na2O > min(ranges_s_OIB[8,"Na2O min"]) &
      Na2O < max(ranges_s_OIB[8,"Na2O max"]) &
      K2O > min(ranges_s_OIB[8,"K2O min"]) &
      K2O < max(ranges_s_OIB[8,"K2O max"]) &
      MgO > min(ranges_s_OIB[8,"MgO min"]) &
      MgO < max(ranges_s_OIB[8,"MgO max"]) &
      Yb > min(ranges_s_OIB[8,"Yb min"]) &
      Yb < max(ranges_s_OIB[8,"Yb max"])) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
OIB <- full_join(OIB,d_price2017)
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

OIB %>% group_by(Location) %>% tally()

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-25")) %>%
  mutate(Location = case_when(grepl("K-12-25", Sample) ~ "K-12-25")) %>%
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
  scale_x_continuous(limits=c(-8, 8)) + scale_y_continuous(limits=c(-4.5, 3)) +
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
  scale_x_continuous(limits=c(-8, 8)) + scale_y_continuous(limits=c(-4.5, 3)) +
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
  Sample = c(d_pca[1:119,"Sample"]),
  Location = c(d_pca[1:119,"Location"]),
  PC1 = c(sqrt(((d_pca[120,"PC1"])-d_pca[1:119,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[120,"PC2"])-d_pca[1:119,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[120,"PC3"])-d_pca[1:119,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[120,"PC4"])-d_pca[1:119,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[120,"PC5"])-d_pca[1:119,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[120,"PC6"])-d_pca[1:119,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[120,"PC7"])-d_pca[1:119,"PC7"])^2)),
  PC8 = c(sqrt(((d_pca[120,"PC8"])-d_pca[1:119,"PC8"])^2)),
  PC9 = c(sqrt(((d_pca[120,"PC9"])-d_pca[1:119,"PC9"])^2)),
  PC10 = c(sqrt(((d_pca[120,"PC10"])-d_pca[1:119,"PC10"])^2)),
  PC11 = c(sqrt(((d_pca[120,"PC11"])-d_pca[1:119,"PC11"])^2)),
  PC12 = c(sqrt(((d_pca[120,"PC12"])-d_pca[1:119,"PC12"])^2)),
  PC13 = c(sqrt(((d_pca[120,"PC13"])-d_pca[1:119,"PC13"])^2)),
  PC14 = c(sqrt(((d_pca[120,"PC14"])-d_pca[1:119,"PC14"])^2)),
  PC15 = c(sqrt(((d_pca[120,"PC15"])-d_pca[1:119,"PC15"])^2)),
  PC16 = c(sqrt(((d_pca[120,"PC16"])-d_pca[1:119,"PC16"])^2)),
  PC17 = c(sqrt(((d_pca[120,"PC17"])-d_pca[1:119,"PC17"])^2)),
  PC18 = c(sqrt(((d_pca[120,"PC18"])-d_pca[1:119,"PC18"])^2)),
  PC19 = c(sqrt(((d_pca[120,"PC19"])-d_pca[1:119,"PC19"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])+(PC8*eig[8,2])+(PC9*eig[9,2])+
      (PC10*eig[10,2])+(PC11*eig[11,2])+(PC12*eig[12,2])+(PC13*eig[13,2])+
      (PC14*eig[14,2])+(PC15*eig[15,2])+(PC16*eig[16,2])+(PC17*eig[17,2])+
      (PC18*eig[18,2])+(PC19*eig[19,2])) / (sum(eig[1:19,2])))

# distances > distance index, from 1 (closer to artefact) to 5 (more distant)
# 1= min > middle point between Min and 1st quartile
# 2= middle point between Min and 1st quartile > 1st quartile
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


#### Fig 4g ####
## K1226 PCA 1
OIB <- q18 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  mutate(Location=recode(Location,"Tonga-Fiji"="North Fiji Basin")) %>%
  dplyr::select(
    Sample,Location,Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

d_price2014 <- price2014 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(
    Nd143_Nd144 > min(ranges_s_OIB[9,"Nd143_Nd144 min"]) &
      Nd143_Nd144 < max(ranges_s_OIB[9,"Nd143_Nd144 max"]) &
      Sr87_Sr86 > min(ranges_s_OIB[9,"Sr87_Sr86 min"]) &
      Sr87_Sr86 < max(ranges_s_OIB[9,"Sr87_Sr86 max"]) &
      Pb206_Pb204 > min(ranges_s_OIB[9,"Pb206_Pb204 min"]) &
      Pb206_Pb204 < max(ranges_s_OIB[9,"Pb206_Pb204 max"]) &
      Pb207_Pb204 > min(ranges_s_OIB[9,"Pb207_Pb204 min"]) &
      Pb207_Pb204 < max(ranges_s_OIB[9,"Pb207_Pb204 max"]) &
      Pb208_Pb204 > min(ranges_s_OIB[9,"Pb208_Pb204 min"]) &
      Pb208_Pb204 < max(ranges_s_OIB[9,"Pb208_Pb204 max"])) %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB <- full_join(OIB,d_price2014)

d_price2017 <- price2017 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(
    Nd143_Nd144 > min(ranges_s_OIB[9,"Nd143_Nd144 min"]) &
      Nd143_Nd144 < max(ranges_s_OIB[9,"Nd143_Nd144 max"]) &
      Sr87_Sr86 > min(ranges_s_OIB[9,"Sr87_Sr86 min"]) &
      Sr87_Sr86 < max(ranges_s_OIB[9,"Sr87_Sr86 max"]) &
      Pb206_Pb204 > min(ranges_s_OIB[9,"Pb206_Pb204 min"]) &
      Pb206_Pb204 < max(ranges_s_OIB[9,"Pb206_Pb204 max"]) &
      Pb207_Pb204 > min(ranges_s_OIB[9,"Pb207_Pb204 min"]) &
      Pb207_Pb204 < max(ranges_s_OIB[9,"Pb207_Pb204 max"]) &
      Pb208_Pb204 > min(ranges_s_OIB[9,"Pb208_Pb204 min"]) &
      Pb208_Pb204 < max(ranges_s_OIB[9,"Pb208_Pb204 max"])) %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB <- full_join(OIB,d_price2017)
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
  scale_x_continuous(limits=c(-3, 5)) + scale_y_continuous(limits=c(-2.5, 3.5)) +
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
  scale_x_continuous(limits=c(-3, 5)) + scale_y_continuous(limits=c(-2.5, 3.5)) +
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
  Sample = c(d_pca[1:85,"Sample"]),
  Location = c(d_pca[1:85,"Location"]),
  PC1 = c(sqrt(((median(d_pca[86,"PC1"]))-d_pca[1:85,"PC1"])^2)),
  PC2 = c(sqrt(((median(d_pca[86,"PC2"]))-d_pca[1:85,"PC2"])^2)),
  PC3 = c(sqrt(((median(d_pca[86,"PC3"]))-d_pca[1:85,"PC3"])^2)),
  PC4 = c(sqrt(((median(d_pca[86,"PC4"]))-d_pca[1:85,"PC4"])^2)),
  PC5 = c(sqrt(((median(d_pca[86,"PC5"]))-d_pca[1:85,"PC5"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+
      (PC4*eig[4,2])+(PC5*eig[5,2])) / (sum(eig[1:5,2])))

# distances > distance index, from 1 (closer to artefact) to 5 (more distant)
# 1= min > middle point between Min and 1st quartile
# 2= middle point between Min and 1st quartile > 1st quartile
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


#### Fig 4h ####
## K1226 PCA 2
OIB <- q19 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  mutate(Location=recode(Location,"Tonga-Fiji"="North Fiji Basin")) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)

d_price2014 <- price2014 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(
    Na2O > min(ranges_s_OIB[9,"Na2O min"]) &
      Na2O < max(ranges_s_OIB[9,"Na2O max"]) &
      K2O > min(ranges_s_OIB[9,"K2O min"]) &
      K2O < max(ranges_s_OIB[9,"K2O max"]) &
      Yb > min(ranges_s_OIB[9,"Yb min"]) &
      Yb < max(ranges_s_OIB[9,"Yb max"])) %>%
  dplyr::select(Sample,Location,
                SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
OIB <- full_join(OIB,d_price2014)

price2017[,"Na2O"]
d_price2017 <- price2017 %>%
  dplyr::mutate(Nb_La = Nb/La) %>% dplyr::filter(Nb_La > 0.86) %>%
  dplyr::filter(
    Na2O > min(ranges_s_OIB[9,"Na2O min"]) &
      Na2O < max(ranges_s_OIB[9,"Na2O max"]) &
      K2O > min(ranges_s_OIB[9,"K2O min"]) &
      K2O < max(ranges_s_OIB[9,"K2O max"]) &
      Yb > min(ranges_s_OIB[9,"Yb min"]) &
      Yb < max(ranges_s_OIB[9,"Yb max"])) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
OIB <- full_join(OIB,d_price2017)
OIB[OIB == 0] <- NA # Replace 0 with NA
OIB <- OIB[rowSums(is.na(OIB)) == 0,] # removes rows with missing info for PCA

OIB %>% group_by(Location) %>% tally()

s <- joined_data %>%
  dplyr::filter(Sample %in% c("K-12-26")) %>%
  mutate(Location = case_when(grepl("K-12-26", Sample) ~ "K-12-26")) %>%
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
  scale_x_continuous(limits=c(-10.5, 8)) + scale_y_continuous(limits=c(-2.2, 5)) +
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
  scale_x_continuous(limits=c(-10.5, 8)) + scale_y_continuous(limits=c(-2.2, 5)) +
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
  Sample = c(d_pca[1:98,"Sample"]),
  Location = c(d_pca[1:98,"Location"]),
  PC1 = c(sqrt(((d_pca[99,"PC1"])-d_pca[1:98,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[99,"PC2"])-d_pca[1:98,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[99,"PC3"])-d_pca[1:98,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[99,"PC4"])-d_pca[1:98,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[99,"PC5"])-d_pca[1:98,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[99,"PC6"])-d_pca[1:98,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[99,"PC7"])-d_pca[1:98,"PC7"])^2)),
  PC8 = c(sqrt(((d_pca[99,"PC8"])-d_pca[1:98,"PC8"])^2)),
  PC9 = c(sqrt(((d_pca[99,"PC9"])-d_pca[1:98,"PC9"])^2)),
  PC10 = c(sqrt(((d_pca[99,"PC10"])-d_pca[1:98,"PC10"])^2)),
  PC11 = c(sqrt(((d_pca[99,"PC11"])-d_pca[1:98,"PC11"])^2)),
  PC12 = c(sqrt(((d_pca[99,"PC12"])-d_pca[1:98,"PC12"])^2)),
  PC13 = c(sqrt(((d_pca[99,"PC13"])-d_pca[1:98,"PC13"])^2)),
  PC14 = c(sqrt(((d_pca[99,"PC14"])-d_pca[1:98,"PC14"])^2)),
  PC15 = c(sqrt(((d_pca[99,"PC15"])-d_pca[1:98,"PC15"])^2)),
  PC16 = c(sqrt(((d_pca[99,"PC16"])-d_pca[1:98,"PC16"])^2)),
  PC17 = c(sqrt(((d_pca[99,"PC17"])-d_pca[1:98,"PC17"])^2)),
  PC18 = c(sqrt(((d_pca[99,"PC18"])-d_pca[1:98,"PC18"])^2)),
  PC19 = c(sqrt(((d_pca[99,"PC19"])-d_pca[1:98,"PC19"])^2))) %>%
  mutate(weight_mean = (
    (PC1*eig[1,2])+(PC2*eig[2,2])+(PC3*eig[3,2])+(PC4*eig[4,2])+(PC5*eig[5,2])+
      (PC6*eig[6,2])+(PC7*eig[7,2])+(PC8*eig[8,2])+(PC9*eig[9,2])+
      (PC10*eig[10,2])+(PC11*eig[11,2])+(PC12*eig[12,2])+(PC13*eig[13,2])+
      (PC14*eig[14,2])+(PC15*eig[15,2])+(PC16*eig[16,2])+(PC17*eig[17,2])+
      (PC18*eig[18,2])+(PC19*eig[19,2])) / (sum(eig[1:19,2])))

head(arrange(dist,weight_mean), 10)

# distances > distance index, from 1 (closer to artefact) to 5 (more distant)
# 1= min > middle point between Min and 1st quartile
# 2= middle point between Min and 1st quartile > 1st quartile
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



