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
            "E-11-03"=0,"E-11-06"=1,"E-11-07"=8,"E-11-10"=24,"E-11-11"=24,
            "E-11-13"=24,"E-11-16"=24,"E-11-18"=24,"E-11-19"=24,"K-12-28"=3,"K-12-29"=4)
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

dir.create(here("analysis","figures","Figure_3"))

#### Fig 3a ####
## E_11_03 PCA1
IAB <- q1 %>% dplyr::select(
  Sample,Location,Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)
s_d <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-18dup")) %>%
  dplyr::mutate(Location = "Vanuatu Arc")%>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)
IAB <- full_join(IAB,s_d)
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% dplyr::filter(Sample %in% c("E-11-03")) %>%
  dplyr::mutate(Location = Sample)%>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)
is.na(s) <- sapply(s, is.infinite) #replace Inf by NA
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(IAB[,3:7], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
E_11_03_PCA_1a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-4, 4)) + scale_y_continuous(limits=c(-2,2.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 8), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1)
E_11_03_PCA_1a
pdf(here("analysis","figures","Figure_3","Fig3-a-PCA.pdf"), width=3.5, height=3.5)
E_11_03_PCA_1a
dev.off()

res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, s[,3:7])
pred <- cbind(s[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

E_11_03_PCA_1b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-4, 4)) + scale_y_continuous(limits=c(-2,2.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
E_11_03_PCA_1b

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:76,"Sample"]),
  Location = c(d_pca[1:76,"Location"]),
  PC1 = c(sqrt(((median(d_pca[77,"PC1"]))-d_pca[1:76,"PC1"])^2)),
  PC2 = c(sqrt(((median(d_pca[77,"PC2"]))-d_pca[1:76,"PC2"])^2)),
  PC3 = c(sqrt(((median(d_pca[77,"PC3"]))-d_pca[1:76,"PC3"])^2)),
  PC4 = c(sqrt(((median(d_pca[77,"PC4"]))-d_pca[1:76,"PC4"])^2)),
  PC5 = c(sqrt(((median(d_pca[77,"PC5"]))-d_pca[1:76,"PC5"])^2))) %>%
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

pdf(here("analysis","figures","Figure_3","Fig3-a.pdf"), width=5.5, height=3.5)
(E_11_03_PCA_1b | distance_index) + plot_layout(widths = c(4, 2))
dev.off()


#### Fig 3b ####
## E_11_03 PCA2
IAB <- full_join(q2,q3) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>%
  dplyr::filter(Sample %in% c("E-11-03")) %>%
  dplyr::mutate(Location = Sample) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)

res.pca <- prcomp(IAB[,3:21],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
E_11_03_PCA_2a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-3, 6)) + scale_y_continuous(limits=c(-4, 9)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
E_11_03_PCA_2a
pdf(here("analysis","figures","Figure_3","Fig3-b-PCA.pdf"), width=3.5, height=3.5)
E_11_03_PCA_2a
dev.off()

pred <- stats::predict(res.pca, s[,3:21])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

E_11_03_PCA_2b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="dashed") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="dashed") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-3, 6)) + scale_y_continuous(limits=c(-4, 9)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
E_11_03_PCA_2b

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:38,"Sample"]),
  Location = c(d_pca[1:38,"Location"]),
  PC1 = c(sqrt(((d_pca[39,"PC1"])-d_pca[1:38,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[39,"PC2"])-d_pca[1:38,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[39,"PC3"])-d_pca[1:38,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[39,"PC4"])-d_pca[1:38,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[39,"PC5"])-d_pca[1:38,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[39,"PC6"])-d_pca[1:38,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[39,"PC7"])-d_pca[1:38,"PC7"])^2)),
  PC8 = c(sqrt(((d_pca[39,"PC8"])-d_pca[1:38,"PC8"])^2)),
  PC9 = c(sqrt(((d_pca[39,"PC9"])-d_pca[1:38,"PC9"])^2)),
  PC10 = c(sqrt(((d_pca[39,"PC10"])-d_pca[1:38,"PC10"])^2)),
  PC11 = c(sqrt(((d_pca[39,"PC11"])-d_pca[1:38,"PC11"])^2)),
  PC12 = c(sqrt(((d_pca[39,"PC12"])-d_pca[1:38,"PC12"])^2)),
  PC13 = c(sqrt(((d_pca[39,"PC13"])-d_pca[1:38,"PC13"])^2)),
  PC14 = c(sqrt(((d_pca[39,"PC14"])-d_pca[1:38,"PC14"])^2)),
  PC15 = c(sqrt(((d_pca[39,"PC15"])-d_pca[1:38,"PC15"])^2)),
  PC16 = c(sqrt(((d_pca[39,"PC16"])-d_pca[1:38,"PC16"])^2)),
  PC17 = c(sqrt(((d_pca[39,"PC17"])-d_pca[1:38,"PC17"])^2)),
  PC18 = c(sqrt(((d_pca[39,"PC18"])-d_pca[1:38,"PC18"])^2)),
  PC19 = c(sqrt(((d_pca[39,"PC19"])-d_pca[1:38,"PC19"])^2))) %>%
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

pdf(here("analysis","figures","Figure_3","Fig3-b.pdf"), width=5.5, height=3.5)
(E_11_03_PCA_2b | distance_index) + plot_layout(widths = c(4, 2))
dev.off()


#### Fig 3c ####
## E_11_06 & E_11_07 PCA1
IAB <- q4 %>% dplyr::select(
  Sample,Location,Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)
s_d <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-18dup")) %>%
  dplyr::mutate(Location = "Vanuatu Arc")%>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)
IAB <- full_join(IAB,s_d)
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% dplyr::filter(Sample %in% c("E-11-06","E-11-07")) %>%
  dplyr::mutate(Location = Sample)%>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)
is.na(s) <- sapply(s, is.infinite) #replace Inf by NA
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(IAB[,3:7],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
E_11_06_PCA_1a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-3.8, 3.2)) + scale_y_continuous(limits=c(-3.6, 2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
pdf(here("analysis","figures","Figure_3","Fig3-c-PCA.pdf"), width=3.5, height=3.5)
E_11_06_PCA_1a
dev.off()

res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, s[,3:7])
pred <- cbind(s[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

E_11_06_PCA_1b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="longdash") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="longdash") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-3.8, 3.2)) + scale_y_continuous(limits=c(-3.6, 2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
E_11_06_PCA_1b

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:146,"Sample"]),
  Location = c(d_pca[1:146,"Location"]),
  PC1 = c(sqrt(((median(d_pca[147:148,"PC1"]))-d_pca[1:146,"PC1"])^2)),
  PC2 = c(sqrt(((median(d_pca[147:148,"PC2"]))-d_pca[1:146,"PC2"])^2)),
  PC3 = c(sqrt(((median(d_pca[147:148,"PC3"]))-d_pca[1:146,"PC3"])^2)),
  PC4 = c(sqrt(((median(d_pca[147:148,"PC4"]))-d_pca[1:146,"PC4"])^2)),
  PC5 = c(sqrt(((median(d_pca[147:148,"PC5"]))-d_pca[1:146,"PC5"])^2))) %>%
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

pdf(here("analysis","figures","Figure_3","Fig3-c.pdf"), width=5.5, height=3.5)
(E_11_06_PCA_1b | distance_index) + plot_layout(widths = c(4, 2))
dev.off()


#### Fig 3d ####
## E_11_06 & E_11_07 PCA2
s <- joined_data %>% dplyr::filter(Sample %in% c(
    "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-18dup",
    "E-11-19","E-11-06","E-11-07")) %>%
  dplyr::mutate(Location = Sample) %>%
  dplyr::select(Sample,Location,
                SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

IAB <- q5 %>% dplyr::select(
  Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
IAB <- full_join(IAB,s[3:7,])
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s <- s[1:2,]

res.pca <- prcomp(IAB[,3:21], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
E_11_06_PCA_2a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-7, 6)) + scale_y_continuous(limits=c(-7, 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
E_11_06_PCA_2a
pdf(here("analysis","figures","Figure_3","Fig3-d-PCA.pdf"), width=3.5, height=3.5)
E_11_06_PCA_2a
dev.off()

res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, s[,3:21])
pred <- cbind(s[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

E_11_06_PCA_2b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="longdash") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="longdash") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-7, 6)) + scale_y_continuous(limits=c(-7, 5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
E_11_06_PCA_2b

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:191,"Sample"]),
  Location = c(d_pca[1:191,"Location"]),
  PC1 = c(sqrt(((median(d_pca[192:193,"PC1"]))-d_pca[1:191,"PC1"])^2)),
  PC2 = c(sqrt(((median(d_pca[192:193,"PC2"]))-d_pca[1:191,"PC2"])^2)),
  PC3 = c(sqrt(((median(d_pca[192:193,"PC3"]))-d_pca[1:191,"PC3"])^2)),
  PC4 = c(sqrt(((median(d_pca[192:193,"PC4"]))-d_pca[1:191,"PC4"])^2)),
  PC5 = c(sqrt(((median(d_pca[192:193,"PC5"]))-d_pca[1:191,"PC5"])^2)),
  PC6 = c(sqrt(((median(d_pca[192:193,"PC6"]))-d_pca[1:191,"PC6"])^2)),
  PC7 = c(sqrt(((median(d_pca[192:193,"PC7"]))-d_pca[1:191,"PC7"])^2)),
  PC8 = c(sqrt(((median(d_pca[192:193,"PC8"]))-d_pca[1:191,"PC8"])^2)),
  PC9 = c(sqrt(((median(d_pca[192:193,"PC9"]))-d_pca[1:191,"PC9"])^2)),
  PC10 = c(sqrt(((median(d_pca[192:193,"PC10"]))-d_pca[1:191,"PC10"])^2)),
  PC11 = c(sqrt(((median(d_pca[192:193,"PC11"]))-d_pca[1:191,"PC11"])^2)),
  PC12 = c(sqrt(((median(d_pca[192:193,"PC12"]))-d_pca[1:191,"PC12"])^2)),
  PC13 = c(sqrt(((median(d_pca[192:193,"PC13"]))-d_pca[1:191,"PC13"])^2)),
  PC14 = c(sqrt(((median(d_pca[192:193,"PC14"]))-d_pca[1:191,"PC14"])^2)),
  PC15 = c(sqrt(((median(d_pca[192:193,"PC15"]))-d_pca[1:191,"PC15"])^2)),
  PC16 = c(sqrt(((median(d_pca[192:193,"PC16"]))-d_pca[1:191,"PC16"])^2)),
  PC17 = c(sqrt(((median(d_pca[192:193,"PC17"]))-d_pca[1:191,"PC17"])^2)),
  PC18 = c(sqrt(((median(d_pca[192:193,"PC18"]))-d_pca[1:191,"PC18"])^2)),
  PC19 = c(sqrt(((median(d_pca[192:193,"PC19"]))-d_pca[1:191,"PC19"])^2))) %>%
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

pdf(here("analysis","figures","Figure_3","Fig3-d.pdf"), width=5.5, height=3.5)
(E_11_06_PCA_2b | distance_index) + plot_layout(widths = c(4, 2))
dev.off()


#### Fig 3e ####
## K_12_28 PCA1
IAB <- q6 %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>%
  dplyr::filter(Sample %in% c("K-12-28")) %>%
  dplyr::mutate(Location = Sample) %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)

res.pca <- prcomp(IAB[,3:7],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
K_12_28_PCA_1a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_x_continuous(limits=c(-2.5, 3)) + scale_y_continuous(limits=c(-2.8, 3.5)) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1)
K_12_28_PCA_1a
pdf(here("analysis","figures","Figure_3","Fig3-e-PCA.pdf"), width=3.5, height=3.5)
K_12_28_PCA_1a
dev.off()

res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, s[,3:7])
pred <- cbind(s[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

K_12_28_PCA_1b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="longdash") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="longdash") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-2.5, 3)) + scale_y_continuous(limits=c(-2.8, 3.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_12_28_PCA_1b

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:78,"Sample"]),
  Location = c(d_pca[1:78,"Location"]),
  PC1 = c(sqrt(((median(d_pca[79,"PC1"]))-d_pca[1:78,"PC1"])^2)),
  PC2 = c(sqrt(((median(d_pca[79,"PC2"]))-d_pca[1:78,"PC2"])^2)),
  PC3 = c(sqrt(((median(d_pca[79,"PC3"]))-d_pca[1:78,"PC3"])^2)),
  PC4 = c(sqrt(((median(d_pca[79,"PC4"]))-d_pca[1:78,"PC4"])^2)),
  PC5 = c(sqrt(((median(d_pca[79,"PC5"]))-d_pca[1:78,"PC5"])^2))) %>%
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

pdf(here("analysis","figures","Figure_3","Fig3-e.pdf"), width=5.5, height=3.5)
(K_12_28_PCA_1b | distance_index) + plot_layout(widths = c(4, 2))
dev.off()


#### Fig 3f ####
## K_12_28 PCA2
IAB <- q7 %>% dplyr::select(
  Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-28")) %>%
  dplyr::mutate(Location = Sample) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)

res.pca <- prcomp(IAB[,3:21], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
K_12_28_PCA_2a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-6, 5.5)) + scale_y_continuous(limits=c(-2.6, 2.3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1)
K_12_28_PCA_2a
pdf(here("analysis","figures","Figure_3","Fig3-f-PCA.pdf"),
    width=3.5, height=3.5)
K_12_28_PCA_2a
dev.off()

pred <- stats::predict(res.pca, s[,3:21])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

K_12_28_PCA_2b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="longdash") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="longdash") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-6, 5.5)) + scale_y_continuous(limits=c(-2.6, 2.3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_12_28_PCA_2b


# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:97,"Sample"]),
  Location = c(d_pca[1:97,"Location"]),
  PC1 = c(sqrt(((d_pca[98,"PC1"])-d_pca[1:97,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[98,"PC2"])-d_pca[1:97,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[98,"PC3"])-d_pca[1:97,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[98,"PC4"])-d_pca[1:97,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[98,"PC5"])-d_pca[1:97,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[98,"PC6"])-d_pca[1:97,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[98,"PC7"])-d_pca[1:97,"PC7"])^2)),
  PC8 = c(sqrt(((d_pca[98,"PC8"])-d_pca[1:97,"PC8"])^2)),
  PC9 = c(sqrt(((d_pca[98,"PC9"])-d_pca[1:97,"PC9"])^2)),
  PC10 = c(sqrt(((d_pca[98,"PC10"])-d_pca[1:97,"PC10"])^2)),
  PC11 = c(sqrt(((d_pca[98,"PC11"])-d_pca[1:97,"PC11"])^2)),
  PC12 = c(sqrt(((d_pca[98,"PC12"])-d_pca[1:97,"PC12"])^2)),
  PC13 = c(sqrt(((d_pca[98,"PC13"])-d_pca[1:97,"PC13"])^2)),
  PC14 = c(sqrt(((d_pca[98,"PC14"])-d_pca[1:97,"PC14"])^2)),
  PC15 = c(sqrt(((d_pca[98,"PC15"])-d_pca[1:97,"PC15"])^2)),
  PC16 = c(sqrt(((d_pca[98,"PC16"])-d_pca[1:97,"PC16"])^2)),
  PC17 = c(sqrt(((d_pca[98,"PC17"])-d_pca[1:97,"PC17"])^2)),
  PC18 = c(sqrt(((d_pca[98,"PC18"])-d_pca[1:97,"PC18"])^2)),
  PC19 = c(sqrt(((d_pca[98,"PC19"])-d_pca[1:97,"PC19"])^2))) %>%
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

pdf(here("analysis","figures","Figure_3","Fig3-f.pdf"), width=5.5, height=3.5)
(K_12_28_PCA_2b | distance_index) + plot_layout(widths = c(4, 2))
dev.off()


#### Fig 3g ####
## K_12_29 PCA1
IAB <- q8 %>% dplyr::select(
  Sample,Location,Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)
s_d <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-18dup")) %>%
  dplyr::mutate(Location = "Vanuatu Arc")%>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)
IAB <- full_join(IAB,s_d)
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-29")) %>%
  dplyr::mutate(Location = Sample)%>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)
is.na(s) <- sapply(s, is.infinite) #replace Inf by NA
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(IAB[,3:7],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
K_12_29_PCA_1a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_x_continuous(limits=c(-3.5, 3.5)) + scale_y_continuous(limits=c(-3.2, 2.5)) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1)
K_12_29_PCA_1a
pdf(here("analysis","figures","Figure_3","Fig3-g-PCA.pdf"), width=3.5, height=3.5)
K_12_29_PCA_1a
dev.off()

res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
pred <- stats::predict(res.pca, s[,3:7])
pred <- cbind(s[,1:2], pred)
d_pca <- full_join(res.pca.df, pred)

K_12_29_PCA_1b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="longdash") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="longdash") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-3.5, 3.5)) + scale_y_continuous(limits=c(-3.2, 2.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_12_29_PCA_1b

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:128,"Sample"]),
  Location = c(d_pca[1:128,"Location"]),
  PC1 = c(sqrt(((median(d_pca[129,"PC1"]))-d_pca[1:128,"PC1"])^2)),
  PC2 = c(sqrt(((median(d_pca[129,"PC2"]))-d_pca[1:128,"PC2"])^2)),
  PC3 = c(sqrt(((median(d_pca[129,"PC3"]))-d_pca[1:128,"PC3"])^2)),
  PC4 = c(sqrt(((median(d_pca[129,"PC4"]))-d_pca[1:128,"PC4"])^2)),
  PC5 = c(sqrt(((median(d_pca[129,"PC5"]))-d_pca[1:128,"PC5"])^2))) %>%
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

pdf(here("analysis","figures","Figure_3","Fig3-g.pdf"), width=5.5, height=3.5)
(K_12_29_PCA_1b | distance_index) + plot_layout(widths = c(4, 2))
dev.off()


#### Fig 3h ####
## K_12_29 PCA2
IAB <- q9 %>% dplyr::select(
  Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
s_d <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-18dup")) %>%
  dplyr::mutate(Location = "Vanuatu Arc")%>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
IAB <- full_join(IAB,s_d)
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% dplyr::filter(Sample %in% c("K-12-29")) %>%
  dplyr::mutate(Location = Sample)%>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Ti,Eu,Gd,Y,Yb)
is.na(s) <- sapply(s, is.infinite) #replace Inf by NA
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(IAB[,3:21], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
K_12_29_PCA_2a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-5, 7)) + scale_y_continuous(limits=c(-5.5, 4)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1)
K_12_29_PCA_2a
pdf(here("analysis","figures","Figure_3","Fig3-h-PCA.pdf"), width=3.5, height=3.5)
K_12_29_PCA_2a
dev.off()

pred <- stats::predict(res.pca, s[,3:21])
pred <- cbind(s[,1:2], pred)
res.pca.df <- cbind(IAB[,1:2], (as.data.frame(res.pca$x)))
d_pca <- full_join(res.pca.df, pred)

K_12_29_PCA_2b <- d_pca %>%
  ggplot(aes(x=PC1,y=PC2, shape=factor(Location), fill=factor(Location),
             color=factor(Location), group=Sample)) +
  geom_vline(aes(xintercept = 0), size=.25, linetype="longdash") +
  geom_hline(aes(yintercept = 0), size=.25, linetype="longdash") +
  geom_point(size = 3, stroke=.25) + scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  scale_x_continuous(limits=c(-5, 7)) + scale_y_continuous(limits=c(-5.5, 4)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_12_29_PCA_2b

# PC values > distance to artefacts (individual or median of group)
# distance within all PCs > weight mean distance
dist <- data.frame(
  Sample = c(d_pca[1:178,"Sample"]),
  Location = c(d_pca[1:178,"Location"]),
  PC1 = c(sqrt(((d_pca[179,"PC1"])-d_pca[1:178,"PC1"])^2)),
  PC2 = c(sqrt(((d_pca[179,"PC2"])-d_pca[1:178,"PC2"])^2)),
  PC3 = c(sqrt(((d_pca[179,"PC3"])-d_pca[1:178,"PC3"])^2)),
  PC4 = c(sqrt(((d_pca[179,"PC4"])-d_pca[1:178,"PC4"])^2)),
  PC5 = c(sqrt(((d_pca[179,"PC5"])-d_pca[1:178,"PC5"])^2)),
  PC6 = c(sqrt(((d_pca[179,"PC6"])-d_pca[1:178,"PC6"])^2)),
  PC7 = c(sqrt(((d_pca[179,"PC7"])-d_pca[1:178,"PC7"])^2)),
  PC8 = c(sqrt(((d_pca[179,"PC8"])-d_pca[1:178,"PC8"])^2)),
  PC9 = c(sqrt(((d_pca[179,"PC9"])-d_pca[1:178,"PC9"])^2)),
  PC10 = c(sqrt(((d_pca[179,"PC10"])-d_pca[1:178,"PC10"])^2)),
  PC11 = c(sqrt(((d_pca[179,"PC11"])-d_pca[1:178,"PC11"])^2)),
  PC12 = c(sqrt(((d_pca[179,"PC12"])-d_pca[1:178,"PC12"])^2)),
  PC13 = c(sqrt(((d_pca[179,"PC13"])-d_pca[1:178,"PC13"])^2)),
  PC14 = c(sqrt(((d_pca[179,"PC14"])-d_pca[1:178,"PC14"])^2)),
  PC15 = c(sqrt(((d_pca[179,"PC15"])-d_pca[1:178,"PC15"])^2)),
  PC16 = c(sqrt(((d_pca[179,"PC16"])-d_pca[1:178,"PC16"])^2)),
  PC17 = c(sqrt(((d_pca[179,"PC17"])-d_pca[1:178,"PC17"])^2)),
  PC18 = c(sqrt(((d_pca[179,"PC18"])-d_pca[1:178,"PC18"])^2)),
  PC19 = c(sqrt(((d_pca[179,"PC19"])-d_pca[1:178,"PC19"])^2))) %>%
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

pdf(here("analysis","figures","Figure_3","Fig3-h.pdf"), width=5.5, height=3.5)
(K_12_29_PCA_2b | distance_index) + plot_layout(widths = c(4, 2))
dev.off()

