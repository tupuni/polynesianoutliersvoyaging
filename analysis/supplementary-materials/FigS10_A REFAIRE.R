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

dir.create(here("analysis","supplementary-materials","FigS10"))

#### Fig S10a ####
## E_11_03 PCA1
q1 <- q1 %>% dplyr::select(
  Sample,Location,Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)

s <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-18dup",
  "E-11-19","E-11-03")) %>%
  dplyr::mutate(Location = Sample)%>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)

IAB <- full_join(q1,s[3:6,])
s <- s[1,]

is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

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
  Sample = c(d_pca[1:75,"Sample"]),
  Location = c(d_pca[1:75,"Location"]),
  PC1 = c(sqrt(((median(d_pca[76,"PC1"]))-d_pca[1:75,"PC1"])^2)),
  PC2 = c(sqrt(((median(d_pca[76,"PC2"]))-d_pca[1:75,"PC2"])^2)),
  PC3 = c(sqrt(((median(d_pca[76,"PC3"]))-d_pca[1:75,"PC3"])^2)),
  PC4 = c(sqrt(((median(d_pca[76,"PC4"]))-d_pca[1:75,"PC4"])^2)),
  PC5 = c(sqrt(((median(d_pca[76,"PC5"]))-d_pca[1:75,"PC5"])^2))) %>%
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


PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[76,"PC1"], xend=d_pca[76,"PC1"],
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
  annotate("segment", x=d_pca[76,"PC2"], xend=d_pca[76,"PC2"],
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
  annotate("segment", x=d_pca[76,"PC3"], xend=d_pca[76,"PC3"],
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
  annotate("segment", x=d_pca[76,"PC4"], xend=d_pca[76,"PC4"],
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
  annotate("segment", x=d_pca[76,"PC5"], xend=d_pca[76,"PC5"],
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

pdf(here("analysis","supplementary-materials","FigS10","FigS10-a.pdf"), width=6, height=3.5)
(E_11_03_PCA_1b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()


#### Fig S10b ####
## E_11_03 PCA2
IAB <- full_join(q2,q3) %>%
  dplyr::select(
    Sample,Location,TiO2,Al2O3,MgO,CaO,V,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Eu,Y,Yb)

s <- joined_data %>%
  filter(Sample %in% c("E-11-03")) %>%
  mutate(Location = case_when(grepl("E-11-03", Sample) ~ "E-11-03")) %>%
  dplyr::select(
    Sample,Location,TiO2,Al2O3,MgO,CaO,V,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Eu,Y,Yb)

is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(IAB[,3:21],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
fviz_eig(res.pca)
eig <- get_eig(res.pca)
E_11_03_PCA_2a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) + scale_x_continuous(limits=c(-6, 5)) +
  scale_y_continuous(limits=c(-7, 3), breaks=c(-6, -3, 0, 3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
E_11_03_PCA_2a
pdf(here("analysis","supplementary-materials","FigS10","FigS10-b-PCA.pdf"), width=3.5, height=3.5)
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
  scale_x_continuous(limits=c(-6, 5)) +
  scale_y_continuous(limits=c(-7, 3), breaks=c(-6, -3, 0, 3)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
E_11_03_PCA_2b

## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[34,"PC1"], xend=d_pca[34,"PC1"],
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
  annotate("segment", x=d_pca[34,"PC2"], xend=d_pca[34,"PC2"],
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
  annotate("segment", x=d_pca[34,"PC3"], xend=d_pca[34,"PC3"],
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
  annotate("segment", x=d_pca[34,"PC4"], xend=d_pca[34,"PC4"],
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
  annotate("segment", x=d_pca[34,"PC5"], xend=d_pca[34,"PC5"],
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

pdf(here("analysis","supplementary-materials","FigS10","FigS10-b.pdf"), width=6, height=3.5)
(E_11_03_PCA_2b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()

#### Fig S10c ####
## E_11_06 & E_11_07 PCA1
s <- joined_data %>%
  filter(Sample %in% c(
    "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-18dup",
    "E-11-19","E-11-06","E-11-07")) %>%
  mutate(
    Location = case_when(
      grepl("E-11-10", Sample) ~ "Vanuatu Arc",
      grepl("E-11-11", Sample) ~ "Vanuatu Arc",
      grepl("E-11-13", Sample) ~ "Vanuatu Arc",
      grepl("E-11-16", Sample) ~ "Vanuatu Arc",
      grepl("E-11-18", Sample) ~ "Vanuatu Arc",
      grepl("E-11-18dup", Sample) ~ "Vanuatu Arc",
      grepl("E-11-19", Sample) ~ "Vanuatu Arc",
      grepl("E-11-06", Sample) ~ "E-11-06",
      grepl("E-11-07", Sample) ~ "E-11-07")) %>% dplyr::select(
        Sample,Location, Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)
s <- s[c("3","4","5","6","7","8","9","1","2"),]

IAB <- q4 %>% dplyr::select(
  Sample,Location,Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204,Nd143_Nd144)

IAB <- full_join(IAB,s[1:5,])
s <- s[8:9,]

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
pdf(here("analysis","supplementary-materials","FigS10","FigS10-c-PCA.pdf"), width=3.5, height=3.5)
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

## PCs
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[154:155,"PC1"], xend=d_pca[154:155,"PC1"],
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
  annotate("segment", x=d_pca[154:155,"PC2"], xend=d_pca[154:155,"PC2"],
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
  annotate("segment", x=d_pca[154:155,"PC3"], xend=d_pca[154:155,"PC3"],
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
  annotate("segment", x=d_pca[154:155,"PC4"], xend=d_pca[154:155,"PC4"],
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
  annotate("segment", x=d_pca[154:155,"PC5"], xend=d_pca[154:155,"PC5"],
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

pdf(here("analysis","supplementary-materials","FigS10","FigS10-c.pdf"), width=6, height=3.5)
(E_11_06_PCA_1b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()


#### Fig S10d ####
## E_11_06 & E_11_07 PCA2
s <- joined_data %>% filter(Sample %in% c(
    "E-11-10","E-11-11","E-11-13","E-11-16","E-11-18","E-11-18dup",
    "E-11-19","E-11-06","E-11-07")) %>%
  mutate(
    Location = case_when(
      grepl("E-11-10", Sample) ~ "Vanuatu Arc",
      grepl("E-11-11", Sample) ~ "Vanuatu Arc",
      grepl("E-11-13", Sample) ~ "Vanuatu Arc",
      grepl("E-11-16", Sample) ~ "Vanuatu Arc",
      grepl("E-11-18", Sample) ~ "Vanuatu Arc",
      grepl("E-11-18dup", Sample) ~ "Vanuatu Arc",
      grepl("E-11-19", Sample) ~ "Vanuatu Arc",
      grepl("E-11-06", Sample) ~ "E-11-06",
      grepl("E-11-07", Sample) ~ "E-11-07")) %>% dplyr::select(
  #Sample,Location,SiO2,TiO2,Al2O3,MgO,CaO,Na2O,Rb,Ba,Th,Nb,Ce,Nd,Sr,Sm,Zr,Yb)
Sample,Location,SiO2,K2O,Na2O,MgO,V,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Y,Yb,Pb)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA
s <- s[c("3","4","5","6","7","8","9","1","2"),]

IAB <- q5 %>% dplyr::select(
  Sample,Location,SiO2,K2O,Na2O,MgO,V,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Y,Yb,Pb)

IAB <- full_join(IAB,s[1:5,])
s <- s[8:9,]

is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(IAB[,3:21], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
E_11_06_PCA_2a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  scale_x_continuous(limits=c(-4.5, 7.5)) + scale_y_continuous(limits=c(-3, 4.2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.key.size = unit(.2, 'cm'), legend.position = "none", aspect.ratio=1)
E_11_06_PCA_2a
pdf(here("analysis","supplementary-materials","FigS10","FigS10-d-PCA.pdf"), width=3.5, height=3.5)
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
  scale_x_continuous(limits=c(-4.5, 7.5)) + scale_y_continuous(limits=c(-3, 4.2)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
E_11_06_PCA_2b

## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[90:91,"PC1"], xend=d_pca[90:91,"PC1"],
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
  annotate("segment", x=d_pca[90:91,"PC2"], xend=d_pca[90:91,"PC2"],
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
  annotate("segment", x=d_pca[90:91,"PC3"], xend=d_pca[90:91,"PC3"],
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
  annotate("segment", x=d_pca[90:91,"PC4"], xend=d_pca[90:91,"PC4"],
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
  annotate("segment", x=d_pca[90:91,"PC5"], xend=d_pca[90:91,"PC5"],
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

pdf(here("analysis","supplementary-materials","FigS10","FigS10-d.pdf"), width=6, height=3.5)
(E_11_06_PCA_2b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()


#### Fig S10e ####
## K_12_28 PCA1
s <- joined_data %>%
  filter(Sample %in% c("K-12-28")) %>%
  mutate(Location = case_when(grepl("K-12-28", Sample) ~ "K-12-28")) %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

IAB <- q6 %>%
  dplyr::select(Sample,Location,
                Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

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
pdf(here("analysis","supplementary-materials","FigS10","FigS10-e-PCA.pdf"), width=3.5, height=3.5)
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

## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[79,"PC1"], xend=d_pca[79,"PC1"],
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
  annotate("segment", x=d_pca[79,"PC2"], xend=d_pca[79,"PC2"],
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
  annotate("segment", x=d_pca[79,"PC3"], xend=d_pca[79,"PC3"],
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
  annotate("segment", x=d_pca[79,"PC4"], xend=d_pca[79,"PC4"],
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
  annotate("segment", x=d_pca[79,"PC5"], xend=d_pca[79,"PC5"],
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

pdf(here("analysis","supplementary-materials","FigS10","FigS10-e.pdf"), width=6, height=3.5)
(K_12_28_PCA_1b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()


#### Fig S10f ####
## K_12_28 PCA2
IAB <- q7 %>% dplyr::select(
  Sample,Location,SiO2,K2O,Na2O,MgO,V,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Y,Yb,Pb)
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% filter(Sample %in% c("K-12-28")) %>%
  mutate(Location = case_when(grepl("K-12-28", Sample) ~ "K-12-28")) %>%
  dplyr::select(
    Sample,Location,SiO2,K2O,Na2O,MgO,V,Rb,Ba,Th,U,Nb,La,Ce,Nd,Sr,Sm,Zr,Y,Yb,Pb)

res.pca <- prcomp(IAB[,3:21], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
#fviz_pca_ind(res.pca, geom = "text") # plot of individual data points
K_12_28_PCA_2a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_x_continuous(limits=c(-5, 5.5)) + scale_y_continuous(limits=c(-3.8, 4)) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1)
K_12_28_PCA_2a
pdf(here("analysis","supplementary-materials","FigS10","FigS10-f-PCA.pdf"),
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
  scale_x_continuous(limits=c(-5, 5.5)) + scale_y_continuous(limits=c(-3.8, 4)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_12_28_PCA_2b

## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[53,"PC1"], xend=d_pca[53,"PC1"],
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
  annotate("segment", x=d_pca[53,"PC2"], xend=d_pca[53,"PC2"],
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
  annotate("segment", x=d_pca[53,"PC3"], xend=d_pca[53,"PC3"],
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
  annotate("segment", x=d_pca[53,"PC4"], xend=d_pca[53,"PC4"],
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
  annotate("segment", x=d_pca[53,"PC5"], xend=d_pca[53,"PC5"],
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

pdf(here("analysis","supplementary-materials","FigS10","FigS10-f.pdf"), width=6, height=3.5)
(K_12_28_PCA_2b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()


#### Fig S10g ####
## K_12_29 PCA1
s <- joined_data %>% filter(Sample %in% c("K-12-29")) %>%
  mutate(Location = case_when(grepl("K-12-29", Sample) ~ "K-12-29")) %>%
  dplyr::select(Sample,Location,
                Nd143_Nd144,Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
s[s == 0] <- NA # Replace 0 with NA
s <- s[rowSums(is.na(s)) == 0,] # removes rows with missing info for PCA

IAB <- q8 %>%
  dplyr::select(Sample,Location,
                Nd143_Nd144,Sr87_Sr86,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

res.pca <- prcomp(IAB[,3:7],  scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
K_12_29_PCA_1a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_x_continuous(limits=c(-3.5, 3)) + scale_y_continuous(limits=c(-2.5, 2.8)) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1)
K_12_29_PCA_1a
pdf(here("analysis","supplementary-materials","FigS10","FigS10-g-PCA.pdf"), width=3.5, height=3.5)
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
  scale_x_continuous(limits=c(-3.5, 3)) + scale_y_continuous(limits=c(-2.5, 2.8)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_12_29_PCA_1b

## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[129,"PC1"], xend=d_pca[129,"PC1"],
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
  annotate("segment", x=d_pca[129,"PC2"], xend=d_pca[129,"PC2"],
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
  annotate("segment", x=d_pca[129,"PC3"], xend=d_pca[129,"PC3"],
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
  annotate("segment", x=d_pca[129,"PC4"], xend=d_pca[129,"PC4"],
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
  annotate("segment", x=d_pca[129,"PC5"], xend=d_pca[129,"PC5"],
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

pdf(here("analysis","supplementary-materials","FigS10","FigS10-g.pdf"), width=6, height=3.5)
(K_12_29_PCA_1b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()


#### Fig S10h ####
## K_12_29 PCA2
IAB <- q9 %>%  dplyr::select(
  Sample,Location,TiO2,MnO,MgO,V,Cu,Rb,Sr,Y,Zr,Nb,La,Ce,Nd,Eu,Tb,Dy,Yb,Lu,Hf)
is.na(IAB) <- sapply(IAB, is.infinite) #replace Inf by NA
IAB[IAB == 0] <- NA # Replace 0 with NA
IAB <- IAB[rowSums(is.na(IAB)) == 0,] # removes rows with missing info for PCA

s <- joined_data %>% filter(Sample %in% c("K-12-29")) %>%
  mutate(Location = case_when(grepl("K-12-29", Sample) ~ "K-12-29")) %>%
  dplyr::select(
    Sample,Location,TiO2,MnO,MgO,V,Cu,Rb,Sr,Y,Zr,Nb,La,Ce,Nd,Eu,Tb,Dy,Yb,Lu,Hf)

res.pca <- prcomp(IAB[,3:21], scale = TRUE, center = TRUE) # Dimension reduction using PCA
eig <- get_eig(res.pca)
K_12_29_PCA_2a <- fviz_pca_biplot(
  res.pca, label = "var", col.var = "black", alpha.var = .2,
  habillage = IAB$Location, fill.ind = IAB$Location,
  pointsize = 2, invisible = "quali", labelsize = 3, repel = T) +
  scale_x_continuous(limits=c(-5, 7.5)) + scale_y_continuous(limits=c(-9, 5.5)) +
  scale_shape_manual(values=shapes) + scale_fill_manual(values=cols) +
  scale_color_manual(values=cols) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1)
K_12_29_PCA_2a
pdf(here("analysis","supplementary-materials","FigS10","FigS10-h-PCA.pdf"),
    width=3.5, height=3.5)
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
  scale_x_continuous(limits=c(-5, 7.5)) + scale_y_continuous(limits=c(-9, 5.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"), title = element_blank(),
        axis.title = element_text(size = 9), axis.text = element_text(size = 8),
        legend.position = "none", aspect.ratio=1) +
  labs(x=paste0("PC1 (",round(eig["Dim.1","variance.percent"], digits = 1),"%)"),
       y=paste0("PC2 (",round(eig["Dim.2","variance.percent"], digits = 1),"%)"))
K_12_29_PCA_2b

## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[160,"PC1"], xend=d_pca[160,"PC1"],
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
  annotate("segment", x=d_pca[160,"PC2"], xend=d_pca[160,"PC2"],
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
  annotate("segment", x=d_pca[160,"PC3"], xend=d_pca[160,"PC3"],
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
  annotate("segment", x=d_pca[160,"PC4"], xend=d_pca[160,"PC4"],
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
  annotate("segment", x=d_pca[160,"PC5"], xend=d_pca[160,"PC5"],
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

pdf(here("analysis","supplementary-materials","FigS10","FigS10-h.pdf"), width=6, height=3.5)
(K_12_29_PCA_2b | PCs) + plot_layout(widths = c(3.5, 2.5))
dev.off()
