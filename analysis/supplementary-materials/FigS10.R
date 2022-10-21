require(here)
require(tidyverse)
require(patchwork)
require(stats)
require(FactoMineR)
require(factoextra)
library(ggridges)

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
pdf(here("analysis","supplementary-materials","FigS10","FigS10-a-PCA.pdf"), width=3.5, height=3.5)
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

## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[77,"PC1"], xend=d_pca[77,"PC1"],
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
  annotate("segment", x=d_pca[77,"PC2"], xend=d_pca[77,"PC2"],
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
  annotate("segment", x=d_pca[77,"PC3"], xend=d_pca[77,"PC3"],
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
  annotate("segment", x=d_pca[77,"PC4"], xend=d_pca[77,"PC4"],
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
  annotate("segment", x=d_pca[77,"PC5"], xend=d_pca[77,"PC5"],
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
  scale_x_continuous(limits=c(-3, 6)) + scale_y_continuous(limits=c(-4, 9)) +
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
  annotate("segment", x=d_pca[39,"PC1"], xend=d_pca[39,"PC1"],
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
  annotate("segment", x=d_pca[39,"PC2"], xend=d_pca[39,"PC2"],
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
  annotate("segment", x=d_pca[39,"PC3"], xend=d_pca[39,"PC3"],
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
  annotate("segment", x=d_pca[39,"PC4"], xend=d_pca[39,"PC4"],
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
  annotate("segment", x=d_pca[39,"PC5"], xend=d_pca[39,"PC5"],
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
  annotate("segment", x=d_pca[147:148,"PC1"], xend=d_pca[147:148,"PC1"],
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
  annotate("segment", x=d_pca[147:148,"PC2"], xend=d_pca[147:148,"PC2"],
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
  annotate("segment", x=d_pca[147:148,"PC3"], xend=d_pca[147:148,"PC3"],
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
  annotate("segment", x=d_pca[147:148,"PC4"], xend=d_pca[147:148,"PC4"],
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
  annotate("segment", x=d_pca[147:148,"PC5"], xend=d_pca[147:148,"PC5"],
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
  scale_x_continuous(limits=c(-7, 6)) + scale_y_continuous(limits=c(-7, 5)) +
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
  annotate("segment", x=d_pca[192:193,"PC1"], xend=d_pca[192:193,"PC1"],
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
  annotate("segment", x=d_pca[192:193,"PC2"], xend=d_pca[192:193,"PC2"],
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
  annotate("segment", x=d_pca[192:193,"PC3"], xend=d_pca[192:193,"PC3"],
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
  annotate("segment", x=d_pca[192:193,"PC4"], xend=d_pca[192:193,"PC4"],
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
  annotate("segment", x=d_pca[192:193,"PC5"], xend=d_pca[192:193,"PC5"],
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
  scale_x_continuous(limits=c(-6, 5.5)) + scale_y_continuous(limits=c(-2.6, 2.3)) +
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
  annotate("segment", x=d_pca[98,"PC1"], xend=d_pca[98,"PC1"],
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
  annotate("segment", x=d_pca[98,"PC2"], xend=d_pca[98,"PC2"],
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
  annotate("segment", x=d_pca[98,"PC3"], xend=d_pca[98,"PC3"],
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
  annotate("segment", x=d_pca[98,"PC4"], xend=d_pca[98,"PC4"],
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
  annotate("segment", x=d_pca[98,"PC5"], xend=d_pca[98,"PC5"],
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
  scale_x_continuous(limits=c(-3.5, 3.5)) + scale_y_continuous(limits=c(-3.2, 2.5)) +
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
pdf(here("analysis","supplementary-materials","FigS10","FigS10-h-PCA.pdf"), width=3.5, height=3.5)
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

## PCs : density plots
PC1 <- ggplot(d_pca, aes(x = PC1, y = 0)) +
  geom_density_ridges(aes(color=Location, fill=Location), alpha=0.4, size=.25) +
  annotate("segment", x=d_pca[179,"PC1"], xend=d_pca[179,"PC1"],
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
  annotate("segment", x=d_pca[179,"PC2"], xend=d_pca[179,"PC2"],
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
  annotate("segment", x=d_pca[179,"PC3"], xend=d_pca[179,"PC3"],
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
  annotate("segment", x=d_pca[179,"PC4"], xend=d_pca[179,"PC4"],
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
  annotate("segment", x=d_pca[179,"PC5"], xend=d_pca[179,"PC5"],
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
