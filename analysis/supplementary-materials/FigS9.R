require(here)
require(tidyverse)
require(patchwork)
require(BBmisc)

## Figure S9
# load primitive mantle and average values for OIB and IAB from Sun & McDonough (1989) & Niu & O'hara (2003)
sunmcdonough <- read.csv(here("analysis", "data", "raw_data", "sun_mcdonough.csv"),
                         header=TRUE, sep=",", stringsAsFactors=FALSE)
# tidy and normalize datasets
oib <- sunmcdonough %>% dplyr::filter(Sample=="OIB") %>% normalize_to_pm()
iab <- sunmcdonough %>% dplyr::filter(Sample=="IAB") %>% normalize_to_pm()

shapes <- c("E-11-03"=0,"E-11-06"=1,"E-11-07"=8,"E-11-10"=0,"E-11-11"=1,
            "E-11-13"=2,"E-11-16"=5,"E-11-18"=6,"E-11-19"=8,"K-12-28"=3,"K-12-29"=4,
            "E-11-08"=5,"E-11-08dup"=9,"T-12-06"=2,"T-12-06dup"=14,
            "T-12-07"=7,"T-12-08"=6,"T-12-09"=10,"T-12-10"=11,"K-12-24"=12,
            "K-12-25"=13,"K-12-26"=14,"OIB"=21,"IAB"=23)
cols <- c("E-11-03"="red","E-11-06"="red","E-11-07"="red","E-11-10"="#7AD04F",
          "E-11-11"="#7AD04F","E-11-13"="#7AD04F","E-11-16"="#7AD04F",
          "E-11-18"="#7AD04F","E-11-19"="#7AD04F","K-12-28"="red","K-12-29"="red",
          "E-11-08"="red","E-11-08dup"="red","T-12-06"="red","T-12-06dup"="red",
          "T-12-07"="red","T-12-08"="red","T-12-09"="red",
          "T-12-10"="red","K-12-24"="red","K-12-25"="red","K-12-26"="red",
          "OIB"="yellow","IAB"="blue")
contour <- c("E-11-03"="red","E-11-06"="red","E-11-07"="red","E-11-10"="#7AD04F",
          "E-11-11"="#7AD04F","E-11-13"="#7AD04F","E-11-16"="#7AD04F",
          "E-11-18"="#7AD04F","E-11-19"="#7AD04F","K-12-28"="red","K-12-29"="red",
          "E-11-08"="red","E-11-08dup"="red","T-12-06"="red","T-12-06dup"="red",
          "T-12-07"="red","T-12-08"="red","T-12-09"="red",
          "T-12-10"="red","K-12-24"="red","K-12-25"="red","K-12-26"="red",
          "OIB"="black","IAB"="black")

p1 <- joined_data_norm %>%
  dplyr::filter(Sample %in% c(
    "E-11-10", "E-11-11", "E-11-13", "E-11-16", "E-11-18", "E-11-19")) %>%
  dplyr::mutate(var = fct_relevel(
    var, "Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr","Pb","Nd","Sr","Sm",
    "Zr","Hf","Ti","Eu","Gd","Tb","Dy","Ho","Y","Er","Li","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, color=factor(Sample), shape=factor(Sample),
             fill=factor(Sample), group=Sample)) +
  geom_line() + geom_point(size=3) +
  geom_line(data=iab) + geom_point(data=iab, size=3) +
  scale_shape_manual(values = shapes) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = contour) +
  scale_y_log10(breaks=c(1,10,100), limits=c(.1,200), labels = scales::comma) +
  ylab("Sample / Primitive mantle") + annotation_logticks(sides="l") +
  theme(panel.border=element_rect(fill=NA,size=.8),panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text=element_text(size=9),axis.ticks.y=element_blank(),
        legend.title=element_blank(),legend.text=element_text(size=9),
        legend.position = "none") +
  annotate("text", x=27, y=140, size=6, label="A", fontface="bold")
p2 <- joined_data_norm %>%
  dplyr::filter(Sample %in% c("E-11-03", "E-11-06", "E-11-07", "K-12-28", "K-12-29")) %>%
  dplyr::mutate(var = fct_relevel(
    var,"Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr","Pb","Nd","Sr","Sm",
    "Zr","Hf","Ti","Eu","Gd","Tb","Dy","Ho","Y","Er","Li","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, color=factor(Sample), shape=factor(Sample),
             fill=factor(Sample), group=Sample)) +
  geom_line() + geom_point(size=3) +
  geom_line(data=iab) + geom_point(data=iab, size=3) +
  scale_shape_manual(values = shapes) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = contour) +
  scale_y_log10(breaks=c(1,10,100), limits=c(.1,200), labels = scales::comma) +
  ylab("Sample / Primitive mantle") + annotation_logticks(sides="l") +
  theme(panel.border=element_rect(fill=NA,size=.8),panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_text(size=9),
        axis.text=element_text(size=9),axis.ticks.y=element_blank(),
        legend.title=element_blank(),legend.text=element_text(size=9),
        legend.position = "none") +
  annotate("text", x=27, y=140, size=6, label="B", fontface="bold")
p3 <- joined_data_norm %>%
  dplyr::filter(Sample %in% c(
    "E-11-08","K-12-24","K-12-25","K-12-26","T-12-06","T-12-06dup",
    "T-12-07","T-12-08","T-12-09","T-12-10")) %>%
  dplyr::mutate(var = fct_relevel(
    var,"Cs","Rb","Ba","Th","U","Nb","Ta","La","Ce","Pr","Pb","Nd","Sr","Sm",
    "Zr","Hf","Ti","Eu","Gd","Tb","Dy","Ho","Y","Er","Li","Yb","Lu")) %>%
  ggplot(aes(x=var, y=conc, color=factor(Sample), shape=factor(Sample),
             fill=factor(Sample), group=Sample)) +
  geom_line() + geom_point(size=3) +
  geom_line(data=oib) + geom_point(data=oib, size=3) +
  scale_shape_manual(values = shapes) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = contour) +
  scale_y_log10(breaks=c(1,10,100), limits=c(.1,200), labels = scales::comma) +
  ylab("Sample / Primitive mantle") + annotation_logticks(sides="l") +
  theme(panel.border=element_rect(fill=NA,size=.8),panel.background=element_blank(),
        panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text=element_text(size=9),axis.ticks.y=element_blank(),
        legend.title=element_blank(),legend.text=element_text(size=9),
        legend.position = "none") +
  annotate("text", x=27, y=140, size=6, label="C", fontface="bold")
S9 <- p1/p2/p3
S9

pdf(here("analysis","supplementary-materials","FigS9.pdf"), width=8, height=8)
S9
dev.off()

