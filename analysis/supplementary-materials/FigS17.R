require(here)
require(tidyverse)
require(osmdata)
require(patchwork)

#### Fig S17 ####
OIB <- q33
OIB %>% group_by(site_name) %>% tally() %>% print(n=30)

OIB <- OIB %>%
  dplyr::mutate(Category = case_when(
    grepl("Artefact", sample_comment) ~ "artefact",
    sample_category == "SOURCE" ~ "source",
    sample_category == "ARTEFACT" ~ "artefact")) %>%
  dplyr::mutate(Location = case_when(
    site_name == "Tatagamatau" & Category == "source" ~ "Tatagamatau-source",
    site_name == "Tatagamatau" & Category == "artefact" ~ "Tatagamatau-artefact",
    grepl("MALAELOA", Sample) & Category == "source" ~ "Malaeloa-source",
    grepl("MALAELOA", Sample) & Category == "artefact" ~ "Malaeloa-artefact",
    grepl("MALOATA", Sample) & Category == "source" ~ "Maloata-source",
    grepl("MALOATA", Sample) & Category == "artefact" ~ "Maloata-artefact",
    TRUE ~ "Tutuila"))

cols <- c("Tatagamatau-source"="#77246C","Tatagamatau-artefact"="#77246C",
          "Malaeloa-source"="#D694C9","Malaeloa-artefact"="#D694C9",
          "Maloata-source"="#B262A7","Maloata-artefact"="#B262A7")
shapes <- c("Tatagamatau-source"=21,"Tatagamatau-artefact"=1,
            "Malaeloa-source"=25,"Malaeloa-artefact"=6,
            "Maloata-source"=23,"Maloata-artefact"=5)
contour <- c("Tatagamatau-source"="black","Tatagamatau-artefact"="#77246C",
             "Malaeloa-source"="black","Malaeloa-artefact"="#D694C9",
             "Maloata-source"="black","Maloata-artefact"="#B262A7")

coast <- getbb("Tutuila Samoa")%>% opq()%>%
  add_osm_feature(key = "natural", value = c("coastline")) %>%
  osmdata_sf()
rivers <- getbb("Tutuila Samoa")%>% opq()%>%
  add_osm_feature(key = "waterway", value = c("river","stream")) %>%
  osmdata_sf()

m <- ggplot() +
  geom_sf(data = coast$osm_lines, inherit.aes = FALSE, color = "black",
          size = .4, alpha = .8) +
  geom_sf(data = rivers$osm_lines, inherit.aes = FALSE, color = "#a6daff",
          size = .2, alpha = .8) +
  coord_sf(xlim = c(-170.85, -170.56), ylim = c(-14.39, -14.225), expand = FALSE) +
  theme_void()
tutuila <- m +
  geom_point(data=OIB,
             mapping=aes(x=long, y=lat, shape=factor(Location), fill=factor(Location),
                         color=factor(Location), group = Location), size=2.5) +
  scale_shape_manual(values=shapes) +
  scale_fill_manual(values=cols) + scale_color_manual(values=contour) +
  theme(panel.border = element_rect(colour = NA, fill= NA, size=1),
        panel.background = element_rect(colour = "black", fill= NA, size=1),
        legend.title = element_blank(), legend.text = element_text(size = 10),
        legend.position = c(.83,.32))

pdf(here("analysis","supplementary-materials","FigS17.pdf"), width=8, height=5)
tutuila
dev.off()
