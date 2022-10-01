require(here)
require(tidyverse)
require(RSQLite)
require(osmdata)
require(patchwork)

georoc <- dbConnect(RSQLite::SQLite(), path_to_georoc)
pofatu <- dbConnect(RSQLite::SQLite(), path_to_pofatu)

#### Fig S17 ####
OIB <- dbGetQuery(pofatu,
"SELECT s.id AS sample_id, s.sample_category,
s.location_subregion, s.site_name, s.sample_comment,
s.location_latitude, s.location_longitude,
max(CASE WHEN m.parameter='SiO2 [%]' then m.value END) AS 'SiO2 [%]',
max(CASE WHEN m.parameter='TiO2 [%]' then m.value END) AS 'TiO2 [%]',
max(CASE WHEN m.parameter='Al2O3 [%]' then m.value END) AS 'Al2O3 [%]',
max(CASE WHEN m.parameter='Fe2O3 [%]' then m.value END) AS 'Fe2O3 [%]',
max(CASE WHEN m.parameter='FeO [%]' then m.value END) AS 'FeO [%]',
max(CASE WHEN m.parameter='MnO [%]' then m.value END) AS 'MnO [%]',
max(CASE WHEN m.parameter='MgO [%]' then m.value END) AS 'MgO [%]',
max(CASE WHEN m.parameter='CaO [%]' then m.value END) AS 'CaO [%]',
max(CASE WHEN m.parameter='Na2O [%]' then m.value END) AS 'Na2O [%]',
max(CASE WHEN m.parameter='K2O [%]' then m.value END) AS 'K2O [%]',
max(CASE WHEN m.parameter='Li [ppm]' then m.value END) AS 'Li [ppm]',
max(CASE WHEN m.parameter='Sc [ppm]' then m.value END) AS 'Sc [ppm]',
max(CASE WHEN m.parameter='Ti [ppm]' then m.value END) AS 'Ti [ppm]',
max(CASE WHEN m.parameter='V [ppm]' then m.value END) AS 'V [ppm]',
max(CASE WHEN m.parameter='Cr [ppm]' then m.value END) AS 'Cr [ppm]',
max(CASE WHEN m.parameter='Co [ppm]' then m.value END) AS 'Co [ppm]',
max(CASE WHEN m.parameter='Ni [ppm]' then m.value END) AS 'Ni [ppm]',
max(CASE WHEN m.parameter='Cu [ppm]' then m.value END) AS 'Cu [ppm]',
max(CASE WHEN m.parameter='Zn [ppm]' then m.value END) AS 'Zn [ppm]',
max(CASE WHEN m.parameter='As [ppm]' then m.value END) AS 'As [ppm]',
max(CASE WHEN m.parameter='Rb [ppm]' then m.value END) AS 'Rb [ppm]',
max(CASE WHEN m.parameter='Sr [ppm]' then m.value END) AS 'Sr [ppm]',
max(CASE WHEN m.parameter='Y [ppm]' then m.value END) AS 'Y [ppm]',
max(CASE WHEN m.parameter='Zr [ppm]' then m.value END) AS 'Zr [ppm]',
max(CASE WHEN m.parameter='Nb [ppm]' then m.value END) AS 'Nb [ppm]',
max(CASE WHEN m.parameter='Cd [ppm]' then m.value END) AS 'Cd [ppm]',
max(CASE WHEN m.parameter='Cs [ppm]' then m.value END) AS 'Cs [ppm]',
max(CASE WHEN m.parameter='Ba [ppm]' then m.value END) AS 'Ba [ppm]',
max(CASE WHEN m.parameter='La [ppm]' then m.value END) AS 'La [ppm]',
max(CASE WHEN m.parameter='Ce [ppm]' then m.value END) AS 'Ce [ppm]',
max(CASE WHEN m.parameter='Pr [ppm]' then m.value END) AS 'Pr [ppm]',
max(CASE WHEN m.parameter='Nd [ppm]' then m.value END) AS 'Nd [ppm]',
max(CASE WHEN m.parameter='Sm [ppm]' then m.value END) AS 'Sm [ppm]',
max(CASE WHEN m.parameter='Eu [ppm]' then m.value END) AS 'Eu [ppm]',
max(CASE WHEN m.parameter='Gd [ppm]' then m.value END) AS 'Gd [ppm]',
max(CASE WHEN m.parameter='Tb [ppm]' then m.value END) AS 'Tb [ppm]',
max(CASE WHEN m.parameter='Dy [ppm]' then m.value END) AS 'Dy [ppm]',
max(CASE WHEN m.parameter='Ho [ppm]' then m.value END) AS 'Ho [ppm]',
max(CASE WHEN m.parameter='Er [ppm]' then m.value END) AS 'Er [ppm]',
max(CASE WHEN m.parameter='Tm [ppm]' then m.value END) AS 'Tm [ppm]',
max(CASE WHEN m.parameter='Yb [ppm]' then m.value END) AS 'Yb [ppm]',
max(CASE WHEN m.parameter='Lu [ppm]' then m.value END) AS 'Lu [ppm]',
max(CASE WHEN m.parameter='Hf [ppm]' then m.value END) AS 'Hf [ppm]',
max(CASE WHEN m.parameter='Ta [ppm]' then m.value END) AS 'Ta [ppm]',
max(CASE WHEN m.parameter='Tl [ppm]' then m.value END) AS 'Tl [ppm]',
max(CASE WHEN m.parameter='Pb [ppm]' then m.value END) AS 'Pb [ppm]',
max(CASE WHEN m.parameter='Th [ppm]' then m.value END) AS 'Th [ppm]',
max(CASE WHEN m.parameter='U [ppm]' then m.value END) AS 'U [ppm]',
max(CASE WHEN m.parameter='Nd143_Nd144' then m.value END) AS 'Nd143_Nd144',
max(CASE WHEN m.parameter='Sr87_Sr86' then m.value END) AS 'Sr87_Sr86',
max(CASE WHEN m.parameter='Pb206_Pb204' then m.value END) AS 'Pb206_Pb204',
max(CASE WHEN m.parameter='Pb207_Pb204' then m.value END) AS 'Pb207_Pb204',
max(CASE WHEN m.parameter='Pb208_Pb204' then m.value END) AS 'Pb208_Pb204'
FROM 'samples.csv' AS s JOIN 'measurements.csv' AS m ON s.id=m.sample_id
WHERE location_subregion = 'TUTUILA' AND
m.parameter IN ('SiO2 [%]', 'TiO2 [%]', 'Al2O3 [%]', 'Fe2O3 [%]', 'FeO [%]',
'MnO [%]', 'MgO [%]', 'CaO [%]', 'Na2O [%]', 'K2O [%]',
'Li [ppm]', 'Sc [ppm]', 'Ti [ppm]', 'V [ppm]',
'Cr [ppm]', 'Co [ppm]', 'Ni [ppm]', 'Cu [ppm]', 'Zn [ppm]', 'As [ppm]',
'Rb [ppm]', 'Sr [ppm]', 'Y [ppm]', 'Zr [ppm]', 'Nb [ppm]', 'Cd [ppm]',
'Cs [ppm]', 'Ba [ppm]', 'La [ppm]', 'Ce [ppm]', 'Pr [ppm]', 'Nd [ppm]',
'Sm [ppm]', 'Eu [ppm]', 'Gd [ppm]', 'Tb [ppm]', 'Dy [ppm]', 'Ho [ppm]',
'Er [ppm]', 'Tm [ppm]', 'Yb [ppm]', 'Lu [ppm]', 'Hf [ppm]', 'Ta [ppm]',
'Tl [ppm]', 'Pb [ppm]', 'Th [ppm]', 'U [ppm]', 'Nd143_Nd144', 'Sr87_Sr86',
'Pb206_Pb204', 'Pb207_Pb204', 'Pb208_Pb204') GROUP BY sample_id") %>%
  rename_pofatu_elements() %>%
  Ti_from_TiO2() %>% K_from_K2O() %>% Fe2O3_from_FeO()

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
    TRUE ~ "Tutuila")) %>%
  dplyr::select(
    Sample,Location,Category,lat,long,SiO2,TiO2,Al2O3,Fe2O3,MnO,MgO,CaO,Na2O,K2O,
    Li,Sc,Ti,V,Cr,Co,Ni,Cu,Zn,As,Rb,Sr,Y,Zr,Nb,Cd,Cs,Ba,La,Ce,Pr,Nd,
    Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Hf,Ta,Pb,Th,U,K,
    Sr87_Sr86,Nd143_Nd144,Pb206_Pb204,Pb207_Pb204,Pb208_Pb204)
OIB %>% dplyr::select(Sample, Location, Category)
OIB[OIB == 0] <- NA # Replace 0 with NA

s <- joined_data %>% dplyr::filter(Sample %in% c(
  "E-11-08","T-12-06","T-12-07","T-12-08","T-12-09","T-12-10")) %>%
  mutate(Location = case_when(
    grepl("E-11-08", Sample) ~ "E-11-08",
    grepl("T-12-06", Sample) ~ "T-12-06",
    grepl("T-12-07", Sample) ~ "T-12-07",
    grepl("T-12-08", Sample) ~ "T-12-08",
    grepl("T-12-09", Sample) ~ "T-12-09",
    grepl("T-12-10", Sample) ~ "T-12-10"))


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
