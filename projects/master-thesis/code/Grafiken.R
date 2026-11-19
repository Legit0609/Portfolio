# ==============================================================================
# Auswertung SWY: Landnutzung x Wertebereiche (B, L, AET)
# ==============================================================================


# ---- 1. PACKAGES -------------------------------------------------------------
library(readxl)    # Excel-Dateien einlesen
library(readr)     # Legende (TSV) einlesen
library(dplyr)     # Datenaufbereitung
library(tidyr)
library(ggplot2)   # Grafiken
library(scales)    # Achsenformate, Textumbruch
library(terra)     # Raster (Szenario-Statistik)
library(magick)    # Bildmontage


# ---- 2. INPUT ----------------------------------------------------------------
# 2.1 Pfade ---------------------------------------------------------------------
dir_base     <- "Your_Folder_Path"
dir_stats    <- file.path(dir_base, "output_run14_3.19_reduce50_Topodata/results_statistic")
dir_scenario <- file.path(dir_base, "output_run15_scenario/rastercalculator")
file_soil    <- file.path(dir_base, "soil_group/soil_group_raster.tif")
file_legend  <- "Your_Folder_Path"
dir_figs     <- "Your_Folder_Path"
dir_anhang   <- "Your_Folder_Path"

# 2.2 Variable waehlen: genau EINEN Block aktiv lassen --------------------------
# (RStudio/Mac: Zeilen markieren, Cmd + Shift + C schaltet Kommentar um)
# modus "B_L": Dateien sind "kleiner als" (sma) + eine "groesser als"-Datei (big)
# modus "AET": erste Datei "kleiner als", alle weiteren "groesser als"
# files: 6 Dateien in aufsteigender Reihenfolge, cut_values: 5 Schwellen
# 
# ## --- Baseflow (B) ---
# var_label  <- "B"
# prefix     <- "B_"
# modus      <- "B_L"
# cut_values <- c(100, 200, 400, 600, 800)
# files      <- paste0(prefix, c("sma_100", "sma_200", "sma_400", "sma_600", "sma_800", "big_800"), ".xlsx")

#--- Local Recharge (L) --- (Dateinamen ggf. anpassen)
var_label  <- "L"
prefix     <- "L_"
modus      <- "B_L"
cut_values <- c(0, 200, 400, 600, 800)
files      <- paste0(prefix, c("sma_0", "sma_200", "sma_400", "sma_600", "sma_800", "big_800"), ".xlsx")

# ## --- Actual Evapotranspiration (AET) --- (Dateinamen ggf. anpassen)
# var_label  <- "AET"
# prefix     <- "AET_"
# modus      <- "AET"
# cut_values <- c(200, 400, 600, 800, 1000)
# files      <- paste0(prefix, c("sma_200", "big_200", "big_400", "big_600", "big_800", "big_1000"), ".xlsx")

# 2.3 Darstellung ---------------------------------------------------------------
min_area_share <- 3          # nur Klassen mit >= x % Flaechenanteil zeigen
make_p6        <- FALSE      # Linienvariante zusaetzlich erzeugen?

fam        <- "Times"        # Schrift (passend zum Springer-Nature-Layout)
fig_width  <- 16             # cm, ca. Textbreite (siehe Hinweis unten)
fig_height <- 7            # cm
save_as    <- c("jpg")       # z. B. c("jpg", "pdf") fuer zusaetzlich Vektorgrafik

# Farbskala der Wertebereiche (hell = niedrige Werte, dunkel = hohe Werte)
#pal <- colorRampPalette(c("#deebf7", "#08306b"))(length(cut_values) + 1)
pal <- c("#c6dbef", "#9ecae1", "#4292c6", "#2166ac", "#08306b", "#041f42")  # Kartenfarben


# ---- 3. DATEN EINLESEN & AUFBEREITEN (Voraussetzung fuer die Grafiken) -------
stopifnot(length(files) == 6, length(cut_values) == 5)

lulc_labels <- read_tsv(file_legend, show_col_types = FALSE) %>%
  select(zone = Class_ID, LULC = Description, color = Color)

color_map <- lulc_labels %>%
  filter(!is.na(color)) %>%
  { setNames(.$color, .$LULC) }

all_data <- lapply(seq_along(files), function(i) {
  read_xlsx(file.path(dir_stats, files[i])) %>% mutate(idx = i)
}) %>%
  bind_rows() %>%
  left_join(lulc_labels, by = "zone") %>%
  filter(!is.na(LULC), zone != 0)

bin_labels <- c(paste0("< ", cut_values[1]),
                paste(cut_values[-5], cut_values[-1], sep = "–"),
                paste0("> ", cut_values[5]))

# Kumulative Dateien -> ueberschneidungsfreie Bereiche pro Klasse
within_w <- all_data %>%
  select(zone, LULC, color, idx, count) %>%
  complete(nesting(zone, LULC, color), idx = 1:6, fill = list(count = 0)) %>%
  pivot_wider(names_from = idx, values_from = count, names_prefix = "c")

if (modus == "B_L") {
  within_w <- within_w %>%
    mutate(b1 = c1, b2 = c2 - c1, b3 = c3 - c2, b4 = c4 - c3, b5 = c5 - c4,
           b6 = c6, total = c5 + c6)
} else if (modus == "AET") {
  within_w <- within_w %>%
    mutate(b1 = c1, b2 = c2 - c3, b3 = c3 - c4, b4 = c4 - c5, b5 = c5 - c6,
           b6 = c6, total = c1 + c2)
} else stop("modus muss 'B_L' oder 'AET' sein")

# Sicherheitscheck: negative Pixelzahlen = Dateien passen nicht zur Richtung
stopifnot(all(within_w[paste0("b", 1:6)] >= 0))

# Referenz "ganzes Gebiet" (vor dem Flaechenfilter)
alle <- within_w %>%
  summarise(across(c(b1:b6, total), sum)) %>%
  mutate(LULC = "Entire area", zone = NA, color = NA)

within <- within_w %>%
  mutate(area_share = 100 * total / sum(total)) %>%
  filter(area_share >= min_area_share) %>%
  arrange(desc(total)) %>%
  bind_rows(alle %>% mutate(area_share = 100)) %>%
  mutate(x_lab = ifelse(LULC == "Entire area", "Entire area",
                        paste0(label_wrap(14)(LULC), "\n(", round(area_share), " %)")),
         x_lab = factor(x_lab, levels = unique(x_lab))) %>%
  pivot_longer(b1:b6, names_to = "bin", values_to = "n") %>%
  mutate(bin   = factor(bin_labels[as.integer(sub("b", "", bin))], levels = bin_labels),
         share = 100 * n / total)

# Kontrolle
print(within %>% select(LULC, bin, n, share) %>% arrange(LULC, bin), n = 60)

# Als CSV Speichern
write.csv2(within %>% select(LULC, bin, n, share), file.path(dir_anhang, paste0(var_label, "_LULC_Wertebereiche_Prozent.csv")), row.names = FALSE, fileEncoding = "UTF-8")

# ---- 4. GRAFIKEN -------------------------------------------------------------
# Einheitliches Theme: Schriftgroessen sind echte Druckgroessen (pt),
# da die Grafik in Textbreite eingebunden wird.
theme_sn <- theme_classic(base_family = fam, base_size = 9) +
  theme(axis.text        = element_text(size = 8, colour = "black"),
        axis.title       = element_text(size = 9),
        legend.text      = element_text(size = 8),
        legend.title     = element_text(size = 9),
        legend.key.size  = unit(0.4, "cm"),
        legend.position  = "bottom")

# Plot 5: Verteilung der Wertebereiche innerhalb jeder Landnutzungsklasse
p5 <- ggplot(within, aes(x = x_lab, y = share, fill = bin)) +
  geom_col(color = "white", linewidth = 0.3) +
  scale_fill_manual(values = setNames(pal, bin_labels),
                    name = paste0(var_label, " range (mm/year)")) +
  scale_y_continuous(labels = percent_format(scale = 1), expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(x = NULL, y = "Share of class pixels (%)") +
  theme_sn
print(p5)

# Plot 6 (optional): gleiche Daten als Linien, gestrichelt = ganzes Gebiet
if (make_p6) {
  p6 <- ggplot(within %>% filter(LULC != "Entire area"),
               aes(x = bin, y = share, color = LULC, group = LULC)) +
    geom_line(linewidth = 0.8) + geom_point(size = 1.5) +
    geom_line(data = within %>% filter(LULC == "Entire area"),
              aes(group = 1), color = "black", linetype = "dashed", linewidth = 0.7) +
    scale_color_manual(values = color_map, name = "Land Use") +
    guides(color = guide_legend(nrow = 2)) +
    labs(x = paste0(var_label, " range (mm/year)"), y = "Share of class pixels (%)") +
    theme_sn
  print(p6)
}


# ---- 5. GRAFIKEN SPEICHERN ---------------------------------------------------
for (ext in save_as) {
  ggsave(file.path(dir_figs, paste0(prefix, "within_class_stacked.", ext)),
         plot = p5, width = fig_width, height = fig_height, units = "cm", dpi = 300)
  if (make_p6) {
    ggsave(file.path(dir_figs, paste0(prefix, "within_class_lines.", ext)),
           plot = p6, width = fig_width, height = fig_height * 0.75, units = "cm", dpi = 300)
  }
}


