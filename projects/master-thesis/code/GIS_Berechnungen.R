
# ---- 2. INPUT ----------------------------------------------------------------
# 2.1 Pfade ---------------------------------------------------------------------
dir_base     <- "Your_Folder_Path"
dir_stats    <- file.path(dir_base, "output_run14_3.19_reduce50_Topodata/results_statistic")
dir_scenario <- file.path(dir_base, "output_run15_scenario/rastercalculator")
file_soil    <- file.path(dir_base, "soil_group/soil_group_raster.tif")
file_legend  <- "Your_Folder_Path"
dir_figs     <- "Your_Folder_Path"
dir_anhang   <- "Your_Folder_Path"
# 
# ---- 6. SONSTIGES (nicht Grafik-Erstellung) ----------------------------------

if (!require("terra")) install.packages("terra")
library(terra)

# 6.1 Tabelle fuer den Anhang (ueberschneidungsfreie Bereiche pro Klasse)
write.csv2(within %>% select(LULC, bin, n, share) %>% mutate(share = round(share, 2)),
           file = file.path(dir_anhang, paste0(prefix, "within_class_table.csv")),
           row.names = FALSE)

# 6.2 Szenario FP -> SF: Differenzraster (nur ehemalige FP-Pixel, "mit FP minus ohne FP")
diff_file <- "L_diff_V2.tif"          # oder z. B. "AET_diff.tif"

d <- rast(file.path(dir_scenario, diff_file))
v <- values(d, na.rm = TRUE)
round(c(mean = mean(v), sd = sd(v), median = median(v), min = min(v), max = max(v),
        q25 = unname(quantile(v, .25)), q75 = unname(quantile(v, .75)), n = length(v)), 2)

# Nach Bodengruppe
soil <- rast(file_soil)
if (!same.crs(d, soil)) {
  soil <- project(soil, d, method = "near")
} else if (!compareGeom(d, soil, stopOnError = FALSE)) {
  soil <- resample(soil, d, method = "near")
}
zonal(d, soil, fun = "mean",   na.rm = TRUE)
zonal(d, soil, fun = "sd",     na.rm = TRUE)
zonal(d, soil, fun = "length", na.rm = TRUE)   # Pixelzahl je Bodengruppe

# 6.3 Bildmontage: Niederschlag Juli | ET Juli | ET Dezember
imgs <- lapply(c("precip_07.png", "et07_map.png", "et12_map.png"),
               function(f) image_read(file.path(dir_figs, f)))
h <- min(sapply(imgs, function(i) image_info(i)$height))
imgs <- lapply(imgs, image_resize, geometry = paste0("x", h))
image_write(image_append(do.call(c, imgs), stack = FALSE),
            path = file.path(dir_figs, "precip07_et07_et12_combined.png"),
            format = "png", density = "300x300")



# 6.4 Rasterstatistik
# Benötigtes Paket laden (und installieren, falls nicht vorhanden)

# Pfad zum Raster definieren
raster_path <- "/Users/leonhaberberger/SynologyDrive/0_Masterarbeit/0_InVesT/InVEST_SWY_2024_Rio_Pardo/output_run15_scenario/rastercalculator/L_FP_14_15.tif"

# Raster einlesen
r <- rast(raster_path)

# Alle Pixelwerte als Vektor extrahieren und NA-Werte (NoData) entfernen
# (terra::values liest die Daten effizient aus)
v <- na.omit(as.vector(values(r)))

# Ihre Statistik-Funktion anwenden
stats <- round(c(
  mean   = mean(v), 
  sd     = sd(v), 
  median = median(v), 
  min    = min(v), 
  max    = max(v),
  q25    = unname(quantile(v, .25)), 
  q75    = unname(quantile(v, .75)), 
  n      = length(v)
), 2)

# Ergebnisse anzeigen
print(stats)
