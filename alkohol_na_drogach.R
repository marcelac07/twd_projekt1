library(dplyr)
library(ggplot2)
library(colorspace)
library(sf)

pijani_woj <- read.csv("pijani_kierowcy_wojewodztwa.csv")
pijani_rok <- read.csv("liczba_nietrzezwych_kierujacych.csv")
mandaty <- read.csv("Dane_z_mandatów_karnych.csv")


fotoradary <- read.csv("fotoradary_wojewodztwa.csv")
pijani_woj <- read.csv("pijani_kierowcy_wojewodztwa.csv")

dane_woj <- inner_join(fotoradary, pijani_woj, by = "Województwo")

wojewodztwa <- st_read("PRG_jednostki_administracyjne_2025/A01_Granice_wojewodztw.shp", quiet = TRUE) %>%
  st_transform(4326)

woj_coords <- data.frame(
  Województwo = c(
    "Zachodniopomorskie", "Pomorskie", "Warmińsko-Mazurskie", "Podlaskie",
    "Lubuskie", "Wielkopolskie", "Kujawsko-Pomorskie", "Mazowieckie",
    "Dolnośląskie", "Łódzkie", "Lubelskie", "Opolskie",
    "Śląskie", "Świętokrzyskie", "Małopolskie", "Podkarpackie"
  ),
  long = c(15.55, 18.20, 20.68, 22.90, 15.24, 16.93, 18.50, 21.01, 16.50, 19.46, 22.57, 17.70, 19.03, 20.63, 19.94, 22.00),
  lat  = c(53.60, 54.30, 53.85, 53.13, 52.40, 52.35, 53.12, 52.20, 51.10, 51.65, 51.25, 50.60, 50.26, 50.75, 49.95, 50.04)
)

dane_mapa <- inner_join(dane_woj, woj_coords, by = "Województwo") %>%
  mutate(
    h_fotoradary = (Liczba.fotoradarów / max(Liczba.fotoradarów)) * 0.75,
    h_nietrzezwi = (Liczba.zatrzymanych.kierowców.pod.wpływem.alkoholu / max(Liczba.zatrzymanych.kierowców.pod.wpływem.alkoholu)) * 0.75
  )

ggplot() +
  geom_sf(
    data = wojewodztwa,
    fill = "#A9A9A9",
    color = "#2c3e50",
    linewidth = 0.5
  ) +
  geom_rect(
    data = dane_mapa,
    aes(xmin = long - 0.3, xmax = long - 0.05, ymin = lat - 0.3, ymax = lat - 0.3 + h_fotoradary, fill = "Liczba fotoradarów")
  ) +
  geom_rect(
    data = dane_mapa,
    aes(xmin = long + 0.06, xmax = long + 0.3, ymin = lat - 0.3, ymax = lat - 0.3 + h_nietrzezwi, fill = "Nietrzeźwi kierowcy")
  ) +
  scale_fill_manual(values = c("Liczba fotoradarów" = '#6BA3D6', "Nietrzeźwi kierowcy" = '#8B0000')) +
  coord_sf() +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "#555555", hjust = 0.5)
  ) +
  labs(
    title = "Porównanie liczby fotoradarów i nietrzeźwych kierowców wg województw",
    fill = ""
  )



dane_woj <- inner_join(fotoradary, pijani_woj, by = "Województwo")

wojewodztwa <- st_read("PRG_jednostki_administracyjne_2025/A01_Granice_wojewodztw.shp", quiet = TRUE) %>%
  st_transform(4326)

woj_coords <- data.frame(
  Województwo = c(
    "Zachodniopomorskie", "Pomorskie", "Warmińsko-Mazurskie", "Podlaskie",
    "Lubuskie", "Wielkopolskie", "Kujawsko-Pomorskie", "Mazowieckie",
    "Dolnośląskie", "Łódzkie", "Lubelskie", "Opolskie",
    "Śląskie", "Świętokrzyskie", "Małopolskie", "Podkarpackie"
  ),
  long = c(15.55, 18.20, 20.68, 22.90, 15.24, 16.93, 18.50, 21.01, 16.50, 19.46, 22.57, 17.70, 19.03, 20.63, 19.94, 22.00),
  lat  = c(53.60, 54.30, 53.85, 53.13, 52.40, 52.35, 53.12, 52.20, 51.10, 51.65, 51.25, 50.60, 50.26, 50.75, 49.95, 50.04)
)

dane_mapa <- inner_join(dane_woj, woj_coords, by = "Województwo") %>%
  mutate(
    h_fotoradary = (Liczba.fotoradarów / max(Liczba.fotoradarów)) * 0.75,
    h_nietrzezwi = (Liczba.zatrzymanych.kierowców.pod.wpływem.alkoholu / max(Liczba.zatrzymanych.kierowców.pod.wpływem.alkoholu)) * 0.75,
    roznica_wys = abs(h_fotoradary - h_nietrzezwi)
  )

# Wspólny motyw z przezroczystym tłem
motyw_transparentny <- theme(
  panel.background = element_rect(fill = "transparent", color = NA),
  plot.background  = element_rect(fill = "transparent", color = NA),
  legend.background = element_rect(fill = "transparent", color = NA),
  legend.box.background = element_rect(fill = "transparent", color = NA),
  legend.key = element_rect(fill = "transparent", color = NA)
)

wykres_mapa <- ggplot() +
  geom_sf(
    data = wojewodztwa,
    fill = "#A9A9A9",
    color = "#2c3e50",
    linewidth = 0.5
  ) +
  geom_rect(
    data = dane_mapa,
    aes(xmin = long - 0.3, xmax = long - 0.05, ymin = lat - 0.3, ymax = lat - 0.3 + h_fotoradary, fill = "Liczba fotoradarów")
  ) +
  geom_rect(
    data = dane_mapa,
    aes(xmin = long + 0.06, xmax = long + 0.3, ymin = lat - 0.3, ymax = lat - 0.3 + h_nietrzezwi, fill = "Nietrzeźwi kierowcy")
  ) +
  scale_fill_manual(values = c("Liczba fotoradarów" = "#6BA3D6", "Nietrzeźwi kierowcy" = "#8B0000")) +
  coord_sf() +
  theme_minimal() +
  motyw_transparentny +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, color = "#555555", hjust = 0.5)
  ) +
  labs(
    title = "Porównanie liczby fotoradarów i nietrzeźwych kierowców wg województw",
    fill = ""
  )

najwieksza_roznica <- dane_mapa %>%
  slice_max(roznica_wys, n = 1, with_ties = FALSE)

max_fotoradary <- max(dane_mapa$Liczba.fotoradarów)
max_nietrzezwi <- max(dane_mapa$Liczba.zatrzymanych.kierowców.pod.wpływem.alkoholu)

df_skala <- data.frame(
  Kategoria = factor(
    c("Liczba fotoradarów", "Nietrzeźwi kierowcy"),
    levels = c("Liczba fotoradarów", "Nietrzeźwi kierowcy")
  ),
  Wartosc_znormalizowana = c(
    najwieksza_roznica$Liczba.fotoradarów / max_fotoradary,
    najwieksza_roznica$Liczba.zatrzymanych.kierowców.pod.wpływem.alkoholu / max_nietrzezwi
  )
)

wykres_skala <- ggplot(df_skala, aes(x = Kategoria, y = Wartosc_znormalizowana, fill = Kategoria)) +
  geom_col(width = 0.5, color = "#2c3e50", linewidth = 0.4, show.legend = FALSE) +
  scale_fill_manual(values = c("Liczba fotoradarów" = "#6BA3D6", "Nietrzeźwi kierowcy" = "#8B0000")) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  theme_classic() +
  motyw_transparentny +
  theme(
    axis.line = element_line(color = "#2c3e50", arrow = arrow(length = unit(0.18, "cm"), type = "closed")),
    axis.text.x = element_text(size = 9, face = "bold"),
    axis.text.y = element_text(size = 8.5, face = "bold"),
    axis.title.y = element_text(size = 9.5, face = "bold"),
    axis.title.x = element_blank(),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 8.5, hjust = 0.5),
    plot.caption = element_text(size = 8, color = "#2c3e50", hjust = 0.5, margin = margin(t = 8))
  ) +
  labs(
    y = "Udział względem maksimum (%)",
    caption = paste0(
      "100% = ", max_fotoradary, " fotoradarów  |  ",
      "100% = ", max_nietrzezwi, " nietrzeźwych kierowców"
    )
  )

print(wykres_skala)

ggsave("skala_slupki_transparentna.png", plot = wykres_skala, bg = "transparent", width = 4.4, height = 4.4, dpi = 300)