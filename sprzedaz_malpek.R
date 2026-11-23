library(sf)        
library(ggplot2)   
library(dplyr)     
library(tidyr)    
library(readxl)    

mapa_gmin <- st_read("A03_Granice_gmin.shp")


dane_malpki <- read_excel("Informacja_o_udziałach_JST_w_opłacie_ALK_w_okresie_1.01.2026-30.09.2026.xlsx") %>%
  mutate(
    teryt_czysty = sprintf("%07d", as.numeric(`Kod jednostki terytorialnej`))
  )


nazwy_arkuszy <- excel_sheets("tabela12.xls")

dane_gus <- bind_rows(
  lapply(nazwy_arkuszy, function(arkusz) {
    read_excel("tabela12.xls", sheet = arkusz, skip = 6, col_names = FALSE) %>%
      select(kategoria = 1, teryt = 2, ludnosc = 3) %>%
      mutate(
        teryt = as.character(teryt),
        ludnosc = as.character(ludnosc)
      )
  })
)

populacja_doroslych <- dane_gus %>%
  fill(teryt, .direction = "down") %>%
  filter(grepl("produkcyjny|poprodukcyjny", kategoria)) %>%
  filter(!grepl("przed", kategoria)) %>%
  group_by(teryt) %>%
  summarise(liczba_doroslych = sum(as.numeric(ludnosc), na.rm = TRUE)) %>%
  mutate(teryt_czysty = sprintf("%07d", as.numeric(teryt))) %>%
  filter(teryt_czysty != "00000NA")

mapa_gotowa <- mapa_gmin %>%
  left_join(dane_malpki, by = c("JPT_KOD_JE" = "teryt_czysty")) %>%
  left_join(populacja_doroslych, by = c("JPT_KOD_JE" = "teryt_czysty")) %>%
  mutate(
    caly_podatek = `Dochody przekazane w okresie sprawozdawczym (w zł)` * 2,
    liczba_malpek_szt = caly_podatek / 0.75,
    malpki_per_dorosly = liczba_malpek_szt / liczba_doroslych
  )

granice_woj <- mapa_gotowa %>%
  mutate(woj = substr(JPT_KOD_JE, 1, 2)) %>%
  group_by(woj) %>%
  summarise(geometry = st_union(geometry))


wykres <- ggplot(data = mapa_gotowa) +
  geom_sf(aes(fill = malpki_per_dorosly), color = NA) + 
  geom_sf(
    data = filter(mapa_gotowa, is.na(malpki_per_dorosly)),
    aes(color = "Brak danych"),
    fill = "#A9A9A9",
    linewidth = 0.05
  ) +
  scale_fill_gradientn(
    colours = c("#F2F7FC", "#A8CBE8", "#6BA3D6", "#3A66A8", "#212478"),
    values = scales::rescale(c(0, 15, 30, 50, 75)), # 5 wartości dla 5 kolorów
    name = "Liczba małpek\n(szt./dorosłego)",
    limits = c(0, 75),
    oob = scales::squish,
    breaks = c(0, 15, 30, 45, 60, 75),
    labels = c("0 szt.", "15 szt.", "30 szt.", "45 szt.", "60 szt.", ">= 75 szt."),
    na.value = "#A9A9A9"
  ) + 
  scale_color_manual(
    name = NULL,
    values = c("Brak danych" = "#A9A9A9")
  ) +
  guides(
    fill = guide_colorbar(order = 1),
    color = guide_legend(
      order = 2,
      override.aes = list(fill = "#A9A9A9", color = NA)
    )
  )  +
  theme_void() +
  labs(
    title = "Szacunkowa sprzedaż 'małpek' na osobę pełnoletnią",
    subtitle = "Podział na gminy w Polsce (styczeń - wrzesień 2026 r.)"
  ) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = -6)),
    legend.position = "right",
    legend.title = element_text(margin = margin(b = 12)),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

wykres <- wykres + 
  geom_sf(data = granice_woj, fill = NA, color = "#4A4A4A", linewidth = 0.5)

wykres

ggsave("mapa_ostateczna.png", plot = wykres, bg = "transparent", width = 10, height = 8)

