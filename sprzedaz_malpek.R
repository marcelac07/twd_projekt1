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
    malpki_per_dorosly = liczba_malpek_szt / liczba_doroslych,
    # Nowa zmienna z 6 przedziałami
    przedzial = cut(
      malpki_per_dorosly,
      breaks = c(-Inf, 10, 20, 30, 45, 60, Inf),
      labels = c("0–10", "10–20", "20–30", "30–45", "45–60", "> 60"),
      right = TRUE
    )
  )

granice_woj <- mapa_gotowa %>%
  mutate(woj = substr(JPT_KOD_JE, 1, 2)) %>%
  group_by(woj) %>%
  summarise(geometry = st_union(geometry))

wykres <- ggplot(data = mapa_gotowa) +
  geom_sf(aes(fill = przedzial), color = NA) +
  geom_sf(
    data = filter(mapa_gotowa, is.na(malpki_per_dorosly)),
    aes(color = "Brak danych"),
    fill = "#A9A9A9",
    linewidth = 0.05
  ) +
  geom_sf(data = granice_woj, fill = NA, color = "#4A4A4A", linewidth = 0.5) +
  scale_fill_manual(
    values = c(
      "0–10"  = "#D6E7F5",
      "10–20" = "#A6CBE8",
      "20–30" = "#72A7D6",
      "30–45" = "#447BB8",
      "45–60" = "#264D8E",
      "> 60"  = "#131854"
    ),
    name = "Liczba „małpek”\n(szt./dorosłego)",
    na.value = "#A9A9A9",
    na.translate = FALSE
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Brak danych" = "#A9A9A9")
  ) +
  guides(
    fill = guide_legend(order = 1, reverse = TRUE), 
    color = guide_legend(
      order = 2,
      override.aes = list(fill = "#A9A9A9", color = NA)
    )
  ) +
  theme_void() +
  labs(
    title = "Szacunkowa sprzedaż „małpek” na osobę pełnoletnią",
    subtitle = "Podział na gminy w Polsce (styczeń–wrzesień 2026 r.)"
  ) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(t = 15, b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = -6)),
    legend.position = "right",
    legend.title = element_text(margin = margin(b = 10)),
    legend.key.size = unit(0.6, "cm"),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

ggsave("mapa_ostateczna.png", plot = wykres, bg = "transparent", width = 10, height = 8, dpi = 300)



