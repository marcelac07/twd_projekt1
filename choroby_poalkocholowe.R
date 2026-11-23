library(tidyverse)
library(dplyr)

leczenie_szpitalne <- read.csv("hospitalizacja- Leczenie szpitalne.csv", skip = 2, header = TRUE)

lata <- as.character(leczenie_szpitalne[1, 4:ncol(leczenie_szpitalne)])
colnames(leczenie_szpitalne) <- c("Kod_Rozpoznania", "Nazwa_Rozpoznania", "Wojewodztwo", lata)

leczenie_szpitalne <- leczenie_szpitalne[-1, ] %>%
  select(-Kod_Rozpoznania) %>%
  mutate(across(`2010`:last_col(), ~ as.numeric(gsub("<\\s*5", "0", .x))))

liczba_mieszkancow_woj <- read.csv("~/Desktop/twd- projekt/wojewodztwa_liczba_mieszkancow_v2.csv") %>%
  mutate(Wojewodztwo = case_when(
    Wojewodztwo == "Kujawsko-pomorskie" ~ "Kujawsko-Pomorskie",
    Wojewodztwo == "Warmińsko-mazurskie" ~ "Warmińsko-Mazurskie",
    TRUE ~ Wojewodztwo
  ))

top3_polska <- leczenie_szpitalne %>%
  filter(Wojewodztwo == "Polska") %>%
  mutate(suma_polska = rowSums(across(`2010`:`2022`), na.rm = TRUE)) %>%
  slice_max(order_by = suma_polska, n = 3) %>%
  pull(Nazwa_Rozpoznania)

leczenie_suma_woj <- leczenie_szpitalne %>%
  filter(Wojewodztwo != "Polska") %>%
  mutate(
    suma_lat = rowSums(across(`2010`:`2022`), na.rm = TRUE),
    Kategoria = ifelse(
      Nazwa_Rozpoznania %in% top3_polska,
      Nazwa_Rozpoznania,
      "Pozostałe choroby poalkoholowe"
    )
  )

df_plot_global <- leczenie_suma_woj %>%
  group_by(Wojewodztwo, Kategoria) %>%
  summarise(Suma_zachorowan = sum(suma_lat, na.rm = TRUE), .groups = "drop") %>%
  left_join(liczba_mieszkancow_woj, by = "Wojewodztwo") %>%
  mutate(
    Odsetek = (Suma_zachorowan / Liczba_mieszkancow) * 100,
    Kategoria = factor(Kategoria, levels = c("Pozostałe choroby poalkoholowe", top3_polska))
  )

plot_koncowy <- ggplot(df_plot_global, aes(x = reorder(Wojewodztwo, -Odsetek, FUN = sum), y = Odsetek, fill = Kategoria)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(values = c("#8B0000", "#A9A9A9", "#6BA3D6", "#212478")) +
  labs(
    title = "Odsetek zachorowań na choroby poalkoholowe w województwach",
    subtitle = "Z wyróżnieniem 3 najczęstszych chorób w Polsce oraz pozostałych",
    x = "Województwo",
    y = "Odsetek zachorowań (% populacji)",
    fill = "Choroba / Kategoria"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.box = "vertical"
  ) +
  guides(fill = guide_legend(ncol = 1))

plot_plakat <- plot_koncowy +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
    plot.subtitle = element_text(hjust = 0.5, color = "black"),
    legend.position = "inside",
    legend.position.inside = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    legend.title = element_text(size = 10, face = "plain", color = "black"),
    legend.text = element_text(size = 9, color = "black")
  )

ggsave("zachorowania_woj_wykres.png", plot = plot_plakat, width = 10, height = 7, dpi = 300, bg = "transparent")
