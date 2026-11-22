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
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "inside",
    legend.position.inside = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    legend.title = element_text(size = 10, face = "plain"),
    legend.text = element_text(size = 9)
  )

ggsave("zachorowania_woj_wykres.png", plot = plot_plakat, width = 10, height = 7, dpi = 300, bg = "transparent")