install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("scales")
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(scales)

plik_2024 <- "dostawy_na_rynek_krajowy_oraz_spozycie_niektorych_artykulow_konsumpcyjnych_na_1_mieszkanca_w_2024_r._tablica_2.xlsx"
plik_2025 <- "Dostawy na rynek krajowy oraz spożycie niektórych artykułów konsumpcyjnych na 1 mieszkańca w 2025 r..xlsx"


dane_2024 <- read_excel(plik_2024, sheet = "Spożycie", skip = 1)
dane_2025 <- read_excel(plik_2025, sheet = "Tablica 2. Spożycie ", skip = 1)

przygotuj_alkohol <- function(df) {
  df %>%
    rename(Wyszczegolnienie = 1, Jednostka = 2) %>%
    filter(grepl("wódki|Wina|Piwo", Wyszczegolnienie, ignore.case = TRUE)) %>%
    mutate(Kategoria = case_when(
      grepl("wódki", Wyszczegolnienie, ignore.case = TRUE) ~ "Wyroby spirytusowe (100% alk.)",
      grepl("wina", Wyszczegolnienie, ignore.case = TRUE) ~ "Wino i miody",
      grepl("piwo", Wyszczegolnienie, ignore.case = TRUE) ~ "Piwo",
      TRUE ~ "Inne"
    )) %>%
    select(-Wyszczegolnienie, -Jednostka) %>%
    pivot_longer(cols = -Kategoria, names_to = "Rok", values_to = "Litry_na_mieszkanca") %>%
    drop_na(Litry_na_mieszkanca)
}

alkohol_2024 <- przygotuj_alkohol(dane_2024)
alkohol_2025 <- przygotuj_alkohol(dane_2025)


ramka_alkohol <- bind_rows(alkohol_2024, alkohol_2025) %>%
  distinct(Kategoria, Rok, .keep_all = TRUE) %>%
  pivot_wider(names_from = Kategoria, values_from = Litry_na_mieszkanca) %>%
  mutate(Rok = as.numeric(Rok)) %>%
  arrange(Rok) %>%
  mutate(
    Spirytusowe = `Wyroby spirytusowe (100% alk.)`, 
    Wino = `Wino i miody` * 0.12,                   
    Piwo = Piwo * 0.055,                            
    Ogolem = Spirytusowe + Wino + Piwo              
  ) %>%
  select(Rok, Spirytusowe, Wino, Piwo, Ogolem) %>%
  mutate(across(-Rok, ~round(., 2)))

print(ramka_alkohol)





lata <- c(2000, 2005, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)
spirytusowe <- c(2.0, 2.5, 3.2, 3.2, 3.2, 3.0, 3.6, 3.2, 3.2, 3.2, 3.3, 3.3, 3.7, 3.7, 3.8, 3.5, 3.4)
wina_litry <- c(12.0, 8.6, 7.0, 6.9, 6.4, 5.9, 5.8, 6.3, 6.3, 5.9, 6.2, 6.0, 6.2, 6.5, 6.7, 6.3, 6.0)
piwo_litry <- c(66.9, 80.7, 91.2, 90.2, 94.3, 99.2, 97.7, 98.9, 99.1, 99.5, 98.5, 100.5, 97.1, 94.0, 93.2, 92.9, 87.4)

dane_reczne <- data.frame(
  Rok = lata,
  Spirytusowe_gus = spirytusowe,
  Wino_gus = wina_litry,
  Piwo_gus = piwo_litry
)

ramka_alkohol_pdf <- dane_reczne %>%
  mutate(
    Spirytusowe = Spirytusowe_gus,       
    Wino = Wino_gus * 0.12,              
    Piwo = Piwo_gus * 0.055,             
    Ogolem = Spirytusowe + Wino + Piwo   
  ) %>%
  select(Rok, Spirytusowe, Wino, Piwo, Ogolem) %>%
  mutate(across(-Rok, ~round(., 2)))

print(ramka_alkohol_pdf)


pelna_ramka <- bind_rows(ramka_alkohol_pdf, ramka_alkohol) %>%
  distinct(Rok, .keep_all = TRUE) %>%
  arrange(Rok)

print(pelna_ramka)
pelna_ramka <- pelna_ramka %>% filter(Rok > 2005)
pelna_ramka



ramka_do_wykresu <- pelna_ramka %>%
  pivot_longer(
    cols = c(Spirytusowe, Wino, Piwo, Ogolem),
    names_to = "Kategoria",
    values_to = "Litry"
  ) %>%
  mutate(Kategoria = factor(Kategoria, levels = c("Ogolem", "Piwo", "Spirytusowe", "Wino")))


wykres <- ramka_do_wykresu %>%
  filter(Kategoria != "Ogolem") %>%
  ggplot(aes(x = Rok, y = Litry, fill = Kategoria)) +
  geom_col() + 
  scale_x_continuous(breaks = seq(min(pelna_ramka$Rok), max(pelna_ramka$Rok), by = 1)) +
  scale_fill_manual(values  = c("#8B0000" ,"#A9A9A9", "#212478" , "#6BA3D6")) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "white", alpha = 0.5) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "white", alpha = 0.5) +
  geom_vline(xintercept = 2021, linetype = "dotted", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "white", alpha = 0.5) +
  annotate("text", x = 2013.8, y = 6.5, label = "Akcyza na wódkę +15%", angle = 90, size = 3.4, color = 'white') +
  annotate("text", x = 2019.8, y = 7, label = "Akcyza ogółem +10%", angle = 90, size = 3.4, color = "white") +
  annotate("text", x = 2020.8, y = 7, label = "Podatek 'małpkowy'", angle = 90, size = 3.4, color = "white") +
  annotate("text", x = 2022.2, y = 5.5, label = "Początek 5% corocznych podwyżek", angle = 90, size = 3.4, color = "white") +
  labs(
    title = "Spożycie poszczególnych rodzajów napojów alkoholowych",
    subtitle = "w przeliczeniu na 100% alkohol",
    x = "Rok",
    y = "Litry 100% alkoholu",
    fill = "" 
  )  +
  theme_minimal() + 
  theme(
    panel.background = element_rect(fill = "transparent", color = NA), 
    plot.background = element_rect(fill = "transparent", color = NA), 
    legend.background = element_rect(fill = "transparent", color = NA), 
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom"
  )
ggsave(
  filename = "spozycie.png", 
  plot = wykres, 
  dpi = 300,                     
  bg = "transparent"            
)
print(wykres)



file_path <- "Kopia pliku Sprzedaż alkoholu-2.xlsx"

godziny <- read_excel(file_path, sheet = "Arkusz1", range = "A5:B29")
colnames(godziny) <- c("Przedzial_godzinowy", "Udzial")
dni_tygodnia <- read_excel(file_path, sheet = "Arkusz1", range = "E5:F12")
colnames(dni_tygodnia) <- c("Dzien_tygodnia", "Udzial")

dni_tygodnia$Dzien_tygodnia <- factor(dni_tygodnia$Dzien_tygodnia, 
                                      levels = c("Poniedziałek", "Wtorek", "Środa", 
                                                 "Czwartek", "Piątek", "Sobota", "Niedziela"))


wykres_godziny <- ggplot(godziny, aes(x = Przedzial_godzinowy, y = Udzial)) +
  geom_col(fill = "#6BA3D6", alpha = 0.9) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  labs(
    title = "Udział zakupów alkoholu w poszczególnych godzinach",
    subtitle = "Dane na podstawie paragonów - 1.10.2025-1.10.2026",
    x = "Przedział godzinowy",
    y = "Udział w zakupach"
  ) +
  theme_minimal() + 
  theme(
  panel.background = element_rect(fill = "transparent", color = NA), 
  plot.background = element_rect(fill = "transparent", color = NA),  
  legend.background = element_rect(fill = "transparent", color = NA), 
  plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
  plot.subtitle = element_text(size = 10, hjust = 0.5),
  axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
  panel.grid.major.x = element_blank())

ggsave(
  filename = "wykres_godz.png", 
  plot = wykres_godziny, 
  dpi = 300,                    
  bg = "transparent"             
)
print(wykres_godziny)








