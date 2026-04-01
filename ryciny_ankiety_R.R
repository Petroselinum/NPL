# =========================================================
# RYCINY DO ANALIZY KONSOLIDACJI STRATEGICZNEJ
# Wersja podglądowa - bez zapisu do plików
# Ryc. 1, 2, 4, 7, 8
# =========================================================

# Jeśli potrzeba, odkomentuj:
# install.packages(c(
#   "readxl", "dplyr", "ggplot2", "ggalluvial",
#   "stringr", "forcats", "scales", "grid"
# ))

library(readxl)
library(dplyr)
library(ggplot2)
library(ggalluvial)
library(stringr)
library(forcats)
library(scales)
library(grid)

# =========================================================
# 1. PLIK I DANE
# =========================================================

xlsx_path <- "H:/.shortcut-targets-by-id/1PDXlz_ld72WsjIsG2l4pTXC7d_nhn5yO/NPL/Ankiety ws. NPL/Kopia konsolidacja_strategiczna_5_15_45.xlsx"

tematy_15   <- read_excel(xlsx_path, sheet = "Tematy_15")
problemy_45 <- read_excel(xlsx_path, sheet = "Problemy_45")

# =========================================================
# 2. FUNKCJE POMOCNICZE
# =========================================================

# Mapowanie grup źródłowych na skróty G1-G5
mapuj_grupe_5 <- function(x) {
  kod <- stringr::str_extract(as.character(x), "^GRUPA\\s+[IVX]+")
  
  dplyr::case_when(
    kod == "GRUPA I"   ~ "G1",
    kod == "GRUPA II"  ~ "G2",
    kod == "GRUPA III" ~ "G3",
    kod == "GRUPA IV"  ~ "G4",
    kod == "GRUPA V"   ~ "G5",
    TRUE ~ as.character(x)
  )
}

# Dłuższe etykiety do modelu konceptualnego
mapuj_grupe_long <- function(x) {
  dplyr::case_when(
    x == "G1" ~ "G1\nKlimat i adaptacja",
    x == "G2" ~ "G2\nOchrona ekosystemów",
    x == "G3" ~ "G3\nDrewno i ekonomika",
    x == "G4" ~ "G4\nZarządzanie lasami",
    x == "G5" ~ "G5\nB+R i komunikacja",
    TRUE ~ as.character(x)
  )
}

theme_pub <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0, colour = "grey10"),
      plot.subtitle = element_text(size = 11, colour = "grey35"),
      axis.title = element_text(face = "bold", colour = "grey10"),
      axis.text = element_text(colour = "grey20"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "none",
      plot.margin = margin(15, 20, 15, 15)
    )
}

# =========================================================
# 3. PALETA KOLORÓW
# =========================================================

pal_5 <- c(
  "G1" = "#2E86AB",
  "G2" = "#3C9D5D",
  "G3" = "#D98E04",
  "G4" = "#8E5EA2",
  "G5" = "#D1495B"
)

pal_5_soft <- c(
  "G1" = "#D6EAF8",
  "G2" = "#D5F5E3",
  "G3" = "#FDEBD0",
  "G4" = "#E8DAEF",
  "G5" = "#FADBD8"
)

# =========================================================
# 4. DANE DO RYC. 1 i 2
# =========================================================

df_funnel <- data.frame(
  stage = c(
    "Odpowiedzi surowe",
    "Po czyszczeniu i kwalifikacji",
    "Problemy",
    "Tematy",
    "Grupy strategiczne"
  ),
  n = c(369, 367, 45, 15, 5),
  y = c(5, 4, 3, 2, 1)
)

# =========================================================
# 5. DANE DO RYC. 4
# =========================================================

df_hier_simple <- tematy_15 %>%
  transmute(
    temat_15_short = stringr::str_wrap(as.character(TEMAT_15), width = 26),
    grupa = mapuj_grupe_5(GRUPA),
    n = as.numeric(LICZBA_ZGLOSZEN)
  ) %>%
  filter(!is.na(temat_15_short), !is.na(grupa), !is.na(n))

# Stabilna kolejność grup
df_hier_simple$grupa <- factor(df_hier_simple$grupa, levels = c("G1", "G2", "G3", "G4", "G5"))

# =========================================================
# 6. DANE DO RYC. 7
# =========================================================

df_5_plot <- tematy_15 %>%
  group_by(GRUPA) %>%
  summarise(n = sum(as.numeric(LICZBA_ZGLOSZEN), na.rm = TRUE), .groups = "drop") %>%
  mutate(grupa = mapuj_grupe_5(GRUPA)) %>%
  select(grupa, n) %>%
  distinct() %>%
  mutate(grupa = factor(grupa, levels = c("G1", "G2", "G3", "G4", "G5"))) %>%
  arrange(grupa)

# =========================================================
# 7. DANE DO RYC. 8
# =========================================================

df_model_nodes <- data.frame(
  x = c(1, 3, 5, 2, 4),
  y = c(4, 5, 4, 2, 2),
  grupa = c("G1", "G2", "G3", "G4", "G5"),
  label = c(
    "G1\nKlimat,\nadaptacja i łagodzenie\nzmian klimatu",
    "G2\nOchrona ekosystemów\nleśnych i usług",
    "G3\nZrównoważone\nwykorzystanie drewna\ni ekonomika",
    "G4\nZarządzanie lasami\ni organizacja leśnictwa",
    "G5\nBadania,\nrozwój, komunikacja\ni kształcenie"
  ),
  fill = c("#D6EAF8", "#D5F5E3", "#FDEBD0", "#E8DAEF", "#FADBD8"),
  stringsAsFactors = FALSE
)

# =========================================================
# 8. RYCINA 1 - SCHEMAT PROCESU
# =========================================================

df_flow <- data.frame(
  x = c(1, 3, 5, 7, 9),
  y = 1,
  label = c(
    "369\nzgłoszeń\nsurowych",
    "367\nrekordów\nw analizie głównej",
    "45\nproblemów\nskonsolidowanych",
    "15\ntematów\nnadrzędnych",
    "5\ngrup\nstrategicznych"
  ),
  fill = c("#CFE8F3", "#B7DCC7", "#F8D49D", "#D8C3E5", "#F3C1C6"),
  stringsAsFactors = FALSE
)

df_arrows <- data.frame(
  x = c(1.65, 3.65, 5.65, 7.65),
  xend = c(2.35, 4.35, 6.35, 8.35),
  y = 1,
  yend = 1
)

p1 <- ggplot() +
  geom_curve(
    data = df_arrows,
    aes(x = x, y = y, xend = xend, yend = yend),
    curvature = 0,
    linewidth = 0.8,
    colour = "grey45",
    arrow = arrow(length = unit(0.22, "cm"), type = "closed")
  ) +
  geom_label(
    data = df_flow,
    aes(x = x, y = y, label = label, fill = fill),
    label.size = 0.35,
    label.r = unit(0.22, "lines"),
    colour = "grey10",
    fontface = "bold",
    size = 4.2,
    label.padding = unit(0.35, "lines"),
    show.legend = FALSE
  ) +
  scale_fill_identity() +
  annotate(
    "text", x = 5, y = 1.60,
    label = "Schemat procesu konsolidacji danych",
    fontface = "bold", size = 5.5, colour = "grey10"
  ) +
  annotate(
    "text", x = 5, y = 0.42,
    label = "Od danych źródłowych do syntetycznych grup strategicznych",
    size = 4.0, colour = "grey35"
  ) +
  coord_cartesian(xlim = c(0.2, 9.8), ylim = c(0.2, 1.9), clip = "off") +
  theme_void() +
  theme(plot.margin = margin(20, 20, 20, 20))

# =========================================================
# 9. RYCINA 2 - LEJEK REDUKCJI DANYCH
# =========================================================

max_n <- max(df_funnel$n)

funnel_poly <- df_funnel %>%
  mutate(
    xmin = -n / 2,
    xmax =  n / 2,
    ymin = y - 0.42,
    ymax = y + 0.42
  )

p2 <- ggplot() +
  geom_rect(
    data = funnel_poly,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = n),
    colour = "white",
    linewidth = 0.9,
    alpha = 0.97
  ) +
  geom_text(
    data = funnel_poly,
    aes(x = 0, y = y, label = paste0(stage, "\n(n = ", n, ")")),
    colour = "black",
    fontface = "bold",
    size = 4.3,
    lineheight = 1.0
  ) +
  scale_fill_gradient(low = "#9FD3C7", high = "#1D5C63") +
  labs(
    title = "Lejek redukcji danych",
    subtitle = "Kolejne etapy konsolidacji: 369 → 367 → 45 → 15 → 5"
  ) +
  coord_cartesian(
    xlim = c(-max_n / 2 * 1.08, max_n / 2 * 1.08),
    ylim = c(0.4, 5.6),
    clip = "off"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 16, colour = "grey10"),
    plot.subtitle = element_text(size = 11, colour = "grey35"),
    legend.position = "none",
    plot.margin = margin(20, 20, 20, 20)
  )

# =========================================================
# 10. RYCINA 4 - HIERARCHIA 15 -> 5
# =========================================================

p4 <- ggplot(
  df_hier_simple,
  aes(axis1 = temat_15_short, axis2 = grupa, y = n)
) +
  geom_alluvium(
    aes(fill = grupa),
    width = 0.28,
    alpha = 0.85
  ) +
  geom_stratum(
    width = 0.28,
    fill = "grey96",
    colour = "grey60",
    linewidth = 0.4
  ) +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 3.2,
    colour = "grey15"
  ) +
  scale_x_discrete(
    limits = c("15 tematów", "5 grup"),
    expand = c(0.08, 0.08)
  ) +
  scale_fill_manual(values = pal_5) +
  labs(
    title = "Hierarchia konsolidacji kategorii",
    subtitle = "Przepływ od 15 tematów do 5 grup strategicznych (G1–G5)",
    x = NULL,
    y = "Liczba zgłoszeń"
  ) +
  theme_pub()

# =========================================================
# 11. RYCINA 7 - FINALNE 5 GRUP
# =========================================================

p7 <- ggplot(df_5_plot, aes(x = grupa, y = n, fill = grupa)) +
  geom_col(width = 0.72, alpha = 0.96) +
  geom_text(
    aes(label = n),
    hjust = -0.15,
    size = 4.4,
    fontface = "bold",
    colour = "grey15"
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = pal_5) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12)),
    labels = label_number(accuracy = 1)
  ) +
  labs(
    title = "Rozkład zgłoszeń w grupach strategicznych",
    subtitle = "Końcowy rozkład liczby zgłoszeń przypisanych do grup G1–G5",
    x = NULL,
    y = "Liczba zgłoszeń"
  ) +
  theme_pub()

# =========================================================
# 12. RYCINA 8 - MODEL KONCEPTUALNY
# =========================================================

p8 <- ggplot() +
  geom_curve(
    aes(x = 1, y = 4, xend = 3, yend = 5),
    curvature = 0.12, linewidth = 0.9, colour = "grey55",
    arrow = arrow(length = unit(0.18, "cm"), type = "closed")
  ) +
  geom_curve(
    aes(x = 1, y = 4, xend = 2, yend = 2),
    curvature = -0.18, linewidth = 0.9, colour = "grey55",
    arrow = arrow(length = unit(0.18, "cm"), type = "closed")
  ) +
  geom_curve(
    aes(x = 3, y = 5, xend = 5, yend = 4),
    curvature = 0.10, linewidth = 0.9, colour = "grey55",
    arrow = arrow(length = unit(0.18, "cm"), type = "closed")
  ) +
  geom_curve(
    aes(x = 3, y = 5, xend = 4, yend = 2),
    curvature = -0.15, linewidth = 0.9, colour = "grey55",
    arrow = arrow(length = unit(0.18, "cm"), type = "closed")
  ) +
  geom_curve(
    aes(x = 5, y = 4, xend = 4, yend = 2),
    curvature = 0.12, linewidth = 0.9, colour = "grey55",
    arrow = arrow(length = unit(0.18, "cm"), type = "closed")
  ) +
  geom_curve(
    aes(x = 2, y = 2, xend = 4, yend = 2),
    curvature = -0.10, linewidth = 0.9, colour = "grey55",
    arrow = arrow(length = unit(0.18, "cm"), type = "closed")
  ) +
  geom_label(
    data = df_model_nodes,
    aes(x = x, y = y, label = label),
    fill = df_model_nodes$fill,
    colour = "grey10",
    fontface = "bold",
    size = 3.8,
    label.size = 0.35,
    label.r = unit(0.2, "lines"),
    label.padding = unit(0.35, "lines"),
    lineheight = 1.0
  ) +
  annotate(
    "text", x = 3, y = 5.85,
    label = "Model konceptualny relacji między grupami strategicznymi",
    fontface = "bold", size = 5.2, colour = "grey10"
  ) +
  annotate(
    "text", x = 3, y = 0.95,
    label = "Strzałki wskazują relacje interpretacyjne pomiędzy obszarami klimatu, ochrony, gospodarki, zarządzania i zaplecza wiedzy.",
    size = 3.7, colour = "grey35"
  ) +
  coord_cartesian(xlim = c(0.3, 5.7), ylim = c(0.7, 6.1), clip = "off") +
  theme_void() +
  theme(plot.margin = margin(20, 20, 20, 20))

# =========================================================
# 13. PODGLĄD RYCIN
# =========================================================
# Uruchamiaj pojedynczo:
# p1
# p2
# p4
# p7
# p8

# Albo wszystkie po kolei:
p1
p2
p4
p7
p8
#===============================
# model konceptualny jeszcze raz
# =========================================================
# DANE: liczba zgłoszeń w grupach
# =========================================================
library(ggplot2)
library(grid)
library(dplyr)
library(tidyr)

df_model_nodes <- data.frame(
  grupa = c("G1", "G2", "G3", "G4", "G5"),
  x = c(1, 3, 5, 2, 4),
  y = c(4, 5, 4, 2, 2),
  n = c(71, 124, 45, 84, 43),
  label = c(
    "G1\nKlimat i adaptacja",
    "G2\nOchrona ekosystemów",
    "G3\nDrewno i ekonomika",
    "G4\nZarządzanie lasami",
    "G5\nB+R i komunikacja"
  )
)

pal_5 <- c(
  "G1" = "#2E86AB",
  "G2" = "#3C9D5D",
  "G3" = "#D98E04",
  "G4" = "#8E5EA2",
  "G5" = "#D1495B"
)

# wszystkie pary bez powtórzeń
df_model_edges <- expand_grid(
  from = df_model_nodes$grupa,
  to   = df_model_nodes$grupa
) %>%
  filter(from < to) %>%
  left_join(df_model_nodes %>% select(grupa, x, y), by = c("from" = "grupa")) %>%
  rename(x = x, y = y) %>%
  left_join(df_model_nodes %>% select(grupa, x, y), by = c("to" = "grupa")) %>%
  rename(xend = x, yend = y) %>%
  mutate(
    curvature = case_when(
      abs(y - yend) < 0.3 ~ 0.18,
      x < xend & y < yend ~ 0.12,
      x < xend & y > yend ~ -0.12,
      TRUE ~ 0.10
    )
  )

p8 <- ggplot() +
  geom_curve(
    data = df_model_edges,
    aes(x = x, y = y, xend = xend, yend = yend),
    curvature = df_model_edges$curvature,
    linewidth = 0.85,
    colour = "grey60",
    alpha = 0.45
  ) +
  geom_point(
    data = df_model_nodes,
    aes(x = x, y = y, size = n, fill = grupa),
    shape = 21,
    colour = "white",
    stroke = 1.3,
    alpha = 0.97
  ) +
  geom_text(
    data = df_model_nodes,
    aes(x = x, y = y + 0.10, label = label),
    colour = "black",
    fontface = "bold",
    size = 3.6,
    lineheight = 0.95
  ) +
  geom_text(
    data = df_model_nodes,
    aes(x = x, y = y - 0.45, label = paste0("n = ", n)),
    colour = "black",
    fontface = "bold",
    size = 3.1
  ) +
  scale_fill_manual(values = pal_5) +
  scale_size_continuous(range = c(18, 34)) +
  coord_cartesian(xlim = c(0.3, 5.7), ylim = c(0.5, 6.2), clip = "off") +
  guides(size = "none", fill = "none") +
  theme_void() +
  theme(plot.margin = margin(20, 20, 20, 20))

p8
#=================================
#jeszcze jeden bąbelkowy do tematów
library(dplyr)
library(ggplot2)
library(readxl)
library(stringr)
library(forcats)
library(scales)

# =========================================================
# DANE
# =========================================================

xlsx_path <- "H:/.shortcut-targets-by-id/1PDXlz_ld72WsjIsG2l4pTXC7d_nhn5yO/NPL/Ankiety ws. NPL/Kopia konsolidacja_strategiczna_5_15_45.xlsx"
tematy_15 <- read_excel(xlsx_path, sheet = "Tematy_15")

mapuj_grupe_5 <- function(x) {
  kod <- stringr::str_extract(as.character(x), "^GRUPA\\s+[IVX]+")
  dplyr::case_when(
    kod == "GRUPA I"   ~ "G1",
    kod == "GRUPA II"  ~ "G2",
    kod == "GRUPA III" ~ "G3",
    kod == "GRUPA IV"  ~ "G4",
    kod == "GRUPA V"   ~ "G5",
    TRUE ~ as.character(x)
  )
}

pal_5 <- c(
  "G1" = "#2E86AB",
  "G2" = "#3C9D5D",
  "G3" = "#D98E04",
  "G4" = "#8E5EA2",
  "G5" = "#D1495B"
)

theme_pub <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0, colour = "grey10"),
      plot.subtitle = element_text(size = 11, colour = "grey35"),
      axis.title = element_text(face = "bold", colour = "grey10"),
      axis.text = element_text(colour = "grey20"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "none",
      plot.margin = margin(15, 20, 15, 15)
    )
}

# =========================================================
# PRZYGOTOWANIE DANYCH
# =========================================================

df_bubble <- tematy_15 %>%
  transmute(
    temat = as.character(TEMAT_15),
    grupa = mapuj_grupe_5(GRUPA),
    n = as.numeric(LICZBA_ZGLOSZEN)
  ) %>%
  filter(!is.na(temat), !is.na(grupa), !is.na(n)) %>%
  mutate(
    grupa = factor(grupa, levels = c("G1", "G2", "G3", "G4", "G5"))
  ) %>%
  group_by(grupa) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  mutate(
    pozycja = row_number()
  ) %>%
  ungroup()

# skrócone etykiety do bąbli: numer tematu + liczba zgłoszeń
df_bubble <- df_bubble %>%
  mutate(
    temat_id = paste0("T", row_number()),
    label_short = paste0(temat_id, "\n", n)
  )

# =========================================================
# WYKRES BĄBELKOWY
# =========================================================

p_bubble <- ggplot(df_bubble, aes(x = grupa, y = -pozycja)) +
  geom_point(
    aes(size = n, fill = grupa),
    shape = 21,
    colour = "white",
    stroke = 1.1,
    alpha = 0.95
  ) +
  geom_text(
    aes(label = label_short),
    colour = "white",
    fontface = "bold",
    size = 3.4,
    lineheight = 0.95
  ) +
  scale_fill_manual(values = pal_5) +
  scale_size_continuous(range = c(8, 23)) +
  scale_y_continuous(
    breaks = -df_bubble$pozycja,
    labels = df_bubble$temat,
    expand = expansion(mult = c(0.08, 0.12))
  ) +
  labs(
    title = "Rozkład 15 tematów w obrębie grup strategicznych",
    subtitle = "Wielkość bąbla odpowiada liczbie zgłoszeń; etykieta wewnątrz oznacza numer tematu i liczbę zgłoszeń",
    x = NULL,
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_pub() +
  theme(
    axis.text.y = element_text(size = 10, hjust = 1),
    axis.text.x = element_text(face = "bold", size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey88"),
    plot.margin = margin(20, 20, 20, 30)
  ) +
  guides(size = "none")

p_bubble
#=============v2 bąble
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)
library(scales)
library(grid)

# =========================================================
# DANE
# =========================================================

xlsx_path <- "H:/.shortcut-targets-by-id/1PDXlz_ld72WsjIsG2l4pTXC7d_nhn5yO/NPL/Ankiety ws. NPL/Kopia konsolidacja_strategiczna_5_15_45.xlsx"
tematy_15 <- read_excel(xlsx_path, sheet = "Tematy_15")

mapuj_grupe_5 <- function(x) {
  kod <- stringr::str_extract(as.character(x), "^GRUPA\\s+[IVX]+")
  dplyr::case_when(
    kod == "GRUPA I"   ~ "G1",
    kod == "GRUPA II"  ~ "G2",
    kod == "GRUPA III" ~ "G3",
    kod == "GRUPA IV"  ~ "G4",
    kod == "GRUPA V"   ~ "G5",
    TRUE ~ as.character(x)
  )
}

pal_5 <- c(
  "G1" = "#2E86AB",
  "G2" = "#3C9D5D",
  "G3" = "#D98E04",
  "G4" = "#8E5EA2",
  "G5" = "#D1495B"
)

theme_pub <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0, colour = "grey10"),
      plot.subtitle = element_text(size = 11, colour = "grey35"),
      axis.title = element_text(face = "bold", colour = "grey10"),
      axis.text = element_text(colour = "grey20"),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.margin = margin(15, 20, 15, 15)
    )
}

# =========================================================
# PRZYGOTOWANIE DANYCH
# =========================================================

df_bubble_rank <- tematy_15 %>%
  transmute(
    temat = as.character(TEMAT_15),
    grupa = mapuj_grupe_5(GRUPA),
    n = as.numeric(LICZBA_ZGLOSZEN)
  ) %>%
  filter(!is.na(temat), !is.na(grupa), !is.na(n)) %>%
  arrange(desc(n), temat) %>%
  mutate(
    T_id = paste0("T", row_number()),
    temat_factor = factor(T_id, levels = rev(T_id))
  )

# legenda tematów pod wykresem
legend_df <- df_bubble_rank %>%
  mutate(
    legenda = paste0(T_id, " — ", temat)
  )

legend_text <- paste(legend_df$legenda, collapse = "\n")

# =========================================================
# FINALNA RYCINA
# =========================================================

p_bubble_final <- ggplot(df_bubble_rank, aes(x = n, y = temat_factor)) +
  geom_segment(
    aes(x = 0, xend = n, y = temat_factor, yend = temat_factor),
    colour = "grey84",
    linewidth = 0.7
  ) +
  geom_point(
    aes(size = n, fill = grupa),
    shape = 21,
    colour = "white",
    stroke = 1.2,
    alpha = 0.96
  ) +
  geom_text(
    aes(label = T_id),
    colour = "white",
    fontface = "bold",
    size = 3.5
  ) +
  scale_fill_manual(values = pal_5) +
  scale_size_continuous(range = c(6, 18)) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.08)),
    labels = label_number(accuracy = 1)
  ) +
  labs(
    title = "Nasycenie tematów na poziomie 15 kategorii",
    subtitle = "Wielkość bąbla odpowiada liczbie zgłoszeń; kolory oznaczają grupy strategiczne G1–G5",
    x = "Liczba zgłoszeń",
    y = NULL
  ) +
  annotate(
    "text",
    x = 0,
    y = -1.1,
    label = legend_text,
    hjust = 0,
    vjust = 1,
    size = 3.4,
    colour = "grey15",
    family = "",
    lineheight = 1.05
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(1, nrow(df_bubble_rank) + 7)
  ) +
  theme_pub() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey90"),
    plot.margin = margin(20, 25, 210, 25)
  ) +
  guides(size = "none")

p_bubble_final
#==================jeszcze raz bąble
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)
library(scales)
library(grid)

# =========================================================
# DANE
# =========================================================

xlsx_path <- "H:/.shortcut-targets-by-id/1PDXlz_ld72WsjIsG2l4pTXC7d_nhn5yO/NPL/Ankiety ws. NPL/Kopia konsolidacja_strategiczna_5_15_45.xlsx"
tematy_15 <- read_excel(xlsx_path, sheet = "Tematy_15")

mapuj_grupe_5 <- function(x) {
  kod <- stringr::str_extract(as.character(x), "^GRUPA\\s+[IVX]+")
  dplyr::case_when(
    kod == "GRUPA I"   ~ "G1",
    kod == "GRUPA II"  ~ "G2",
    kod == "GRUPA III" ~ "G3",
    kod == "GRUPA IV"  ~ "G4",
    kod == "GRUPA V"   ~ "G5",
    TRUE ~ as.character(x)
  )
}

pal_5 <- c(
  "G1" = "#2E86AB",
  "G2" = "#3C9D5D",
  "G3" = "#D98E04",
  "G4" = "#8E5EA2",
  "G5" = "#D1495B"
)

theme_pub <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0, colour = "grey10"),
      plot.subtitle = element_text(size = 11, colour = "grey35"),
      axis.title = element_text(face = "bold", colour = "grey10"),
      axis.text = element_text(colour = "grey20"),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.margin = margin(15, 20, 15, 15)
    )
}

# =========================================================
# PRZYGOTOWANIE DANYCH
# =========================================================

df_bubble_final <- tematy_15 %>%
  transmute(
    temat = as.character(TEMAT_15),
    grupa = mapuj_grupe_5(GRUPA),
    n = as.numeric(LICZBA_ZGLOSZEN)
  ) %>%
  filter(!is.na(temat), !is.na(grupa), !is.na(n)) %>%
  arrange(desc(n), temat) %>%
  mutate(
    T_id = paste0("T", row_number()),
    temat_factor = factor(T_id, levels = rev(T_id))
  )
# legenda tematów
legend_tematy <- df_bubble_final %>%
  mutate(opis = paste0(T_id, " — ", temat))

legend_tematy_text <- paste(legend_tematy$opis, collapse = "\n")

# legenda grup roboczych
legend_grupy_text <- paste(
  "Grupy robocze:",
  "G1 — klimat i adaptacja",
  "G2 — ochrona ekosystemów",
  "G3 — drewno i ekonomika",
  "G4 — zarządzanie lasami",
  "G5 — badania, rozwój, komunikacja i kształcenie",
  sep = "\n"
)


# =========================================================
# FINALNA RYCINA
# =========================================================

p_bubble_final <- ggplot(df_bubble_final, aes(x = n, y = temat_factor)) +
  geom_segment(
    aes(x = 0, xend = n, y = temat_factor, yend = temat_factor),
    colour = "grey84",
    linewidth = 0.7
  ) +
  geom_point(
    aes(size = n, fill = grupa),
    shape = 21,
    colour = "white",
    stroke = 1.2,
    alpha = 0.96
  ) +
  geom_text(
    aes(label = T_id),
    colour = "black",
    fontface = "bold",
    size = 3.4,
    vjust = -0.15
  ) +
  geom_text(
    aes(label = grupa),
    colour = "black",
    fontface = "bold",
    size = 2.7,
    vjust = 1.15
  ) +
  scale_fill_manual(values = pal_5) +
  scale_size_continuous(range = c(6, 18)) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.08)),
    labels = label_number(accuracy = 1)
  ) +
  labs(
    title = "Nasycenie tematów na poziomie 15 kategorii",
    subtitle = "Wielkość bąbla odpowiada liczbie zgłoszeń; bąble oznaczono numerem tematu (T) i grupą roboczą (G1–G5)",
    x = "Liczba zgłoszeń",
    y = NULL
  ) +
  annotate(
    "text",
    x = 0,
    y = -1.0,
    label = legend_grupy_text,
    hjust = 0,
    vjust = 1,
    size = 3.5,
    colour = "grey10",
    fontface = "bold",
    lineheight = 1.05
  ) +
  annotate(
    "text",
    x = 0,
    y = -7.2,
    label = legend_tematy_text,
    hjust = 0,
    vjust = 1,
    size = 3.3,
    colour = "grey15",
    lineheight = 1.04
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(1, nrow(df_bubble_final) + 14)
  ) +
  theme_pub() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey90"),
    plot.margin = margin(20, 25, 320, 25)
  ) +
  guides(size = "none")

p_bubble_final
#=================
#bąble problemy
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)
library(scales)

# =========================================================
# DANE
# =========================================================

xlsx_path <- "H:/.shortcut-targets-by-id/1PDXlz_ld72WsjIsG2l4pTXC7d_nhn5yO/NPL/Ankiety ws. NPL/Kopia konsolidacja_strategiczna_5_15_45.xlsx"
problemy_45 <- read_excel(xlsx_path, sheet = "Problemy_45")

mapuj_grupe_5 <- function(x) {
  kod <- stringr::str_extract(as.character(x), "^GRUPA\\s+[IVX]+")
  dplyr::case_when(
    kod == "GRUPA I"   ~ "G1",
    kod == "GRUPA II"  ~ "G2",
    kod == "GRUPA III" ~ "G3",
    kod == "GRUPA IV"  ~ "G4",
    kod == "GRUPA V"   ~ "G5",
    TRUE ~ as.character(x)
  )
}

pal_5 <- c(
  "G1" = "#2E86AB",
  "G2" = "#3C9D5D",
  "G3" = "#D98E04",
  "G4" = "#8E5EA2",
  "G5" = "#D1495B"
)

theme_pub <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0, colour = "grey10"),
      plot.subtitle = element_text(size = 11, colour = "grey35"),
      axis.title = element_text(face = "bold", colour = "grey10"),
      axis.text = element_text(colour = "grey20"),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 11, colour = "grey10"),
      plot.margin = margin(15, 20, 120, 20)
    )
}

# =========================================================
# WYBÓR PROBLEMÓW ZGODNIE Z AKAPITEM
# =========================================================

selected_problems <- c("P09", "P12", "P13", "P01", "P02", "P07", "P38", "P30", "P28", "P17")

df_bubbles <- problemy_45 %>%
  filter(PROBLEM_ID %in% selected_problems) %>%
  transmute(
    problem_id = PROBLEM_ID,
    problem = as.character(PROBLEM_45),
    temat = as.character(TEMAT_15),
    grupa = mapuj_grupe_5(GRUPA),
    n = as.numeric(LICZBA_ZGLOSZEN)
  ) %>%
  mutate(
    temat = str_replace(temat, "^\\d+\\.\\s*", ""),
    grupa = factor(grupa, levels = c("G1", "G2", "G3", "G4", "G5"))
  )

# kolejność tematów zgodna z numeracją
theme_order <- c(
  "Ryzyka klimatyczne i trwałość lasów",
  "Adaptacja i przebudowa drzewostanów",
  "Ochrona obszarów cennych i system ochrony",
  "Lasy społeczne, kulturowe i relacje z otoczeniem",
  "Standardy gospodarowania i równoważenie funkcji",
  "Strategia podaży surowca i rola drewna",
  "Model zarządzania i architektura systemu",
  "Prawo, nadzór i odpowiedzialność",
  "Edukacja społeczna i komunikacja"
)

df_bubbles <- df_bubbles %>%
  mutate(
    temat = factor(temat, levels = theme_order)
  ) %>%
  arrange(grupa, temat, desc(n), problem_id) %>%
  group_by(grupa, temat) %>%
  mutate(
    within_theme = row_number(),
    n_in_theme = n()
  ) %>%
  ungroup()

# =========================================================
# RĘCZNE ROZRZUCENIE BĄBLI W OBRĘBIE TEMATÓW
# =========================================================
# x = temat
# y = niewielkie przesunięcia, żeby bąble nie stały dokładnie w jednej linii

offset_fun <- function(k) {
  if (k == 1) return(0)
  if (k == 2) return(c(-0.18, 0.18))
  if (k == 3) return(c(-0.22, 0, 0.22))
  seq(-0.24, 0.24, length.out = k)
}

df_bubbles <- df_bubbles %>%
  group_by(grupa, temat) %>%
  arrange(desc(n), .by_group = TRUE) %>%
  mutate(
    x_num = cur_group_id(), # placeholder, nadpiszemy niżej
    y = offset_fun(n())[row_number()]
  ) %>%
  ungroup()

# prawidłowe pozycje x według tematu wewnątrz panelu
df_bubbles <- df_bubbles %>%
  group_by(grupa) %>%
  mutate(
    temat_local = forcats::fct_inorder(temat),
    x = as.numeric(temat_local)
  ) %>%
  ungroup()

# legenda pod wykresem
legend_text <- df_bubbles %>%
  arrange(desc(n), problem_id) %>%
  mutate(
    opis = paste0(problem_id, " — ", problem, " (", n, ")")
  ) %>%
  pull(opis) %>%
  paste(collapse = "\n")

# =========================================================
# WYKRES
# =========================================================

p_problems <- ggplot(df_bubbles, aes(x = x, y = y)) +
  geom_vline(
    data = df_bubbles %>% distinct(grupa, x, temat_local),
    aes(xintercept = x),
    colour = "grey92",
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +
  geom_point(
    aes(size = n, fill = grupa),
    shape = 21,
    colour = "white",
    stroke = 1.2,
    alpha = 0.96
  ) +
  geom_text(
    aes(label = problem_id),
    colour = "white",
    fontface = "bold",
    size = 3.4
  ) +
  facet_wrap(~ grupa, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = pal_5) +
  scale_size_continuous(range = c(10, 24)) +
  scale_x_continuous(
    breaks = df_bubbles %>% distinct(grupa, x, temat_local) %>% pull(x),
    labels = df_bubbles %>% distinct(grupa, x, temat_local) %>% pull(temat_local),
    expand = expansion(mult = c(0.08, 0.08))
  ) +
  labs(
    title = "Najczęściej reprezentowane problemy w obrębie tematów i grup",
    subtitle = "Dziesięć problemów skupiających łącznie 189 zgłoszeń (51,5% materiału analitycznego)",
    x = NULL,
    y = NULL
  ) +
  annotate(
    "text",
    x = -Inf, y = -0.48,
    label = legend_text,
    hjust = 0, vjust = 1,
    size = 3.3,
    colour = "grey15",
    lineheight = 1.06
  ) +
  coord_cartesian(ylim = c(-0.58, 0.38), clip = "off") +
  theme_pub() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 9.5, colour = "grey20"),
    plot.margin = margin(20, 20, 190, 20)
  ) +
  guides(size = "none")

p_problems