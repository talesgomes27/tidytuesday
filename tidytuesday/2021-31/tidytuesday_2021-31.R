#Inspiração
#https://twitter.com/SaintZainn/status/1420348744147509249


# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!



library(raster)
library(grid)
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2021, week = 31)
#tuesdata <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')



#Mesclando as bases para dar utilidade a coluna "region" da tabela "regions"
olympics <- left_join(tuesdata$regions, tuesdata$olympics, by = c("NOC" = "noc"))

####Limpando os Dados####
#limpando a base de algumas colunas que não serao utilizadas,
#bem como transformando outras em fatores para melhor manipulação
olympics <- olympics |>
  mutate(
    medal = replace_na(medal, "None"),
    sex = factor(sex, levels = c("F", "M")),
    medal = ordered(medal, levels = c("None", "Bronze", "Silver", "Gold")),
    season = factor(season, levels = c("Summer", "Winter")),
    year = factor(year, ordered = TRUE),
    id = factor(id),
    NOC = factor(NOC)
  ) |>
  select(-c(notes, age, height, weight, team))




####Variaveis auxiliares####
#Paises na análise
countrys <- c("CAN", "GBR", "USA", "BRA")
#Cores das bandeiras dos paises
country_colors <- cbind(
  c("#FFFFFF", "#FF0000"),
  c("#ffffff", "#012169"),
  c("#3C3B6E", "#B22234"),
  c("#FFDF00", "#009C3B")
)
olympic_color <- c("#35B2C9", "#FFBF00")
colnames(country_colors) <- countrys
#Lista que armazenará as Imagens geradas
myplots <- list()


# Figuras auxiliares
Olympic_rings <- png::readPNG("fig/Olympic_rings.png") |> 
  rasterGrob(interpolate = TRUE)
flag_canada <- png::readPNG("tidytuesday/2021-31/fig/canada.png")  |> 
  rasterGrob(interpolate = TRUE)
flag_uk <- png::readPNG("tidytuesday/2021-31/fig/uk.png") |> 
  rasterGrob(interpolate = TRUE)
flag_usa <- png::readPNG("tidytuesday/2021-31/fig/usa.png") |> 
  rasterGrob(interpolate = TRUE)
flag_brazil <- png::readPNG("tidytuesday/2021-31/fig/brazil.png") |> 
  rasterGrob(interpolate = TRUE)


flags <- list(flag_canada, flag_uk, flag_usa, flag_brazil)



#### Analise geral ####


#Filtrando os dados
sex_per_sport <- olympics |>
  filter(season == "Summer" & year >= 1948) |>
  group_by(sport) |>
  count(sex) |>
  group_by(sport) |>
  mutate(percent = 100 * n / sum(n)) |>
  filter(percent < 100) |>
  ungroup() |>
  dplyr::select(-n) |>
  tidyr::pivot_wider(
    names_from = sex,
    values_from = percent,
    names_prefix = "percent_"
  ) |>
  dplyr::mutate(sport = forcats::fct_reorder(sport, desc(percent_F))) |>
  tidyr::pivot_longer(cols = c("percent_F", "percent_M"),
                      values_to = "percent") |>
  dplyr::rename("sex" = name) |>
  dplyr::mutate(
    sex = stringr::str_remove(sex, "percent_"),
    sex = forcats::fct_relevel(sex, c("M", "F"))
  ) 

#Filtro de participação por esporte praticado

#Filtrando os dados
sex_per_year <- olympics |>
  filter(season == "Summer" & year >= 1948) |>
  group_by(year) |>
  count(sex) |>
  group_by(year) |>
  mutate(percent = 100 * n / sum(n)) |>
  filter(percent < 100) |>
  ungroup() |>
  dplyr::select(-n) |>
  tidyr::pivot_wider(
    names_from = sex,
    values_from = percent,
    names_prefix = "percent_"
  ) |>
  dplyr::mutate(year = forcats::fct_reorder(year, desc(percent_F))) |>
  tidyr::pivot_longer(cols = c("percent_F", "percent_M"),
                      values_to = "percent") |>
  dplyr::rename("sex" = name) |>
  dplyr::mutate(
    sex = stringr::str_remove(sex, "percent_"),
    sex = forcats::fct_relevel(sex, c("M", "F"))
  ) 



sex_per_year |>
  ggplot(mapping = aes(percent, year)) +
  geom_col(aes(fill = sex, color = sex), position = "stack") +
  scale_color_manual(values = olympic_color, labs("")) +
  scale_fill_manual(values = olympic_color, labs("")) +
  geom_vline(xintercept = 50,
             linetype = "dashed",
             size = 0.7) +
  hrbrthemes::scale_x_percent(scale = 1) +
  hrbrthemes::theme_ipsum_pub(axis_text_size = 20) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(color = "darkgray", size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor  = element_blank(),
    axis.text = element_text(color = "darkgray"),
    plot.subtitle = element_text(
      color = "darkgray",
      size = 20,
      family = "sans"
    )
  ) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(color = "darkgray", size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor  = element_blank(),
    axis.title = element_text(color = "darkgray", size = 20),
    axis.text = element_text(color = "darkgray"),
    plot.caption = element_text(color = "darkgray", size = 20),
    plot.title = element_text(
      color = "darkgray",
      hjust = 0.5,
      size = 22,
      family = "sans"
    ),
    plot.subtitle = element_text(
      color = "darkgray",
      size = 20,
      family = "sans"
    )
  ) +
  labs(
    y = "",
    x = "",
    title = "Total olympic participation from 1948 to 2017" ,
    subtitle = "Summary by gender and year",
    caption = "@talesgomes2709 | #tidytuesday | source: kaggle"
  )







#participação por olímpiada e gênero
sex_per_sport |>
  ggplot(mapping = aes(percent, sport)) +
  geom_col(aes(fill = sex, color = sex), position = "stack") +
  scale_color_manual(values = olympic_color, labs("")) +
  scale_fill_manual(values = olympic_color, labs("")) +
  geom_vline(xintercept = 50,
             linetype = "dashed",
             size = 0.7) +
  hrbrthemes::scale_x_percent(scale = 1) +
  hrbrthemes::theme_ipsum_pub(axis_text_size = 20) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(color = "darkgray", size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor  = element_blank(),
    axis.text = element_text(color = "darkgray"),
    plot.subtitle = element_text(
      color = "darkgray",
      size = 20,
      family = "sans"
    )
  ) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(color = "darkgray", size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor  = element_blank(),
    axis.title = element_text(color = "darkgray", size = 20),
    axis.text = element_text(color = "darkgray"),
    plot.caption = element_text(color = "darkgray", size = 20),
    plot.title = element_text(
      color = "darkgray",
      hjust = 0.5,
      size = 22,
      family = "sans"
    ),
    plot.subtitle = element_text(
      color = "darkgray",
      size = 20,
      family = "sans"
    )
  ) +
  labs(
    y = "",
    x = "",
    title = "Total olympic participation from 1948 to 2017" ,
    subtitle = "Summary by gender and sports",
    caption = "@talesgomes2709 | #tidytuesday | source: kaggle"
  )





####Filtrando agregado por esportes e gênero####


#Gerando imagens esportes vs gênero
for (i in 1:4) {
  olympics_country <- olympics |>
    filter(NOC == countrys[i] & year >= 1948)
  country_title <- str_c(
      "Summer Olympics male to famale athletes proportion from 1964 to 2016 in",
      olympics_country$region[1],
      sep = " "
    )
  
  
  #Filtrando os dados
  sex_per_sport <- olympics_country |>
    filter(season == "Summer") |>
    group_by(sport) |>
    count(sex) |>
    group_by(sport) |>
    mutate(percent = 100 * n / sum(n)) |>
    filter(percent < 100) |>
    ungroup() |>
    dplyr::select(-n) |>
    tidyr::pivot_wider(
      names_from = sex,
      values_from = percent,
      names_prefix = "percent_"
    ) |>
    dplyr::mutate(sport = forcats::fct_reorder(sport, desc(percent_F))) |>
    tidyr::pivot_longer(cols = c("percent_F", "percent_M"),
                        values_to = "percent") |>
    dplyr::rename("sex" = name) |>
    dplyr::mutate(
      sex = stringr::str_remove(sex, "percent_"),
      sex = forcats::fct_relevel(sex, c("M", "F"))
    ) 
  
  #Gerando Lista de figuras para
  fig <- sex_per_sport |>
    ggplot(mapping = aes(percent, sport)) +
    geom_col(aes(fill = sex, color = sex), position = "stack") +
    scale_color_manual(values = country_colors[, i], labs("")) +
    scale_fill_manual(values = country_colors[, i], labs("")) +
    geom_vline(xintercept = 50,
               linetype = "dashed",
               size = 0.7) +
    hrbrthemes::scale_x_percent(scale = 1) +
    hrbrthemes::theme_ipsum_pub(axis_text_size = 20) +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(color = "darkgray", size = 20),
      panel.grid.major = element_blank(),
      panel.grid.minor  = element_blank(),
      axis.text = element_text(color = "darkgray"),
      plot.subtitle = element_text(
        color = "darkgray",
        size = 20,
        family = "sans"
      )
    ) +
    labs(y = "",
         x = "", )+
    ggtitle("")
  
  
  
  fig <- ggplotGrob(fig)
  
  new_title <- gtable(unit(0.8, "cm"), unit(4, "cm")) |>
    gtable_add_grob(grobs = flags[i], t = 1, l = 1) |>
    gtable_add_cols(widths = unit(1, "null")) |>
    gtable_add_grob(textGrob(label = olympics_country$region[1],
                             x = unit(0, "npc"), just = "left"),
                    t = 1, l = 2) |>
    gtable_add_col_space(width = unit(5, "pt"))
  
  fig$grobs[[which(fig$layout$name == "title")]] <- new_title
  
  
  
  myplots[[i]] <- fig
  
  # 
  # sex_per_sport |>
  #   ggplot(mapping = aes(percent, sport)) +
  #   geom_col(aes(fill = sex, color = sex), position = "stack") +
  #   scale_color_manual(values = country_colors[, i], labs("")) +
  #   scale_fill_manual(values = country_colors[, i], labs("")) +
  #   geom_vline(xintercept = 50,
  #              linetype = "dashed",
  #              size = 0.7) +
  #   hrbrthemes::scale_x_percent(scale = 1) +
  #   hrbrthemes::theme_ipsum_pub(axis_text_size = 20) +
  #   theme(
  #     legend.title = element_blank(),
  #     legend.text = element_text(color = "darkgray", size = 20),
  #     panel.grid.major = element_blank(),
  #     panel.grid.minor  = element_blank(),
  #     axis.title = element_text(color = "darkgray", size = 20),
  #     axis.text = element_text(color = "darkgray"),
  #     plot.caption = element_text(color = "darkgray", size = 20),
  #     plot.title = element_text(
  #       color = "darkgray",
  #       hjust = 0.5,
  #       size = 22,
  #       family = "sans"
  #     ),
  #     plot.subtitle = element_text(
  #       color = "darkgray",
  #       size = 20,
  #       family = "sans"
  #     )
  #   ) +
  #   labs(
  #     y = "",
  #     x = "",
  #     title = country_title,
  #     subtitle = "Summary by sports",
  #     caption = "@talesgomes2709 | #tidytuesday | source: kaggle"
  #   )
  # 
  # fig_name <- str_c(
  #   "tidytuesday/2021-31/fig/",
  #   olympics_country$region[1],
  #   "_tidytuesday_2021-07-27.png"
  # )
  # ggsave(
  #   fig_name,
  #   scale = 1,
  #   dpi = 400,
  #   width = 45,
  #   height = 35,
  #   units = c("cm")
  # )
}

#Gerando imagem unificada
comp_plot <- ggpubr::ggarrange(
    myplots[[1]],
    myplots[[2]],
    myplots[[3]],
    myplots[[4]],
    labels = c("Canada", "China", "USA", "Brazil"),
    ncol = 2,
    nrow = 2,
    font.label = list(size = 14,
                      color = "darkgray", face = "bold")
  )

ggpubr::annotate_figure(
  comp_plot,
  top = ggpubr::text_grob(
    "Summer olympics male to famale athletes proportion from 1948 to 2016",
    color = "darkgray",
    face = "bold",
    size = 20
  ),
  bottom = ggpubr::text_grob(
    "@talesgomes2709 | #tidytuesday | source: kaggle",
    color = "darkgray",
    face = "bold",
    size = 18
  )
)

#Salvando iagens unificadas
ggsave(
  "tidytuesday/2021-31/fig/Comp_tidytuesday_2021-07-27.png",
  scale = 1,
  dpi = 400,
  width = 90,
  height = 50,
  units = c("cm")
)




#### Filtrando por esportes e número de medalhas ####
for (i in 1:4) {
  olympics_country <- olympics |>
    filter(NOC == countrys[i] & year >= 1948)
  country_title <- str_c(
    "Summer Olympics male to famale medal proportion from 1964 to 2016 in",
    olympics_country$region[1],
    sep = " "
  )
  
  
  
  medal_per_sport_sex <- olympics_country |>
    filter(season == "Summer" & medal != "None") |>
    with_groups(c(sport, sex, event), count, sex) |> 
    with_groups(sport, mutate, percent = 100 * n / sum(n)) |> 
    select(-n) |>
    pivot_wider(
      names_from = sex,
      values_from = percent,
      names_prefix = "percent_"
    ) |>
    mutate(percent_M = replace_na(percent_M, 0),
           percent_F = replace_na(percent_F, 0)) |> 
    select(-event) |> 
    with_groups(sport, summarise, sport,
                percent_F = sum(percent_F),
                percent_M = sum(percent_M)) |> 
    unique() |> 
    pivot_longer(cols = c("percent_F", "percent_M"),
                 values_to = "percent") |>
    rename("sex" = name) |>
    mutate(
      sex = str_remove(sex, "percent_"),
      sex = fct_relevel(sex, c("M", "F"))
    )

  
  fig <- medal_per_sport_sex |>
    ggplot(mapping = aes(percent, fct_reorder2(sport, sex, percent, .desc = TRUE))) +
    geom_col(aes(fill = sex, color = sex), position = "stack") +
    scale_color_manual(values = country_colors[, i], labs("")) +
    scale_fill_manual(values = country_colors[, i], labs("")) +
    geom_vline(xintercept = 50,
               linetype = "dashed",
               size = 0.7) +
    hrbrthemes::scale_x_percent(scale = 1) +
    hrbrthemes::theme_ipsum_pub(axis_text_size = 20) +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(color = "darkgray", size = 20),
      panel.grid.major = element_blank(),
      panel.grid.minor  = element_blank(),
      axis.text = element_text(color = "darkgray"),
      plot.subtitle = element_text(
        color = "darkgray",
        size = 20,
        family = "sans"
      )
    ) +
    labs(y = "",
         x = "")+
    ggtitle("")
  
  
  
  fig <- ggplotGrob(fig)
  
  new_title <- gtable(unit(0.8, "cm"), unit(4, "cm")) |>
    gtable_add_grob(grobs = flags[i], t = 1, l = 1) |>
    gtable_add_cols(widths = unit(1, "null")) |>
    gtable_add_grob(textGrob(label = olympics_country$region[1],
                             x = unit(0, "npc"), just = "left"),
                    t = 1, l = 2) |>
    gtable_add_col_space(width = unit(5, "pt"))
  
  fig$grobs[[which(fig$layout$name == "title")]] <- new_title
  
  myplots[[i]] <- fig
  
  
}


#Gerando imagem unificada
comp_plot <- ggpubr::ggarrange(
  myplots[[1]],
  myplots[[2]],
  myplots[[3]],
  myplots[[4]],
  labels = c("Canada", "China", "USA", "Brazil"),
  ncol = 2,
  nrow = 2,
  font.label = list(size = 14,
                    color = "darkgray", face = "bold")
)

ggpubr::annotate_figure(
  comp_plot,
  top = ggpubr::text_grob(
    "Summer olympics male to famale medals proportion from 1948 to 2016",
    color = "darkgray",
    face = "bold",
    size = 20
  ),
  bottom = ggpubr::text_grob(
    "@talesgomes2709 | #tidytuesday | source: kaggle",
    color = "darkgray",
    face = "bold",
    size = 18
  )
)

#Salvando iagens unificadas
ggsave(
  "tidytuesday/2021-31/fig/Comp_medal_oer gender_and_sports.png",
  scale = 1,
  dpi = 400,
  width = 90,
  height = 50,
  units = c("cm")
)


####Filtrando participação por gênero####


for (i in 1:4) {
  olympics_country <- olympics |>
    filter(NOC == countrys[i] & year >= 1948)
  
  country_title <- str_c("Participation by gender in",
                          olympics_country$region[1],
                          "from 1964 to 2016",
                          sep = " ")
  
  #Filtrando os dados
  participation_per_sex <- olympics_country |>
    filter(season == "Summer") |>
    group_by(year, sex) |>
    summarise(n = n(), .groups = "drop") |>
    ungroup() |>
    group_by(year) |>
    mutate(percent = n / sum(n) * 100)
  
  #Criando lista de figuras
  fig <- participation_per_sex |>
    ggplot(mapping = aes(year, percent)) +
    geom_col(aes(fill = sex, color = sex), position = "stack") +
    scale_color_manual(values = country_colors[, i], labs("")) +
    scale_fill_manual(values = country_colors[, i], labs("")) +
    geom_hline(yintercept = 50,
               linetype = "dashed",
               size = 0.7) +
    hrbrthemes::scale_y_percent(scale = 1) +
    hrbrthemes::theme_ipsum_pub(axis_text_size = 20) +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(color = "gray81", size = 20),
      panel.grid.major = element_blank(),
      panel.grid.minor  = element_blank(),
      axis.text = element_text(color = "gray81"),
    ) +
    labs(y = "",
         x = "")+
    ggtitle("")
  
  
  
  fig <- ggplotGrob(fig)
  
  new_title <- gtable(unit(0.8, "cm"), unit(4, "cm")) |>
    gtable_add_grob(grobs = flags[i], t = 1, l = 1) |>
    gtable_add_cols(widths = unit(1, "null")) |>
    gtable_add_grob(textGrob(label = olympics_country$region[1],
                             x = unit(0, "npc"), just = "left"),
                    t = 1, l = 2) |>
    gtable_add_col_space(width = unit(5, "pt"))
  
  fig$grobs[[which(fig$layout$name == "title")]] <- new_title
  
  
  myplots[[i]] <- fig
  
  
  # #Criando FIguras individuais
  # participation_per_sex |>
  #   ggplot(mapping = aes(year, percent)) +
  #   geom_col(aes(fill = sex, color = sex), position = "stack") +
  #   scale_color_manual(values = country_colors[, i], labs("")) +
  #   scale_fill_manual(values = country_colors[, i], labs("")) +
  #   geom_hline(yintercept = 50,
  #              linetype = "dashed",
  #              size = 0.7) +
  #   hrbrthemes::scale_y_percent(scale = 1) +
  #   hrbrthemes::theme_ipsum_pub(axis_text_size = 20) +
  #   theme(
  #     legend.title = element_blank(),
  #     legend.text = element_text(color = "gray81", size = 20),
  #     panel.grid.major = element_blank(),
  #     panel.grid.minor  = element_blank(),
  #     axis.title = element_text(color = "gray81", size = 20),
  #     axis.text = element_text(color = "gray81"),
  #     plot.caption = element_text(color = "gray81", size = 20),
  #     plot.title = element_text(
  #       color = "white",
  #       hjust = 0.5,
  #       size = 22,
  #       family = "sans"
  #     )
  #   ) +
  #   labs(
  #     y = "",
  #     x = "",
  #     title = country_title,
  #     caption = "@talesgomes2709 | #tidytuesday | source: kaggle"
  #   )
  # 
  # fig_name <- str_c(
  #   "tidytuesday/2021-31/fig/",
  #   olympics_country$region[1],
  #   "_participation_per_sex.png"
  # )
  # ggsave(
  #   fig_name,
  #   scale = 1,
  #   dpi = 400,
  #   width = 45,
  #   height = 35,
  #   units = c("cm")
  # )
}

#Gerando imagem unificada
comp_plot <- ggpubr::ggarrange(
    myplots[[1]],
    myplots[[2]],
    myplots[[3]],
    myplots[[4]],
    labels = c("Canada", "UK", "USA", "Brazil"),
    ncol = 2,
    nrow = 2,
    font.label = list(size = 14,
                      color = "darkgray", face = "bold")
  )

ggpubr::annotate_figure(
  comp_plot,
  top = ggpubr::text_grob(
    "Summer olympics participation by gender from 1948 to 2016",
    color = "darkgray",
    face = "bold",
    size = 24
  ),
  bottom = ggpubr::text_grob(
    "@talesgomes2709 | #tidytuesday | source: kaggle",
    color = "darkgray",
    face = "bold",
    size = 18
  )
)

#Salvando iagens unificadas
ggsave(
  "tidytuesday/2021-31/fig/Comp_participation_per_sex.png",
  scale = 1,
  dpi = 400,
  width = 90,
  height = 50,
  units = c("cm")
)




####Filtrando por medalhas por gênero####


for (i in 1:4) {
  olympics_country <- olympics |>
    filter(NOC == countrys[i] & year >= 1948)
  
  country_title <-  str_c(
    "Summer olympics medals by gender",
    olympics_country$region[1],
    "from 1964 to 2016",
    sep = " "
  )
  #Filtrando os dados
  medal_per_sex <- olympics_country |>
    filter(season == "Summer" & medal != "None") |>
    group_by(year, medal, sex, sport, event) |>
    summarise(medal_n = if_else(n() >= 1, 1, 0), .groups = "drop") |>
    ungroup() |>
    select(-c(sport:event)) |>
    group_by(year) |>
    mutate(percent = medal_n / sum(medal_n) * 100)
  
  #Criando lista de figuras
  fig <- medal_per_sex |>
    ggplot(mapping = aes(year, percent)) +
    geom_col(aes(fill = sex, color = sex), position = "stack") +
    scale_color_manual(values = country_colors[, i], labs("")) +
    scale_fill_manual(values = country_colors[, i], labs("")) +
    geom_hline(yintercept = 50,
               linetype = "dashed",
               size = 0.7) +
    hrbrthemes::scale_y_percent(scale = 1) +
    hrbrthemes::theme_ipsum_pub(axis_text_size = 20) +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(color = "gray81", size = 20),
      panel.grid.major = element_blank(),
      panel.grid.minor  = element_blank(),
      axis.text = element_text(color = "gray81"),
    ) +
    labs(y = "",
         x = "",)+
    ggtitle("")
  
  
  
  fig <- ggplotGrob(fig)
  
  new_title <- gtable(unit(0.8, "cm"), unit(4, "cm")) |>
    gtable_add_grob(grobs = flags[i], t = 1, l = 1) |>
    gtable_add_cols(widths = unit(1, "null")) |>
    gtable_add_grob(textGrob(label = olympics_country$region[1],
                             x = unit(0, "npc"), just = "left"),
                    t = 1, l = 2) |>
    gtable_add_col_space(width = unit(5, "pt"))
  
  fig$grobs[[which(fig$layout$name == "title")]] <- new_title
  
  myplots[[i]] <- fig
  
  
  
  # #Criando figuras individuais
  # medal_per_sex |>
  #   ggplot(mapping = aes(year, percent)) +
  #   geom_col(aes(fill = sex, color = sex), position = "stack") +
  #   scale_color_manual(values = country_colors[, i], labs("")) +
  #   scale_fill_manual(values = country_colors[, i], labs("")) +
  #   geom_hline(yintercept = 50,
  #              linetype = "dashed",
  #              size = 0.7) +
  #   hrbrthemes::scale_y_percent(scale = 1) +
  #   hrbrthemes::theme_ipsum_pub(axis_text_size = 20) +
  #   theme(
  #     legend.title = element_blank(),
  #     legend.text = element_text(color = "gray81", size = 20),
  #     panel.grid.major = element_blank(),
  #     panel.grid.minor  = element_blank(),
  #     axis.title = element_text(color = "gray81", size = 20),
  #     axis.text = element_text(color = "gray81"),
  #     plot.caption = element_text(color = "gray81", size = 20),
  #     plot.title = element_text(
  #       color = "white",
  #       hjust = 0.5,
  #       size = 22,
  #       family = "sans"
  #     )
  #   ) +
  #   labs(
  #     y = "",
  #     x = "",
  #     title = country_title,
  #     caption = "@talesgomes2709 | #tidytuesday | source: kaggle"
  #   )
  # 
  # 
  # fig_name <- str_c("tidytuesday/2021-31/fig/",
  #                   olympics_country$region[1],
  #                   "_medal_per_gender.png")
  # ggsave(
  #   fig_name,
  #   scale = 1,
  #   dpi = 400,
  #   width = 45,
  #   height = 35,
  #   units = c("cm")
  # )
}


#Gerando imagem unificada
comp_plot <- ggpubr::ggarrange(
    myplots[[1]],
    myplots[[2]],
    myplots[[3]],
    myplots[[4]],
    labels = c("Canada", "UK", "USA", "Brazil"),
    ncol = 2,
    nrow = 2,
    font.label = list(size = 14,
                      color = "darkgray", face = "bold")
  )

ggpubr::annotate_figure(
  comp_plot,
  top = ggpubr::text_grob(
    "Summer olympics medals by gender from 1948 to 2016",
    color = "darkgray",
    face = "bold",
    size = 24
  ),
  bottom = ggpubr::text_grob(
    "@talesgomes2709 | #tidytuesday | source: kaggle",
    color = "darkgray",
    face = "bold",
    size = 18
  )
)

#Salvando iagens unificadas
ggsave(
  "tidytuesday/2021-31/fig/Comp_medals_per_sex.png",
  scale = 1,
  dpi = 400,
  width = 90,
  height = 50,
  units = c("cm")
)
