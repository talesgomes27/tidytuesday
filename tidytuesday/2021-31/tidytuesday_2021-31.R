# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

#Inspiração
#https://twitter.com/SaintZainn/status/1420348744147509249

  
library(tidyverse)


tuesdata <- tidytuesdayR::tt_load(2021, week = 31)


str(tuesdata)
head(tuesdata)

#Mesclando as bases para dar utilidade a coluna "region" da tabela "regions"
olympics <- left_join(tuesdata$regions, tuesdata$olympics, by= c("NOC" = "noc"))


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
  select(-c(notes,age, height, weight, team))
  

countrys <- c("CAN", "CHN", "USA", "BRA")
country_colors <- cbind(c("#FFFFFF", "#FF0000"), c("#FFDE00", "#DE2910"), c("#3C3B6E", "#B22234"), c("#FFDF00", "#009C3B"))
colnames(country_colors) <- countrys
myplots <- list()


for(i in 1:4){
  
#Country <- "BRA"
olympics_country <- olympics |> 
  filter(NOC == countrys[i] & year >= 1964)

country_title <-  str_c("Summer Olympics male to famale athletes proportion from 1964 to 2016 in", olympics_country$region[1], sep = " ") 

#Filtrando os dados
sex_per_sport <- olympics_country |>
  filter(season == "Summer") |>
  group_by(sport) |>
  count(sex) |>
  group_by(sport) |>
  mutate(percent = 100 *n/sum(n)) |>
  filter(percent < 100) |>
  ungroup() |> 
  dplyr::select(-n) |>
  tidyr::pivot_wider(names_from = sex,
                     values_from = percent,
                     names_prefix = "percent_") |>
  dplyr::mutate(sport = forcats::fct_reorder(sport, desc(percent_F))) |>
  tidyr::pivot_longer(cols = c("percent_F", "percent_M"),
                      values_to = "percent") |>
  dplyr::rename("sex" = name) |>
  dplyr::mutate(sex = stringr::str_remove(sex, "percent_"),
                sex = forcats::fct_relevel(sex, c("M", "F")))

fig <- sex_per_sport |> 
  ggplot(mapping = aes(percent, sport)) +  
  geom_col(aes(fill = sex, color = sex), position = "stack") +
  #hrbrthemes::scale_color_ft()+
  #hrbrthemes::scale_fill_ft()+
  scale_color_manual(values=country_colors[,i], labs(""))+
  scale_fill_manual(values=country_colors[,i], labs(""))+
  #geom_vline(xintercept = 50, color = "darkgray", linetype = "dashed", size = 0.7)+
  geom_vline(xintercept = 50, linetype = "dashed", size = 0.7)+
  #scale_x_continuous(labels = scales::unit_format(unit = "%"))+
  hrbrthemes::scale_x_percent(scale = 1)+
  hrbrthemes::theme_ipsum_pub(axis_text_size = 20)+
  theme(
    legend.title = element_blank(),
    legend.text = element_text(color = "darkgray", size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor  = element_blank(),
    axis.title = element_text(color = "darkgray", size = 20),
    axis.text = element_text(color = "darkgray"),
    plot.caption = element_text(color = "darkgray", size = 20),
    plot.title = element_text(color = "darkgray", hjust = 0.5, size = 22, family = "sans"),
    plot.subtitle = element_text(color = "darkgray", size = 20, family = "sans")
  )+
  labs(
    y = "",
    x = "",
    #title = country_title,
    #subtitle = "Summary by sports",
    #caption = "@talesgomes2709 | #tidytuesday | source: kaggle"
  )

# fig_name <- str_c("tidytuesday/2021-31/fig/", olympics_country$region[1], "_tidytuesday_2021-07-27.png")
# ggsave(fig_name,
#         scale = 1,
#         dpi = 600,
#         width = 45,
#         height = 35,
#         units = c("cm"))
myplots[[i]] <- fig
}

ggpubr::ggarrange(myplots[[1]], myplots[[2]], myplots[[3]], myplots[[4]],
                 labels = countrys,
                 ncol = 2, nrow = 2,
                 font.label = list(size = 14, color = "darkgray", face = "bold"))



ggsave("tidytuesday/2021-31/fig/Comp_tidytuesday_2021-07-27.png",
       scale = 1,
       dpi = 600,
       width = 45,
       height = 35,
       units = c("cm"))

#### Medal per Sex #####

#Filtrando os dados
medal_per_sex <- olympics_country |>
   filter(season == "Summer" & medal != "None") |>
   group_by(year, medal, sex, sport, event) |>
   summarise(medal_n = if_else(n()>=1,1,0), .groups = "drop") |> 
   ungroup()

 

medal_per_sex |> 
ggplot(mapping = aes(year, medal_n)) +  
  geom_col(aes(fill = medal, color = sex), position = "stack") +
  scale_color_manual(values=c("#229954", "#D4AC0D"), labs(""))+
  scale_fill_manual(values=c("#8B4513", "#C0C0C0", "#FFD700"), labs(""))+
  geom_vline(xintercept = 50, color = "gray81", linetype = "dashed", size = 0.7)+
  hrbrthemes::theme_ft_rc(axis_text_size = 20)+
  theme(
    legend.title = element_blank(),
    legend.text = element_text(color = "gray81", size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor  = element_blank(),
    axis.title = element_text(color = "gray81", size = 20),
    axis.text = element_text(color = "gray81"),
    plot.caption = element_text(color = "gray81", size = 20),
    plot.title = element_text(color = "white", hjust = 0.5, size = 22, family = "sans")
  )+
  labs(
    y = "",
    x = "",
    title = "Male to Famale Athletes Proportion Across All Summer Olympics in Brazil by Sports",
    caption = "@talesgomes2709 | #tidytuesday | source: kaggle"
  )
