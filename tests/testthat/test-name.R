library(tidyverse)
library(ggforestplot)

# linear associations
df_linear <-
  ggforestplot::df_linear_associations %>%
  dplyr::arrange(name) %>%
  dplyr::filter(dplyr::row_number() <= 30)

#Forest plot
forestplot(
  df = df_linear,
  estimate = beta,
  logodds = FALSE,
  colour = trait,
  title = "Associations to metabolic traits",
  xlab = "1-SD increment in cardiometabolic trait
  per 1-SD increment in biomarker concentration"
)

#Odds ratio
df_logodds <-
  df_logodds_associations %>%
  dplyr::arrange(name) %>%
  dplyr::left_join(ggforestplot::df_NG_biomarker_metadata, by = "name") %>% 
  dplyr::filter(group == "Amino acids") %>%
  # Set the study variable to a factor to preserve order of appearance
  # Set class to factor to set order of display.
  dplyr::mutate(
    study = factor(
      study,
      levels = c("Meta-analysis", "NFBC-1997", "DILGOM", "FINRISK-1997", "YFS")
    )
  )
# Forestplot
forestplot(
  df = df_logodds,
  estimate = beta,
  logodds = TRUE,
  colour = study,
  shape = study,
  title = "Associations to type 2 diabetes",
  xlab = "Odds ratio for incident type 2 diabetes (95% CI)
  per 1−SD increment in metabolite concentration"
) +
  # You may also want to add a manual shape scale to mark meta-analysis with a
  # diamond shape
  ggplot2::scale_shape_manual(
    values = c(23L, 21L, 21L, 21L, 21L),
    labels = c("Meta-analysis", "NFBC-1997", "DILGOM", "FINRISK-1997", "YFS")
  )

options(crayon.enabled=FALSE)
library(dplyr)
as_tibble(t)

library(ggplot2)
fp <- t %>%
  as_tibble() %>%
  ggplot(aes(y=Covariate, x=OddsRatio, label=Covariate)) +
    geom_point(size=4, shape=19) +
    geom_errorbarh(aes(xmin=ciLow, xmax=ciHigh), height=.3) +
    coord_fixed(ratio=.3) +
    geom_vline(xintercept=1, linetype='longdash') +
    facet_wrap(~Model, ncol=1)
ggsave('./forest-plot.png', fp, device='png', width=5, units="in")


library(gtable)
library(grid)
library(gridExtra)
library(ggplot2)

tg <- tableGrob(iris[1:5,1:3], rows = NULL, cols=NULL)
tg$heights <- unit(rep(1,nrow(tg)), "null")

p <- qplot(1:5,1:5) + ggtitle("Title", subtitle = "another line") + 
theme_grey(12) +
scale_y_continuous(expand=c(0,0.5))
g <- ggplotGrob(p)
g <- gtable::gtable_add_cols(g, widths = sum(tg$widths), pos = 0)
g <- gtable::gtable_add_cols(g, widths = sum(tg$widths), pos = -1)
g <- gtable::gtable_add_grob(g, list(tg, tg), t = 6, l=c(1,ncol(g)), r=c(1,ncol(g)))
grid.newpage()
grid.draw(g)

library(forestmodel)
set.seed(500)
Data1 <- data.frame(
    TXT_MoD = sample(0:1,20, replace = TRUE),
    W_Male = sample(0:1,20, replace = TRUE),
    Tumor_Stage = sample(1:3,20, replace = TRUE),
    W_AGE_60 = sample(c(1, 0), 20, replace = TRUE)
)
print(forest_model(glm(TXT_MoD ~ W_Male + W_AGE_60 + Tumor_Stage, data =Data1, family = "binomial")))
colnames(Data1)[colnames(Data1)=="W_Male"] <- "Male Gender"
colnames(Data1)[colnames(Data1)=="W_AGE_60"] <- "Age>60"
print(forest_model(glm(TXT_MoD ~ "Male Gender" + "Age>60" + "Tumor_Stage", data =Data1, family = "binomial")))