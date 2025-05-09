#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: fig-pr_sm
#| echo: false 
#| warning: false 
#| eval: true 
#| fig-cap: "FROG"
#| fig-alt: "FROG"

knitr::include_graphics("figures/pr_index_SM.png")

#
#
#
#
#| label: fig-pr_sm_roc
#| echo: false 
#| warning: false 
#| eval: true 
#| fig-cap: "FROG"
#| fig-alt: "FROG"

knitr::include_graphics("figures/pr_index_SM_ROC.png")

#
#
#
#| label: fig-pr_avg_cpue
#| echo: false 
#| warning: false 
#| eval: true 
#| fig-cap: "FROG"
#| fig-alt: "FROG"


knitr::include_graphics("figures/pr_index_average_cpue_by_district.png")

#
#
#
#
#| label: fig-pr_model_cpue
#| echo: false 
#| warning: false 
#| eval: true 
#| fig-cap: "FROG"
#| fig-alt: "FROG"

knitr::include_graphics("figures/pr_index_model_cpue_by_year.png")

#
#
#
#| label: fig-pr_index_qq
#| echo: false 
#| warning: false 
#| eval: true 
#| fig-cap: "FROG"
#| fig-alt: "FROG"

knitr::include_graphics("figures/pr_index_qq.png")

#
#
#
#
#
#| label: fig-pr_avgcpue_district
#| echo: false 
#| warning: false 
#| eval: true 
#| fig-cap: "FROG"
#| fig-alt: "FROG"

knitr::include_graphics("figures/pr_index_average_cpue_by_district.png")

#
#
#
#
#
#| label: tbl-pr_filter
#| echo: false
#| warning: false
#| tbl-cap: "FROG"
library(tidyr)
library(gt)

pr_filter <- read.csv("tables/pr_index_data_filters.csv", check.names = F)
pr_filter |>
    gt() |>
     cols_width(starts_with("Description") ~ px(250),
                starts_with("Positive") ~ px(100),
                starts_with("Filter") ~ px(100)) |>
     tab_options(table.font.size = 12) |>
     fmt_number(sep_mark = ",", decimals = 0) |>
    as_latex()

#
#
#
#
#| label: tbl-pr_model_select
#| echo: false
#| warning: false
#| tbl-cap: "FROG"

pr_filter <- read.csv("tables/pr_index_model_selection.csv", check.names = F)
pr_filter |>
    gt() |>
     tab_options(table.font.size = 12) |>
     cols_width(everything() ~ px(60)) |>
    as_latex()

#
#
#
#| label: tbl-pr_index
#| echo: false
#| warning: false
#| tbl-cap: "FROG"

pr_filter <- read.csv("tables/pr_index_values.csv")
pr_filter |>
    gt() |>
    tab_options(table.font.size = 12) |>
    cols_width(everything() ~ px(60)) |>
    as_latex()

#
#
#
