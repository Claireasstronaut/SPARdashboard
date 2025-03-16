library(shiny)
library(data.table)
library(plotly)
library(DT)
library(ggmosaic)
library(echarts4r)
library(htmlwidgets)
library(tidyverse)
library(wordcloud2)

options(digits = 3)

# 导入数据 --------------------------------------------------------------------
load("data/dashboard_data.RData")
load("data/SPAR_comment.RData")
jee_rec_sentence <- readRDS("data/JEE_rec_sentence.rds")
jee_ta <- readRDS("data/JEE_ta.rds")
stop_words_all <- readRDS("data/stop_words_all.rds")

cap <- rbind(cap_2021, cap_2022, cap_2023)[order(year, region, country)]
cap_by_region <- cap[, map(.SD, mean, na.rm = T), by = .(year, region), .SDcols = C1:C15]
cap_by_region[, `:=`(country = "Total", iso = "Total")]
cap_by_global <- cap[, map(.SD, mean, na.rm = T), by = year, .SDcols = C1:C15]
cap_by_global[, `:=`(region = "Total", country = "Total", iso = "Total")]
cap_total <- rbind(cap, cap_by_region, cap_by_global)

ind <- rbind(ind_2021, ind_2022, ind_2023)[order(year, region, country)]
ind_by_region <- ind[, map(.SD, mean, na.rm = T), by = .(year, region), .SDcols = C1.1:C15.1]
ind_by_region[, `:=`(country = "Total", iso = "Total")]
ind_by_global <- ind[, map(.SD, mean, na.rm = T), by = year, .SDcols = C1.1:C15.1]
ind_by_global[, `:=`(region = "Total", country = "Total", iso = "Total")]
ind_total <- rbind(ind, ind_by_region, ind_by_global)

# 提交状态 --------------------------------------------------------------------
regions <- c("AFRO", "AMRO", "EMRO", "EURO", "SEARO", "WPRO")
countries <- sort(c(cap_2023$country, "Andorra", "Tuvalu"))

cty_count_region <- data.table(
  region = regions, 
  total_count = c(47, 35, 21, 55, 11, 27)
)

# 建一个函数，用来计算各区域每年的上报数量
sub_count <- function(year) {
  x <- get(paste0("cap_", year))[, .N, by = region][
    cty_count_region, on = "region"][
      , .(region, Yes = N, No = as.integer(total_count) - N)]
  x <- melt.data.table(x,, 2:3, "submission", "count")
  x[, submission := fct_relevel(submission, c("No", "Yes"))]
}

# 建一个dt，包含每年上报总数
dt_sub_pct <- data.table(
  year = factor(2021:2023), 
  count = map_dbl(list(cap_2021, cap_2022, cap_2023), nrow)
)

# 建一个函数，计算每个国家的总体平均值
overall_cal <- function(year) {
  x <- get(paste0("cap_", year))[, !"year"]
  x[, .(country, region, iso, overall = rowMeans(.SD)), .SDcols = is.numeric][]
}

# 建一个函数，计算每个区域各个capacity的平均值
cap_mean_region_cal <- function(year) {
  x <- get(paste0("cap_", year))[, !c("country", "iso", "year")]
  x_mean <- x[, map(.SD, mean, na.rm = T), .SDcols = is.numeric]
  x_mean[, region := "Global"]
  x_group <- x[, map(.SD, mean, na.rm = T), .SDcols = is.numeric, by = region]
  x_total <- rbind(x_group, x_mean)
  x_total[, region := factor(region, 
                             c("AFRO", "AMRO", "EMRO", "EURO", "SEARO", "WPRO", "Global"))]
  x_total[, region := fct_rev(region)]
  x_long <- melt(x_total, "region",, "capacity", "value")
  x_long[, level := cut(value, seq(0, 100, 20), 1:5)][]
}

# 建一个函数，将每个国家每个能力均计算出等级
cap_level_cal <- function(year) {
  x <- get(paste0("cap_", year))[, !c("country", "year")]
  x <- melt(x, c("iso", "region"),, "capacity", "value")
  x[, level := cut(value, seq(0, 100, 20), 1:5)][]
}

# 构建implementation status
names_imp <- c("region", "country", "year", str_subset(names(raw_data), "_imp"))
dt_imp_wide <- raw_data[, ..names_imp]
dt_imp <- melt(dt_imp_wide, 1:3,, "indicator", "imp") %>% 
  separate_longer_delim("imp", "\r\n") %>% 
  as.data.table() %>% 
  na.omit()
dt_imp[, indicator := fct_rev(fct(str_replace(indicator, "_imp", "")))]
dt_imp[, imp := fct_other(imp, c("Ongoing", "Challenges/gaps", "Planned", 
                           "Achieved", "Strength/best practice", "Other"))]
dt_imp[, imp := fct_rev(imp)]

# 构建area
names_area <- c("region", "country", "year", str_subset(names(raw_data), "_area"))
dt_area_wide <- raw_data[, ..names_area]
dt_area <- melt(dt_area_wide, 1:3,, "indicator", "area") %>% 
  separate_longer_delim("area", "/") %>% 
  separate_longer_delim("area", "\r\n") %>% 
  as.data.table() %>% 
  na.omit()
dt_area[, indicator := (str_replace(indicator, "_area", ""))]

# 构建SPAR和JEE共有indicator的数据框
ind_2023_long <- melt(ind_2023, 1:4,, "spar_ind", "spar_score")
spar_jee_score <- jee_ind[
  na.omit(
    ind_2023_long[
      spar_jee[
        jee3_ind[
          , 
          .(region, country, iso, jee_ind = as.character(indicator), 
            jee_score = score)
          ], 
        on = c("jee" = "jee_ind")
        ], 
      on = c("iso", "spar_ind" = "spar")
      ]
    ), 
  on = c("indicator" = "jee")]
setnames(spar_jee_score, "indicator", "jee_ind")
spar_jee_score_long <- melt(spar_jee_score[, .(ta = jee_detail, country, 
                                               jee = jee_score, spar = spar_score)], 
                            c("ta", "country"),, "tool", "score")

# Shiny部分 -----------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Global Health Security Tracker: SPAR & JEE Insights"), 
  # 下面这行，2023之后需要更新
  sidebarLayout(
    sidebarPanel(
      radioButtons("year", "Please select a year:", choices = 2021:2023, selected = 2023), 
      radioButtons("region", "Please select a region:", 
                   choices = c("Total", regions), 
                   selected = "Total"), 
      selectInput("country", "Please select a country", 
                  choices = c("Total", countries), 
                  selected = "Total"), 
      width = 2, 
    ), 
    mainPanel(
      tabsetPanel(
        id = "tabset", 
        tabPanel(
          "Overall development", 
          tabsetPanel(
            id = "global_preparedness", 
            tabPanel(
              "Overall preparedness", 
              br(), 
              fluidRow(sliderInput("gis_border", "Please select the border:", 
                                   min = 0, max = 100, value = 60)), 
              fluidRow(column(12, echarts4rOutput("gis_preparedness"))),
              fluidRow("This map displays the overall development of countries 
                       worldwide. Blue indicates better developed, while green 
                       signifies the others. The assessment is based 
                       on the annual SPAR overall score, with scores of 
                       60 or above classified as blue and those below 60
                       as green."), 
              br(), 
              fluidRow(
                column(5, echarts4rOutput("preparedness_pie")), 
                column(7, echarts4rOutput("preparedness_bar"))
              ), 
              br(), 
              fluidRow("These two charts reflect the proportions of overall 
                        capacity development globally and across various 
                        WHO regions.")
            ), 
            tabPanel(
              "Capacity development", 
              fluidRow(
                column(7, selectInput(
                  "capacity_preparedness", 
                  "Please select a capacity:", 
                  choices = spar_cap$detail, 
                  selected = spar_cap$detail[1], 
                  width = "80%"
                )), 
                column(5, sliderInput(
                  "gis_border2", 
                  "Please select the border:", 
                  min = 0, max = 100, value = 60
                ))
              ), 
              fluidRow(column(12, echarts4rOutput("gis_capacity_preparedness"))),
              fluidRow("This world map displays the performance of countries 
                       in various SPAR capacities. Countries at level 3 and 
                       above are classified as blue, while those at 
                       levels 1 and 2 are in green."), 
              br(), 
              fluidRow(
                column(5, echarts4rOutput("capacity_preparedness_pie")), 
                column(7, echarts4rOutput("capacity_preparedness_bar"))
              ), 
              br(), 
              fluidRow("These two charts show the proportions of capacity 
                        development in various SPAR capacities globally 
                        and across each WHO region.")
            )
          )
        ), 
        tabPanel(
          "Region scores", 
          fluidRow(
            column(6, echarts4rOutput("overall_boxplot"), 
                   "This boxplot reflects the distribution of SPAR overall 
                   scores across various WHO regions. You can select different 
                   years."), 
            column(6, echarts4rOutput("overall_barplot"), 
                   "This bar chart shows the average SPAR overall scores 
                   across different WHO regions. You can choose different 
                   years.")
          ), 
          br(), 
          br(), 
          fluidRow(
            column(6, echarts4rOutput("overall_heatmap"), 
                   "This heatmap reflects the levels of various SPAR capacities 
                   globally and in each WHO region, with the color intensity 
                   indicating the level height. You can select different years 
                   for analysis."), 
            column(6, echarts4rOutput("overall_fill_barplot"), 
                   "This bar chart shows the number and proportion of countries 
                   at each level within different SPAR capacities. The depth of 
                   color represents the level's height. You can select different 
                   years and different WHO regions for analysis.")
          ), 
          br(), 
          br(), 
          fluidRow(
            column(5, echarts4rOutput("level_pie"), 
                   "This pie chart reflects the number and proportion of 
                   States Parties at each SPAR overall score level in 
                   the selected year."), 
            column(7, plotlyOutput("level_mosaic"), 
                   "This mosaic chart reflects the number and proportion of 
                   countries at each SPAR overall score level within different
                   WHO regions for the selected year. The area of each tile 
                   represents the number of States Parties, while the color
                   intensity indicates the level.")
          )
        ), 
        tabPanel(
          "Country scores", 
          tabsetPanel(
            id = "country_scores", 
            tabPanel(
              "SPAR scores", 
              br(),
              fluidRow(textOutput("country_text")), 
              br(), 
              fluidRow(echarts4rOutput("country_capacity")), 
              br(), 
              fluidRow(echarts4rOutput("country_indicator")), 
              br(), 
              fluidRow(textOutput("country_text2")), 
              br(), 
              fluidRow(
                column(6, textOutput("title_top_capacity"), 
                       tableOutput("country_capacity_table_top")), 
                column(6, textOutput("title_bottom_capacity"), 
                       tableOutput("country_capacity_table_bottom"))
              ), 
              br(), 
              fluidRow(
                column(6, textOutput("title_top_indicator"), 
                       tableOutput("country_indicator_table_top")), 
                column(6, textOutput("title_bottom_indicator"), 
                       tableOutput("country_indicator_table_bottom"))
              )
            ), 
            tabPanel(
              "JEE score", 
              br(), 
              fluidRow("Currently, this section only supports JEE edition 3. 
                       In future updates, I will gradually include edition 1 
                       and edition 2."), 
              br(), 
              fluidRow(
                selectInput(
                  "jee_country1", 
                  "Please select a country where JEE edition 3 has been conducted here:", 
                  choices = sort(unique(spar_jee_score_long$country)), 
                  selected = sort(unique(spar_jee_score_long$country))[1], 
                  width = "80%"
                )
              ), 
              br(), 
              fluidRow("The following two charts display the JEE technical area 
                       and indicator scores for the selected country in the 
                       chosen year. In each chart, the scores are arranged 
                       in ascending order."), 
              br(), 
              fluidRow(echarts4rOutput("ta_barplot", width = "90%")), 
              br(), 
              fluidRow(echarts4rOutput("ind_barplot", width = "90%")), 
              br(), 
              fluidRow("The following tables present the top technical areas, 
                       bottom technical areas, top indicators, and bottom 
                       indicators, allowing users to easily identify the 
                       strengths and weaknesses of the selected country."), 
              br(), 
              fluidRow(
                column(6, textOutput("title_top_jee_ta"), 
                       tableOutput("country_jee_ta_table_top")), 
                column(6, textOutput("title_bottom_jee_ta"), 
                       tableOutput("country_jee_ta_table_bottom"))
              ), 
              br(), 
              fluidRow(
                column(6, textOutput("title_top_jee_indicator"), 
                       tableOutput("country_jee_indicator_table_top")), 
                column(6, textOutput("title_bottom_jee_indicator"), 
                       tableOutput("country_jee_indicator_table_bottom"))
              )
            ), 
            tabPanel(
              "SPAR-JEE", 
              br(), 
              fluidRow("I matched SPAR and JEE (edition 3) indicators to identify 
                       the overlapping parts. Then I selected the countries which 
                       have conducted JEE edition 3 and compared the scores 
                       of the corresponding indicators."), 
              br(), 
              fluidRow(
                column(
                  7, 
                  selectInput(
                    "jee_country2", 
                    "Please select a country where JEE edition 3 has been conducted here:", 
                    choices = sort(unique(spar_jee_score_long$country)), 
                    selected = sort(unique(spar_jee_score_long$country))[1], 
                    width = "95%"
                  )
                ), 
                column(
                  5, 
                  selectInput("jee_indicator", "Please select an indicator:", 
                              choices = fct_unique(spar_jee_score_long$ta), 
                              width = "95%")
                )
              ), 
              fluidRow(
                column(9, echarts4rOutput("spar_jee_bar"))
              )
            )
          )
        ), 
        tabPanel(
          "Compare years", 
          br(), 
          fluidRow("SPAR edition 2 has been implemented since 2021, and we now 
                   have results from three years. Here, you can view the changes 
                   in SPAR overall scores over these three years, as well as 
                   select specific capacities and indicators to see how the scores 
                   have evolved. Additionally, you can choose the years which you 
                   want to compare."), 
          br(), 
          fluidRow(
            column(4, checkboxGroupInput("years", "Please select all years you want to compare:", 
                                         choices = 2021:2023, selected = 2021:2023, inline = T)), 
            column(4, selectInput("capacity", "Please choose a capacity", 
                                  choices = spar_cap$capacity, selected = "C1")), 
            column(4, selectInput("indicator", "Please choose a indicator", 
                                  choices = spar_ind$indicator, selected = "C1.1"))
          ), 
          fluidRow(
            column(4, echarts4rOutput("compare_years_overall")), 
            column(4, echarts4rOutput("compare_years_capacity")), 
            column(4, echarts4rOutput("compare_years_indicator"))
          ), 
          fluidRow(tableOutput("compare_years_overall_table")), 
          fluidRow(tableOutput("compare_years_capacity_table")), 
          fluidRow(tableOutput("compare_years_indicator_table"))
        ), 
        tabPanel(
          "Comments", 
          tabsetPanel(
            id = "comments", 
            tabPanel(
              "Interested words", 
              br(), 
              fluidRow(
                "Historically, when States Parties submit their SPAR results, 
                in addition to scores, they also provide comments for each 
                capacity and indicator. These comments are textual information in 
                various languages, and previously, no analysis had been conducted 
                on them."
              ), 
              br(), 
              fluidRow(
                "Here, I have developed a method to categorize all comments by 
                capacity, then break them down into individual words. Common and 
                insignificant words such as a, an, the, le, la, and de 
                are filtered out. The remaining words are the counted."
              ), 
              br(), 
              fluidRow(
                "In this section, you can input keywords of interest, and the chart 
                below will show you how many times your keywords appear in the 
                comments for each capacity. Generally, the frequency of occurence 
                is positively correlated with the importance of the term."
              ), 
              br(), 
              fluidRow(
                column(
                  10, 
                  textInput(
                    "words", 
                    "Please input the words which you want to analyze; 
            these words should be divided by ', ':", 
                    value = "training, capacity building", 
                    width = "90%"
                  )
                )
              ), 
              fluidRow(echarts4rOutput("spar_comment")), 
              fluidRow(tableOutput("spar_comment_table"))
            ), 
            tabPanel(
              "Total words", 
              br(), 
              fluidRow("This chart simply counts and ranks the words that appear 
                       in the comments for each capacity. You can select year, 
                       region, or country."), 
              br(), 
              fluidRow(column(9, selectInput(
                "capacity_preparedness_1", 
                "Please select a capacity:", 
                choices = c("Total", spar_cap$detail), 
                selected = "Total", 
                width = "80%"
              ))), 
              fluidRow(wordcloud2Output("wordcloud")), 
              fluidRow(echarts4rOutput("total_words_barplot"))
            ), 
            tabPanel(
              "JEE recommendations", 
              br(), 
              fluidRow("In this section, you can input keywords of interest, 
                       and the chart below will show you how many times your 
                       keywords appear in the comments for each TA. 
                       Generally, the frequency of occurence is positively 
                       correlated with the importance of the term."), 
              br(), 
              fluidRow(
                column(
                  10, 
                  textInput(
                    "jee_words", 
                    "Please input the words which you want to analyze; 
            these words should be divided by ', ':", 
                    value = "training, capacity building", 
                    width = "90%"
                  )
                )
              ), 
              fluidRow(echarts4rOutput("jee_rec_barplot")), 
              fluidRow(tableOutput("jee_rec_table"))
            )
          )
        ), 
        tabPanel(
          "Tables", 
          tabsetPanel(
            id = "data_table", 
            tabPanel(
              "Capacity table", 
              fluidRow(textOutput("sub_count_text1")), 
              fluidRow(DTOutput("cap_table"))
            ),
            tabPanel(
              "Indicator table", 
              fluidRow(textOutput("sub_count_text2")), 
              fluidRow(DTOutput("ind_table"))
            ), 
            tabPanel("Capacity name", tableOutput("capacity_name")), 
            tabPanel("Indicator name", tableOutput("indicator_name"))
          )
        ), 
        tabPanel(
          "Others", 
          tabsetPanel(
            id = "others", 
            tabPanel(
              "Submission", 
              br(), 
              fluidRow(
                column(6, echarts4rOutput("sub_status"), 
                       "This bar chart reflects the SPAR submission status 
                       of each WHO region in selected year. The blue bars indicate 
                       regions that submitted SPAR, while the green bars represent 
                       regions that did not submit SPAR."), 
                column(6, echarts4rOutput("sub_status_year"), 
                       "This bar chart shows the number of States Parties that 
                       submitted SPAR since 2021. The horizontal line in the chart 
                       represents the total number of States Parties: 196.")
              )
            ), 
            tabPanel(
              "Implementation status", 
              br(), 
              fluidRow("This chart shows the proportion of each implementation status 
                       for different indicators. You can select years, regions, and 
                       countries to explore the data."), 
              br(), 
              fluidRow(echarts4rOutput("implementation_bar", 
                                       width = "850px", height = "700px"))
            ), 
            tabPanel(
              "Area involved", 
              br(), 
              fluidRow("When reporting SPAR, for each capacity and indicator, the 
                       *area involved* should be specified. Here I did not differentiate 
                       between capacities but combined all *area involved* together 
                       to observe the relative importance of each area."), 
              br(), 
              fluidRow(echarts4rOutput("area_bar"))
            ), 
            tabPanel(
              "Compilation", 
              fluidRow(
                column(5, echarts4rOutput("compilation_pie"), 
                       "The pie chart shows the proportion of single-sector 
                       versus multi-sector approaches in conducting SPAR 
                       globally or within the selected WHO region for the 
                       chosen year."), 
                column(7, plotlyOutput("compilation_mosaic"), 
                       "The mosaic chart illustrates the proportion of 
                       single-sector versus multi-sector approaches in 
                       conducting SPAR across different WHO regions for 
                       the selected year.")
              ), 
              br(), 
              fluidRow(
                column(5, echarts4rOutput("consultative_pie"), 
                       "The pie chart shows the methods of consultation used 
                       during SPAR for the selected year, either globally or 
                       within the chosen WHO region."), 
                column(7, echarts4rOutput("sector_bar"), 
                       "The bar chart displays the number of sectors involved 
                       in the SPAR process for the selected year, region, 
                       or country.")
              )
            )
          )
        )
      ), 
      width = 10
    )
  )
)


server <- function(input, output, session) {

  # 反应表达式，capacity全体结果
  dt_cap <- reactive({
    get(paste0("cap_", input$year))
  })
  # 反应表达式，indicator全体结果
  dt_ind <- reactive({
    get(paste0("ind_", input$year))
  })
  
  # 根据region更新capacity
  observeEvent(
    input$region, {
      updateSelectInput(
        session, 
        "country", 
        choices = if (input$region == "Total") {
          c("Total", sort(dt_cap()$country))
        } else {
          c("Total", sort(dt_cap()[region == input$region]$country))
        }, 
        selected = "Total"
      )
    }
  )
  
  # 根据region更新jee_country1
  observeEvent(
    input$region, {
      updateSelectInput(
        session, 
        "jee_country1", 
        choices = if (input$region == "Total") {
          sort(unique(spar_jee_score_long$country))
        } else {
          sort(unique(cap_2023[, .(region, country)][
            spar_jee_score_long, on = "country"][
              region == input$region]$country))
        }
      )
    }
  )
  
  # 根据region更新jee_country2
  observeEvent(
    input$region, {
      updateSelectInput(
        session, 
        "jee_country2", 
        choices = if (input$region == "Total") {
          sort(unique(spar_jee_score_long$country))
        } else {
          sort(unique(cap_2023[, .(region, country)][
            spar_jee_score_long, on = "country"][
              region == input$region]$country))
        }
      )
    }
  )
  
  # 反应表达式，选择region
  region <- reactive({
    if (input$region == "Total") {
      regions
    } else {
      input$region
    }
  })
  
  # 反应表达式，选择country
  country <- reactive({
    if (input$country == "Total") {
      countries
    } else {
      input$country
    }
  })
  
  # 反应表达式，选择capacity
  capacity1 <- reactive({
    if (input$capacity_preparedness_1 == "Total") {
      spar_cap$capacity
    } else {
      spar_cap[detail == input$capacity_preparedness_1, capacity]
    }
  })
  
  # 反应表达式，capacity，选择region和country
  dt_cap_selected <- reactive(dt_cap()[region %in% region() & country %in% country(), !"year"])
  # 反应表达式，indicator，选择region和country
  dt_ind_selected <- reactive(dt_ind()[region %in% region() & country %in% country(), !"year"])
  # 建一个反应表达式，每年各区域上报情况
  dt_sub_count <- reactive(sub_count(input$year))
  # 建一个反应表达式，各国的总体分数
  dt_overall_cal <- reactive(overall_cal(input$year))
  # 建一个反应表达式，各国的总体分数加上级别
  dt_overall_level <- reactive({
    overall_level <- dt_overall_cal()
    overall_level[, level := cut(overall, seq(0, 100, 20), paste("level", 1:5))]
  })
  # 建一个反应表达式，各区域的平均分数
  dt_overall_cal_mean_region <- reactive(dt_overall_cal()[, .(mean = mean(overall)), by = region])
  # 建一个反应表达式，各区域及全球的各个能力的分数
  dt_cap_mean_region_cal <- reactive(spar_cap[cap_mean_region_cal(input$year)[capacity != "year"], on = "capacity"])
  # 建一个反应表达式，各个能力各个等级对应的国家数
  dt_cap_level_cal <- reactive(cap_level_cal(input$year)[capacity != "year"])
  # 反应表达式，各年的capacity
  dt_compare_years_capacity <- reactive({
    if (input$country != "Total") {
      cap_total[country == input$country & year %in% input$years][, year := as.integer(year)][]
    } else {
      cap_total[country == input$country & year %in% input$years & region == input$region][, year := as.integer(year)][]
    }
  })
  # 反应表达式，各年的indicator
  dt_compare_years_indicator <- reactive({
    if (input$country != "Total") {
      ind_total[country == input$country & year %in% input$years][, year := as.integer(year)][]
    } else {
      ind_total[country == input$country & year %in% input$years & region == input$region][, year := as.integer(year)][]
    }
  })
  # 反应表达式，implementation status
  dt_imp_selected <- reactive({
    if (input$country != "Total") {
      dt_imp[year == input$year & country == input$country]
    } else {
      if (input$region != "Total") {
        dt_imp[year == input$year & region == input$region]
      } else {
        dt_imp[year == input$year]
      }
    }
  })
  # 反应表达式，JEE TA
  dt_jee3_ta_selected <- reactive({
    jee_detail[jee3_ta, on = c("id" = "ta")][country == input$jee_country1, 
                                             .(ta = fct_reorder(id, score), score, detail)]
  })
  # 反应表达式，JEE indicator
  dt_jee3_ind_selected <- reactive({
    jee_detail[jee3_ind, on = c("id" = "indicator")][country == input$jee_country1, 
                                                     .(indicator = fct_reorder(id, score), score, detail)]
  })
  
  # 反应表达式，area
  dt_area_selected <- reactive({
    if (input$country != "Total") {
      dt_area[year == input$year & country == input$country]
    } else {
      if (input$region != "Total") {
        dt_area[year == input$year & region == input$region]
      } else {
        dt_area[year == input$year]
      }
    }
  })
  
  # 反应表达式，C1到C15在各年或各区域的相关系数
  dt_cap_cor <- reactive(cor(dt_cap()[region %in% region(), C1:C15], method = "spearman"))
  
  # 反应表达式，word_comment
  word_comment <- reactive(str_trim(str_split_1(input$words, ",")))
  
  # 反应表达式，jee_word_comment
  jee_word_comment <- reactive(str_trim(str_split_1(input$jee_words, ",")))
  
  # 反应表达式，各年各国各capacity的word
  dt_comment <- reactive({
    spar_comment_lower[
      year == input$year & region %in% region() & country %in% country(), 
      .(count = map_int(word_comment(), 
                        ~sum(str_count(comment, .x)))), by = cap_id
    ][
      , .(count = sum(count)), by = cap_id
    ][
      , .(capacity = fct_reorder(cap_id, count), count)
    ]
  })
  
  # 反应表达式，各年各国各capacity的word统计
  dt_comment_count <- reactive({
    spar_comment_word[!is.na(words) & !words %in% stop_words_all$word
                      ][year == input$year & region %in% region() & 
                   country %in% country() & cap_id %in% capacity1()][
                     , .N, by = words
                   ][order(-N)][1:15]
  })
  
  # 反应表达式，各年各国JEE各TA的word
  dt_jee_comment <- reactive({
    jee_rec_sentence[
      , 
      .(count = map_int(jee_word_comment(), 
                        ~sum(str_count(combination, .x)))), by = ta
    ][
      , .(count = sum(count)), by = ta
    ][
      , .(ta = fct_reorder(ta, count), count)
    ]
  })
  
  # 反应表达式，global preparedness
  cap_overall <- reactive({
    cap[, .(year, region, country, iso, 
            overall = pmap_dbl(.SD, ~mean(c(...), na.rm = T))), 
        .SDcols = C1:C15][, .(year, region, country, iso, 
                              yn = ifelse(overall >= input$gis_border, 1L, 0L))]
  })
  
  # 反应表达式，detail对应的capacity
  capacity_preparedness_id <- reactive({
    spar_cap[detail == input$capacity_preparedness, capacity]
  })

  # 反应表达式，country SPAR capacity对应的yes or no
  cap_pre_yn <- reactive({
    e_country_names(dt_cap(), iso, country1, type = "iso3c") %>% 
      mutate(country1 = ifelse(iso == "CIV", "Côte d'Ivoire", country1)) %>% 
      mutate(across(C1:C15, ~ifelse(.x >= input$gis_border2, 1L, 0L))) %>% 
      filter(year == input$year) %>% 
      select(all_of(c("country1", "region", capacity_preparedness_id()))) %>% 
      set_names("country1", "region", "capacity")
  })

  # 表格，capacity
  output$cap_table <- renderDT(dt_cap_selected())
  
  # 表格，indicator
  output$ind_table <- renderDT(dt_ind_selected())
  
  # 表格，capacity name
  output$capacity_name <- renderTable(spar_cap)
  
  # 表格，indicator name
  output$indicator_name <- renderTable(spar_ind)
  
  # 文本，报送国家数量
  output$sub_count_text1 <- renderText({
    paste0(nrow(dt_cap_selected()), " States Parties in selected conditions.")
  })
  output$sub_count_text2 <- renderText({
    paste0(nrow(dt_ind_selected()), " States Parties in selected conditions.")
  })
  
  # 每年各区域上报结果图
  output$sub_status <- renderEcharts4r({
    dt_sub_count() %>% 
      mutate(submission = factor(submission, c("Yes", "No"))) %>% 
      group_by(submission) %>% 
      e_chart(region, renderer = T) %>% 
      e_bar(count, stack = "total") %>% 
      e_tooltip() %>% 
      e_title("SPAR submission status of WHO regions") %>% 
      e_legend(bottom = "bottom")
  })
  
  # 各年度的上报情况图
  output$sub_status_year <- renderEcharts4r({
    dt_sub_pct %>% 
      e_chart(year, renderer = "svg") %>% 
      e_bar(count) %>% 
      e_mark_line(data = list(yAxis = 196), silent = T) %>% 
      e_y_axis(min = 0, max = 200, interval = 50) %>% 
      e_tooltip(formatter = e_tooltip_item_formatter()) %>% 
      e_legend(F) %>% 
      e_title("Number of submissions from 2021 to 2023", 
              "The vertical axis represents the number of submissions in each year")
  })
  
  # 各区域总体分数的箱线图
  output$overall_boxplot <- renderEcharts4r({
    dt_overall_cal()[order(region)] %>% 
      group_by(region) %>% 
      e_chart(renderer = "svg") %>% 
      e_boxplot(overall) %>% 
      e_legend(F) %>% 
      e_tooltip(
        formatter = JS("
        function(x) {
          return x.marker + x.name + '<br/>' + 
                 'min: ' + x.data[1].toFixed(1) + '<br/>' +
                 'Q1: ' + x.data[2].toFixed(1) + '<br/>' +
                 'median: ' + x.data[3].toFixed(1) + '<br/>' +
                 'Q3: ' + x.data[4].toFixed(1) + '<br/>' +
                 'max: ' + x.data[5].toFixed(1);
        }
      ")
      ) %>% 
      e_x_axis(axisLabel = list(interval = 0)) %>% 
      e_title("Over SPAR score of States Parties in each region", 
              "(affected by year)")
  })
  
  # 各区域总体分数平均值的柱状图
  output$overall_barplot <- renderEcharts4r({
    dt_overall_cal_mean_region()[order(region)] %>% 
      e_chart(region, renderer = "svg") %>% 
      e_bar(mean, legend = F) %>% 
      e_y_axis(min = 0, max = 100, interval = 20, 
               name = "Average of overall SPAR scores", 
               nameLocation = "center", nameGap = 30) %>% 
      e_tooltip() %>% 
      e_title("Average of overall SPAR scores of States Parties in each region", 
              "(affected by year)")
  })
  
  # 各区域及全球，各个能力的热图
  output$overall_heatmap <- renderEcharts4r({
    dt_cap_mean_region_cal()[order(-region)] %>% 
      mutate(level = as.numeric(as.character(level))) %>% 
      e_chart(capacity, renderer = "svg") %>% 
      e_heatmap(region, level, detail, itemStyle = list(borderColor = "darkblue")) %>% 
      e_visual_map(
        min = min(as.numeric(as.character(dt_cap_mean_region_cal()$level)), na.rm = T), 
        max = max(as.numeric(as.character(dt_cap_mean_region_cal()$level)), na.rm = T), 
        inRange = list(color = c("#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")), 
        show = F
      ) %>% 
      e_title("Heatmap of SPAR scores in each region", "(affected by year)") %>% 
      e_x_axis(axisLabel = list(interval = 0)) %>% 
      e_y_axis(inverse = T) %>% 
      e_tooltip(formatter = JS(
        "
        function(x) {
          return x.name + '<br/>' + 'level ' + x.value[2];
        }
        "
      ))
  })
  
  # 各个能力对应的等级
  output$overall_fill_barplot <- renderEcharts4r({
    spar_cap[dt_cap_level_cal()[region %in% region()][
      , .N, by = .(capacity, level)][
        , .(capacity, level = str_c("level ", level), N)][
          level %in% str_c("level ", 1:5)
        ], on = "capacity"] %>% 
      group_by(level) %>% 
      e_chart(capacity, renderer = "svg") %>% 
      e_bar(N, detail, legend = F, stack = "total", itemStyle = list(borderColor = "darkblue")) %>% 
      e_tooltip() %>% 
      e_y_axis(name = "Number of countries", 
               nameLocation = "center", 
               nameGap = 30) %>% 
      e_x_axis(axisLabel = list(interval = 0)) %>% 
      e_title("Number of countries in each capacity and level", 
              "(affected by year and region)") %>% 
      e_color(c("#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c"))
    })
  
  # 文字说明
  output$country_text <- renderText({
    if (input$country == "Total") {
      validate("Please select only one country")
    }
    "The following two charts display the SPAR capacity and indicator
     scores for the selected State Party in the chosen year. In each 
     chart, the scores are arranged in ascending order." 
  })
  
  # 各个国家对应的capacity
  output$country_capacity <- renderEcharts4r({
    if (input$country == "Total") {
      validate("")
    }
    dt_cap_selected_country <- dt_cap_selected()[country == input$country] %>% 
      melt(c("region", "country", "iso"),, "capacity", "value")
    dt_cap_selected_country[, capacity := fct_reorder(capacity, value)]
    dt_cap_selected_country[order(value)] %>% 
      e_chart(capacity, renderer = "svg") %>% 
      e_bar(value, legend = F) %>% 
      e_x_axis(axisLabel = list(interval = 0), name = "SPAR capacity", 
               nameLocation = "center", nameGap = 30) %>% 
      e_y_axis(min = 0, max = 100, interval = 20, name = "Capacity score", 
               nameLocation = "center", nameGap = 30) %>% 
      e_tooltip() %>% 
      e_title("SPAR capacity scores in the selected State Party")
  })
  
  # 各个国家对应的indicator
  output$country_indicator <- renderEcharts4r({
    if (input$country == "Total") {
      validate(" ")
    }
    dt_ind_selected_country <- dt_ind_selected()[country == input$country] %>% 
      melt(c("region", "country", "iso"),, "indicator", "value")
    dt_ind_selected_country[, indicator := fct_reorder(indicator, value)]
    dt_ind_selected_country[order(value)] %>% 
      e_chart(indicator, renderer = "svg") %>% 
      e_bar(value, legend = F) %>% 
      e_x_axis(axisLabel = list(interval = 0, rotate = 90), name = "SPAR indicator", 
               nameLocation = "center", nameGap = 50) %>% 
      e_y_axis(min = 0, max = 100, interval = 20, name = "Indicator score", 
               nameLocation = "center", nameGap = 30) %>% 
      e_tooltip() %>% 
      e_title("SPAR indicator scores in the selected State Party")
  })
  
  # 文字说明2
  output$country_text2 <- renderText({
    if (input$country == "Total") {
      validate("Please select only one country")
    }
    "The following tables present the top capacities, bottom capacities, 
    top indicators, and bottom indicators, allowing users to easily identify 
    the strengths and weaknesses of the selected country." 
  })
  
  # Top 3和bottom 3的capacity和indicator
  output$title_top_capacity <- renderText({
    if (input$country == "Total") {
      validate("")
    }
    "Top capacities"
  })
  output$title_bottom_capacity <- renderText({
    if (input$country == "Total") {
      validate("")
    }
    "Bottom capacities"
  })
  output$title_top_indicator <- renderText({
    if (input$country == "Total") {
      validate("")
    }
    "Top indicators"
  })
  output$title_bottom_indicator <- renderText({
    if (input$country == "Total") {
      validate("")
    }
    "Bottom indicators"
  })
  output$country_capacity_table_top <- renderTable({
    if (input$country == "Total") {
      validate("")
    }
    dt_cap_selected_country <- dt_cap_selected()[country == input$country] %>% 
      melt(c("region", "country", "iso"),, "capacity", "value")
    spar_cap[dt_cap_selected_country, on = "capacity"][order(-value)][min_rank(desc(value)) <= 3, .(capacity, detail, score = round(value))]
  })
  output$country_capacity_table_bottom <- renderTable({
    if (input$country == "Total") {
      validate("")
    }
    dt_cap_selected_country <- dt_cap_selected()[country == input$country] %>% 
      melt(c("region", "country", "iso"),, "capacity", "value")
    spar_cap[dt_cap_selected_country, on = "capacity"][order(value)][min_rank(value) <= 3, .(capacity, detail, score = round(value))]
  })
  output$country_indicator_table_top <- renderTable({
    if (input$country == "Total") {
      validate("")
    }
    dt_ind_selected_country <- dt_ind_selected()[country == input$country] %>% 
      melt(c("region", "country", "iso"),, "indicator", "value")
    spar_ind[dt_ind_selected_country, on = "indicator"][order(-value)][min_rank(desc(value)) <= 3, .(indicator, detail, score = round(value))]
  })
  output$country_indicator_table_bottom <- renderTable({
    if (input$country == "Total") {
      validate("")
    }
    dt_ind_selected_country <- dt_ind_selected()[country == input$country] %>% 
      melt(c("region", "country", "iso"),, "indicator", "value")
    spar_ind[dt_ind_selected_country, on = "indicator"][order(value)][min_rank(value) <= 3, .(indicator, detail, score = round(value))]
  })
  
  # 比较各年的分值
  output$compare_years_overall <- renderEcharts4r({
    dt_compare_years_capacity()[, .(year = factor(year), region, country, iso, 
                                    score = rowMeans(dt_compare_years_capacity()[, C1:C15]))] %>% 
      e_chart(year, renderer = "svg") %>% 
      e_bar(score, legend = F) %>% 
      e_y_axis(min = 0, max = 100, interval = 20) %>% 
      e_x_axis(axisLabel = list(interval = 0)) %>% 
      e_tooltip() %>% 
      e_title("Overall")
  })
  output$compare_years_capacity <- renderEcharts4r({
    dt_compare_years_capacity_long <- melt(dt_compare_years_capacity(), 1:4,, "capacity", "score")[capacity == input$capacity]
    dt_compare_years_capacity_long[, year := factor(year)]
    dt_compare_years_capacity_long %>% 
      e_chart(year, renderer = "svg") %>% 
      e_bar(score, legend = F) %>% 
      e_y_axis(min = 0, max = 100, interval = 20) %>% 
      e_x_axis(axisLabel = list(interval = 0)) %>% 
      e_tooltip() %>% 
      e_title("Capacity")
  })
  output$compare_years_indicator <- renderEcharts4r({
    dt_compare_years_indicator_long <- melt(dt_compare_years_indicator(), 1:4,, "indicator", "score")[indicator == input$indicator]
    dt_compare_years_indicator_long[, year := factor(year)]
    dt_compare_years_indicator_long %>% 
      e_chart(year, renderer = "svg") %>% 
      e_bar(score, legend = F) %>% 
      e_y_axis(min = 0, max = 100, interval = 20) %>% 
      e_x_axis(axisLabel = list(interval = 0)) %>% 
      e_tooltip() %>% 
      e_title("Indicator")
  })
  output$compare_years_overall_table <- renderTable(dt_compare_years_capacity()[, .(year, region, country, iso, overall = rowMeans(dt_compare_years_capacity()[, C1:C15]))])
  output$compare_years_capacity_table <- renderTable(dt_compare_years_capacity())
  output$compare_years_indicator_table <- renderTable(dt_compare_years_indicator())
  
  # 不同score level的饼图
  output$level_pie <- renderEcharts4r({
    level_pie_table <- if (input$region == "Total") {
      dt_overall_level()[, .N, by = level]
    } else {
      dt_overall_level()[region == input$region][, .N, by = level]
    }
    level_pie_table[, level := fct_relevel(level, paste("level", 1:5))]
    level_pie_table[order(level)] %>% 
      e_chart(level, renderer = "svg") %>% 
      e_pie(N) %>% 
      e_title("Pie chart of SPAR average score levels", 
              "(affected by regions)") %>% 
      e_tooltip() %>% 
      e_legend(bottom = "bottom") %>% 
      e_color(c("#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c"))
  })
  
  # 不同score level的马赛克图
  output$level_mosaic <- renderPlotly({
    ggplot(dt_overall_level()) +
      geom_mosaic(aes(x = product(region), fill = level)) +
      scale_fill_brewer(palette = "Blues") +
      labs(x = "", y = "", title = "Mosaic plot of SPAR levels") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  # 不同compilation of report的饼图
  output$compilation_pie <- renderEcharts4r({
    compilation_pie_table <- if (input$region == "Total") {
      raw_data[year == input$year][, .N, by = approach]
    } else {
      raw_data[year == input$year][region == input$region][, .N, by = approach]
    }
    compilation_pie_table %>% 
      e_chart(approach, renderer = "svg") %>% 
      e_pie(N, legend = F) %>% 
      e_tooltip() %>% 
      e_title("Pie chart of compilation of report")
  })
  
  # 不同compilation of report的马赛克图
  output$compilation_mosaic <- renderPlotly({
    ggplot(raw_data[year == input$year]) +
      geom_mosaic(aes(x = product(region), fill = approach)) +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_y_productlist(labels = c("Individual", "Multisectoral")) +
      scale_fill_manual(values = c("orange", "steelblue")) +
      labs(x = "", y = "", title = "Mosaic plot of compilation of report")
  })
  
  # 不同consultative process的饼图
  output$consultative_pie <- renderEcharts4r({
    consultative_region <- raw_data[, .(year, region, country, consultative)] %>% 
      separate_rows(consultative, sep = ", ") %>% 
      as.data.table() %>% 
      count(year, region, consultative)
    consultative_total <- consultative_region[, .(n = sum(n)), by = .(year, consultative)]
    consultative_total[, region := factor("Total")]
    consultative <- na.omit(rbind(consultative_region, consultative_total))
    consultative[year == input$year & region == input$region] %>% 
      e_chart(consultative, renderer = "svg") %>% 
      e_pie(n, legend = F) %>%
      e_tooltip() %>% 
      e_title("Pie chart of the preferred mechanisms in SPAR consultations")
  })
  
  # 不同sectors involved的柱形图
  output$sector_bar <- renderEcharts4r({
    dt_sector_region <- raw_data[, .(year, region, country, sector)] %>% 
      separate_longer_delim(sector, ", ") %>% 
      count(year, region, sector) %>% 
      as.data.table()
    dt_sector_total <- dt_sector_region[, .(n = sum(n)), by = .(year, sector)]
    dt_sector_total[, region := "Total"]
    dt_sector <- na.omit(rbind(dt_sector_region, dt_sector_total))
    dt_sector[, sector := str_replace(sector, "points of entry", "PoE")]
    dt_sector[year == input$year & region == input$region][order(n)] %>% 
      e_chart(sector, renderer = "svg") %>% 
      e_bar(n, legend = F) %>% 
      e_y_axis(
        name = "Number of States Parties", 
        nameLocation = "center", nameGap = 30
      ) %>% 
      e_x_axis(
        axisLabel = list(interval = 0, overflow = "break")
      ) %>% 
      e_tooltip() %>% 
      e_title("Sectors involved in compiling the SPAR") %>% 
      e_flip_coords() %>% 
      e_grid(left = "30%")
    })
  
  # 不同implementation status的柱形图
  output$implementation_bar <- renderEcharts4r({
    dt_imp_selected()[, .N, by = .(indicator, imp)][
      , .(indicator, imp = fct_rev(imp), N)][
        , .(imp, N, prop = N / sum(N)), by = indicator] %>% 
      group_by(imp) %>% 
      e_chart(indicator, renderer = "svg") %>%
      e_bar(prop, stack = "total", itemStyle = list(borderColor = "darkblue")) %>% 
      e_title("Implementation status of SPAR") %>% 
      e_x_axis(inverse = T) %>% 
      e_y_axis(min = 0, max = 1, 
               formatter = e_axis_formatter(style = "percent")) %>% 
      e_flip_coords() %>% 
      e_legend(bottom = "bottom", type = "scroll") %>% 
      e_tooltip(formatter = e_tooltip_item_formatter(style = "percent", digits = 1))
  })
  
  # 不同area的柱形图
  output$area_bar <- renderEcharts4r({
    dt_area_selected_ordered <- dt_area_selected()
    dt_area_selected_ordered[, area := fct_rev(fct_infreq(area))]
    dt_area_selected_ordered[, .N, by = area][order(N)] %>% 
      e_chart(area, renderer = "svg") %>% 
      e_bar(N, legend = F) %>% 
      e_y_axis(name = "Frequencies of areas", 
               nameLocation = "center", 
               nameGap = 30
               ) %>% 
      e_x_axis(axisLabel = list(
        interval = 0, width = 200, overflow = "break"
      )) %>% 
      e_tooltip() %>% 
      e_title("Frequencies of areas in selected States Parties") %>% 
      e_flip_coords() %>% 
      e_grid(left = "20%")
  })
  
  # 不同国家的JEE TA柱形图
  output$ta_barplot <- renderEcharts4r({
    dt_jee3_ta_selected() %>% 
      arrange(score) %>% 
      e_chart(ta, renderer = "svg") %>% 
      e_bar(score, legend = F) %>% 
      e_y_axis(min = 0, max = 100, interval = 20, 
               name = "TA score", nameLocation = "center", nameGap = 30) %>% 
      e_x_axis(name = "JEE TA", nameLocation = "center", nameGap = 30, 
               axisLabel = list(interval = 0)) %>% 
      e_tooltip() %>% 
      e_title("JEE TA scores in the selected State Party")
  })
  
  # 不同国家的JEE indicator柱形图
  output$ind_barplot <- renderEcharts4r({
    dt_jee3_ind_selected() %>% 
      arrange(score) %>% 
      e_chart(indicator, renderer = "svg") %>% 
      e_bar(score, legend = F) %>% 
      e_y_axis(min = 0, max = 100, interval = 20, 
               name = "Indicator score", nameLocation = "center", nameGap = 30) %>% 
      e_x_axis(name = "JEE indicator", nameLocation = "center", nameGap = 45, 
               axisLabel = list(rotate = 90, interval = 0)) %>% 
      e_tooltip() %>% 
      e_title("JEE indicator scores in the selected State Party")
    })
  
  output$title_top_jee_ta <- renderText("Top JEE technical areas")
  output$title_bottom_jee_ta <- renderText("Bottom JEE technical areas")
  output$title_top_jee_indicator <- renderText("Top JEE indicators")
  output$title_bottom_jee_indicator <- renderText("Bottom JEE indicators")
  
  output$country_jee_ta_table_top <- renderTable({
    dt_jee3_ta_selected()[order(-score), 
                          .(ta, detail, score = round(score))][min_rank(-score) <= 3]
  })
  output$country_jee_ta_table_bottom <- renderTable({
    dt_jee3_ta_selected()[order(score), 
                          .(ta, detail, score = round(score))][min_rank(score) <= 3]
  })
  output$country_jee_indicator_table_top <- renderTable({
    dt_jee3_ind_selected()[order(-score), 
                          .(indicator, detail, score = round(score))][min_rank(-score) <= 3]
  })
  output$country_jee_indicator_table_bottom <- renderTable({
    dt_jee3_ind_selected()[order(score), 
                           .(indicator, detail, score = round(score))][min_rank(score) <= 3]
  })
  
  # 各国的SPAR-JEE对应指标
  output$spar_jee_bar <- renderEcharts4r({
    spar_jee_score_long[country == input$jee_country2 & 
                          ta == input$jee_indicator, 
                        .(ta, country, tool = str_to_upper(tool), score)] %>% 
      e_chart(tool, renderer = "svg") %>% 
      e_bar(score, legend = F) %>% 
      e_y_axis(min = 0, max = 100, interval = 20, 
               name = "Scores of JEE and SPAR", 
               nameLocation = "center", nameGap = 30) %>% 
      e_tooltip() %>% 
      e_title("Scores of JEE and SPAR on selected area", 
              textStyle = list(color = "black"))
  })
  
  # SPAR comments graph
  output$spar_comment <- renderEcharts4r({
    dt_comment() %>% 
      arrange(count) %>% 
      e_chart(capacity, renderer = "svg") %>% 
      e_bar(count, legend = F) %>% 
      e_y_axis(name = "Counts of select words", 
               nameLocation = "center", nameGap = 30) %>% 
      e_tooltip() %>% 
      e_title("Counts of select words in selected year, region, and country", 
              textStyle = list(color = "black"))
   })
  
  # SPAR comments table
  output$spar_comment_table <- renderTable({
    spar_cap[dt_comment(), on = "capacity"][order(-count)]
  })
  
  # SPAR comments wordcloud
  output$wordcloud <- renderWordcloud2({
    wordcloud2(dt_comment_count())
  })
  
  # SPAR comments words count barplot
  output$total_words_barplot <- renderEcharts4r({
    dt_comment_count() %>% 
      arrange(N) %>% 
      e_chart(words, renderer = "svg") %>% 
      e_bar(N, legend = F) %>% 
      e_tooltip() %>% 
      e_flip_coords() %>% 
      e_grid(left = "20%")
  })

  
  # JEE recommendations graph
  output$jee_rec_barplot <- renderEcharts4r({
    dt_jee_comment() %>% 
      arrange(count) %>% 
      e_chart(ta, renderer = "svg") %>% 
      e_bar(count, legend = F) %>% 
      e_y_axis(name = "Counts of select words", 
               nameLocation = "center", nameGap = 30) %>% 
      e_tooltip() %>% 
      e_title("Counts of select words in JEE recommendations", 
              textStyle = list(color = "black"))
  })
  
  # JEE recommendations table
  output$jee_rec_table <- renderTable({
    jee_ta[dt_jee_comment(), on = "ta"][order(-count)]
  })
  
  # global preparedness gis
  output$gis_preparedness <- renderEcharts4r({
    cap_yn <- e_country_names(cap_overall(), iso, country1, type = "iso3c") %>% 
      mutate(country1 = ifelse(iso == "CIV", "Côte d'Ivoire", country1))
    cap_yn[year == input$year] %>% 
      e_chart(country1, renderer = "svg") %>% 
      e_map(yn, map = "world") %>% 
      e_visual_map(yn, inRange = list(color = c("#91cc75", "#5470c6")), 
                   show = F) %>% 
      e_title("GIS map of global preparedness", 
              left = "center", top = 0)
  })
  
  # capacity preparedness gis
  output$gis_capacity_preparedness <- renderEcharts4r({
    cap_pre_yn() %>% 
      e_chart(country1, renderer = "svg") %>% 
      e_map(capacity, map = "world") %>% 
      e_visual_map(capacity, inRange = list(color = c("#91cc75", "#5470c6")), 
                   show = F) %>% 
      e_title("GIS map of selected capacity preparedness", 
              left = "center", top = 0)
  })
  
  # global preparedness pie
  output$preparedness_pie <- renderEcharts4r({
    cap_overall()[year == input$year & region %in% region()][
      , .N, by = yn][
        , .(yn = ifelse(yn == 1, "Prepared", "Unprepared"), N)] %>% 
      e_chart(yn, renderer = "svg") %>% 
      e_pie(N) %>% 
      e_tooltip() %>% 
      e_title("Pie chart of global preparedness globally") %>% 
      e_legend(bottom = "bottom")
  })
  
  # capacity preparedness pie
  output$capacity_preparedness_pie <- renderEcharts4r({
    cap_pre_yn()[region %in% region()][, .N, by = capacity][
      , .(capacity = ifelse(capacity == 1, "Prepared", "Unprepared"), N)] %>% 
      e_chart(capacity, renderer = "svg") %>% 
      e_pie(N) %>% 
      e_tooltip() %>% 
      e_title("Pie chart of selected capacity preparedness globally") %>% 
      e_legend(bottom = "bottom")
  })
  
  # global preparedness bar
  output$preparedness_bar <- renderEcharts4r({
    cap_overall()[year == input$year][
      , .N, by = .(region, yn)][
        , .(region, yn = ifelse(yn == 1, "Prepared", "Unprepared"), N)][
          order(region)
        ] %>% 
      group_by(yn) %>% 
      e_chart(region, renderer = "svg") %>% 
      e_bar(N, stack = "total") %>% 
      e_tooltip() %>% 
      e_title("Barplot of global preparedness in WHO regions") %>% 
      e_legend(bottom = "bottom")
  })
  
  # capacity preparedness bar
  output$capacity_preparedness_bar <- renderEcharts4r({
    cap_pre_yn()[, .N, by = .(region, capacity)][
      , .(region, capacity = ifelse(capacity == 1, "Prepared", "Unprepared"), N)][
        order(region)
      ] %>% 
      group_by(capacity) %>% 
      e_chart(region, renderer = "svg") %>% 
      e_bar(N, stack = "total") %>% 
      e_tooltip() %>% 
      e_title("Barplot of global preparedness in WHO regions") %>% 
      e_legend(bottom = "bottom")
  })
}

shinyApp(ui, server)