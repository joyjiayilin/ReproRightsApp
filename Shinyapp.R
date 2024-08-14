# ===============================================
# Title: EGAL
# Description: ReproRights
# Author: Joy Lin


# ===============================================
# Packages
# ===============================================
library(shiny)
library(tidyverse)
library(readxl)
library(ggrepel)
library(RColorBrewer)
library(reactable)
library(htmltools)
library(reactablefmtr)
library(DT)
library(lubridate)
library(grid)
library(zoo)
library(scales)

# ===============================================
# Import data
# ===============================================



dat <- read_excel("Tracker.xlsx")
dat <- select(dat, 1:23)
dat$stance <- as.numeric(dat$stance)
dat$comm_other <- as.numeric(dat$comm_other)
dat[4:23][is.na(dat[4:23])] <- 0

# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Reproductive Rights Analysis"),
  
  fluidRow(
    column(3,
           checkboxGroupInput(inputId = "industry", 
                       label = "Select industry", 
                       choices = list("Aerospace & Defense" = "Aerospace & Defense",
                                      "Automotive" = "Automotive",
                                      "Communication" = "Communication",
                                      "Consumer Discretionary" = "Consumer Discretionary",
                                      "Energy" = "Energy",
                                      "Financial" = "Financial",
                                      "Food, Beverage & Tobacco" = "Food, Beverage & Tobacco",
                                      "Health Care" = "Health Care",
                                      "Industrials" = "Industrials",
                                      "Materials" = "Materials",
                                      "Real Estate" = "Real Estate",
                                      "Retail" = "Retail",
                                      "Technology" = "Technology",
                                      "Transportation" = "Transportation",
                                      "Utilities" = "Utilities"), 
                       selected = c("Financial", "Technology"))
    ),
    column(3,
           checkboxGroupInput(inputId = "action_category", 
                       label = "Select action category", 
                       choices = list("Corporate / headquarters employees" = "Corporate / headquarters employees",
                                      "Contractors / gig workers, factory workers, and/or store employees" = "Contractors / gig workers, factory workers, and/or store employees",
                                      "User" = "User",
                                      "Community" = "Community"), 
                       selected = c("Corporate / headquarters employees",
                                    "Contractors / gig workers, factory workers, and/or store employees",
                                    "User",
                                    "Community"))
    )
  ),
  hr(),
  htmlOutput("text1"),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Company Lists", 
                       h3("Breakdown of Fortune 250 by industry"),
                       plotOutput("plot0"),
                       hr(),
                       htmlOutput("text2"),
                       hr(),
                       htmlOutput("text3"),
                       hr(),
                       # htmlOutput("text4"),
                       # hr(),
                       htmlOutput("text5a"),
                       htmlOutput("text5b"),
                       htmlOutput("text5c"),
                       # hr(),
                       # htmlOutput("text6"),
                       hr(),
                       htmlOutput("text7"),
                       plotOutput("plot7a"),
                       hr(),
                       htmlOutput("text8"),
                       hr(),
                       htmlOutput("text9"),
                       hr(),
                       htmlOutput("text10"),
                       hr(),
                       htmlOutput("text11"),
                       hr(),
                       htmlOutput("text12"),
                       htmlOutput("text13")),
              tabPanel("Table: Percentage of actions",
                       h3("What do companies do?"),
                       reactableOutput('table1')),
              tabPanel("Graph: Percentage of actions", 
                       h3("What do companies do?"),
                       plotOutput("plot1")),
              tabPanel("Most & least popular actions", 
                       h3("What do companies do?"),
                       plotOutput("plot2")),
              tabPanel("Percentage of companies", 
                       plotOutput("plot3a",
                                  height = "700px"),
                       h3("Pro-ReproRights actions"),
                       plotOutput("plot3",
                                  height = "700px"),
                       h3("Anti-ReproRights actions"),
                       plotOutput("plot4")),
              tabPanel("ReproRights Scoring", 
                       h3("The more positive, the more pro-ReproRights"),
                       plotOutput("plot5"),
                       plotOutput("plot6"))
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  output$text1 <- renderText({
    
    dat_industry <- dat %>%
      filter(industry %in% input$industry)
    
    colsum <- data.frame(matrix(ncol = 0, nrow = nrow(dat_industry)))
    
    if ("Corporate / headquarters employees" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(corp_abortion,
               corp_lodging,
               corp_travel,
               corp_dep_travel,
               #corp_attitude,
               corp_other)
      
      colsum <- cbind(colsum, colsum_i)
    } 
    
    if ("Contractors / gig workers, factory workers, and/or store employees" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(gig_abortion,
               gig_lodging,
               gig_travel,
               gig_dep_travel,
               #gig_attitude,
               gig_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    if ("User" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(delete_data,
               refer_else,
               no_comply,
               user_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    if ("Community" %in% input$action_category) {

      colsum_i <- dat_industry %>%
        select(lobby,
               donate_pro,
               restrict_activity,
               donate_anti,
               comm_other)

      colsum <- cbind(colsum, colsum_i)

    }

    action <- colnames(colsum)
    
    colsum <- colsum %>%
      summarise_at(vars(action),
                   mean)    
    
    action_neg <- colnames(colsum[colSums(colsum < 0) > 0])
    
    if (length(action_neg) > 0) {
      paste(c("<span style='background-color: pink'><b>Anti-Reprorights actions:</b></span>",
              "<br>",
              paste(action_neg, collapse=", ")),
      collapse= " ")
    } 
    
  })
  
  
  output$plot0 <- renderPlot({
    
    dat_industry <- dat %>%
      filter(industry %in% input$industry) %>%
      count(industry) %>%
      summarise(industry,
                perc = n / sum(n) * 100) %>%
      arrange(desc(perc))
    
    dat_industry <- dat_industry %>% 
      mutate(Year = "2006")
    
    dat_industry$industry <- factor(dat_industry$industry, 
                                    levels = dat_industry$industry)
    
    colourCount = length(unique(dat_industry$industry))
    getPalette = colorRampPalette(brewer.pal(12, "Set3"))
    
    labels <- paste0(dat_industry$industry, ": ", round(dat_industry$perc,2), "%")
    
    ggplot(dat_industry, aes(x = Year, y = perc, fill = industry)) +
      geom_col(width = 0.3) +
      labs(title = "Breakdown of Fortune 250 by Industry",
           fill = "Industry") +
      scale_fill_manual(values = getPalette(colourCount),
                        labels=labels) +
      theme_void() +
      theme(plot.title = element_text(face = "bold"),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank())
    
  })
  
  output$text2 <- renderText({
    paste(
      c("<span style='background-color: #FFFF00'><b>Companies include:</b></span>",
        "<br>",
        paste(dat$sub_company[dat$industry %in% input$industry], collapse=", ")),
      collapse= " ")
  })
  
  output$text3 <- renderText({
    
    dat_industry <- filter(dat,
                          industry %in% input$industry)
    dat_no_stance <- filter(dat_industry, stance == -1)$sub_company
    
    if (length(dat_no_stance) > 0) {
      paste(
        c("<span style='background-color: pink'>",
          "<b>",
          round(length(dat_no_stance) / nrow(dat_industry) * 100, 2),
        "% companies do not take a public stance:",
        "</b>",
        "</span>",
          "<br>",
          paste(dat_no_stance, collapse=", ")),
        collapse= " ")
    }
    
  })
  
  
  # output$text4 <- renderText({
  #   
  #   dat_industry <- filter(dat,
  #                          industry %in% input$industry)
  #   
  #   company <- c()
  #   
  #   if ("Corporate / headquarters employees" %in% input$action_category) {
  #     
  #     company_i <- filter(dat_industry,
  #                        corp_abortion == 0 & corp_lodging == 0 & corp_travel == 0 & corp_dep_travel == 0 & corp_other == 0)$sub_company
  #     
  #     if (length(company_i) == 0) {
  #       stop("No company has taken any actions.")
  #     }
  #     
  #     company <- c(company, company_i)
  #   } 
  #   
  #   if ("Contractors / gig workers, factory workers, and/or store employees" %in% input$action_category) {
  #     
  #     company_i <- filter(dat_industry,
  #                        gig_abortion == 0 & gig_lodging == 0 & gig_travel == 0 & gig_dep_travel == 0 & gig_other == 0)$sub_company
  #     
  #     if (length(company_i) == 0) {
  #       stop("No company has taken any actions.")
  #     }
  #     
  #     
  #     company <- c(company, company_i)
  #     
  #   }
  #   
  #   if ("User" %in% input$action_category) {
  #     
  #     company_i <- filter(dat_industry,
  #              delete_data == 0 & refer_else == 0 & no_comply == 0 & user_other == 0)$sub_company
  #    
  #     
  #     if (length(company_i) == 0) {
  #       stop("No company has taken any actions.")
  #     }
  #     
  #     company <- c(company, company_i)
  #     
  #   }
  #   
  #   if ("Community" %in% input$action_category) {
  #     
  #     company_i <- filter(dat_industry,
  #              lobby == 0 & donate_pro == 0 & restrict_activity == 0 & donate_anti == 0 & comm_other == 0)$sub_company
  #     
  #     if (length(company_i) == 0) {
  #       stop("No company has taken any actions.")
  #     }
  #     
  #     company <- c(company, company_i)
  #     
  #   }
  #   
  #   no_action <- unique(company)
  #   
  #   if (length(no_action) > 0) {
  #     paste(
  #       c("<span style='background-color: pink'>",
  #         "<b>",
  #         round(length(no_action) / nrow(dat_industry) * 100, 2),
  #       "% companies do not take any actions:",
  #       "</b>",
  #       "</span>",
  #         "<br>",
  #         paste(no_action, collapse=", ")),
  #       collapse= " ")
  #   }
  #   
  # })
  # 
  
  output$text5a <- renderText({
    
    dat_industry <- filter(dat,
                           industry %in% input$industry)
    
    colsum <- data.frame(matrix(ncol = 0, nrow = nrow(dat_industry)))
    
    if ("Corporate / headquarters employees" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(sub_company,
               stance,
               corp_abortion,
               corp_lodging,
               corp_travel,
               corp_dep_travel,
               #corp_attitude,
               corp_other)
      
      colsum <- cbind(colsum, colsum_i)
    } 
    
    if ("Contractors / gig workers, factory workers, and/or store employees" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(sub_company,
               gig_abortion,
               gig_lodging,
               gig_travel,
               gig_dep_travel,
               #gig_attitude,
               gig_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    if ("User" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(sub_company,
               delete_data,
               refer_else,
               no_comply,
               user_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    if ("Community" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(sub_company,
               lobby,
               donate_pro,
               restrict_activity,
               donate_anti,
               comm_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    row_sub = apply(select(colsum, 
                           -sub_company), 
                    1, 
                    function(row) any(row != 0))
    dat_more_action <- drop_na(colsum[row_sub,])
    
    if (length(dat_more_action$sub_company) > 0) {
      paste(
        c("<span style='background-color: yellow'>",
          "<b>",
          round(length(dat_more_action$sub_company) / nrow(dat_industry) * 100, 2),
          "% companies take actions at all:",
          "</b>",
          "</span>",
          "<br>",
          paste(dat_more_action$sub_company, collapse=", ")),
        collapse = " ")
      
    }
    
    
  })
  
  output$text5b <- renderText({
    
    dat_industry <- filter(dat,
                           industry %in% input$industry)
    
    colsum <- data.frame(matrix(ncol = 0, nrow = nrow(dat_industry)))
    
    if ("Corporate / headquarters employees" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(sub_company,
               stance,
               corp_abortion,
               corp_lodging,
               corp_travel,
               corp_dep_travel,
               #corp_attitude,
               corp_other)
      
      colsum <- cbind(colsum, colsum_i)
    } 
    
    if ("Contractors / gig workers, factory workers, and/or store employees" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(sub_company,
               gig_abortion,
               gig_lodging,
               gig_travel,
               gig_dep_travel,
               #gig_attitude,
               gig_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    if ("User" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(sub_company,
               delete_data,
               refer_else,
               no_comply,
               user_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    if ("Community" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(sub_company,
               lobby,
               donate_pro,
               restrict_activity,
               donate_anti,
               comm_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    row_sub = apply(select(colsum, 
                           -sub_company), 
                    1, 
                    function(row) any(row > 0))
    dat_more_action <- drop_na(colsum[row_sub,])
    
    if (length(dat_more_action$sub_company) > 0) {
      paste(
        c("<span style='background-color: lightgreen'>",
          "<b>",
          round(length(dat_more_action$sub_company) / nrow(dat_industry) * 100, 2),
          "% companies take at least one positive action:",
          "</b>",
          "</span>",
          "<br>",
          paste(dat_more_action$sub_company, collapse=", ")),
        collapse = " ")
      
    }
    
    
  })
  
  output$text5c <- renderText({
    
    dat_industry <- filter(dat,
                           industry %in% input$industry)
    
    colsum <- data.frame(matrix(ncol = 0, nrow = nrow(dat_industry)))
    
    if ("Corporate / headquarters employees" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(sub_company,
               stance,
               corp_abortion,
               corp_lodging,
               corp_travel,
               corp_dep_travel,
               #corp_attitude,
               corp_other)
      
      colsum <- cbind(colsum, colsum_i)
    } 
    
    if ("Contractors / gig workers, factory workers, and/or store employees" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(sub_company,
               gig_abortion,
               gig_lodging,
               gig_travel,
               gig_dep_travel,
               #gig_attitude,
               gig_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    if ("User" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(sub_company,
               delete_data,
               refer_else,
               no_comply,
               user_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    if ("Community" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(sub_company,
               lobby,
               donate_pro,
               restrict_activity,
               donate_anti,
               comm_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }

    row_sub = apply(select(colsum, 
                           -sub_company), 
                    1, 
                    function(row) any(row < 0))
    dat_more_action <- drop_na(colsum[row_sub,])
    
    if (length(dat_more_action$sub_company) > 0) {
      paste(
        c("<span style='background-color: pink'>",
          "<b>",
          round(length(dat_more_action$sub_company) / nrow(dat_industry) * 100, 2),
          "% companies take at least one negative action:",
          "</b>",
          "</span>",
          "<br>",
          paste(dat_more_action$sub_company, collapse=", ")),
        collapse = " ")
      
    }
    
    
  })
  
  
  
  # 
  # output$text6 <- renderText({
  #   
  #   dat_industry <- filter(dat_perc,
  #                          industry %in% input$industry)
  #   
  #   colsum <- data.frame(matrix(ncol = 0, nrow = nrow(dat_industry)))
  #   
  #   if ("Corporate / headquarters employees" %in% input$action_category) {
  #     
  #     colsum_i <- dat_industry %>%
  #       select(sub_company,
  #              corp_abortion,
  #              corp_lodging,
  #              corp_travel,
  #              corp_dep_travel,
  #              #corp_attitude,
  #              corp_other)
  #     
  #     colsum <- cbind(colsum, colsum_i)
  #   } 
  #   
  #   if ("Contractors / gig workers, factory workers, and/or store employees" %in% input$action_category) {
  #     
  #     colsum_i <- dat_industry %>%
  #       select(sub_company,
  #              gig_abortion,
  #              gig_lodging,
  #              gig_travel,
  #              gig_dep_travel,
  #              #gig_attitude,
  #              gig_other)
  #     
  #     colsum <- cbind(colsum, colsum_i)
  #     
  #   }
  #   
  #   if ("User" %in% input$action_category) {
  #     
  #     colsum_i <- dat_industry %>%
  #       select(sub_company,
  #              delete_data,
  #              refer_else,
  #              no_comply,
  #              user_other)
  #     
  #     colsum <- cbind(colsum, colsum_i)
  #     
  #   }
  #   
  #   if ("Community" %in% input$action_category) {
  #     
  #     colsum_i <- dat_industry %>%
  #       select(sub_company,
  #              lobby,
  #              donate_pro,
  #              restrict_activity,
  #              donate_anti,
  #              comm_other)
  #     
  #     colsum <- cbind(colsum, colsum_i)
  #     
  #   }
  #   
  #   row_sub = apply(select(colsum, 
  #                          -sub_company), 
  #                   1, 
  #                   function(row) any(row != 100))
  #   dat_more_action <- drop_na(colsum[row_sub,])
  #   
  #   if (length(dat_more_action$sub_company) > 0) {
  #     paste(
  #       c("<span style='background-color: lightgreen'>",
  #         "<b>",
  #         round(length(dat_more_action$sub_company) / nrow(dat_industry), 2),
  #       "% companies take more than one action:",
  #       "</b>",
  #       "</span>",
  #         "<br>",
  #     paste(dat_more_action$sub_company, collapse=", ")),
  #     collapse = " ")
  #   }
  #   
  # })
  # 
  
  
  output$text7 <- renderText({
  
    paste0(
      "<span style='background-color: lightgreen'><b>0.79% companies among all provide flexible location accommodations:</b></span>",
      "<br>",
      "Google, Salesforce.", 
      "<br>",
      "<span style='background-color: pink'>Explicitly disallowed by Apple.</span>")
  
   })
  
  output$plot7a <- renderPlot({
    
    company <- c("Salesforce helps employees relocate for abortion access",
                 "Google employees may apply to relocate ‘without justification’",
                 "Apple won’t let staff work remotely to escape Texas abortion limits")
    
    date <- c("2022-05-12",
              "2022-06-24",
              "2022-11-30")
    number <- c(1,1,-0.5)
    
    dat_pro <- data.frame(company,
                          date,
                          number)
    
    dat_pro$date <- ymd(dat_pro$date)
    
    g <- rasterGrob(c("lightgreen", "pink"), 
                    width=unit(1,"npc"), height = unit(1,"npc"), 
                    interpolate = TRUE) 
    
    ggplot(dat_pro, aes(x = date, 
                        y = number)) +
      annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
      annotate("text", 
               label = "Salesforce\nhelps\nemployees\nrelocate\nfor abortion\naccess",
               x = as.Date("2022-05-12"), y = 1, size = 4, colour = "darkgreen") +
      annotate("text", 
               label = "Google\nemployees\nmay apply\nto relocate\n‘without\njustification’",
               x = as.Date("2022-06-24"), y = 1, size = 4, colour = "darkgreen") +
      annotate("text", 
               label = "Apple won’t\nlet staff\nwork remotely\nto escape Texas\nabortion limits",
               x = as.Date("2022-11-30"), y = -0.5, size = 4, colour = "red") +
      xlab("Date")+
      ylab("Positive or Negative Actions") +
      ggtitle("Among Fortune 250, 0.79 % companies provided the option to relocate") +
      theme_bw() +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold.italic")) +
      ylim(-1.5, 1.8) +
      scale_x_date(limit=c(as.Date("2022-04-01"),
                           as.Date("2022-12-30")),
                   breaks = "1 month", labels=date_format("%b %y"))
    
    
  })
  
  
  
  output$text8 <- renderText({
    
    dat_industry <- filter(dat,
                           industry %in% input$industry)
    
    if ("Corporate / headquarters employees" %in% input$action_category) {
      
      dat_corp <- filter(dat_industry,
             corp_abortion != 0 | corp_lodging != 0 | corp_travel != 0 | corp_dep_travel != 0 | corp_other != 0)
      
      paste(c("<B>",
             "<span style='background-color: #FFFF00'>",
             round(nrow(dat_corp) / nrow(dat_industry) * 100, 2),
             "</span>",
             "</B>",
             "<span style='background-color: #FFFF00'><b>% companies take actions for headquarters employees.</b></span> Among which:",
             "<br>",
             "<span style='background-color: lightgreen'>",
             round(nrow(filter(dat_industry,
                               corp_abortion > 0)) / nrow(dat_corp) * 100, 2),
             "% companies pay for employees' procedure in case of abortion:",
             "</span>",
             "<br>",
             paste(filter(dat_industry,
                    corp_abortion > 0)$sub_company, collapse=", "),
             "<br>",
             "<span style='background-color: lightgreen'>",
             round(nrow(filter(dat_industry,
                               corp_lodging != 0)) / nrow(dat_corp) * 100, 2),
             "% companies provide lodging support for employees traveling for abortion:",
             "</span>",
             "<br>",
             paste(filter(dat_industry,
                    corp_lodging != 0)$sub_company, collapse=", "),
             "<br>",
             "<span style='background-color: lightgreen'>",
             round(nrow(filter(dat_industry,
                               corp_travel != 0)) / nrow(dat_corp) * 100, 2),
             "% companies provide travel support for employees:",
             "</span>",
             "<br>",
             paste(filter(dat_industry,
                    corp_travel != 0)$sub_company, collapse=", "),
             "<br>",
             "<span style='background-color: lightgreen'>",
             round(nrow(filter(dat_industry,
                               corp_dep_travel > 0)) / nrow(dat_corp) * 100, 2),
             "% companies provide travel support for dependents of employees:",
             "</span>",
             "<br>",
             paste(filter(dat_industry,
                    corp_travel > 0)$sub_company, collapse=", "),
             # "<br>",
             # "<span style='background-color: pink'>",
             # round(nrow(filter(dat_industry,
             #                   corp_attitude < 0)) / nrow(dat_corp) * 100, 2),
             # "% companies do not support employees' voice:",
             # "</span>",
             # "<br>",
             # paste(filter(dat_industry,
             #        corp_attitude < 0)$sub_company, collapse=", "),
             "<br>",
             "<span style='background-color: lightgreen'>",
             round(nrow(filter(dat_industry,
                               corp_other > 0)) / nrow(dat_corp) * 100, 2),
             "% companies take other pro-repro actions:",
             "</span>",
             "<br>",
             paste(filter(dat_industry,
                    corp_other > 0)$sub_company, collapse=", "),
             "<br>",
             "<span style='background-color: pink'>",
             round(nrow(filter(dat_industry,
                               corp_other < 0)) / nrow(dat_corp) * 100, 2),
             "% companies take other anri-repro actions:",
             "</span>",
             "<br>",
             paste(filter(dat_industry,
                    corp_other < 0)$sub_company, collapse=", ")
      ),
      collapse= " "
      )
        
    }
    
  })
  
  output$text9 <- renderText({
  
    dat_industry <- filter(dat,
                           industry %in% input$industry)
    
    if ("Contractors / gig workers, factory workers, and/or store employees" %in% input$action_category) {
      
      dat_gig <- filter(dat_industry,
                         gig_abortion != 0 | gig_lodging != 0 | gig_travel != 0 | gig_dep_travel != 0 | gig_other != 0)
      
      paste(c("<B>",
              "<span style='background-color: #FFFF00'>",
              round(nrow(dat_gig) / nrow(dat_industry) * 100, 2),
              "</span>",
              "</B>",
              "<span style='background-color: #FFFF00'><b>% companies take actions for gig workers.</b></span> Among which:",
              "<br>",
              "<span style='background-color: lightgreen'>",
              round(nrow(filter(dat_industry,
                                gig_abortion != 0)) / nrow(dat_gig) * 100, 2),
              "% companies pay for employees' procedure in case of abortion:",
              "</span>",
              "<br>",
              paste(filter(dat_industry,
                     gig_abortion != 0)$sub_company, collapse=", "),
              "<br>",
              "<span style='background-color: lightgreen'>",
              round(nrow(filter(dat_industry,
                                gig_lodging != 0)) / nrow(dat_gig) * 100, 2),
              "% companies provide lodging support for employees traveling for abortion:",
              "</span>",
              "<br>",
              paste(filter(dat_industry,
                     gig_lodging != 0)$sub_company, collapse=", "),
              "<br>",
              "<span style='background-color: lightgreen'>",
              round(nrow(filter(dat_industry,
                                gig_travel != 0)) / nrow(dat_gig) * 100, 2),
              "% companies provide travel support for employees:",
              "</span>",
              "<br>",
              paste(filter(dat_industry,
                     gig_travel != 0)$sub_company, collapse=", "),
              "<br>",
              "<span style='background-color: lightgreen'>",
              round(nrow(filter(dat_industry,
                                gig_dep_travel != 0)) / nrow(dat_gig) * 100, 2),
              "% companies provide travel support for dependents of employees:",
              "</span>",
              "<br>",
              paste(filter(dat_industry,
                           gig_dep_travel != 0)$sub_company, collapse=", ")#,
              # "<br>",
              # "<mark>",
              # round(nrow(filter(dat_industry,
              #                   gig_attitude != 0)) / nrow(dat_gig) * 100, 2),
              # "% companies support or do not support employees' voice:",
              # "</mark>",
              # "<br>",
              # paste(filter(dat_industry,
              #        gig_attitude != 0)$sub_company, collapse=", "),
              # "<br>",
              # "<span style='background-color: lightgreen'>",
              # round(nrow(filter(dat_industry,
              #                   gig_other != 0)) / nrow(dat_gig) * 100, 2),
              # "% companies take other pro-repro actions:",
              # "</span>",
              # "<br>",
              # paste(filter(dat_industry,
              #        gig_other != 0)$sub_company, collapse=", ")
      ),
      collapse= " "
      )
      
    }
    
  })
  
  
  output$text10 <- renderText({
    
    dat_industry <- filter(dat,
                           industry %in% input$industry)
    
    if ("User" %in% input$action_category) {
      
      dat_user <- filter(dat_industry,
                         delete_data != 0 | refer_else != 0 | no_comply != 0 | user_other != 0)
      
      paste(
        c("<B>",
          "<span style='background-color: #FFFF00'>",
          round(nrow(dat_user) / nrow(dat_industry) * 100, 2),
          "</span>",
          "</B>",
          "<span style='background-color: #FFFF00'><b>% companies take actions for users.</b></span> Among which:",
          "<br>",
          "<span style='background-color: lightgreen'>",
          round(nrow(filter(dat_industry,
                            delete_data > 0)) / nrow(dat_user) * 100, 2),
          "% companies delete search / healthcare data:",
          "</span>",
          "<br>",
          paste(filter(dat_industry,
                 delete_data > 0)$sub_company, collapse=", "),
          "<br>",
          "<span style='background-color: pink'>",
          round(nrow(filter(dat_industry,
                            delete_data < 0)) / nrow(dat_user) * 100, 2),
          "% companies do not delete search / healthcare data:",
          "</span>",
          "<br>",
          paste(filter(dat_industry,
                 delete_data < 0)$sub_company, collapse=", "),
          "<br>",
          "<span style='background-color: lightgreen'>",
          round(nrow(filter(dat_industry,
                            refer_else != 0)) / nrow(dat_user) * 100, 2),
          "% companies refer consumers elsewhere for support:",
          "</span>",
          "<br>",
          paste(filter(dat_industry,
                 refer_else != 0)$sub_company, collapse=", "),
          "<br>",
          "<span style='background-color: lightgreen'>",
          round(nrow(filter(dat_industry,
                            no_comply > 0)) / nrow(dat_user) * 100, 2),
          "% companies do not comply with requests for consumer data:",
          "</span>",
          "<br>",
          paste(filter(dat_industry,
                 no_comply > 0)$sub_company, collapse=", "),
          "<br>",
          "<span style='background-color: pink'>",
          round(nrow(filter(dat_industry,
                            no_comply < 0)) / nrow(dat_user) * 100, 2),
          "% companies comply with requests for consumer data:",
          "</span>",
          "<br>",
          paste(filter(dat_industry,
                 no_comply < 0)$sub_company, collapse=", "),
          "<br>",
          "<span style='background-color: lightgreen'>",
          round(nrow(filter(dat_industry,
                            user_other > 0)) / nrow(dat_user) * 100, 2),
          "% companies take other pro-repro actions:",
          "</span>",
          "<br>",
          paste(filter(dat_industry,
                 user_other > 0)$sub_company, collapse=", "),
          "<br>",
          "<span style='background-color: pink'>",
          round(nrow(filter(dat_industry,
                            user_other < 0)) / nrow(dat_user) * 100, 2),
          "% companies take other anti-repro actions:",
          "</span>",
          "<br>",
          paste(filter(dat_industry,
                 user_other < 0)$sub_company, collapse=", ")
      ),
      collapse = " "
      )
      
    }
    
  })
  
  output$text11 <- renderText({
    
    dat_industry <- filter(dat,
                           industry %in% input$industry)
    
    if ("Community" %in% input$action_category) {
      
    dat_comm <- filter(dat_industry,
                       lobby != 0 | donate_pro != 0 | restrict_activity != 0 | donate_anti != 0 | comm_other != 0)
        
        paste(
          c("<B>",
            "<span style='background-color: #FFFF00'>",
            round(nrow(dat_comm) / nrow(dat_industry) * 100, 2),
            "</span>",
            "</B>",
            "<span style='background-color: #FFFF00'><b>% companies take actions for community.</b></span> Among which:",
            "<br>",
            "<span style='background-color: lightgreen'>",
            round(nrow(filter(dat_industry,
                              lobby != 0)) / nrow(dat_comm) * 100, 2),
            "% companies lobby for reproductive rights and abortion access:",
            "</span>",
            "<br>",
            paste(filter(dat_industry,
                   lobby != 0)$sub_company, collapse=", "),
            "<br>",
            "<span style='background-color: lightgreen'>",
            round(nrow(filter(dat_industry,
                              donate_pro != 0)) / nrow(dat_comm) * 100, 2),
            "% companies donate to reproductive rights groups:",
            "</span>",
            "<br>",
            paste(filter(dat_industry,
                   donate_pro != 0)$sub_company, collapse=", "),
            "<br>",
            "<span style='background-color: lightgreen'>",
            round(nrow(filter(dat_industry,
                              restrict_activity > 0)) / nrow(dat_comm) * 100, 2),
            "% companies restrict business activity in states that don't allow reproductive rights:",
            "</span>",
            "<br>",
            paste(filter(dat_industry,
                   restrict_activity > 0)$sub_company, collapse=", "),
            "<br>",
            "<span style='background-color: pink'>",
            round(nrow(filter(dat_industry,
                              donate_anti < 0)) / nrow(dat_comm) * 100, 2),
            "% companies donate to anti-abortion groups:",
            "</span>",
            "<br>",
            paste(filter(dat_industry,
                   donate_anti < 0)$sub_company, collapse=", "),
            "<br>",
            # "<span style='background-color: lightgreen'>",
            # round(nrow(filter(dat_industry,
            #                   comm_other > 0)) / nrow(dat_comm) * 100, 2),
            # "% companies take other pro-repro actions:",
            # "</span>",
            # "<br>",
            # paste(filter(dat_industry,
            #        comm_other > 0)$sub_company, collapse=", "),
            # "<br>",
            "<span style='background-color: pink'>",
            round(nrow(filter(dat_industry,
                              comm_other < 0)) / nrow(dat_comm) * 100, 2),
            "% companies take other anti-repro actions:",
            "</span>",
            "<br>",
            paste(filter(dat_industry,
                   comm_other < 0)$sub_company, collapse=", ")
          ),
          collapse = " "
        )
    
      
    }
    
  })
  
  output$text12 <- renderText({
    
    dat_industry <- filter(dat,
                           industry %in% input$industry)
    
    if ("Community" %in% input$action_category) {
      
      tf <- (rowSums(sign(dat_industry[19:23])<0)>0) & (rowSums(sign(dat_industry[19:23])>0)>0)
      
      paste(c("<B>",
              "<span style='background-color: #FFFF00'>",
              round(sum(tf) / nrow(dat_industry) * 100,
                    2),
              "% companies have made both a positive and negative action generally for community:",
              "</span>",
              "</B>",
              "<br>",
              paste(dat_industry$sub_company[tf], 
                    collapse=", ")),
            collapse = " "
      )
      
    }
    
  })
  
  output$text13 <- renderText({
    
    dat_industry <- filter(dat,
                           industry %in% input$industry)
    
    if ("Community" %in% input$action_category) {
      
      paste(c("<B>",
             "<span style='background-color: #FFFF00'>",
             round(nrow(filter(dat_industry,
                               donate_pro == 1 & donate_anti == -1)) / nrow(dat_industry) * 100, 2),
             "% companies donated to both pro- and anti-abortion groups:",
             "</span>",
             "</B>",
             "<br>",
            paste(filter(dat_industry,
                                 donate_pro == 1 & donate_anti == -1)$sub_company, collapse=", ")),
            collapse = " "
             )
    }
    
  })
  
 
    output$table1 <- renderReactable({
      
      description <- c("Pay for corporate / headquarters employees' procedure in case of abortion",
                       "Lodging support for corporate / headquarters employees traveling for abortion",
                       "Travel support for corporate / headquarters employees",
                       "Travel support for dependents of corporate / headquarters employees",
                       #"Support worker voice among corporate / headquarters employees",
                       "Other actions for corporate / headquarters employees",
                       "Pay for contractors / gig workers, factory workers, and/or store employees' procedure in case of abortion",
                       "Lodging support for contractors / gig workers, factory workers, and/or store employees traveling for abortion",
                       "Travel support for contractors / gig workers, factory workers, and/or store employees",
                       "Travel support for dependents of contractors / gig workers, factory workers, and/or store employees",
                       #"Support worker voice among contractors / gig workers, factory workers, and/or store employees",
                       "Other actions for contractors / gig workers, factory workers, and/or store employees",
                       "Deleting search / healthcare data",
                       "Refer consumers elsewhere for support",
                       "Non compliance with requests for consumer data",
                       "Other actions",
                       "Lobbying for reproductive rights and abortion access",
                       "Donate to reproductive rights groups (e.g., Planned Parenthood)",
                       "Restrict business activity in states that don't allow reproductive rights",
                       "Donate to anti-abortion groups",
                       "Other actions"
      )
      
      dat_industry <- dat %>%
        filter(industry %in% input$industry)
     
      colsum <- data.frame(matrix(ncol = 0, nrow = nrow(dat_industry)))
      description_all <- c()
      
      if ("Corporate / headquarters employees" %in% input$action_category) {
        
        colsum_i <- dat_industry %>%
          select(corp_abortion,
                 corp_lodging,
                 corp_travel,
                 corp_dep_travel,
                 #corp_attitude,
                 corp_other)
        
        colsum <- cbind(colsum, colsum_i)
        
        description_i <- description[1:5]
        description_all <- c(description_all, description_i)
        
      } 
      
      if ("Contractors / gig workers, factory workers, and/or store employees" %in% input$action_category) {
        
        colsum_i <- dat_industry %>%
          select(gig_abortion,
                 gig_lodging,
                 gig_travel,
                 gig_dep_travel,
                 #gig_attitude,
                 gig_other)
        
        colsum <- cbind(colsum, colsum_i)
        
        description_i <- description[6:10]
        description_all <- c(description_all, description_i)
        
      } 
      
      if ("User" %in% input$action_category) {
        
        colsum_i <- dat_industry %>%
          select(delete_data,
                 refer_else,
                 no_comply,
                 user_other)
        
        colsum <- cbind(colsum, colsum_i)
        
        description_i <- description[11:14]
        description_all <- c(description_all, description_i)
        
      } 
      
      if ("Community" %in% input$action_category) {
        
        colsum_i <- dat_industry %>%
          select(lobby,
                 donate_pro,
                 restrict_activity,
                 donate_anti,
                 comm_other)
        
        colsum <- cbind(colsum, colsum_i)
        
        description_i <- description[15:19]
        description_all <- c(description_all, description_i)
        
      } 
      
      action <- colnames(colsum)
      
      colsum <- colsum %>%
        summarise_at(vars(action),
                     mean)          
      
      action_neg <- colnames(colsum[colSums(colsum < 0) > 0])
      colsum_pos <- colsum %>%
        select(-action_neg)
      
      action <- colnames(colsum_pos)
      colsum_pos_sum <- colsum_pos %>%
        rowSums()
      colsum_pos_perc <- colsum_pos / colsum_pos_sum * 100
      
      Percentage <- c()
      for (action_i in colnames(colsum)) { 
        if (pull(colsum, action_i) < 0) {
          perc <- pull(colsum, action_i)
        } else {
          if (is.na(pull(colsum_pos_perc, action_i))) {
            perc <- 0
          } else {
            perc <- pull(colsum_pos_perc, action_i)
          }
          
        }
        Percentage <- append(Percentage, round(perc, 0))
      }
      
      perc_tbl <- data.frame("Label" = colnames(colsum),
                             "Description" = description_all,
                             Percentage)
      
      perc_tbl %>%
        reactable(.,
                  theme = journal(),
                  defaultColDef =
                    colDef(
                      cell = data_bars(., fill_color = viridis::mako(5), text_position = "inside-end")
                    )
        )
      
    })
    
  
  output$plot1 <- renderPlot({
    
    dat_industry <- dat %>%
      filter(industry %in% input$industry)
    
    colsum <- data.frame(matrix(ncol = 0, nrow = nrow(dat_industry)))
    
    if ("Corporate / headquarters employees" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(corp_abortion,
               corp_lodging,
               corp_travel,
               corp_dep_travel,
               #corp_attitude,
               corp_other)
      
      colsum <- cbind(colsum, colsum_i)
    } 
    
    if ("Contractors / gig workers, factory workers, and/or store employees" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(gig_abortion,
               gig_lodging,
               gig_travel,
               gig_dep_travel,
               #gig_attitude,
               gig_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    if ("User" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(delete_data,
               refer_else,
               no_comply,
               user_other)
     
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    if ("Community" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(lobby,
               donate_pro,
               restrict_activity,
               donate_anti,
               comm_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    action <- colnames(colsum)
    
    colsum <- colsum %>%
      summarise_at(vars(action),
                   mean)    
    
    action_neg <- colnames(colsum[colSums(colsum < 0) > 0])
    colsum_pos <- colsum %>%
      select(-action_neg)
    
    action <- colnames(colsum_pos)
    colsum_pos_sum <- colsum_pos %>%
      rowSums()
    colsum_pos_perc <- colsum_pos / colsum_pos_sum * 100
    
    Percentage <- c()
    for (action_i in colnames(colsum)) { 
      if (pull(colsum, action_i) < 0) {
        perc <- pull(colsum, action_i)
      } else {
        if (is.na(pull(colsum_pos_perc, action_i))) {
          perc <- 0
        } else {
          perc <- pull(colsum_pos_perc, action_i)
        }
        
      }
      Percentage <- append(Percentage, round(perc, 0))
    }
    
    perc_tbl <- data.frame("Label" = colnames(colsum),
                           Percentage)
    
    perc_desc <- perc_tbl %>%
      arrange(desc(Percentage)) 
    perc_desc <- perc_desc[perc_desc$Percentage != 0, ]
    perc_desc$Label <- factor(perc_desc$Label,
                              levels = perc_desc$Label)
    
    #colourCount = length(unique(perc_desc$Label))
  
    if (nrow(perc_desc) == 0) {
      stop("No company has taken any actions.")
    } else {
      ggplot(perc_desc, aes(x = " ",
                                         y = Percentage,
                                         group = rev(Label),
                                         fill = Label)) +
        geom_bar(size = .5,
                 stat = "identity",
                 color = "grey") +
        coord_polar("y", start = 0) +
        #scale_fill_manual(values = getPalette(colourCount)) +
        theme_bw() +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(size = 14,
                                        face = "bold.italic",
                                        hjust = 0.5))
      
  }
  })
  
  
  output$plot2 <- renderPlot({
    
    dat_industry <- dat %>%
      filter(industry %in% input$industry)
    
    colsum <- data.frame(matrix(ncol = 0, nrow = nrow(dat_industry)))
    category <- c()
    
    if ("Corporate / headquarters employees" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(corp_abortion,
               corp_lodging,
               corp_travel,
               corp_dep_travel,
               #corp_attitude,
               corp_other)
      
      colsum <- cbind(colsum, colsum_i)
      
      category_i <- rep("Corporate Employees",
                        5)
      category <- c(category, category_i)
      
    } 
    
    if ("Contractors / gig workers, factory workers, and/or store employees" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(gig_abortion,
               gig_lodging,
               gig_travel,
               gig_dep_travel,
               #gig_attitude,
               gig_other)
      
      colsum <- cbind(colsum, colsum_i)
      
      category_i <- rep("Gig Employees",
                        5)
      category <- c(category, category_i)
      
    } 
    
    if ("User" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(delete_data,
               refer_else,
               no_comply,
               user_other)
      
      colsum <- cbind(colsum, colsum_i)
      
      category_i <- rep("User",
                        4)
      category <- c(category, category_i)
      
    } 
    
    if ("Community" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(lobby,
               donate_pro,
               restrict_activity,
               donate_anti,
               comm_other)
      
      colsum <- cbind(colsum, colsum_i)
      
      category_i <- rep("Community",
                        5)
      category <- c(category, category_i)
      
    } 
    
    action <- colnames(colsum)
    
    Total <- colsum %>%  
      colSums() %>% 
      as.vector()
    
    dat_popular <- data.frame("Action" = action,
                              Total,
                              "Target" = category)
    
    ggplot(dat_popular, aes(x = reorder(Action, Total), y = Total)) +
      geom_col(aes(fill = Target)) +
      geom_text(aes(label = Total),
                color = "black",
                position = position_dodge(width = 1),
                inherit.aes = TRUE) +
      coord_flip() + 
      theme_classic() +
      xlab("Actions") +
      ylab("Scores")
    
  })
    
  output$plot3a <- renderPlot({
    
    dat_industry <- dat %>%
      filter(industry %in% input$industry)
    
    colsum <- data.frame(matrix(ncol = 0, nrow = nrow(dat_industry)))
    
    if ("Corporate / headquarters employees" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(corp_abortion,
               corp_lodging,
               corp_travel,
               corp_dep_travel,
               #corp_attitude,
               corp_other)
      
      colsum <- cbind(colsum, colsum_i)
    } 
    
    if ("Contractors / gig workers, factory workers, and/or store employees" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(gig_abortion,
               gig_lodging,
               gig_travel,
               gig_dep_travel,
               #gig_attitude,
               gig_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    if ("User" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(delete_data,
               refer_else,
               no_comply,
               user_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    if ("Community" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(lobby,
               donate_pro,
               restrict_activity,
               donate_anti,
               comm_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    colsum <- colSums(colsum)
    
    action <- names(colsum)
    percentage <- as.vector(round(abs(colsum) / nrow(dat_industry) * 100, 2))
    
    dat_3a <- data.frame("action" = action,
                          "percentage" = percentage)
   
    
    dat_3a %>% ggplot(aes(x = "", y = percentage, fill = action)) +
      geom_bar(stat = "identity", width = 1) +
      facet_wrap(. ~ factor(action, 
                            levels = dat_3a$action),
                 labeller=as_labeller(label_wrap_gen(15)),
                 ncol = 5) +
      coord_polar("x") +
      geom_label_repel(aes(label = paste0(percentage,
                                          "%"))) +
      theme_bw() +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(size = 14, 
                                      face = "bold.italic", 
                                      hjust = 0.5))
    
  })
  
  output$plot3 <- renderPlot({
    
    dat_industry <- dat %>%
      filter(industry %in% input$industry)
    
    colsum <- data.frame(matrix(ncol = 0, nrow = nrow(dat_industry)))
    
    if ("Corporate / headquarters employees" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(corp_abortion,
               corp_lodging,
               corp_travel,
               corp_dep_travel,
               #corp_attitude,
               corp_other)
      
      colsum <- cbind(colsum, colsum_i)
    } 
    
    if ("Contractors / gig workers, factory workers, and/or store employees" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(gig_abortion,
               gig_lodging,
               gig_travel,
               gig_dep_travel,
               #gig_attitude,
               gig_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    if ("User" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(delete_data,
               refer_else,
               no_comply,
               user_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    if ("Community" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(lobby,
               donate_pro,
               restrict_activity,
               donate_anti,
               comm_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    colsum <- colSums(colsum)
    colsum_pro <- colsum[as.vector(colsum) >= 0]
    
    action <- names(colsum_pro)
    percentage <- as.vector(round(abs(colsum_pro) / nrow(dat_industry) * 100, 2))
    
    dat_pro <- data.frame("action" = as.factor(rep(action, 
                                                   2)),
                          "percentage" = c(percentage, 
                                           100 - percentage),
                          "pos" = as.factor(c(rep("Yes", 
                                                  length(action)), 
                                              rep("No", 
                                                  length(action)))))
    
    if (length(colsum_pro) == 0) {
      stop("There are no pro-abortion actions on average.")
    }
    
    ggplot(dat_pro, aes(x=" ", 
                        y = percentage, 
                        group = rev(pos), 
                        fill = pos)) +
      geom_bar(size = .5, 
               stat = "identity", 
               color = "white") + 
      scale_fill_manual(values = c("grey", 
                                   "lightgreen")) +
      coord_polar("y", start = 0) + 
      geom_label_repel(aes(label = ifelse(pos == "Yes", 
                                          paste0(percentage, 
                                                 "%"),
                                          ""))) +
      facet_wrap(~ factor(action, 
                          levels = dat_pro$action[1:(nrow(dat_pro)/2)])) +
      theme_bw() +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(size = 14, 
                                      face = "bold.italic", 
                                      hjust = 0.5))
    
    
  })
  
  
  output$plot4 <- renderPlot({
    
    dat_industry <- dat %>%
      filter(industry %in% input$industry)
    
    colsum <- data.frame(matrix(ncol = 0, nrow = nrow(dat_industry)))
    
    if ("Corporate / headquarters employees" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(corp_abortion,
               corp_lodging,
               corp_travel,
               corp_dep_travel,
               #corp_attitude,
               corp_other)
      
      colsum <- cbind(colsum, colsum_i)
    } 
    
    if ("Contractors / gig workers, factory workers, and/or store employees" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(gig_abortion,
               gig_lodging,
               gig_travel,
               gig_dep_travel,
               #gig_attitude,
               gig_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    if ("User" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(delete_data,
               refer_else,
               no_comply,
               user_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    if ("Community" %in% input$action_category) {
      
      colsum_i <- dat_industry %>%
        select(lobby,
               donate_pro,
               restrict_activity,
               donate_anti,
               comm_other)
      
      colsum <- cbind(colsum, colsum_i)
      
    }
    
    colsum <- colSums(colsum)
    colsum_anti <- colsum[as.vector(colsum) < 0]
    
    action <- names(colsum_anti)
    percentage <- as.vector(round(abs(colsum_anti) / nrow(dat_industry) * 100, 2))
    
    if (length(colsum_anti) == 0) {
      stop("There are no anti-abortion actions on average.")
    }
    
    dat_anti <- data.frame("action" = as.factor(rep(action, 
                                                   2)),
                          "percentage" = c(percentage, 
                                           100 - percentage),
                          "pos" = as.factor(c(rep("Yes", 
                                                  length(action)), 
                                              rep("No", 
                                                  length(action)))))
    
    ggplot(dat_anti, aes(x=" ", 
                        y = percentage, 
                        group = rev(pos), 
                        fill = pos)) +
      geom_bar(size = .5, 
               stat = "identity", 
               color = "white") + 
      scale_fill_manual(values = c("grey", 
                                   "pink")) +
      coord_polar("y", start = 0) + 
      geom_label_repel(aes(label = ifelse(pos == "Yes", 
                                          paste0(percentage, 
                                                 "%"),
                                          ""))) +
      facet_wrap(~ factor(action, 
                          levels = dat_anti$action[1:(nrow(dat_anti)/2)])) +
      theme_bw() +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(size = 14, 
                                      face = "bold.italic", 
                                      hjust = 0.5))
    
    
  })
  
  
  output$plot5 <- renderPlot({
    
    dat_scoring <- dat
    
    # dat_scoring$total = rowSums(dat[,
    #                                 4:23])
    
    dat_scoring$headquarter = rowSums(dat[,
                                          5:9]) 
    
    dat_scoring$contractor = rowSums(dat[,
                                         10:14]) 
    
    dat_scoring$employee = rowSums(dat[,
                                       5:14]) 
    
    dat_scoring$user = rowSums(dat[,
                                   15:18])
    
    dat_scoring$community = rowSums(dat[,
                                        19:23]) 
    
    colsum <- dat_scoring %>%
      select(industry)
    
    if ("Corporate / headquarters employees" %in% input$action_category) {
      
      colsum_i <- dat_scoring %>%
        select(headquarter)
      
      colsum <- cbind(colsum, colsum_i)
      
    } 
    
    if ("Contractors / gig workers, factory workers, and/or store employees" %in% input$action_category) {
      
      colsum_i <- dat_scoring %>%
        select(contractor)
      
      colsum <- cbind(colsum, colsum_i)
      
    } 
    
    if ("User" %in% input$action_category) {
      
      colsum_i <- dat_scoring %>%
        select(user)
      
      colsum <- cbind(colsum, colsum_i)
      
    } 
    
    if ("Community" %in% input$action_category) {
      
      colsum_i <- dat_scoring %>%
        select(community)
      
      colsum <- cbind(colsum, colsum_i)
      
    } 
    
    
    if (ncol(colsum) > 2) {
      dat_scoring$total <- rowSums(colsum[,
                                          2:ncol(colsum)]) 
    } else {
      dat_scoring$total <- colsum[,
                                  2]
    }
    
    dat_scoring %>%
      group_by(industry) %>%
      summarise(industry_ave = sum(total)) %>%
      filter(industry %in% input$industry) %>%
      ggplot() +
      geom_col(aes(x = industry,
                   y = industry_ave,
                   fill = industry)) +
      geom_label_repel(aes(x = industry,
                           y = industry_ave,
                           label = industry,
                           color = industry), 
                       fontface = "bold",
                       nudge_y = 1,
                       size = 5) +
      xlab("Industries") +
      ylab("Total Scoring") +
      theme_bw() +
      ggtitle("Total Scoring") +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            plot.title = element_text(size=14, face="bold.italic", hjust = 0.5),
            plot.subtitle = element_text(size=10, face="italic", hjust = 0.5))
    
    
  })
  
  output$plot6 <- renderPlot({
    
    dat_scoring <- dat
   
    # dat_scoring$total = rowSums(dat[,
    #                                 4:23])
    
    dat_scoring$headquarter = rowSums(dat[,
                                          5:9]) 
    
    dat_scoring$contractor = rowSums(dat[,
                                         10:14]) 
    
    dat_scoring$employee = rowSums(dat[,
                                       5:14]) 
    
    dat_scoring$user = rowSums(dat[,
                                   15:18])
    
    dat_scoring$community = rowSums(dat[,
                                        19:23])
    
    colsum <- dat_scoring %>%
      select(industry)
    
    if ("Corporate / headquarters employees" %in% input$action_category) {
      
      colsum_i <- dat_scoring %>%
        select(headquarter)
      
      colsum <- cbind(colsum, colsum_i)
      
    } 
    
    if ("Contractors / gig workers, factory workers, and/or store employees" %in% input$action_category) {
      
      colsum_i <- dat_scoring %>%
        select(contractor)
      
      colsum <- cbind(colsum, colsum_i)
      
    } 
    
    if ("User" %in% input$action_category) {
      
      colsum_i <- dat_scoring %>%
        select(user)
      
      colsum <- cbind(colsum, colsum_i)
      
    } 
    
    if ("Community" %in% input$action_category) {
      
      colsum_i <- dat_scoring %>%
        select(community)
      
      colsum <- cbind(colsum, colsum_i)
      
    } 
    
    
    if (ncol(colsum) > 2) {
      dat_scoring$total <- rowSums(colsum[,
                                          2:ncol(colsum)]) 
    } else {
      dat_scoring$total <- colsum[,
                                  2]
    }
  
    dat_scoring %>%
      group_by(industry) %>%
      summarise(industry_ave = mean(total)) %>%
      filter(industry %in% input$industry) %>%
      ggplot() +
      geom_col(aes(x = industry,
                   y = industry_ave,
                   fill = industry)) +
      geom_label_repel(aes(x = industry,
                           y = industry_ave,
                           label = industry,
                           color = industry), 
                       fontface = "bold",
                       nudge_y = 1,
                       size = 5) +
      xlab("Industries") +
      ylab("Average Scoring") +
      theme_bw() +
      ggtitle("Average Scoring") +
      theme(legend.position = "none",
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            plot.title = element_text(size=14, face="bold.italic", hjust = 0.5),
            plot.subtitle = element_text(size=10, face="italic", hjust = 0.5))
    
    
  })
  
  
}


# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

