library(plotly)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggvis)
library(ggplot2)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(DT)
library(fs)
library(gtsummary)
library(gt)

## Load data
stanford <- read_csv("data/2022-04-07-stanford-data.csv")
stanfaff <- read_csv("data/2022-04-08-stanford-trials-affiliations.csv")

## Load functions
source("ui_elements.R")
source("start_page_plots.R")
source("umc_plots.R")
source("all_umc_plots.R")

## Load pages
source("impressum.R", encoding="UTF-8")
source("start_page.R")
## source("umc_page.R")
## source("all_umcs_page.R")
source("why_page.R")
source("methods_page.R")
source("datasets_page.R")
source("about_rm.R")
source("faq_page.R")
## source("trial_characteristics_page.R")

## Define UI
ui <- tagList(
    tags$head(tags$script(type="text/javascript", src = "code.js")),
    navbarPage(
        "Dashboard for clinical research transparency", theme = shinytheme("flatly"), id = "navbarTabs",
        start_page,
        ## all_umcs_page,
        ## umc_page,
        why_page,
        ## trial_characteristics_page,
        methods_page,
        datasets_page,
        faq_page,
        about_rm_page,
        tags$head
        (
            tags$script
            ('
                        var width = 0;
                        $(document).on("shiny:connected", function(e) {
                          width = window.innerWidth;
                          Shiny.onInputChange("width", width);
                        });
                        '
            )
        )
    )
)

## Define server function
server <- function (input, output, session) {

    ## Define button actions

    ## observeEvent(
    ##     input$buttonUMC, {
    ##         updateTabsetPanel(
    ##             session, "navbarTabs",
    ##             selected = "tabUMC"
    ##         )
    ##     }
    ## )

    ## observeEvent(
    ##     input$buttonAllUMCs, {
    ##         updateTabsetPanel(
    ##             session, "navbarTabs",
    ##             selected = "tabAllUMCs"
    ##         )
    ##     }
    ## )
    
    observeEvent(
        input$buttonMethods, {
            updateTabsetPanel(
                session, "navbarTabs",
                selected = "tabMethods"
            )
        }
    )

    observeEvent(
        input$buttonDatasets, {
            updateTabsetPanel(
                session, "navbarTabs",
                selected = "tabDatasets"
            )
        }
    )

    observeEvent(
        input$link_to_methods, {
            updateTabsetPanel(
                session, "navbarTabs",
                selected = "tabMethods"
            )
        }
    )

    observeEvent(
        input$link_to_methods2, {
            updateTabsetPanel(
                session, "navbarTabs",
                selected = "tabMethods"
            )
        }
    )

    observeEvent(
        input$link_to_why_these_practices, {
            updateTabsetPanel(
                session, "navbarTabs",
                selected = "tabWhy"
            )
        }
    )

    ## Dynamically generate options for UMC drop-down menu

    output$startpage <- renderUI({

        wellPanel(
            br(),
            fluidRow(
                column(
                    8,
                    h1(style = "margin-left:0cm", strong("Dashboard for clinical research transparency"), align = "left"),
                    h4(style = "margin-left:0cm",
                       "This dashboard displays the performance of North American medical schools (USA and Canada) on established registration and reporting practices for clinical research transparency. The dashboard displays data for interventional clinical trials conducted at North American medical schools, registered in ClinicalTrials.gov, and reported as complete between 2014 - 2017. The dashboard was developed as part of a scientific research project with the overall aim to support the adoption of responsible research practices at biomedical research institution. The dashboard is a pilot and continues to be updated. More metrics and more clinical trials may be added in the future."),
                    h4(style = "margin-left:0cm",
                       HTML("The Start page displays data across all included North American medical schools. The All Medical Schools page displays the data of all medical schools side-by-side. The One Medical School page allows you to focus on any given medical school by selecting it in the drop-down menu. The data for this medical school is then contextualized to that across all included medical schools. Besides each plot, you can find an overview of the methods and limitations by clicking on the associated widgets. For more detailed information on the methods and underlying datasets used to assess the practices displayed in this dashboard, visit the Methods and Datasets pages. The Trial Characteristics page provides an overview of the characteristics of trials included in the dashboard. The FAQ and Why these practices? pages provide more general information about this dashboard and our selection of practices.")),
                    h3(style = "margin-left:0cm; color: purple",
                       "More information on the overall aim and methodology can be
                       found in the associated publication [enter DOI]. "),
                    br()
                ),
                column(
                    4,
                    hr(),
                    br(),
                    br(),
                    ## actionButton(
                    ##     style = "color: white; background-color: #aa1c7d;",
                    ##     'buttonUMC',
                    ##     'See one UMC'
                    ## ),
                    ## actionButton(
                    ##     style = "color: white; background-color: #aa1c7d;",
                    ##     'buttonAllUMCs',
                    ##     'See all UMCs'
                    ## ),
                    actionButton(
                        style = "color: white; background-color: #aa1c7d;",
                        'buttonMethods',
                        'See methods'
                    ),
                    actionButton(
                        style = "color: white; background-color: #aa1c7d;",
                        'buttonDatasets',
                        'See datasets'
                    ),
                    br()
                )
            )
        )
        
    })

                                        # Start page metrics #

    ## Start page: Trial Registration
    output$registry_metrics <- renderUI({

        req(input$width)

        if (input$width < 1400) {
            col_width <- 6
            first_lim_align <- "left"
        } else {
            col_width <- 4
            first_lim_align <- "right"
        }

         ## Value for prereg

        stanfaff$start_year <- format(stanfaff$start_date, "%Y")

        max_start_year <- max(stanfaff$start_year, na.rm=TRUE)
        
                                        # Filter for max start date for the pink descriptor text
        all_numer_prereg <- stanfaff %>%
            filter(start_year == max_start_year) %>%
            filter(is_prospective) %>%
            nrow()
        
                                        # Filter for 2017 completion date for the pink descriptor text
        all_denom_prereg <- stanfaff %>%
            filter(start_year == max_start_year) %>%
            nrow()

        if (all_denom_prereg == 0) {
            preregval <- "Not applicable"
            preregvaltext <- "No clinical trials for this metric were captured by this method for this UMC"
        } else {
            preregval <- paste0(round(100*all_numer_prereg/all_denom_prereg), "%")
            preregvaltext <- paste0("of registered clinical trials started in ", max_start_year, " (n=", all_denom_prereg, ") were prospectively registered")
        }
        
        ## Value for TRN in abstract
        
        ## all_numer_trn <- iv_all %>%
        ##     filter(has_iv_trn_abstract == TRUE) %>%
        ##     nrow()
        
        ## all_denom_trn <- iv_all %>%
        ##     filter(
        ##         has_publication == TRUE,
        ##         publication_type == "journal publication",
        ##         has_pubmed == TRUE
        ##     ) %>%
        ##     nrow()

        ## Value for linkage

        link_num <- stanford %>%
            filter(has_reg_pub_link == TRUE) %>%
            ## filter(publication_type == "journal publication") %>%
            filter(completion_year == 2017) %>%
            nrow()

        link_den <- stanford %>%
            filter(has_publication == TRUE) %>%
            ## filter(publication_type == "journal publication") %>%
            filter(completion_year == 2017) %>%
            filter(has_pubmed == TRUE | ! is.na (doi)) %>%
            nrow()
        
        linkage <- paste0(round(100*link_num/link_den), "%")

        wellPanel(
            style="padding-top: 0px; padding-bottom: 0px;",
            h2(strong("Trial Registration"), align = "left"),
            fluidRow(
                column(
                    col_width,
                    metric_box(
                        title = "Prospective registration",
                        value = preregval,
                        value_text = preregvaltext,
                        plot = plotlyOutput('plot_clinicaltrials_prereg', height="300px"),
                        info_id = "infoPreReg",
                        info_title = "Prospective registration",
                        info_text = prereg_tooltip,
                        lim_id = "limPreReg",
                        lim_title = "Limitations: Prospective registration",
                        lim_text = lim_prereg_tooltip,
                        lim_align = first_lim_align
                    )
                ),
                column(
                    col_width,
                    metric_box(
                        title = "Reporting of Trial Registration Number in publications",
                        value = "__%",
                        value_text = paste0("of trials with a publication (n=__) reported a trial registration number in the abstract"),
                        ## plot = plotlyOutput('plot_clinicaltrials_trn', height="300px"),
                        info_id = "infoTRN",
                        info_title = "Reporting of Trial Registration Number in publications",
                        info_text = trn_tooltip,
                        lim_id = "limTRN",
                        lim_title = "Limitations: Reporting of Trial Registration Number in publications",
                        lim_text = lim_trn_tooltip
                    )
                ),
                column(
                    col_width,
                    metric_box(
                        title = "Publication link in registry",
                        value = linkage,
                        value_text = paste0("of trials completed in 2017 with a publication (n=", link_den, ") provide a link to this publication in the registry entry"),
                        plot = plotlyOutput('plot_linkage', height="300px"),
                        info_id = "infoLinkage",
                        info_title = "Publication link in registry",
                        info_text = linkage_tooltip,
                        lim_id = "limLinkage",
                        lim_title = "Limitations: Publication link in registry",
                        lim_text = lim_linkage_tooltip
                    )
                )
                
            )
            
        )

        
    })

    ## Start page: Trial reporting
    output$publication_metrics <- renderUI({

        req(input$width)

        if (input$width < 1400) {
            col_width <- 6
            first_lim_align <- "left"
        } else {
            col_width <- 4
            first_lim_align <- "right"
        }
        
        wellPanel(
            style="padding-top: 0px; padding-bottom: 0px;",
            h2(strong("Trial Reporting"), align = "left"),
            fluidRow(
                column(
                    col_width,
                    metric_box(
                        title = "Summary Results Reporting",
                        value = "__%",
                        value_text = "of due clinical trials registered in EUCTR (n=__) reported summary results",
                        ## plot = plotlyOutput('plot_clinicaltrials_sumres', height="300px"),
                        info_id = "infoSumRes",
                        info_title = "Summary Results Reporting",
                        info_text = sumres_tooltip,
                        lim_id = "limSumRes",
                        lim_title = "Limitations: Summary Results Reporting",
                        lim_text = lim_sumres_tooltip,
                        lim_align = first_lim_align
                    )
                ),
                column(
                    col_width,
                    uiOutput("startreport2a"),
                    selectInput(
                        "startreporttype2a",
                        strong("Reporting type"),
                        choices = c(
                            "Summary results or publication",
                            "Publication only",
                            "Summary results only"
                        )
                    )
                ),
                column(
                    col_width,
                    uiOutput("startreport5a"),
                    selectInput(
                        "startreporttype5a",
                        strong("Reporting type"),
                        choices = c(
                            "Summary results or publication",
                            "Publication only",
                            "Summary results only"
                        )
                    )
                )
                
            )

        )

    })

    ## Start page 2 year reporting toggle
    output$startreport2a <- renderUI({
        
        # Filter for 2017 completion date for pink descriptor text
        iv_data_unique <- stanford %>%
            filter(completion_year == 2017)

        all_numer_timpub <- iv_data_unique %>%
            filter(
                (has_followup_2y_sumres & is_summary_results_2y) | (has_followup_2y_pub & is_publication_2y)
            ) %>%
            nrow()
        
        all_denom_timpub <- iv_data_unique %>%
            filter(
                has_followup_2y_sumres | has_followup_2y_pub
                ) %>%
            nrow()

        if (input$startreporttype2a == "Summary results only") {
            all_numer_timpub <- iv_data_unique %>%
                filter(
                    has_followup_2y_sumres,
                    is_summary_results_2y) %>%
                nrow()
            
            all_denom_timpub <- iv_data_unique %>%
                filter(
                    has_followup_2y_sumres
                ) %>%
                nrow()
        }

        if (input$startreporttype2a == "Publication only") {
            all_numer_timpub <- iv_data_unique %>%
                filter(
                    has_followup_2y_pub,
                    is_publication_2y) %>%
                nrow()
            
            all_denom_timpub <- iv_data_unique %>%
                filter(
                    has_followup_2y_pub
                ) %>%
                nrow()
        }

        if (all_denom_timpub == 0) {
            timpubval <- "Not applicable"
            timpubvaltext <- "No clinical trials for this metric were captured by this method for this UMC"
        } else {
                        
            timpubval <- paste0(round(100*all_numer_timpub/all_denom_timpub), "%")
            timpubvaltext <- paste0("of clinical trials completed in 2017 (n=", all_denom_timpub, ") reported results within 2 years")
        }
        
        metric_box(
            title = "Results reporting within 2 years of trial completion (timely)",
            value = timpubval,
            value_text = timpubvaltext,
            plot = plotlyOutput('plot_clinicaltrials_timpub_2a', height="300px"),
            info_id = "infoTimPub2",
            info_title = "Results reporting (2 years)",
            info_text = timpub_tooltip2,
            lim_id = "limTimPub2",
            lim_title = "Limitations: Results reporting (2 years)",
            lim_text = lim_timpub_tooltip2
        )
    })

    ## Start page 5 year reporting toggle
    output$startreport5a <-  renderUI({
        
        max_completion_year <- stanford %>%
            filter(
                has_followup_5y_sumres | has_followup_5y_pub
            ) %>%
            select(completion_year) %>%
            max(na.rm=TRUE)
        
        iv_data_unique <- stanford %>%
            filter(completion_year == max_completion_year)

        ## Double check this
        all_denom_timpub5a <- iv_data_unique %>%
            filter(
                has_followup_5y_sumres | has_followup_5y_pub
            ) %>%
            nrow()
        all_numer_timpub5a <- iv_data_unique %>%
            filter(
            (has_followup_5y_sumres & is_summary_results_5y) | (has_followup_5y_pub & is_publication_5y)
            ) %>%
            nrow()
        
        if (input$startreporttype5a == "Summary results only") {

            max_completion_year <- stanford %>%
                filter(
                    has_followup_5y_sumres
                ) %>%
                select(completion_year) %>%
                max(na.rm=TRUE)

            iv_data_unique <- stanford %>%
                filter(completion_year == max_completion_year)
            
            all_numer_timpub5a <- iv_data_unique %>%
                filter(
                (has_followup_5y_sumres & is_summary_results_5y) | (has_followup_5y_pub & is_publication_5y)
                ) %>%
                nrow()
            
            all_denom_timpub5a <- iv_data_unique %>%
                filter(
                    has_followup_5y_sumres | has_followup_5y_pub
                ) %>%
                nrow()
            
            all_numer_timpub5a <- iv_data_unique %>%
                filter(
                    has_followup_5y_sumres,
                    is_summary_results_5y
                ) %>%
                nrow()
            
            all_denom_timpub5a <- iv_data_unique %>%
                filter(
                    has_followup_5y_sumres
                ) %>%
                nrow()
        }

        if (input$startreporttype5a == "Publication only") {
            
            max_completion_year <- stanford %>%
                filter(
                    has_followup_5y_pub
                ) %>%
                select(completion_year) %>%
                max(na.rm=TRUE)

            iv_data_unique <- stanford %>%
                filter(completion_year == max_completion_year)
            
            all_numer_timpub5a <- iv_data_unique %>%
                filter(
                (has_followup_5y_sumres & is_summary_results_5y) | (has_followup_5y_pub & is_publication_5y)
                ) %>%
                nrow()
            
            all_numer_timpub5a <- iv_data_unique %>%
                filter(
                    has_followup_5y_pub,
                    is_publication_5y) %>%
                nrow()
            
            all_denom_timpub5a <- iv_data_unique %>%
                filter(
                    has_followup_5y_pub
                ) %>%
                nrow()
        }

        if (all_denom_timpub5a == 0) {
            timpubval5a <- "Not applicable"
            timpubvaltext5a <- "No clinical trials for this metric were captured by this method for this UMC"
        } else {
            timpubval5a <- paste0(round(100*all_numer_timpub5a/all_denom_timpub5a), "%")
            timpubvaltext5a <- paste0("of clinical trials completed in ", max_completion_year, " (n=", all_denom_timpub5a, ") reported results within 5 years")
        }

        metric_box(
            title = "Results reporting within 5 years of trial completion",
            value = timpubval5a,
            value_text = timpubvaltext5a,
            plot = plotlyOutput('plot_clinicaltrials_timpub_5a', height="300px"),
            info_id = "infoTimPub5",
            info_title = "Results reporting (5 years)",
            info_text = timpub_tooltip5,
            lim_id = "limTimPub5",
            lim_title = "Limitations: Results reporting (5 years)",
            lim_text = lim_timpub_tooltip5
        )
        
    })

    ## Start page: Open Access
    output$openscience_metrics <- renderUI({

        req(input$width)

        if (input$width < 1400) {
            col_width <- 6
            first_lim_align <- "left"
        } else {
            col_width <- 6
            first_lim_align <- "right"
        }

        ## Value for Open Access
        
        #Create set for OA percentage plot
        oa_set <- stanford %>%
            filter(
                has_publication == 1,
                ! is.na(doi),
                ! is.na(publication_date_unpaywall)
            ) %>%
            distinct(doi, .keep_all=TRUE)

        oa_set$oa_year <- oa_set$publication_date_unpaywall %>%
            format("%Y")
        
        all_numer_oa <- oa_set %>%
            filter(
                color == "gold" | color == "green" | color == "hybrid",
                oa_year == "2021"
            ) %>%
            nrow()

        # Keep pubs with NA color for now 
        all_denom_oa <- oa_set %>%
            filter(oa_year == "2021") %>%
            nrow()
        
        #Create set for Green OA percentage plot
        ## oa_set_green <- iv_all %>%
        ##     filter(
        ##         has_publication == TRUE,
        ##         publication_type == "journal publication",
        ##         ! is.na(doi),
        ##         ! is.na(publication_date_unpaywall),
        ##         is_closed_archivable == TRUE | color_green_only == "green"
        ##     ) %>%
        ##     distinct(doi, .keep_all=TRUE)

        ## oa_set_green$oa_year <- oa_set_green$publication_date_unpaywall %>%
        ##     format("%Y")

        ## oa_set_green <- oa_set_green %>%
        ##     filter(oa_year == "2020")
        
        ## denom_greenoa <- oa_set_green %>%
        ##     nrow()
        
        ## numer_greenoa <- oa_set_green %>%
        ##     filter(
        ##         color_green_only == "green"
        ##     ) %>%
        ##     nrow()
        
        wellPanel(
            style="padding-top: 0px; padding-bottom: 0px;",
            h2(strong("Open Access"), align = "left"),
            selectInput(
                "opensci_absnum",
                strong("Show proportions or absolute numbers"),
                choices = c(
                    "Show proportions",
                    "Show absolute numbers"
                )
            ),
            fluidRow(
                column(
                    col_width,
                    metric_box(
                        title = "Open Access (OA)",
                        value = paste0(round(100*all_numer_oa/all_denom_oa), "%"),
                        value_text = paste0("of publications from 2021 (n=", all_denom_oa, ") are Open Access (Gold, Green or Hybrid)"),
                        plot = plotlyOutput('plot_opensci_oa', height="300px"),
                        info_id = "infoOpenAccess",
                        info_title = "Open Access",
                        info_text = openaccess_tooltip,
                        lim_id = "limOpenAccess",
                        lim_title = "Limitations: Open Access",
                        lim_text = lim_openaccess_tooltip,
                        lim_align = first_lim_align
                    )
                ),
                column(
                    col_width,
                    metric_box(
                        title = "Realized potential of green Open Access (OA)",
                        value = "__%",
                        value_text = paste0("of paywalled publications from 2020 with a permission for green OA (n=__) have been made openly accessible via this route"),
                        ## plot = plotlyOutput('plot_opensci_green_oa', height="300px"),
                        info_id = "infoGreenOA",
                        info_title = "Realized potential of green OA",
                        info_text = greenopenaccess_tooltip,
                        lim_id = "limGreenOA",
                        lim_title = "Limitations: Realized potential of green OA",
                        lim_text = lim_greenopenaccess_tooltip
                    )
                )
                
            )
        )

    })


                                        # Color palettes #

    color_palette <- c("#B6B6B6", "#879C9D", "#F1BA50", "#AA493A",
                     "#303A3E", "#007265", "#634587", "#000000",   #363457 #533A71 #011638 #634587
                     "#DCE3E5")
    
    color_palette_delwen <- c("#B6B6B6", "#879C9D", "#F1BA50", "#cf9188",  
                              "#303A3E", "#20303b", "#158376", "#007265", 
                              "#DCE3E5", "#634587", "#000000", "#539d66",
                              "#ab880c")

    color_palette_bars <- c("#AA1C7D", "#879C9D", "#F1BA50", "#AA493A", "#303A3E", "#007265", "#634587", "#AA1C7D", "#879C9D", "#F1BA50", "#AA493A", "#303A3E", "#007265", "#634587", "#AA1C7D", "#879C9D", "#F1BA50", "#AA493A", "#303A3E", "#007265", "#634587", "#AA1C7D", "#879C9D", "#F1BA50", "#AA493A", "#303A3E", "#007265", "#634587", "#AA1C7D", "#879C9D", "#F1BA50", "#AA493A", "#303A3E", "#007265", "#634587", "#AA1C7D")

                                        # Start page plots #
    
    ## Preregistration plot
    output$plot_clinicaltrials_prereg <- renderPlotly({
        return (plot_clinicaltrials_prereg(stanfaff, color_palette))
    })
    
    ## TRN plot
    output$plot_clinicaltrials_trn <- renderPlotly({
        return (plot_clinicaltrials_trn(iv_all, color_palette))
    })

    ## Linkage plot
    output$plot_linkage <- renderPlotly({
        return (plot_linkage(stanford, color_palette))
    })
    
    ## Summary results plot
    output$plot_clinicaltrials_sumres <- renderPlotly({
        return (plot_clinicaltrials_sumres(eutt_hist, iv_all, input$startsumresregistry, color_palette))
    })
    
    ## Timely Publication plot 2a
    output$plot_clinicaltrials_timpub_2a <- renderPlotly({
        return (plot_clinicaltrials_timpub_2a(stanford, input$startreporttype2a, color_palette))
    })
    
    ## Timely Publication plot 5a
    output$plot_clinicaltrials_timpub_5a <- renderPlotly({
        return (plot_clinicaltrials_timpub_5a(stanford, input$startreporttype5a, color_palette))
    })

    ## Open Access plot
    output$plot_opensci_oa <- renderPlotly({
        return (plot_opensci_oa(stanford, input$opensci_absnum, color_palette_delwen))
    })
    
    ## Green Open Access plot
    output$plot_opensci_green_oa <- renderPlotly({
        return (plot_opensci_green_oa(iv_all, input$opensci_absnum, color_palette_delwen))
    })
    
                                        # Data tables #
    
    output$data_table_stanford_data <- DT::renderDataTable({
        make_datatable(stanford)
    })
    
}

## Create Shiny object
shinyApp(ui, server)
