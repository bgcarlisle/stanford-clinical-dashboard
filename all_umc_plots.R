## Prospective registration
plot_allumc_clinicaltrials_prereg <- function (dataset, iv_dataset, toggled_registry, color_palette, color_palette_bars) {

    if (toggled_registry == "ClinicalTrials.gov") {

        dataset <- dataset %>%
            filter( ! is.na (start_date) )

    }

    if (toggled_registry == "DRKS") {

        dataset <- iv_dataset %>%
            filter( ! is.na (start_date) ) %>%
            filter(registry == toggled_registry)
        
    }

    plot_data <- tribble (
        ~x_label, ~percentage, ~mouseover
    )

    for (umc in unique(dataset$city)) {

        umc_numer <- dataset %>%
            filter(
                city == umc,
                is_prospective
            ) %>%
            nrow()

        umc_denom <- dataset %>%
            filter(city == umc) %>%
            nrow()

        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~percentage, ~mouseover,
                    umc, round(100*umc_numer/umc_denom, digits=1), paste0(umc_numer, "/", umc_denom)
                )
            )
    }

    plot_data$x_label <- factor(
        plot_data$x_label,
        levels = unique(plot_data$x_label)[order(plot_data$percentage, decreasing=TRUE)]
    )

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        text = ~mouseover,
        type = 'bar',
        marker = list(
            color = color_palette_bars[1],
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Percentage of trials (%)</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## TRN
plot_allumc_clinicaltrials_trn <- function (dataset, location, color_palette) {

    plot_data <- tribble (
        ~x_label, ~colour, ~percentage, ~mouseover,
    )

    for (umc in unique(dataset$city)) {

        umc_numer_abs <- dataset %>%
            filter(city == umc) %>%
            select(has_iv_trn_abstract) %>%
            filter(has_iv_trn_abstract == TRUE) %>%
            nrow()

        umc_numer_ft <- dataset %>%
            filter(city == umc) %>%
            select(has_iv_trn_ft) %>%
            filter(has_iv_trn_ft == TRUE) %>%
            nrow()

        umc_numer_either <- dataset %>%
            filter(
                city == umc,
                has_iv_trn_abstract == TRUE | has_iv_trn_ft == TRUE
            ) %>%
            nrow()

        umc_ft_denom <- dataset %>%
            filter(city == umc) %>%
            filter(
                has_publication == TRUE,
                publication_type == "journal publication",
                has_ft,
                ! is.na(has_iv_trn_ft)
            ) %>%
            nrow()
        
        umc_abs_denom <- dataset %>%
            filter(city == umc) %>%
            filter(
                has_publication == TRUE,
                publication_type == "journal publication",
                has_pubmed,
                ! is.na(has_iv_trn_abstract)
            ) %>%
            nrow()

        umc_either_denom <- dataset %>%
            filter(city == umc) %>%
            filter(
                has_publication == TRUE,
                publication_type == "journal publication",
                has_ft | has_pubmed,
                ! is.na(has_iv_trn_ft) | ! is.na(has_iv_trn_abstract)
            ) %>%
            nrow()

        umc_numer_both <- dataset %>%
            filter(
                city == umc,
                has_iv_trn_abstract == TRUE & has_iv_trn_ft == TRUE
            ) %>%
            nrow()

        umc_both_denom <- dataset %>%
            filter(city == umc) %>%
            filter(
                has_publication == TRUE,
                publication_type == "journal publication",
                has_ft | has_pubmed,
                ! is.na(has_iv_trn_ft) & ! is.na(has_iv_trn_abstract)
            ) %>%
            nrow()

        if (location == "In abstract") {
            numer <- umc_numer_abs
            denom <- umc_abs_denom
        }

        
        if (location == "In full-text") {
            numer <- umc_numer_ft
            denom <- umc_ft_denom
        }

        if (location == "In abstract or full-text") {
            numer <- umc_numer_either
            denom <- umc_either_denom
        }

        if (location == "In abstract and full-text") {
            numer <- umc_numer_both
            denom <- umc_both_denom
        }
        
        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~percentage, ~mouseover,
                    umc, round(100*numer/denom, digits=1), paste0(numer, "/", denom)
                )
            )

    }

     plot_ly(
        plot_data,
        x = ~reorder(x_label,1/percentage),
        y = ~percentage,
        text = ~mouseover,
        type = 'bar',
        colors = c(
            "#F1BA50",
            "#007265",
            "#634587"
        ),
        marker = list(
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Trials with publication (%)</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Publication link in registry
plot_allumc_linkage <- function (dataset, chosenregistry, color_palette, color_palette_bars) {

    dataset <- dataset %>%
        filter(has_publication == TRUE) %>%
        filter(publication_type == "journal publication") %>%
        filter (has_pubmed == TRUE | ! is.na (doi))

    if (chosenregistry != "All") {
        dataset <- dataset %>%
            filter(registry == chosenregistry)
    }

    plot_data <- tribble(
        ~x_label, ~percentage, ~mouseover
    )

    for (umc in unique(dataset$city)) {

        umcdata <- dataset %>%
            filter(city == umc)

        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~percentage, ~mouseover,
                    umc, round(100*mean(umcdata$has_reg_pub_link, na.rm=TRUE), digits=1), paste0(sum(umcdata$has_reg_pub_link), "/", nrow(umcdata))
                )
            )
        
    }

    plot_data$x_label <- factor(
        plot_data$x_label,
        levels = unique(plot_data$x_label)[order(plot_data$percentage, decreasing=TRUE)]
    )

     plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        text = ~mouseover,
        type = 'bar',
        marker = list(
            color = "#3d754b",
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Trials with publication (%)</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Summary results

plot_allumc_clinicaltrials_sumres <- function (dataset, iv_dataset, toggled_registry, color_palette, color_palette_bars) {

    if (toggled_registry == "EUCTR") {

        max_date <- max(dataset$date)

        dataset <- dataset %>%
            filter( date == max_date ) %>%
            distinct (city, .keep_all=TRUE)

        plot_data <- dataset %>%
            rename (x_label = city) %>%
            rename (percentage = percent_reported) %>%
            mutate (mouseover = paste0(total_reported, "/", total_due))

        plot_data$x_label <- factor(
            plot_data$x_label,
            levels = unique(plot_data$x_label)[order(plot_data$percentage, decreasing=TRUE)]
        )
        
    } else {

        dataset <- iv_dataset %>%
            filter(
                registry == toggled_registry
            )

        plot_data <- tribble(
            ~x_label, ~percentage, ~mouseover
        )

        for (currentcity in unique(iv_dataset$city)) {

            city_trials <- dataset %>%
                filter(city == currentcity)

            currentcity_denom <- nrow(city_trials)

            currentcity_numer <- city_trials %>%
                filter(has_summary_results == TRUE) %>%
                nrow()

            plot_data <- plot_data %>%
                bind_rows(
                    tribble(
                        ~x_label,    ~percentage, ~mouseover,
                        currentcity, round(100*currentcity_numer/currentcity_denom, digits=1),
                        paste0(currentcity_numer, "/", currentcity_denom)
                    )
                )

        }
        
    }
    
    plot_data$x_label <- factor(
        plot_data$x_label,
        levels = unique(plot_data$x_label)[order(plot_data$percentage, decreasing=TRUE)]
    )

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        text = ~mouseover,
        type = 'bar',
        marker = list(
            color = "#F1BA50",
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Percentage of trials (%)</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Timely reporting (2 years)
plot_allumc_clinicaltrials_timpub <- function (dataset, rt, color_palette, color_palette_bars) {
    
    plot_data <- tribble (
        ~x_label, ~percentage, ~mouseover
    )

    for (umc in unique(dataset$city)) {

        umc_numer <- dataset %>%
            filter(
                city == umc,
                (has_followup_2y_sumres & is_summary_results_2y) | (has_followup_2y_pub & is_publication_2y)
            ) %>%
            nrow()
        
        umc_denom <- dataset %>%
            filter(
                has_followup_2y_pub | has_followup_2y_sumres,
                city == umc) %>%
            nrow()

        if (rt == "Summary results only") {

            umc_numer <- dataset %>%
                filter(
                    city == umc,
                    has_followup_2y_sumres,
                    is_summary_results_2y
                ) %>%
                nrow()
            
            umc_denom <- dataset %>%
                filter(
                    city == umc,
                    has_followup_2y_sumres
                ) %>%
                nrow()
        }

        if (rt == "Publication only") {

            umc_numer <- dataset %>%
                filter(
                    city == umc,
                    has_followup_2y_pub,
                    is_publication_2y
                ) %>%
                nrow()
            
            umc_denom <- dataset %>%
                filter(
                    city == umc,
                    has_followup_2y_pub
                ) %>%
                nrow()
        }

        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~percentage, ~mouseover,
                    umc, round(100*umc_numer/umc_denom, digits=1), paste0(umc_numer, "/", umc_denom)
                )
            )
    }

    plot_data$x_label <- factor(
        plot_data$x_label,
        levels = unique(plot_data$x_label)[order(plot_data$percentage, decreasing=TRUE)]
    )

     plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        text = ~mouseover,
        type = 'bar',
        marker = list(
            color = "#639196",
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Percentage of trials (%)</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Timely reporting (5a)

plot_allumc_timpub_5a <- function (dataset, rt, color_palette, color_palette_bars) {

    plot_data <- tribble (
        ~x_label, ~percentage, ~mouseover
    )

    for (umc in unique(dataset$city)) {

        umc_numer <- dataset %>%
            filter(
                city == umc,
                (has_followup_5y_sumres & is_summary_results_5y) | (has_followup_5y_pub & is_publication_5y)
            ) %>%
            nrow()
        
        umc_denom <- dataset %>%
            filter(
                city == umc,
                has_followup_5y_sumres | has_followup_5y_pub) %>%
            nrow()

        if (rt == "Summary results only") {

            umc_numer <- dataset %>%
                filter(
                    city == umc,
                    has_followup_5y_sumres,
                    is_summary_results_5y
                ) %>%
                nrow()
            
            umc_denom <- dataset %>%
                filter(
                    city == umc,
                    has_followup_5y_sumres
                ) %>%
                nrow()
        }

        if (rt == "Publication only") {

            umc_numer <- dataset %>%
                filter(
                    city == umc,
                    has_followup_5y_pub,
                    is_publication_5y
                ) %>%
                nrow()
            
            umc_denom <- dataset %>%
                filter(
                    city == umc,
                    has_followup_5y_pub
                ) %>%
                nrow()
        }

        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~percentage, ~mouseover,
                    umc, round(100*umc_numer/umc_denom, digits=1), paste0(umc_numer, "/", umc_denom)
                )
            )
    }

    plot_data$x_label <- factor(
        plot_data$x_label,
        levels = unique(plot_data$x_label)[order(plot_data$percentage, decreasing=TRUE)]
    )

     plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        text = ~mouseover,
        type = 'bar',
        marker = list(
            color = "#20303b",
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Percentage of trials (%)</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Open Access
plot_allumc_openaccess <- function (dataset, color_palette) {

    dataset <- dataset %>%
        filter(
            has_publication == TRUE,
            publication_type == "journal publication",
            !is.na(doi),
            !is.na(publication_date_unpaywall)
        )

    plot_data <- tribble (
        ~x_label, ~colour, ~percentage
    )

    for (umc in unique(dataset$city)) {

        umc_gold <- dataset %>%
            filter(
                color == "gold",
                city == umc
            ) %>%
            distinct(doi, .keep_all = TRUE) %>%
            nrow()

        umc_green <- dataset %>%
            filter(
                color == "green",
                city == umc
            ) %>%
            distinct(doi, .keep_all = TRUE) %>%
            nrow()

        umc_hybrid <- dataset %>%
            filter(
                color == "hybrid",
                city == umc
            ) %>%
            distinct(doi, .keep_all = TRUE) %>%
            nrow()

        umc_sum <- umc_gold + umc_green + umc_hybrid

        umc_denom <- dataset %>%
            filter(city == umc) %>%
            distinct(doi, .keep_all = TRUE) %>%
            nrow()

        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~colour, ~percentage,                       ~sum,      ~numer, ~sort,
                    umc, "Gold", round(100*umc_gold/umc_denom, digits=1), umc_denom, umc_gold, 1-(umc_gold + umc_green + umc_hybrid)/umc_denom,
                    umc, "Green", round(100*umc_green/umc_denom, digits=1),umc_denom, umc_green, 1-(umc_gold + umc_green + umc_hybrid)/umc_denom,
                    umc, "Hybrid", round(100*umc_hybrid/umc_denom, digits=1), umc_denom, umc_hybrid, 1-(umc_gold + umc_green + umc_hybrid)/umc_denom
                )
            )
    
    }

    plot_data$x_label <- factor(
        plot_data$x_label,
        levels = unique(plot_data$x_label)[order(plot_data$sort, decreasing=TRUE)]
    )

    plot_ly(
        plot_data,
        x = ~reorder(x_label, sort),
        color = ~colour,
        text = ~paste0(numer, "/", sum),
        y = ~percentage,
        type = 'bar',
        colors = c(
            "#F1BA50",
            "#007265",
            "#634587"
        ),
        marker = list(
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            barmode = 'stack',
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Percentage Open Access (%)</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

## Green OA
plot_allumc_greenoa <- function (dataset, color_palette, color_palette_bars) {

    oa_set <- dataset %>%
        filter(
            has_publication == TRUE,
            publication_type == "journal publication",
            !is.na(doi),
            !is.na(publication_date_unpaywall),
            is_closed_archivable == TRUE | color_green_only == "green"
        )
    
    plot_data <- tribble(
        ~x_label, ~percentage, ~mouseover
    )

    for (umc in unique(oa_set$city)) {

        umc_numer <- oa_set %>%
            filter(
                city == umc,
                color_green_only == "green"
            ) %>%
            distinct(doi, .keep_all = TRUE) %>%
            nrow()

        umc_denom <- oa_set %>%
            filter(
                city == umc
            ) %>%
            distinct(doi, .keep_all = TRUE) %>%
            nrow()

        plot_data <- plot_data %>%
            bind_rows(
                tribble(
                    ~x_label, ~percentage, ~mouseover,
                    umc, round(100*umc_numer/umc_denom, digits=1), paste0(umc_numer, "/", umc_denom)
                )
            )
    }

    plot_data$x_label <- factor(
        plot_data$x_label,
        levels = unique(plot_data$x_label)[order(plot_data$percentage, decreasing=TRUE)]
    )

    plot_ly(
        plot_data,
        x = ~x_label,
        y = ~percentage,
        text = ~mouseover,
        type = 'bar',
        marker = list(
            color = "#007265",
            line = list(
                color = 'rgb(0,0,0)',
                width = 1.5
            )
        )
    ) %>%
        layout(
            xaxis = list(
                title = '<b>UMC</b>'
            ),
            yaxis = list(
                title = '<b>Paywalled publications (%)</b>',
                range = c(0, 100)
            ),
            paper_bgcolor = color_palette[9],
            plot_bgcolor = color_palette[9]
        )
    
}

