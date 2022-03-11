library(tidyverse)
library(readxl)

## Read data from Daniel
trials <- read_excel(
    "prep/2022-03-08-Stanford version0_1 25_ cohort 14-17.xlsx",
    sheet="2014-2017"
)

## Remove non-extracted rows
trials <- trials %>%
    filter(! is.na(`any paper`))

## Read OA data from Delwen
oa <- read_csv("prep/2022-03-10-stanford-data-oa.csv")

## Combine extracted data and OA data
trials <- trials %>%
    left_join(oa, by="doi")

## Determine whether the trial is prospectively registered or not
trials$start_month <- format(as.Date(trials$start_date), "%Y-%m-01")

trials$reg_month <- format(
    as.Date(trials$registration_date),
    "%Y-%m-01"
)

trials$is_prospective <- trials$reg_month <= trials$start_month

## Determine the amount of follow-up
search_date <- as.Date("2022-03-08")

trials$has_followup_2y <-
    as.Date(trials$completion_date) + 365*2 < search_date
trials$has_followup_2y_pub <- trials$has_followup_2y
trials$has_followup_2y_sumres <- trials$has_followup_2y

trials$has_followup_5y <-
    as.Date(trials$completion_date) + 365*5 < search_date
trials$has_followup_5y_pub <- trials$has_followup_5y
trials$has_followup_5y_sumres <- trials$has_followup_5y

## Determine whether there are summary results within 2, 5 years

trials$is_summary_results_5y <-
    as.Date(trials$summary_results_date) <=
    as.Date(trials$completion_date) + 365*5

trials$is_summary_results_2y <-
    as.Date(trials$summary_results_date) <=
    as.Date(trials$completion_date) + 365*2

trials$is_publication_2y <-
    as.Date(trials$`publication date`) <=
    as.Date(trials$completion_date) + 365*2

trials$is_publication_5y <-
    as.Date(trials$`publication date`) <=
    as.Date(trials$completion_date) + 365*5

## Rename `any paper`

trials <- trials %>%
    rename(has_publication = `any paper`)

## Write to disk
trials %>%
    write_csv("data/2022-03-11-stanford-data.csv")
