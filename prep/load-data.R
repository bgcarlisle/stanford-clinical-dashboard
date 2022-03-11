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

## Write to disk
trials %>%
    write_csv("data/2022-03-11-stanford-data.csv")
