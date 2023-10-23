library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(purrr)
library(plotly)
library(readxl)
library(writexl)

# Function to construct api path
api_construct <- function(budgetCode, 
                          budgetItem, # "INCOMES","EXPENSES","FINANCING_DEBTS","FINANCING_CREDITOR","CREDITS"),
                          classificationType, # "PROGRAM","FUNCTIONAL","ECONOMIC","CREDIT"
                          period = "MONTH",
                          year) {
  
  api_base <- "https://api.openbudget.gov.ua/api/public/localBudgetData?"
  
  if (budgetItem %in% c("EXPENSES", "CREDITS")) {  
    api_path <- 
      paste(api_base,
            "budgetCode=", budgetCode,
            "&budgetItem=", budgetItem,
            "&classificationType=", classificationType,  # classificationType parameter is mandatory for EXPENSES and CREDITS items
            "&period=", period,
            "&year=", year,
            sep = "")
  } else {
    api_path <- 
      paste(api_base,
            "budgetCode=", budgetCode,
            "&budgetItem=", budgetItem,
            "&period=", period,
            "&year=", year,
            sep = "")
  }
  
  return(api_path)
}


# Function to call api, read in and parse data
call_api <- function(api_path, col_types) {
  data_call <- GET(api_path) |> 
    pluck("content") |> 
    rawToChar()
  
  if (missing(col_types)) {
    data_read <- data_call |> 
    read_delim(delim = ";")
  } else {
    data_read <- data_call |> 
    read_delim(delim = ";", col_types = col_types) |> 
    mutate(REP_PERIOD = readr::parse_date(REP_PERIOD, "%m.%Y"))
  }
  
  return(data_read)
}

# Read in data across multiple periods and categories
codes <- read_excel("./Open Budget variable types.xlsx")

df_m <- codes |> 
  distinct(pick(budgetItem, classificationType)) |> 
  mutate(budgetItem = str_trim(budgetItem),
         classificationType = str_trim(classificationType)) |> # trim white space in category names
  mutate(budgetCode = "26000000000", 
         period = "MONTH") |> 
  mutate(year = list(c(2022,2023))) |> 
  rowwise() |> 
  mutate(api_path = list(api_construct(budgetCode, budgetItem, classificationType, period, year))) |> 
  mutate(data = list(map_dfr(api_path, call_api)))

# Write data to Excel file
data_l <- df_m$data
names(data_l) <- if_else(!is.na(df_m$classificationType),
                         paste(df_m$budgetItem, df_m$classificationType, sep=", "),
                         df_m$budgetItem)
write_xlsx(data_l, "./data_output.xlsx")

# Summarise the data 
inc <- data_l$INCOMES|>
  mutate(TYPE = cut(COD_INCO, 
                       breaks = c(0,19999999,29999999,39999999,60000000),
                       labels = c("Tax","Non-tax","Cap_rev","Transfers")),
         REP_PERIOD = readr::parse_date(REP_PERIOD, "%m.%Y")) |> 
  filter(month(REP_PERIOD) %in% c(month(max(REP_PERIOD)),12),
         FUND_TYP == "T")|>
  group_by(TYPE,REP_PERIOD)%>%
  summarise(FAKT_AMT=sum(FAKT_AMT),ZAT_AMT=sum(ZAT_AMT))

exp <- data_l$`EXPENSES, ECONOMIC`|>
  mutate(TYPE = cut(COD_CONS_EK, 
              breaks = c(0,2280,2281,2399,2421,2999,8999,9001),
              labels = c("Opex","Capex","Opex","Interest","Opex","Capex","Opex")),
         REP_PERIOD = readr::parse_date(REP_PERIOD, "%m.%Y"))|> 
  filter(month(REP_PERIOD) %in% c(month(max(REP_PERIOD)),12),
         FUND_TYP == "T")|>
  group_by(TYPE,REP_PERIOD)%>%
  summarise(FAKT_AMT=sum(FAKT_AMT),ZAT_AMT=sum(ZAT_AMT))


fin <- data_l$FINANCING_DEBTS%>%
  mutate(TYPE=if_else(COD_FINA==401000, {"New Borrowing"},
                      if_else(COD_FINA==402000,"Debt Repayments",
                      if_else(COD_FINA==602300,"Interbudget loans","NA"))),
         REP_PERIOD = readr::parse_date(REP_PERIOD, "%m.%Y"))|> 
  filter(month(REP_PERIOD) %in% c(month(max(REP_PERIOD)),12),
         FUND_TYP == "T")|>
  group_by(TYPE,REP_PERIOD)%>% 
  summarise(FAKT_AMT=sum(FAKT_AMT),ZAT_AMT=sum(ZAT_AMT))

credit


cut(COD_CONS_EK, 
    breaks = c(0,2280,2281,2399,2399,2400,2999,8999,9001),
    labels = c("Opex","Capex1","Opex1","Interest","Opex2","Capex2","Opex3"))),
cut(COD_CONS_EK, 
    breaks = c(0,2999,9001),
    labels = c("Opex","Capex")))





         
         

