library(httr)
library(jsonlite)
library(tidyft)

api_base <- "https://api.openbudget.gov.ua/api/public/localBudgetData?"

budgetCode="1556400000" #works only for year 2022-2023, for earlier data - another code
budgetItem="INCOMES" #EXPENSES,FINANCING_DEBTS,FINANCING_CREDITOR,CREDITS
period="MONTH" #we can import only monthly data, and then combine into annual if needed
year="2023"

api_call <- paste("https://api.openbudget.gov.ua/api/public/localBudgetData?",
"budgetCode=",budgetCode,
"&budgetItem=",budgetItem,
"&period=",period,
"&year=",year,
sep = "")

str(api_call)
get_data_city <- GET(url=api_call)
str(get_data_city$content)
data_city<- httr::content(get_data_city,"text")
str(data_city)
data_table_city<-read.csv(text = data_city, sep = ";", header = TRUE)
str(data_table_city)

#the table contains all available months of the year
#the data should be rearranged to present by-month data by columns
#apply UTF-8 encoding