if(!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

if(!require("DT")) install.packages("DT")
library(DT)

if(!require("stringr")) install.packages("stringr")
library(stringr)

if(!require("lubridate")) install.packages("lubridate")
library(lubridate)


FARES <- readxl::read_xlsx("data/mp-01/2022_fare_revenue.xlsx") |>
  select(-`State/Parent NTD ID`,
         -`Reporter Type`,
         -`Reporting Module`,
         -`TOS`,
         -`Passenger Paid Fares`,
         -`Organization Paid Fares`) |>
  filter(`Expense Type` == "Funds Earned During Period") |>
  select(-`Expense Type`)

EXPENSES <- readr::read_csv("data/mp-01/2022_expenses.csv") |>
  select(`NTD ID`,
         `Agency`,
         `Total`,
         `Mode`) |>
  mutate(`NTD ID` = as.integer(`NTD ID`)) |>
  rename(Expenses = Total) |>
  group_by(`NTD ID`, `Mode`) |>
  summarize(Expenses = sum(Expenses)) |>
  ungroup()


TRIPS <- readxl::read_xlsx("data/mp-01/2022_ridership.xlsx", sheet="UPT") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(-`Legacy NTD ID`,
         -`Reporter Type`,
         -`Mode/Type of Service Status`,
         -`UACE CD`,
         -`TOS`) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`),
               names_to="month",
               values_to="UPT") |>
  drop_na() |>
  mutate(month=my(month)) # Parse _m_onth _y_ear date specs

MILES <- readxl::read_xlsx("data/mp-01/2022_ridership.xlsx", sheet="VRM") |>
  filter(`Mode/Type of Service Status` == "Active") |>
  select(-`Legacy NTD ID`,
         -`Reporter Type`,
         -`Mode/Type of Service Status`,
         -`UACE CD`,
         -`TOS`) |>
  pivot_longer(-c(`NTD ID`:`3 Mode`),
               names_to="month",
               values_to="VRM") |>
  drop_na() |>
  group_by(`NTD ID`, `Agency`, `UZA Name`,
           `Mode`, `3 Mode`, month) |>
  summarize(VRM = sum(VRM)) |>
  ungroup() |>
  mutate(month=my(month)) # Parse _m_onth _y_ear date specs

USAGE <- inner_join(TRIPS, MILES) |>
  mutate(`NTD ID` = as.integer(`NTD ID`))

FINANCIALS <- inner_join(FARES, EXPENSES, join_by(`NTD ID`, `Mode`))


FINANCIALS <- FINANCIALS |>
  rename(
         "mode" = "Mode"
  )


FINANCIALS <- FINANCIALS |>
  mutate(mode=case_when(
    mode == "HR" ~ "Heavy Rail",
    mode == "DR" ~ "Demand Response",
    mode == "FB" ~ "Ferry Boat",
    mode == "MB" ~ "Motor Bus",
    mode == "SR" ~ "Streetcar",
    mode == "TB" ~ "Trolleybus",
    mode == "VP" ~ "Vanpool",
    mode == "CB" ~ "Commuter Bus",
    mode == "RB" ~ "Bus Rapid Transit",
    mode == "LR" ~ "Light Rail",
    mode == "YR" ~ "Hybrid Rail",
    mode == "MG" ~ "Guided Transit (Monorail)",
    mode == "CR" ~ "Commuter Rail",
    mode == "AR" ~ "Alaska Railroad",
    mode == "TR" ~ "Tramway",
    mode == "HR" ~ "Heavy Rail",
    mode == "IP" ~ "Inclined Plane",
    mode == "PB" ~ "Publico",
    mode == "CC" ~ "Cable Car",
    TRUE ~ "Unknown"))


# sample_n(USAGE, 1000) |>
#   mutate(month=as.character(month)) |>
#   DT::datatable()


# TASK 1 - Creating Syntactic Names
# Renaming column UZA Name to metro_area:
USAGE <- USAGE |>
  rename("metro_area" = "UZA Name",
         "unlinked_passenge_trips" = "UPT",
         "vehicle_revenue_miles" = "VRM",
         "agency" = "Agency",
         "mode" = "Mode"
        ) |>
  select(-"3 Mode")

# 
# `NTD ID` agency      metro_area          mode  month      unlinked_passenge_trips vehicle_revenue_miles
# <int> <chr>       <chr>               <chr> <date>                       <dbl>                 <dbl>
# 1        1 King County Seattle--Tacoma, WA DR    2002-01-01                  135144                746158
# 2        1 King County Seattle--Tacoma, WA DR    2002-02-01                  127378                656324
# 3        1 King County Seattle--Tacoma, WA DR    2002-03-01                  136030                726578
# 4        1 King County Seattle--Tacoma, WA DR    2002-04-01                  142204                736975
# 5        1 King County Seattle--Tacoma, WA DR    2002-05-01                  144697                746158
# 6        1 King County Seattle--Tacoma, WA DR    2002-06-01                  131833                696633

# TASK 2 - Recoding the Mode column
# Expanding DOT mode-codes into their full definitions...
# All of the code definitions are found here :
# https://www.transit.dot.gov/ntd/national-transit-database-ntd-glossary
USAGE <- USAGE |>
  mutate(mode=case_when(
    mode == "HR" ~ "Heavy Rail",
    mode == "DR" ~ "Demand Response",
    mode == "FB" ~ "Ferry Boat",
    mode == "MB" ~ "Motor Bus",
    mode == "SR" ~ "Streetcar",
    mode == "TB" ~ "Trolleybus",
    mode == "VP" ~ "Vanpool",
    mode == "CB" ~ "Commuter Bus",
    mode == "RB" ~ "Bus Rapid Transit",
    mode == "LR" ~ "Light Rail",
    mode == "YR" ~ "Hybrid Rail",
    mode == "MG" ~ "Guided Transit (Monorail)",
    mode == "CR" ~ "Commuter Rail",
    mode == "AR" ~ "Alaska Railroad",
    mode == "TR" ~ "Tramway",
    mode == "IP" ~ "Inclined Plane",
    mode == "PB" ~ "Publico",
    mode == "CC" ~ "Cable Car",
    TRUE ~ "Unknown"))

sample_n(USAGE, 20) |>
  select(agency, metro_area, mode)

# 
# agency                                              metro_area                        mode
# <chr>                                               <chr>                             <chr>
#   1 Birmingham-Jefferson County Transit Authority       Birmingham, AL                    Motor Bus
# 2 Yolo County Transportation District                 Sacramento, CA                    Motor Bus
# 3 Michiana Area Council of Governments                Elkhart, IN--MI                   Motor Bus
# 4 Lower Rio Grande Valley Development Council         McAllen, TX                       Demand Response
# 5 Potomac and Rappahannock Transportation Commission  Washington--Arlington, DC--VA--MD Demand Response
# 6 Greater Richmond Transit Company                    Richmond, VA                      Vanpool
# 7 Stanislaus Regional Transit Authority               Modesto, CA                       Demand Response
# 8 Worcester Regional Transit Authority                Worcester, MA--CT                 Demand Response
# 9 Chattanooga Area Regional Transportation Authority  Chattanooga, TN--GA               Motor Bus
# 10 Knoxville-Knox County Community Action Committee    Knoxville, TN                     Demand Response
# 11 Pinellas Suncoast Transit Authority                 Tampa--St. Petersburg, FL         Bus Rapid Transit
# 12 Laketran                                            Cleveland, OH                     Commuter Bus
# 13 METRO Regional Transit Authority                    Akron, OH                         Motor Bus
# 14 Regional Transportation Commission of Washoe County Reno, NV--CA                      Demand Response
# 15 Kanawha Valley Regional Transportation Authority    Charleston, WV                    Motor Bus
# 16 Maryland Transit Administration                     Baltimore, MD                     Light Rail
# 17 Transit Joint Powers Authority for Merced County    Merced, CA                        Demand Response
# 18 Broward County Board of County Commissioners        Miami--Fort Lauderdale, FL        Motor Bus
# 19 Regional Transportation Program, Inc.               Portland, ME                      Demand Response
# 20 Central Midlands Regional Transportation Authority  Columbia, SC                      Commuter Bus


# TASK 3: Answering Instructor Specified Questions with dplyr
# Using functions we have studied in class, including filter, group_by,
# summarize, arrange, answer the following questions in your analysis:

# Q1: What transit agency had the most total VRM in this sample?
USAGE |>
  filter(!is.na(agency)) |>
  group_by(agency) |>
  summarize(total = sum(vehicle_revenue_miles)) |>
  arrange(desc(total))

agency                                                                         total
<chr>                                                                          <dbl>
1 MTA New York City Transit                                                10832855350
2 New Jersey Transit Corporation                                            5645525525
3 Los Angeles County Metropolitan Transportation Authority                  4354016659
4 Washington Metropolitan Area Transit Authority                            2821950701
5 Chicago Transit Authority                                                 2806202144

# A1: MTA New York City Transit: 10832855350



# Q2: What transit mode had the most total VRM in this sample?
USAGE |>
  filter(!is.na(mode)) |>
  group_by(mode) |>
  summarize(total_vrm = sum(vehicle_revenue_miles, na.rm = TRUE)) |>
  arrange(desc(total_vrm))

# mode                        total_vrm
# <chr>                       <dbl>
# 1 Motor Bus                 49444494088
# 2 Demand Response           17955073508
# 3 Heavy Rail                14620362107
# 4 Commuter Rail              6970644241
# 5 Vanpool                    3015783362

# A2:  Motor Bus: 49444494088



# Q3 : How many trips were taken on the NYC Subway (Heavy Rail) in May 2024?
USAGE |>
  filter(
    month =="2004-05-01" &
    mode == "Heavy Rail" &
    agency == "MTA New York City Transit"
  ) |>
  select(agency, mode, month, unlinked_passenge_trips)

# agency                      mode       month      unlinked_passenge_trips
# <chr>                       <chr>      <date>                       <dbl>
# 1 MTA New York City Transit Heavy Rail 2004-05-01               184334430

# A3: 184334430


# Q5 How much did NYC subway ridership fall between April 2019 and April 2020?
 subway_ridership <- USAGE |>
  filter(
      agency == 'MTA New York City Transit' &
      mode == "Heavy Rail" &
      (month == "2019-05-01" | month == "2020-05-01")
      )

subway_ridership_2019 <- subway_ridership[1,]$unlinked_passenge_trips
subway_ridership_2020 <- subway_ridership[2,]$unlinked_passenge_trips

subway_ridership_2019 - subway_ridership_2020

# [1] 210561702


# observation 1
# Pittsburgh Regional Transit
month_map <- c(
  "01" = "January",
  "02" = "February",
  "03" = "March",
  "04" = "April",
  "05" = "May",
  "06" = "June",
  "07" = "July",
  "08" = "August",
  "09" = "September",
  "10" = "October",
  "11" = "November",
  "12" = "December"
)

USAGE |>
  filter(startsWith(agency, "Pitt")) |>
  filter(mode == "Inclined Plane") |>
  mutate(month_split = strsplit(as.character(month), "-")) |>
  mutate(month_num = sapply(month_split, function(x) x[2])) |>
  mutate(month_name = month_map[month_num]) |>
  group_by(month_name) |>
  summarize(total_this_month = sum(unlinked_passenge_trips)) |>
  arrange(desc(total_this_month))

# month_name total_this_month
# <chr>                 <dbl>
# 1 July                1715940
# 2 August              1507425
# 3 June                1424861
# 4 May                 1224003
# 5 September           1128166
# 6 October             1061274
# 7 April               1032881
# 8 November             965257
# 9 March                895805
# 10 December             877248
# 11 January              666924
# 12 February             641834



# observation 2
USAGE |>
  group_by(agency) |>
  summarize(total_trips = sum(unlinked_passenge_trips),
            total_miles = sum(vehicle_revenue_miles),
            average_trip = total_trips / total_miles
            ) |>
  arrange(desc(total_trips)) |>
  slice(1:50) |>
  arrange(desc(average_trip)) |>
  print(n = 50)


# agency                              total_trips total_miles average_trip
# <chr>                                     <dbl>       <dbl>        <  dbl>
# 1 New York City Department of Transp…   440538576     4150573         106.
# 2 Washington State Ferries              502394973    20230767         24.8
# 3 City and County of San Francisco     4533445698   583200690         7.77
# 4 MTA New York City Transit           69101730780 10832855350         6.38
# 5 Port Authority Trans-Hudson Corpor…  1628830279   280789159         5.80
# 6 MTA Bus Company                      2152842068   464938210         4.63
# 7 Chicago Transit Authority           10106154755  2806202144         3.60
# 8 Long Beach Transit                    554866140   156781304         3.54
# 9 Massachusetts Bay Transportation A…  7431129981  2383967378         3.12
# 10 Washington Metropolitan Area Trans…  7997911245  2821950701        2.83
# 11 Metro Transit                        1566986338   587265724        2.67
# 12 Metropolitan Atlanta Rapid Transit…  2719207795  1090615357        2.49
# 13 Southeastern Pennsylvania Transpor…  6421651411  2672630410        2.40
# 14 Westchester County                    474602169   202495794        2.34
# 15 Tri-County Metropolitan Transporta…  2026446640   865298980        2.34
# 16 Regional Transportation Commission…  1283541961   569179865        2.26
# 17 City of Los Angeles                   523294024   235933979        2.22
# 18 Niagara Frontier Transportation Au…   533783961   245737187        2.17
# 19 Sacramento Regional Transit Distri…   557137427   260588087        2.14
# 20 Los Angeles County Metropolitan Tr…  9193172939  4354016659        2.11
# 21 City and County of Honolulu          1199107719   576278263        2.08
# 22 City of Detroit                       629579428   322190313        1.95
# 23 Pittsburgh Regional Transit          1341763785   706818599        1.90
# 24 County of Miami-Dade                 2672371398  1415398043        1.89
# 25 Montgomery County, Maryland           478909200   259162559        1.85
# 26 The Greater Cleveland Regional Tra…   988664589   600377512        1.65
# 27 City of Phoenix                       819889879   505790703        1.62
# 28 San Diego Metropolitan Transit Sys…  1472907007   914869339        1.61
# 29 San Francisco Bay Area Rapid Trans…  2286795999  1522758714        1.50
# 30 Bi-State Development Agency of the…   916558841   614611771        1.49
# 31 MTA Long Island Rail Road            2107890659  1420934752        1.48
# 32 Capital Metropolitan Transportatio…   471155039   327537626        1.44
# 33 Alameda-Contra Costa Transit Distr…  1237328513   860521696        1.44
# 34 Maryland Transit Administration      2194284954  1580207936        1.39
# 35 Northeast Illinois Regional Commut…  1389044413  1030362535        1.35
# 36 Dallas Area Rapid Transit            1391301504  1033422611        1.35
# 37 Metro-North Commuter Railroad Comp…  1681018793  1352197898        1.24
# 38 King County                          2397254307  1936741788        1.24
# 39 Central Puget Sound Regional Trans…   463188436   376321016        1.23
# 40 Central Florida Regional Transport…   547663911   534104830        1.03
# 41 Denver Regional Transportation Dis…  1990421609  1991411970        1.00
# 42 New Jersey Transit Corporation       5480139694  5645525525        0.971
# 43 Santa Clara Valley Transportation …   859989540   889136124        0.967
# 44 Broward County Board of County Com…   752797633   835847157        0.901
# 45 VIA Metropolitan Transit              879495243   997306448        0.882
# 46 Orange County Transportation Autho…  1151813967  1365107738        0.844
# 47 Utah Transit Authority                860124389  1045327430        0.823
# 48 Metropolitan Transit Authority of …  1867123254  2272940948        0.821
# 49 Puerto Rico Highway and Transporta…   516329565  1021270808        0.506
# 50 Pace, the Suburban Bus Division of…   736224831  2379409930        0.309



# observation 3
USAGE |>
  #there is def some code smell here :
  mutate(state_split = strsplit(metro_area, ", ")) |>
  mutate(state = sapply(state_split, function(x) x[2])) |>
  mutate(state_split = strsplit(state, "-")) |>
  mutate(state = sapply(state_split, function(x) x[1])) |>
  group_by(state) |>
  summarize(total_trips = sum(unlinked_passenge_trips)) |>
  arrange(desc(total_trips)) |>
  print(n = 70)

# state total_trips
# <chr>       <dbl>
# 1  NY    85530025008
# 2  CA    27099834336
# 3  IL    12744773889
# 4  DC     9057818580
# 5  PA     8776976555
# 6  MA     7993427892
# 7  FL     5742223542
# 8  TX     5466164685
# 9  WA     4524992934
# 10 GA     3096203662
# 11 OR     2555837796
# 12 OH     2364107111
# 13 MD     2271510025
# 14 CO     2144454426
# 15 MN     1909216918
# 16 MI     1776345495
# 17 NV     1395503197
# 18 AZ     1384679690
# 19 MO     1371031013
# 20 HI     1225521578



# Task 5: Table Summarization
# Create a new table from USAGE that has annual total (sum) UPT and VRM for 2022.
# This will require use of the group_by, summarize, and filter functions. You will
# also want to use the year function, to extract a year from the month column.
USAGE_2022_ANNUAL <- USAGE |>
  mutate(year = year(month)) |>
  filter(year(month) == 2022) |>
  group_by(year, metro_area, agency, mode, `NTD ID`) |>
  summarize(total_upt = sum(unlinked_passenge_trips, na.rm = TRUE),
            total_vrm = sum(vehicle_revenue_miles, na.rm = TRUE)) |>
  select(`NTD ID`, agency, metro_area, mode, total_upt, total_vrm, year) |>
  filter(total_upt > 400000) |>
  ungroup()

USAGE_AND_FINANCIALS <- left_join(USAGE_2022_ANNUAL,
                                  FINANCIALS,
                                  join_by(`NTD ID`, mode)) |>
  rename(
    "total_fares" = "Total Fares",
    "expenses" = "Expenses",
  ) |>
  select(-"Agency Name") |>
  drop_na()

head(USAGE_AND_FINANCIALS)
# `NTD ID` agency                                           metro_area              mode              total_upt total_vrm  year total_fares expenses
# <dbl> <chr>                                               <chr>                   <chr>                 <dbl>     <dbl> <dbl>       <dbl>    <dbl>
# 1    50010 METRO Regional Transit Authority               Akron, OH               Motor Bus           3470765   3252489  2022     2345779 38200740
# 2    50021 Portage Area Regional Transportation Authority Akron, OH               Motor Bus            591881    672463  2022     1933660  6197595
# 3    20002 Capital District Transportation Authority      Albany--Schenectady, NY Motor Bus          12656760   8723783  2022     7947384 95586715
# 4    60019 City of Albuquerque                            Albuquerque, NM         Bus Rapid Transit   1829848    792000  2022       38268  6535431
# 5    60019 City of Albuquerque                            Albuquerque, NM         Motor Bus           4151408   3788377  2022      738682 40436564

# Task 6: Farebox Recovery Among Major Systems

#Q1. Which transit system (agency and mode) had the most UPT in 2022?
USAGE_AND_FINANCIALS |>
  arrange(desc(total_upt)) |>
  select(agency, mode, total_upt)

# agency                                                     mode        total_upt
# <chr>                                                      <chr>       <dbl>
# 1 MTA New York City Transit                                Heavy Rail  1793073801
# 2 MTA New York City Transit                                Motor Bus   458602305
# 3 Los Angeles County Metropolitan Transportation Authority Motor Bus   193637448
# 4 Los Angeles County Metropolitan Transportation Authority Motor Bus   193637448
# 5 Chicago Transit Authority                                Motor Bus   140013945

# A1: MTA New York City Transit Heavy Rail: 1793073801

# Q2. Which transit system (agency and mode) had the highest farebox recovery,
# defined as the highest ratio of Total Fares to Expenses?
USAGE_AND_FINANCIALS |>
  mutate(farebox_recovery = total_fares / expenses ) |>
  arrange(desc(farebox_recovery)) |>
  select(agency, mode, farebox_recovery)

agency                                                mode           farebox_recovery
<chr>                                                 <chr>                     <dbl>
1 Port Imperial Ferry Corporation                     Ferry Boat                 1.43
2 Hyannis Harbor Tours, Inc.                          Ferry Boat                 1.41
3 Trans-Bridge Lines, Inc.                            Commuter Bus               1.33
4 Chattanooga Area Regional Transportation Authority  Inclined Plane             1.31
5 Regional Transportation Commission of Washoe County Vanpool                    1.24

# A2 Port Imperial Ferry Corporation, Ferry Boat : 1.43

# Q3 Which transit system (agency and mode) has the lowest expenses per UPT?
USAGE_AND_FINANCIALS |>
  mutate(expenses_UPT = expenses / total_upt ) |>
  arrange(expenses_UPT) |>
  select(agency, mode, expenses_UPT)

# agency                                                       mode              expenses_UPT
# <chr>                                                        <chr>                    <dbl>
# 1 North Carolina State University                            Motor Bus                 1.18
# 2 Anaheim Transportation Network                             Motor Bus                 1.28
# 3 University of Iowa                                         Motor Bus                 1.54
# 4 Chatham Area Transit Authority                             Ferry Boat                1.60
# 5 Texas State University                                     Motor Bus                 2.05

# A3 North Carolina State University, Motor Bus : 1.18


#Q3 Which transit system (agency and mode) has the lowest expenses per UPT?

USAGE_AND_FINANCIALS |>
  mutate(expenses_UPT = expenses / total_upt ) |>
  arrange(desc(expenses_UPT)) |>
  select(agency, mode, expenses_UPT)

# agency                                             mode           expenses_UPT
# <chr>                                              <chr>                 <dbl>
# 1 MTA New York City Transit                        Demand Response        188.
# 2 Washington Metropolitan Area Transit Authority   Demand Response        115.
# 3 Massachusetts Bay Transportation Authority       Demand Response        108.
# 4 King County                                      Demand Response        91.3
# 5 King County                                      Demand Response        91.3


# Q4 Which transit system (agency and mode) has the highest total fares per UPT?
USAGE_AND_FINANCIALS |>
  mutate(fares_UPT = total_fares / total_upt ) |>
  arrange(desc(fares_UPT)) |>
  select(agency, mode, fares_UPT)

# agency                                                            mode          fares_UPT
# <chr>                                                             <chr>             <dbl>
# 1 Hampton Jitney, Inc.                                            Commuter Bus       41.3
# 2 Pennsylvania Department of Transportation                       Commuter Rail      32.3
# 3 Hyannis Harbor Tours, Inc.                                      Ferry Boat         29.6
# 4 Trans-Bridge Lines, Inc.                                        Commuter Bus       28.1
# 5 SeaStreak, LLC                                                  Ferry Boat         22.1

# A4 Hampton Jitney, Inc., Commuter Bus : 41.3

# Q5 Which transit system (agency and mode) has the lowest expenses per VRM?
USAGE_AND_FINANCIALS |>
  mutate(expenses_VRM = expenses / total_vrm ) |>
  arrange(expenses_VRM) |>
  select(agency, mode, expenses_VRM)

# agency                                                     mode            expenses_VRM
# <chr>                                                      <chr>                  <dbl>
# 1 Metropolitan Transportation Commission                   Vanpool                0.445
# 2 San Joaquin Council                                      Vanpool                0.498
# 3 San Diego Association of Governments                     Vanpool                0.540
# 4 Regional Transportation Commission of Washoe County      Vanpool                0.566
# 5 Los Angeles County Metropolitan Transportation Authority Vanpool                0.581


# Q6 Which transit system (agency and mode) has the highest total fares per VRM?
USAGE_AND_FINANCIALS |>
  mutate(fares_VRM = total_fares / total_vrm ) |>
  arrange(desc(fares_VRM)) |>
  select(agency, mode, fares_VRM)

# agency                                                            mode           fares_VRM
# <chr>                                                             <chr>              <dbl>
# 1 Jacksonville Transportation Authority                           Ferry Boat         158.
# 2 Chattanooga Area Regional Transportation Authority              Inclined Plane     149.
# 3 Hyannis Harbor Tours, Inc.                                      Ferry Boat         138.
# 4 SeaStreak, LLC                                                  Ferry Boat         115.
# 5 Cape May Lewes Ferry                                            Ferry Boat         93.0


USAGE_AND_FINANCIALS |>
  # mutate(month=as.character(month)) |>
  DT::datatable()
