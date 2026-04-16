library(rvest)
library(tidyverse)
library(stringr)
library(ggplot2)

########################################################################################
#MEN
########################################################################################


men100m_html <- read_html("http://www.alltime-athletics.com/m_100ok.htm")
men100m_html |> html_nodes(xpath = "//pre") |> html_text()-> men100m_list
men100m_tbl <- read_fwf(men100m_list) # ---> TODO: fix warning?

# ---> exclude extra entries
last_row <- which(men100m_tbl$X4 == "Joshua Azzopardi" & 
                  men100m_tbl$X2 == "10.09" & 
                  men100m_tbl$X9 == "14.02.2026")
men100m_tbl <- men100m_tbl[1:last_row, ]

names(men100m_tbl) <-c("Position", "TimeResult_s","WindSpeed","Athlete","CountryCode",
                       "AthlBirthDate","AdditionalInfo", "RaceLocation","RaceDate")  
#---> rename columns

#---> check if wrongly formatted birth dates and show corresponding entries
men100m_tbl |>
  mutate(row = row_number()) |>
  mutate(AthlBirthDate = str_trim(AthlBirthDate)) |> #---> remove any leading/trailing spaces first
  mutate(test_date = as.Date(AthlBirthDate, format = "%d.%m.%y")) |>
  filter(is.na(test_date)) -> f
print(f, width = Inf)
## A tibble: 5 × 11
#  Position TimeResult_s WindSpeed Athlete          
#     <dbl> <chr>        <chr>       <chr>            
#1      127 9.99         +1.6        Méba-Mickaël Zézé
#2      277 10.05        +1.1        Tristyn Flores   
#3      312 10.06        +1.4        Deron Dudley     
#4      435 10.09        +1.9        Iván García      
#5      435 10.09        -0.3        Méba-Mickaël Zézé
#  CountryCode AthlBirthDate AdditionalInfo RaceLocation         
#  <chr>       <chr>         <chr>          <chr>                
#1 NA          19.05         NA             La Chaux-de-Fonds    
#2 USA         NA            4q2            College Station      
#3 USA         NA            2rB            Memphis              
#4 CU          29.02.7       1              Ciudad de México     
#5 NA          19.05         NA             0   La Chaux-de-Fonds
#  RaceDate     row test_date
#  <chr>      <int> <date>   
#1 03.07.2022  1411 NA       
#2 30.05.2025  3107 NA       
#3 12.07.2025  3491 NA       
#4 08.05.1993  4398 NA       
#5 03.07.2022  4757 NA 
# ---> therefore, these lines were read wrong at the beginning. Easiest to look at webpage and manually change them:
men100m_tbl[1411,]$CountryCode <- "FRA"
men100m_tbl[1411,]$AthlBirthDate <- "19.05.94"
men100m_tbl[3107,]$CountryCode <- "USA"
men100m_tbl[4398,]$AthlBirthDate <- "29.02.72"
men100m_tbl[4757,]$AthlBirthDate <- "19.05.94"
men100m_tbl[4757,]$CountryCode <- "FRA"
men100m_tbl[4757,]$RaceLocation <- "La Chaux-de-Fonds"
# ---> now we convert. Since we can still some dates > present date, we fix that too:
men100m_tbl <- men100m_tbl |>
  mutate(AthlBirthDate = as.Date(str_trim(AthlBirthDate), format = "%d.%m.%y")) |>
  mutate(AthlBirthDate = if_else(AthlBirthDate > Sys.Date(),
                                 AthlBirthDate - years(100),  #---> subtract 100 years if wrong
                                 AthlBirthDate))

men100m_tbl <- mutate(men100m_tbl, TimeResult_s=str_remove_all(TimeResult_s, "[±A]")) 
# ---> TODO: create new column for "A" info othw lost?
men100m_tbl <- mutate(men100m_tbl, TimeResult_s=as.numeric(TimeResult_s))

# ---> to find out which entries are wrong
which(str_detect(men100m_tbl$WindSpeed, "[^0-9.+±-]"))
#[1] 4303 
men100m_tbl$WindSpeed[4303] <- 1.4 # ---> correct the single wrong entry (still as a charachter)
men100m_tbl <- mutate(men100m_tbl, WindSpeed=as.numeric(str_remove_all(WindSpeed, "[±A]"))) 

men100m_tbl <- mutate(men100m_tbl, AthlBirthDate=as.Date(AthlBirthDate, format="%d.%m.%y"))
# ---> NB specify format; y is 2-digit year

men100m_tbl <- mutate(men100m_tbl, RaceDate=as.Date(RaceDate, format="%d.%m.%Y"))
# ---> Y is 4-digit year

summary(men100m_tbl)
##    Position      TimeResult_s     WindSpeed      
# Min.   :  1.0   Min.   : 9.58   Min.   :-3.5000  
# 1st Qu.:189.0   1st Qu.: 9.98   1st Qu.: 0.0000  
# Median :312.0   Median :10.04   Median : 0.6000  
# Mean   :332.5   Mean   :10.02   Mean   : 0.5961  
# 3rd Qu.:391.0   3rd Qu.:10.07   3rd Qu.: 1.3000  
# Max.   :946.0   Max.   :10.09   Max.   : 2.8000  
#                                 NA's   :9        
#   Athlete          CountryCode        AthlBirthDate       
# Length:4847        Length:4847        Min.   :1942-12-20  
# Class :character   Class :character   1st Qu.:1978-10-27  
# Mode  :character   Mode  :character   Median :1988-10-06  
#                                       Mean   :1986-10-24  
#                                       3rd Qu.:1995-07-13  
#                                       Max.   :2009-12-29  
#                                       NA's   :2           
# AdditionalInfo     RaceLocation          RaceDate         
# Length:4847        Length:4847        Min.   :1964-10-15  
# Class :character   Class :character   1st Qu.:2005-07-29  
# Mode  :character   Mode  :character   Median :2015-05-09  
#                                       Mean   :2012-07-22  
#                                       3rd Qu.:2021-09-03  
#                                       Max.   :2026-03-07 

# ---> there are still some NAs in "WindSpeed", but fine when checking webpage against the output of this:
# ---> TODO: check if some NAs read as zeros here?
men100m_tbl |>
  filter(is.na(WindSpeed)) |> 
  select(WindSpeed, RaceLocation, RaceDate)
#---> (output...)

men100m_tbl_ord <- men100m_tbl |>
  group_by(RaceYear=year(RaceDate)) |>
  summarise(best_times_by_date = min(TimeResult_s))
ggplot(men100m_tbl_ord, aes(x = RaceYear, y = best_times_by_date)) + 
  geom_point() +
  geom_line() +
  xlab("Race date (year)") + ylab("Best time (s)") +
  ggtitle("Best yearly time on 100 m (men)")

#---> to visualise record progression:
#---> find only the races where a new record (best time so far) was set:
today <- as.Date("2026-03-01")
records <- men100m_tbl |>
  arrange(RaceDate) |>                               
  mutate(best_so_far = cummin(TimeResult_s)) |> #---> cumulative minimum
  mutate(prev_best = lag(best_so_far, default = Inf)) |>
  filter(TimeResult_s < prev_best) |>
  select(RaceDate, TimeResult_s)
last_record_date <- records |> slice_tail(n = 1) |> pull(RaceDate)
last_record_time <- records |> slice_tail(n = 1) |> pull(TimeResult_s)
ggplot(records, aes(x = RaceDate, y = TimeResult_s)) +
  geom_point() +
  geom_line() +
  labs(x = "Race date",
       y = "WR time (s)",
       title = "Progression of the 100 m World Record (men)") +
  geom_segment(aes(x = last_record_date, xend = today,
                   y = last_record_time, yend = last_record_time),
               linetype = "solid", color = "black") 

#unique_countries <- unique(men100m_tbl$CountryCode)
#men100m_tbl_ord_2 <- men100m_tbl |>
#  group_by(RaceYear=year(RaceDate)) |>
#  summarise(n_runners_by_year=max(count(unique(*$CountryCode))), top_country=arg(n_runners_by_year)) 
# ---> all those who made the list; don't count repeated appearances
#ggplot(men100m_tbl_ord_2, aes(x=RaceYear, y=n_runners_by_year, ylabels=top_country))+
#geom_point()+
#labs(x="Race date (year)", y="Number of runners of top country", title="Country with most runner by year")

# ---> prepare data for plotting
top_by_year <- men100m_tbl |>
  mutate(RaceYear = year(RaceDate)) |>
  #---> athletes appearing multiple times a year/race are still counted
  group_by(RaceYear, CountryCode) |> # ---> successive grouping
  summarise(n = n(), .groups = "drop") |> # ---> counts, then drop groups (n is as long as number of groups)
  group_by(RaceYear) |> # ---> re-group by year only, so that next we... 
  slice_max(n, n = 1, with_ties = FALSE)  #---> ...keep top 3 countries (ties broken arbitrarily) by year
#---> reorder country factor by total number of appearances (most first)
top_by_year <- top_by_year |>
  mutate(CountryCode = factor(CountryCode, 
                              levels = names(sort(table(CountryCode), decreasing = TRUE))))
ggplot(top_by_year, aes(x = RaceYear, y = n, color = CountryCode)) + # ---> NB simply this
  geom_point() + 
  labs(x = "Race date (year)", y = "Number of runners of top country",
       title = "Countries with most runners by year")

# TODO: study syntax? 

########################################################################################
#WOMEN
########################################################################################
women100m_html <- read_html("http://www.alltime-athletics.com/w_100ok.htm")
women100m_html |> html_nodes(xpath = "//pre") |> html_text()-> women100m_list
women100m_tbl <- read_fwf(women100m_list)

# ---> exclude extra entries
last_row <- which(women100m_tbl$X4 == "Zoe Hobbs" & 
                  women100m_tbl$X2 == "11.09" & 
                  women100m_tbl$X9 == "14.09.2025")
women100m_tbl <- women100m_tbl[1:last_row, ]

names(women100m_tbl) <-c("Position", "TimeResult_s","WindSpeed","Athlete","CountryCode",
                       "AthlBirthDate","AdditionalInfo", "RaceLocation","RaceDate")  
#---> rename columns

women100m_tbl <- mutate(women100m_tbl, TimeResult_s=str_remove_all(TimeResult_s, "[±A]")) 
women100m_tbl <- mutate(women100m_tbl, TimeResult_s=as.numeric(TimeResult_s))

women100m_tbl <- mutate(women100m_tbl, RaceDate=as.Date(RaceDate, format="%d.%m.%Y"))
# ---> Y is 4-digit year

summary(women100m_tbl)
#    Position      TimeResult_s    WindSpeed        
# Min.   :  1.0   Min.   :10.54   Length:3408       
# 1st Qu.:171.0   1st Qu.:10.94   Class :character  
# Median :240.0   Median :11.01   Mode  :character  
# Mean   :299.5   Mean   :10.99                     
# 3rd Qu.:312.0   3rd Qu.:11.06                     
# Max.   :937.0   Max.   :11.09                     
#   Athlete          CountryCode        AthlBirthDate     
# Length:3408        Length:3408        Length:3408       
# Class :character   Class :character   Class :character  
# Mode  :character   Mode  :character   Mode  :character  
#                                                         
#                                                         
#                                                         
# AdditionalInfo     RaceLocation          RaceDate         
# Length:3408        Length:3408        Min.   :1968-10-15  
# Class :character   Class :character   1st Qu.:1999-07-05  
# Mode  :character   Mode  :character   Median :2013-08-12  
#                                       Mean   :2010-04-13  
#                                       3rd Qu.:2022-05-14  
#                                       Max.   :2025-10-10

#---> Time result, race date and country code look ok (no NAs or other issues).
#     here we don't care about wind speed or athlete birth date, so we leave them unformatted.

women100m_tbl_ord <- women100m_tbl |>
  group_by(RaceYear=year(RaceDate)) |>
  summarise(best_times_by_date = min(TimeResult_s))
ggplot(women100m_tbl_ord, aes(x = RaceYear, y = best_times_by_date)) + 
  geom_point() +
  geom_line() +
  xlab("Race date (year)") + ylab("Best time (s)") +
  ggtitle("Best yearly time on 100 m (women)")

#---> to visualise record progression:
#---> find only the races where a new record (best time so far) was set:
today <- as.Date("2026-03-01")
w_records <- women100m_tbl |>
  arrange(RaceDate) |>                               
  mutate(best_so_far = cummin(TimeResult_s)) |> #---> cumulative minimum
  mutate(prev_best = lag(best_so_far, default = Inf)) |>
  filter(TimeResult_s < prev_best) |>
  select(RaceDate, TimeResult_s)
w_last_record_date <- w_records |> slice_tail(n = 1) |> pull(RaceDate)
w_last_record_time <- w_records |> slice_tail(n = 1) |> pull(TimeResult_s)
ggplot(w_records, aes(x = RaceDate, y = TimeResult_s)) +
  geom_point() +
  geom_line() +
  labs(x = "Race date",
       y = "WR time (s)",
       title = "Progression of the 100 m World Record (women)") +
  geom_segment(aes(x = w_last_record_date, xend = today,
                   y = w_last_record_time, yend = w_last_record_time),
               linetype = "solid", color = "black") 

# ---> prepare data for plotting
w_top_by_year <- women100m_tbl |>
  mutate(RaceYear = year(RaceDate)) |>
  #---> athletes appearing multiple times a year/race are still counted
  group_by(RaceYear, CountryCode) |> # ---> successive grouping
  summarise(n = n(), .groups = "drop") |> # ---> counts, then drop groups (n is as long as number of groups)
  group_by(RaceYear) |> # ---> re-group by year only, so that next we... 
  slice_max(n, n = 1, with_ties = FALSE)  #---> ...keep top 3 countries (ties broken arbitrarily) by year
#---> reorder country factor by total number of appearances (most first)
w_top_by_year <- w_top_by_year |>
  mutate(CountryCode = factor(CountryCode, 
                              levels = names(sort(table(CountryCode), decreasing = TRUE))))
ggplot(w_top_by_year, aes(x = RaceYear, y = n, color = CountryCode)) + # ---> NB simply this
  geom_point() + 
  labs(x = "Race date (year)", y = "Number of runners (w) of top country",
       title = "Countries with most runners (women) by year")

