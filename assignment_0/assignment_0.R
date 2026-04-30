#########################################################################################
#########################################################################################
#EXERCISE 1
#########################################################################################
#########################################################################################

# ---> basic R: we follow the exercise text.
scottish.lakes <- data.frame("Loch"="Loch Ness", "Volume_km3"=7.45, "Area_km2"=56,
                              "Length_km"=39, "MaxDepth_m"=230, "MeanDepth_m"=132)

scottish.lakes<-rbind(scottish.lakes, c("Loch Awe", 1.2, 39,	41, 94, 32))
scottish.lakes<-rbind(scottish.lakes, c("Loch Tay", 1.6, 26.4,	23, 150, 60.6))
scottish.lakes<-rbind(scottish.lakes, c("Loch Maree", 1.09, 28.6,	20, 114, 38))
scottish.lakes<-rbind(scottish.lakes, c("Loch Ericht", 1.08, 18.6,	23, 156, 57.6))
scottish.lakes<-rbind(scottish.lakes, c("Loch Lochy", 1.07, 16,	16, 162, 70))
scottish.lakes<-rbind(scottish.lakes, c("Loch Rannoch", 0.97, 19,	15.7, 134, 51))
scottish.lakes<-rbind(scottish.lakes, c("Loch Shiel", 0.79, 19.5,	28, 128, 40))
scottish.lakes<-rbind(scottish.lakes, c("Loch Katrine", 0.77, 12.4,	12.9, 151, 43.4))
scottish.lakes<-rbind(scottish.lakes, c("Loch Arkaig", 0.75, 16,	19.3, 109, 46.5))
scottish.lakes<-rbind(scottish.lakes, c("Loch Shin", 0.35, 22.5,	27.8, 49, 15.5))

scottish.lakes <- type.convert(scottish.lakes, as.is=TRUE)
summary(scottish.lakes)
#     Loch             Volume_km3       Area_km2    
# Length:13          Min.   :0.350   Min.   :12.40  
# Class :character   1st Qu.:0.790   1st Qu.:18.60  
# Mode  :character   Median :1.080   Median :22.50  
#                    Mean   :1.694   Mean   :28.62  
#                    3rd Qu.:1.600   3rd Qu.:28.60  
#                    Max.   :7.450   Max.   :71.00  
#   Lenght_km       MaxDepth_m     MeanDepth_m    
# Min.   :12.90   Min.   : 49.0   Min.   : 15.50  
# 1st Qu.:18.80   1st Qu.:114.0   1st Qu.: 38.00  
# Median :23.00   Median :150.0   Median : 46.50  
# Mean   :24.65   Mean   :152.1   Mean   : 54.66  
# 3rd Qu.:28.00   3rd Qu.:162.0   3rd Qu.: 60.60  
# Max.   :41.00   Max.   :310.0   Max.   :132.00
 
which.max(scottish.lakes$Area_km2)
#[1] 2
scottish.lakes$Loch[2]
#[1] "Loch Lomond"
scottish.lakes$Loch[which.min(scottish.lakes$Area_km2)]
#[1] "Loch Katrine"
scottish.lakes$Loch[which.min(scottish.lakes$Volume_km3)]
#[1] "Loch Shin"
scottish.lakes$Loch[which.max(scottish.lakes$Volume_km3)]
#[1] "Loch Ness"

ord <- order(scottish.lakes$Area, decreasing=TRUE)
#scottish.lakes[ord, ]
#           Loch Volume_km3 Area_km2 Length_km
#1     Loch Ness       7.45     56.0      39.0
#2      Loch Awe       1.20     39.0      41.0
#4    Loch Maree       1.09     28.6      20.0
#3      Loch Tay       1.60     26.4      23.0
#11    Loch Shin       0.35     22.5      27.8
#8    Loch Shiel       0.79     19.5      28.0
#7  Loch Rannoch       0.97     19.0      15.7
#5   Loch Ericht       1.08     18.6      23.0
#6    Loch Lochy       1.07     16.0      16.0
#10  Loch Arkaig       0.75     16.0      19.3
#9  Loch Katrine       0.77     12.4      12.9
#   MaxDepth_m MeanDepth_m
#1         230       132.0
#2          94        32.0
#4         114        38.0
#3         150        60.6
#11         49        15.5
#8         128        40.0
#7         134        51.0
#5         156        57.6
#6         162        70.0
#10        109        46.5
#9         151        43.4

scottish.lakes
### (original database is unchanged)

scottish.lakes[ord[1:2], 1]
#[1] "Loch Katrine" "Loch Lochy"  

s <- sum(scottish.lakes[,3])
s
#[1] 372



#########################################################################################
#########################################################################################
#EXERCISE 2
#########################################################################################
#########################################################################################
library(rvest)
library(tidyverse)
library(stringr)
library(ggplot2)
########################################################################################
#MEN
########################################################################################
men100m_html <- read_html("http://www.alltime-athletics.com/m_100ok.htm")
men100m_html |> html_nodes(xpath = "//pre") |> html_text()-> men100m_list
men100m_tbl <- read_fwf(men100m_list) 

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
  mutate(AthlBirthDate = str_trim(AthlBirthDate)) |>
  #---> remove any leading/trailing spaces first
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
# ---> therefore, these lines were read wrong at the beginning.
#      Easiest to look at webpage and manually change them:
men100m_tbl[1411,]$CountryCode <- "FRA"
men100m_tbl[1411,]$AthlBirthDate <- "19.05.94"
men100m_tbl[3107,]$CountryCode <- "USA"
men100m_tbl[4398,]$AthlBirthDate <- "29.02.72"
men100m_tbl[4757,]$AthlBirthDate <- "19.05.94"
men100m_tbl[4757,]$CountryCode <- "FRA"
men100m_tbl[4757,]$RaceLocation <- "La Chaux-de-Fonds"
# ---> now we convert. Since we can still some birth dates > present date, we fix that too:
men100m_tbl <- men100m_tbl |>
  mutate(AthlBirthDate = as.Date(str_trim(AthlBirthDate), format = "%d.%m.%y")) |>
  mutate(AthlBirthDate = if_else(AthlBirthDate > Sys.Date(),
                                 AthlBirthDate - years(100),  #---> subtract 100 years if wrong
                                 AthlBirthDate))

men100m_tbl <- mutate(men100m_tbl, TimeResult_s=str_remove_all(TimeResult_s, "[±A]")) 

men100m_tbl <- mutate(men100m_tbl, TimeResult_s=as.numeric(TimeResult_s))

# ---> to find out which entries are wrong
which(str_detect(men100m_tbl$WindSpeed, "[^0-9.+±-]"))
#[1] 4303 
men100m_tbl$WindSpeed[4303] <- 1.4 # ---> correct the single wrong entry (still as a charachter)
men100m_tbl <- mutate(men100m_tbl, WindSpeed=as.numeric(str_remove_all(WindSpeed, "[±A]"))) 

men100m_tbl <- mutate(men100m_tbl, AthlBirthDate=as.Date(AthlBirthDate, format="%d.%m.%y"))
# ---> specify format; y is 2-digit year

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

# ---> there are still some NAs in "WindSpeed",
#      but fine when checking webpage against the output of this:
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
ggplot(top_by_year, aes(x = RaceYear, y = n, color = CountryCode)) + 
  geom_point() + 
  labs(x = "Race date (year)", y = "Number of runners of top country",
       title = "Countries with most runners by year")

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

#---> all same as before from here.
women100m_tbl_ord <- women100m_tbl |>
  group_by(RaceYear=year(RaceDate)) |>
  summarise(best_times_by_date = min(TimeResult_s))
ggplot(women100m_tbl_ord, aes(x = RaceYear, y = best_times_by_date)) + 
  geom_point() +
  geom_line() +
  xlab("Race date (year)") + ylab("Best time (s)") +
  ggtitle("Best yearly time on 100 m (women)")

today <- as.Date("2026-03-01")
w_records <- women100m_tbl |>
  arrange(RaceDate) |>                               
  mutate(best_so_far = cummin(TimeResult_s)) |> 
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

w_top_by_year <- women100m_tbl |>
  mutate(RaceYear = year(RaceDate)) |>

  group_by(RaceYear, CountryCode) |> 
  summarise(n = n(), .groups = "drop") |> 
  group_by(RaceYear) |> 
  slice_max(n, n = 1, with_ties = FALSE)  

w_top_by_year <- w_top_by_year |>
  mutate(CountryCode = factor(CountryCode, 
                              levels = names(sort(table(CountryCode), decreasing = TRUE))))
ggplot(w_top_by_year, aes(x = RaceYear, y = n, color = CountryCode)) + 
  geom_point() + 
  labs(x = "Race date (year)", y = "Number of runners (w) of top country",
       title = "Countries with most runners (women) by year")

