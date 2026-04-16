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
     Loch             Volume_km3       Area_km2    
 Length:13          Min.   :0.350   Min.   :12.40  
 Class :character   1st Qu.:0.790   1st Qu.:18.60  
 Mode  :character   Median :1.080   Median :22.50  
                    Mean   :1.694   Mean   :28.62  
                    3rd Qu.:1.600   3rd Qu.:28.60  
                    Max.   :7.450   Max.   :71.00  
   Lenght_km       MaxDepth_m     MeanDepth_m    
 Min.   :12.90   Min.   : 49.0   Min.   : 15.50  
 1st Qu.:18.80   1st Qu.:114.0   1st Qu.: 38.00  
 Median :23.00   Median :150.0   Median : 46.50  
 Mean   :24.65   Mean   :152.1   Mean   : 54.66  
 3rd Qu.:28.00   3rd Qu.:162.0   3rd Qu.: 60.60  
 Max.   :41.00   Max.   :310.0   Max.   :132.00
 
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

ord <- order(scottish.lakes$Area)
scottish.lakes[ord, ]
#           Loch Volume_km3 Area_km2 Lenght_km MaxDepth_m
#11 Loch Katrine       0.77     12.4      12.9        151
#8    Loch Lochy       1.07     16.0      16.0        162
#12  Loch Arkaig       0.75     16.0      19.3        109
#7   Loch Ericht       1.08     18.6      23.0        156
#9  Loch Rannoch       0.97     19.0      15.7        134
#10   Loch Shiel       0.79     19.5      28.0        128
#13    Loch Shin       0.35     22.5      27.8         49
#4      Loch Tay       1.60     26.4      23.0        150
#3    Loch Morar       2.30     27.0      18.8        310
#6    Loch Maree       1.09     28.6      20.0        114
#5      Loch Awe       1.20     39.0      41.0         94
#1     Loch Ness       7.45     56.0      39.0        230
#2   Loch Lomond       2.60     71.0      36.0        190
#   MeanDepth_m
#11        43.4
#8         70.0
#12        46.5
#7         57.6
#9         51.0
#10        40.0
#13        15.5
#4         60.6
#3         87.0
#6         38.0
#5         32.0
#1        132.0
#2         37.0

scottish.lakes
### (original database is unchanged)

scottish.lakes[ord[1:2], 1]
#[1] "Loch Katrine" "Loch Lochy"  

s <- sum(scottish.lakes[,3])
s
#[1] 372