Stats 433 HW 3
================
2022-10-13

URL of Github: <https://github.com/tzhang479/STAT-433-HW1/tree/main>

HW 3

Due sometime in Week 6. In r4ds flights… What time of day should you fly
if you want to avoid delays as much as possible? Does this choice depend
on anything? Season? Weather? Airport? Airline? Find three patterns
(“null results” are ok!). Write your results into Rmarkdown. Include a
short introduction that summarizes the three results. Then, have a
section for each finding. Support each finding with data summaries and
visualizations. Include your code when necessary. This shouldn’t be
long, but it might take some time to find the things you want to talk
about and lay them out in an orderly way.

Answer:

In addition to “hour”, the time (we only account hour here) that the
flight expected to departure, three factors I am going to explore are
“carrier”, the carrier of the flight, “weather” (“temp”), the
temperature of the departure place, and “wind_speed”, the wind speed of
the departure place. I want to find whether there is a relationship
between the arrival delay time and the three factors in order to avoid
delays as much as possible.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(nycflights13)
library(ggplot2)
flights
```

    ## # A tibble: 336,776 × 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # … with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
flights %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)
```

    ## # A tibble: 20 × 2
    ##     hour arr_delay
    ##    <dbl>     <dbl>
    ##  1     7    -5.30 
    ##  2     5    -4.80 
    ##  3     6    -3.38 
    ##  4     9    -1.45 
    ##  5     8    -1.11 
    ##  6    10     0.954
    ##  7    11     1.48 
    ##  8    12     3.49 
    ##  9    13     6.54 
    ## 10    14     9.20 
    ## 11    23    11.8  
    ## 12    15    12.3  
    ## 13    16    12.6  
    ## 14    18    14.8  
    ## 15    22    16.0  
    ## 16    17    16.0  
    ## 17    19    16.7  
    ## 18    20    16.7  
    ## 19    21    18.4  
    ## 20     1   NaN

``` r
flights %>%
  group_by(hour) %>%
    summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = hour, y = arr_delay)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)
```

    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 1 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](Stats-433-HW-3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Based on the list and the plot, we find that there is a positive
relationship between the scheduled departure time and the arrival delay
time; in other words, generally, the earlier the scheduled departure
time is, the less arrival delay time is.

``` r
flights %>%
  group_by(carrier) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)
```

    ## # A tibble: 16 × 2
    ##    carrier arr_delay
    ##    <chr>       <dbl>
    ##  1 AS         -9.93 
    ##  2 HA         -6.92 
    ##  3 AA          0.364
    ##  4 DL          1.64 
    ##  5 VX          1.76 
    ##  6 US          2.13 
    ##  7 UA          3.56 
    ##  8 9E          7.38 
    ##  9 B6          9.46 
    ## 10 WN          9.65 
    ## 11 MQ         10.8  
    ## 12 OO         11.9  
    ## 13 YV         15.6  
    ## 14 EV         15.8  
    ## 15 FL         20.1  
    ## 16 F9         21.9

``` r
flights %>%
  group_by(carrier) %>%
    summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = carrier, y = arr_delay)) +
  geom_point()
```

![](Stats-433-HW-3_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Based on the list and the plot, we find that AS, HA, and AA have a
better performance overall; in other words, these three carriers are my
first option.

``` r
flights %>%
  left_join(weather) %>%
  group_by(temp) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)
```

    ## Joining, by = c("year", "month", "day", "origin", "hour", "time_hour")

    ## # A tibble: 169 × 2
    ##     temp arr_delay
    ##    <dbl>     <dbl>
    ##  1  46.6    -21   
    ##  2  50.4    -18.2 
    ##  3  58.1    -17.5 
    ##  4  47.8    -15.9 
    ##  5  42.3    -12.5 
    ##  6  52.7    -11.8 
    ##  7  50.5    -10.9 
    ##  8  49.8    -10.6 
    ##  9  51.3     -9.82
    ## 10  55.6     -8.73
    ## # … with 159 more rows

``` r
flights %>%
  left_join(weather) %>%
  group_by(temp) %>%
    summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = temp, y = arr_delay)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)
```

    ## Joining, by = c("year", "month", "day", "origin", "hour", "time_hour")
    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 1 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](Stats-433-HW-3_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Based on the list and the plot, we find that there is a weak positive
relationship between the temperature of the departure place and the
arrival delay time; but from the plot, the temperature between 45 to 50
seem to have a better performance; in other words, generally,
temperature of the departure place has almost no impact on my choice;
but I am more willing to fly when the temperature of the departure place
is between 45 to 50.

``` r
flights %>%
  left_join(weather) %>%
  group_by(wind_speed) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)
```

    ## Joining, by = c("year", "month", "day", "origin", "hour", "time_hour")

    ## # A tibble: 35 × 2
    ##    wind_speed arr_delay
    ##         <dbl>     <dbl>
    ##  1      38.0      0.533
    ##  2       5.75     2.84 
    ##  3       0        3.26 
    ##  4       3.45     3.54 
    ##  5       4.60     3.68 
    ##  6       6.90     4.60 
    ##  7      36.8      5    
    ##  8       9.21     5.70 
    ##  9      11.5      5.81 
    ## 10      10.4      6.17 
    ## # … with 25 more rows

``` r
flights %>%
  left_join(weather) %>%
  group_by(wind_speed) %>%
    summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = wind_speed, y = arr_delay)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)
```

    ## Joining, by = c("year", "month", "day", "origin", "hour", "time_hour")
    ## `geom_smooth()` using formula 'y ~ x'

    ## Warning: Removed 1 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 1 rows containing missing values (geom_point).

![](Stats-433-HW-3_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Based on the list and the plot, we find that there is a positive
relationship between the wind speed of the departure place and the
arrival delay time; but when wind speed is larger than 25, the arrival
delay time are uncertain; in other words, generally, I am more willing
to fly when the wind speed is high but less than 25.

In conclusion, I would like to choose the earliest flight in the morning
with carrier of AS when the temperature of the departure place is
between 45 to 50 while the wind speed is about 25.
