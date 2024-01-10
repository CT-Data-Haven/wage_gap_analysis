# wage gap EDA

## setup

``` r
library(tidyverse)
library(showtext)

showtext_opts(dpi = 150)
sysfonts::font_add_google("Barlow Semi Condensed")

source(here::here("_utils/misc_functions.R"))
vivid <- rcartocolor::carto_pal(12, "Vivid")
pal <- setNames(c(
  vivid[c(1:8, 12)],
  vivid[1:4],
  "gray30"
  ), c(
    "Men", "Women", "White", "Black", "Latino", "API", "Other race", "Full-time", "Part-time", 
    "High school or less", "Some college or AA", "Bachelors", "Graduate degree",
    "Total"))

vir_pal <- viridisLite::turbo(n = 5)

theme_bar <- function(...) {
  camiller::theme_din(base_family = "Barlow Semi Condensed") +
    theme(...)
}

theme_set(theme_bar())

out <- list()
```

``` r
make_bars <- function(data, x = sex, y = income, fill = sex, 
                      position = position_dodge2(), 
                      y_labels = scales::label_dollar(accuracy = 1, scale = 1e-3, suffix = "k"),
                      legend = TRUE) {
  gg <- ggplot(data, aes(x = {{x}}, y = {{y}}, fill = {{fill}}))
  gg <- gg + geom_col(position = position, alpha = 0.9)
  gg <- gg + scale_fill_manual(values = pal) 
  gg <- gg + stylehaven::scale_y_barcontinuous(labels = y_labels)
  gg <- gg + facet_grid(cols = vars(name), scales = "free_x")
  if (!legend) {
    gg <- gg + theme(legend.position = "none")
  }
  gg
}

make_dots <- function(data, x = race, y = income, color = sex, 
                      y_labels = scales::label_dollar(accuracy = 1, scale = 1e-3, suffix = "k"),
                      legend = TRUE, flip = TRUE) {
  gg <- ggplot(data, aes(x = {{x}}, y = {{y}}, color = {{color}}))
  gg <- gg + geom_path(color = "gray80", linewidth = 3)
  gg <- gg + geom_point(size = 5)
  gg <- gg + scale_color_manual(values = pal) 
  gg <- gg + scale_y_continuous(labels = y_labels)
  # gg <- gg + facet_grid(cols = vars(name), scales = "free_x")
  if (!legend) {
    gg <- gg + theme(legend.position = "none")
  }
  if (flip) {
    gg <- gg + coord_flip()
  }
  gg
}

make_quants <- function(data, x = sex, quant = c("q20", "q25", "q50", "q75", "q80"), facet = race,
                        y_labels = scales::label_dollar(accuracy = 1, scale = 1e-3, suffix = "k"),
                        legend = TRUE, flip = FALSE) {
  data |>
    mutate(quant_num = parse_number(as.character(quant))) |>
    mutate(bracket = 100 - abs(quant_num - 50))
  gg <- ggplot(data, aes(x = {{x}}, y = income, color = quant, group = {{x}}))
  gg <- gg + geom_path(linewidth = 3, color = "gray80")
  gg <- gg + geom_point(aes(size = bracket), alpha = 0.8)
  gg <- gg + scale_y_continuous(labels = y_labels)
  gg <- gg + scale_color_viridis_d(option = "turbo", direction = -1)
  gg <- gg + scale_size_continuous(range = c(5, 10), guide = guide_none())
  gg <- gg + facet_grid(cols = vars({{facet}}))  
  if (flip) {
    gg <- gg + coord_flip()
  }
  gg
}
```

force NA values for very small groups–only affects calcs by occupation
group (production, natural resources, military)

``` r
wages <- readRDS(here::here("input_data/income_tbls.rds")) |>
  bind_rows(.id = "dim") |>
  separate(dim, into = c("univ", "dim"), sep = "\\.") |>
  mutate(time = ifelse(is_fulltime | univ == "full_time", "Full-time", "Part-time")) |>
  mutate(edu = as.character(edu)) |>
  mutate(across(c(sex, race, occ_group, time, edu), \(x) replace_na(x, "Total"))) |>
  select(univ, dim, name, time, edu, sex, race, occ_group, count, sample_n, everything(), -is_fulltime) |>
  mutate(across(c(univ, dim, name, time, edu, sex, race, occ_group), as_factor)) |>
  mutate(across(c(time, edu, sex, race, occ_group), \(x) fct_relevel(x, "Total"))) |>
  mutate(univ = fct_relevel(univ, "all_time") |> fct_relabel(str_remove, "_time")) |>
  mutate(dim = fct_relevel(dim, "total", "sex", "time", "sex_x_time", "race", "edu", "occ") |>
           fct_relevel("sex_x_edu_x_occ", after = Inf),
         sex = fct_relevel(sex, "Total", "Male") |> fct_recode(Men = "Male", Women = "Female"),
         race = fct_relevel(race, "Total", "White", "Black", "Latino") |> fct_recode("Other race" = "Other")) |>
  mutate(is_small = sample_n < 30 | count < 100) |>
  filter(name %in% c("US", "Connecticut")) |>
  mutate(across(where(is.factor), fct_drop))

summary(wages)
```

       univ                  dim               name            time    
     all : 18   sex_x_edu_x_occ:143   US         :157   Total    :  6  
     full:295   race_x_edu     : 40   Connecticut:156   Full-time:301  
                sex_x_occ      : 36                     Part-time:  6  
                race_x_sex     : 20                                    
                occ            : 18                                    
                sex_x_edu      : 16                                    
                (Other)        : 40                                    
                      edu         sex              race    
     Total              :106   Total: 82   Total     :243  
     High school or less: 52   Men  :116   White     : 14  
     Some college or AA : 52   Women:115   Black     : 14  
     Bachelors          : 52               Latino    : 14  
     Graduate degree    : 51               API       : 14  
                                           Other race: 14  
                                                           
                                                    occ_group       count          
     Total                                               :116   Min.   :       16  
     Computer, Engineering, and Science                  : 22   1st Qu.:    25214  
     Education, Legal, Community Service, Arts, and Media: 22   Median :   226500  
     Healthcare Practitioners and Technical              : 22   Mean   :  4988851  
     Management, Business, and Financial                 : 22   3rd Qu.:  2799982  
     Natural Resources, Construction, and Maintenance    : 22   Max.   :148789069  
     (Other)                                             : 87                      
        sample_n          count_se        earn_q20        earn_q25     
     Min.   :      1   Min.   :   16   Min.   : 5526   Min.   :  7500  
     1st Qu.:   1229   1st Qu.: 1009   1st Qu.:28737   1st Qu.: 31401  
     Median :  10978   Median : 2883   Median :35588   Median : 39016  
     Mean   : 228134   Mean   : 8001   Mean   :39256   Mean   : 43142  
     3rd Qu.: 127631   3rd Qu.:10877   3rd Qu.:48553   3rd Qu.: 52988  
     Max.   :6882061   Max.   :56492   Max.   :95378   Max.   :105976  
                                                                       
        earn_q50         earn_q75         earn_q80       earn_q20_se     
     Min.   : 16184   Min.   : 31611   Min.   : 37247   Min.   :    0.0  
     1st Qu.: 45845   1st Qu.: 68000   1st Qu.: 75000   1st Qu.:  123.2  
     Median : 58264   Median : 84781   Median : 94203   Median :  351.5  
     Mean   : 62927   Mean   : 93196   Mean   :103534   Mean   :  916.6  
     3rd Qu.: 75243   3rd Qu.:110526   3rd Qu.:121579   3rd Qu.: 1136.8  
     Max.   :160000   Max.   :361378   Max.   :430000   Max.   :10624.0  
                                                        NA's   :7        
      earn_q25_se       earn_q50_se       earn_q75_se     earn_q80_se   
     Min.   :    0.0   Min.   :    0.0   Min.   :    0   Min.   :    0  
     1st Qu.:  124.5   1st Qu.:  183.0   1st Qu.:  271   1st Qu.:  296  
     Median :  365.0   Median :  543.5   Median :  805   Median :  980  
     Mean   :  978.4   Mean   : 1416.9   Mean   : 2053   Mean   : 2529  
     3rd Qu.: 1239.0   3rd Qu.: 1420.8   3rd Qu.: 2412   3rd Qu.: 2740  
     Max.   :14305.0   Max.   :24794.0   Max.   :24552   Max.   :69006  
     NA's   :6         NA's   :5         NA's   :7       NA's   :6      
      is_small      
     Mode :logical  
     FALSE:303      
     TRUE :10       
                    
                    
                    
                    

``` r
count(wages, univ, dim)
```

    # A tibble: 13 × 3
       univ  dim                 n
       <fct> <fct>           <int>
     1 all   total               2
     2 all   sex                 4
     3 all   time                4
     4 all   sex_x_time          8
     5 full  sex                 4
     6 full  race               10
     7 full  edu                 8
     8 full  occ                18
     9 full  race_x_edu         40
    10 full  race_x_sex         20
    11 full  sex_x_edu          16
    12 full  sex_x_occ          36
    13 full  sex_x_edu_x_occ   143

``` r
distros <- readRDS(here::here("input_data/income_hists.rds")) |>
  bind_rows(.id = "dim") |>
  mutate(across(c(sex, race), \(x) replace_na(x, "Total"))) |>
  select(dim, name, bin, sex, race, count, sample_n, everything()) |>
  mutate(across(c(dim, name, sex, race), as_factor)) |>
  mutate(sex = fct_relevel(sex, "Total", "Male") |> fct_recode(Men = "Male", Women = "Female"),
         race = fct_relevel(race, "Total", "White", "Black", "Latino") |> fct_recode("Other race" = "Other")) |>
  filter(name %in% c("US", "Connecticut"))

summary(distros)
```

             dim               name                bin         sex     
     sex       :163   Connecticut:967   [0,10000)    : 17   Total:292  
     race      :292   Maryland   :  0   [10000,20000): 17   Men  :371  
     race_x_sex:512                     [20000,30000): 17   Women:304  
                                        [30000,40000): 17              
                                        [40000,50000): 17              
                                        [50000,60000): 17              
                                        (Other)      :865              
             race         count          sample_n         count_se     
     Total     :163   Min.   :    2   Min.   :   1.0   Min.   :   2.0  
     White     :249   1st Qu.:   38   1st Qu.:   2.0   1st Qu.:  27.0  
     Black     :111   Median :  220   Median :  11.0   Median :  78.0  
     Latino    :138   Mean   : 3691   Mean   : 174.7   Mean   : 207.3  
     API       :171   3rd Qu.: 1753   3rd Qu.:  85.5   3rd Qu.: 238.0  
     Other race:135   Max.   :87280   Max.   :4353.0   Max.   :1755.0  
                                                                       
         lower        
     Min.   :      0  
     1st Qu.: 140000  
     Median : 290000  
     Mean   : 345698  
     3rd Qu.: 480000  
     Max.   :1000000  
                      

## median individual earnings, adults 25+ with positive earnings

### full- and part-time

#### total wages, CT vs US

``` r
out[["total"]] <- wages |>
  filter(univ == "all",
         dim == "total") |>
  select(univ:occ_group, income = earn_q50)

make_bars(out$total, legend = FALSE)
```

![](wage_gap_eda_files/figure-commonmark/unnamed-chunk-5-1.png)

#### wages by sex

``` r
out[["all_by_sex"]] <- wages |>
  filter(univ == "all",
         dim == "sex") |>
  select(univ:occ_group, income = earn_q50)

make_bars(out$all_by_sex, legend = FALSE)
```

![](wage_gap_eda_files/figure-commonmark/unnamed-chunk-6-1.png)

#### wages by time

``` r
out[["all_by_time"]] <- wages |>
  filter(dim == "time") |>
  select(univ:occ_group, income = earn_q50)

make_bars(out$all_by_time, x = time, fill = time, legend = FALSE)
```

![](wage_gap_eda_files/figure-commonmark/unnamed-chunk-7-1.png)

#### employment by sex x time

women more likely work part-time

``` r
out[["share_by_time"]] <- wages |>
  filter(univ == "all",
         dim == "sex_x_time") |>
  group_by(univ, dim, name, edu, sex, race, occ_group) |>
  mutate(share = stylehaven::round_sum100((count / sum(count)) * 100) / 100) |>
  ungroup() |>
  select(univ:occ_group, share)

make_bars(out$share_by_time, y = share, fill = fct_rev(time), position = position_fill(), 
            y_labels = scales::label_percent())
```

![](wage_gap_eda_files/figure-commonmark/unnamed-chunk-8-1.png)

#### wages by sex x time

``` r
out[["all_by_sex_x_time"]] <- wages |>
  filter(univ == "all",
         dim == "sex_x_time") |>
  select(univ:occ_group, income = earn_q50)

make_bars(out$all_by_sex_x_time, x = time)
```

![](wage_gap_eda_files/figure-commonmark/unnamed-chunk-9-1.png)

### full-time only

#### wages by sex

``` r
out[["full_by_sex"]] <- wages |>
  filter(univ == "full",
         dim == "sex") |>
  select(univ:occ_group, income = earn_q50)

make_bars(out$full_by_sex, legend = FALSE)
```

![](wage_gap_eda_files/figure-commonmark/unnamed-chunk-10-1.png)

#### wages by race

``` r
out[["full_by_race"]] <- wages |>
  filter(univ == "full",
         dim == "race") |>
  select(univ:occ_group, income = earn_q50)

make_bars(out$full_by_race, x = race, fill = race, legend = FALSE)
```

![](wage_gap_eda_files/figure-commonmark/unnamed-chunk-11-1.png)

#### wages by edu

``` r
out[["full_by_edu"]] <- wages |>
  filter(univ == "full",
         dim == "edu") |>
  select(univ:occ_group, income = earn_q50)

make_bars(out$full_by_edu, x = edu, fill = edu, legend = FALSE) +
  scale_x_discrete(labels = scales::label_wrap(10))
```

![](wage_gap_eda_files/figure-commonmark/unnamed-chunk-12-1.png)

#### wages by sex x race

``` r
out[["full_by_race_x_sex"]] <- wages |>
  filter(univ == "full",
         dim %in% c("sex", "race_x_sex")) |>
  select(univ:occ_group, income = earn_q50) |>
  mutate(race = fct_reorder(race, income, .fun = max)) |>
  arrange(race)

out$full_by_race_x_sex |>
  filter(name == "Connecticut") |>
  make_dots()
```

![](wage_gap_eda_files/figure-commonmark/unnamed-chunk-13-1.png)

#### wages by sex x edu

``` r
out[["full_by_sex_x_edu"]] <- wages |>
  filter(univ == "full",
         dim %in% c("sex", "sex_x_edu")) |>
  select(univ:occ_group, income = earn_q50) |>
  mutate(edu = fct_reorder(edu, income, .fun = max)) |>
  arrange(edu)

out$full_by_sex_x_edu |>
  filter(name == "Connecticut") |>
  make_dots(x = edu)
```

![](wage_gap_eda_files/figure-commonmark/unnamed-chunk-14-1.png)

#### wages by occupation

``` r
out[["full_by_occ"]] <- wages |>
  filter(univ == "full",
         dim == "occ",
         !is_small) |>
  select(univ:occ_group, income = earn_q50) |>
  mutate(occ_group = fct_reorder(occ_group, income, .fun = max)) |>
  arrange(occ_group)

out$full_by_occ |>
  filter(name == "Connecticut") |>
  make_dots(x = occ_group, legend = FALSE) +
  scale_x_discrete(labels = \(x) substring(x, 1, 25))
```

    `geom_path()`: Each group consists of only one observation.
    ℹ Do you need to adjust the group aesthetic?

![](wage_gap_eda_files/figure-commonmark/unnamed-chunk-15-1.png)

#### employment by occupation x sex

``` r
wages |>
  filter(univ == "full",
         name == "Connecticut",
         dim == "sex_x_occ") |>
  group_by(occ_group) |>
  filter(!any(is_small)) |>
  ungroup() |>
  select(univ, dim, name, time, occ_group, sex, count) |>
  group_by(across(univ:occ_group)) |>
  mutate(share = count / sum(count)) |>
  filter(!any(share == 0)) |>
  ungroup() |>
  mutate(sex = fct_rev(sex)) |>
  arrange(sex, share) |>
  mutate(occ_group = fct_reorder2(occ_group, sex, share)) |>
  ggplot(aes(x = occ_group, y = share, fill = sex)) +
  geom_col(position = position_fill(reverse = TRUE), width = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  coord_flip() +
  scale_x_discrete(labels = \(x) substring(x, 1, 25)) +
  scale_fill_manual(values = pal)
```

![](wage_gap_eda_files/figure-commonmark/unnamed-chunk-16-1.png)

``` r
wages |>
  filter(univ == "full",
         name == "Connecticut",
         dim == "sex_x_occ") |>
  group_by(occ_group) |>
  filter(!any(is_small)) |>
  ungroup() |>
  group_by(c(occ_group)) |>
  mutate(share = count / sum(count)) |>
  filter(!any(share == 0)) |>
  ungroup() |>
  filter(sex == "Women") |>
  ggplot(aes(x = share, y = earn_q50, size = count)) +
  geom_point(alpha = 0.8) +
  scale_size_area()
```

![](wage_gap_eda_files/figure-commonmark/unnamed-chunk-16-2.png)

#### wages by occupation x sex

``` r
out[["full_by_sex_x_occ"]] <- wages |>
  filter(univ == "full",
         dim == "sex_x_occ") |>
  group_by(name, occ_group) |>
  filter(!any(is_small)) |>
  ungroup() |>
  select(univ:occ_group, income = earn_q50) |>
  mutate(occ_group = fct_reorder(occ_group, income, .fun = max)) |>
  arrange(occ_group)

make_dots(filter(out$full_by_sex_x_occ, name == "Connecticut"), x = occ_group) +
  scale_x_discrete(labels = \(x) substring(x, 1, 25))
```

![](wage_gap_eda_files/figure-commonmark/unnamed-chunk-17-1.png)

#### wages by occupation x sex x edu

``` r
out[["full_by_sex_x_edu_x_occ"]] <- wages |>
  filter(univ == "full",
         dim == "sex_x_edu_x_occ") |>
  group_by(name, occ_group) |>
  filter(!any(is_small)) |>
  ungroup() |>
  select(univ:occ_group, income = earn_q50) |>
  mutate(occ_group = fct_reorder(occ_group, income, .fun = max)) |>
  arrange(occ_group)

out$full_by_sex_x_edu_x_occ |>
  filter(name == "Connecticut",
         edu %in% c("High school or less", "Graduate degree")) |>
  ggplot(aes(x = edu, y = income, color = sex, shape = edu, group = edu)) +
  geom_path(color = "gray80", linewidth = 2) +
  geom_point(size = 4) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-3, suffix = "k")) +
  scale_color_manual(values = pal) +
  coord_flip() +
  facet_grid(rows = vars(fct_rev(occ_group)), switch = "y", 
             labeller = labeller(.rows = \(x) substring(x, 1, 25))) +
  theme(axis.text.y = element_blank(),
        strip.text.y.left = element_text(angle = 0, hjust = 1))
```

![](wage_gap_eda_files/figure-commonmark/unnamed-chunk-18-1.png)

## distribution of individual earnings

``` r
out[["full_quant_by_race_x_sex"]] <- wages |>
  filter(name == "Connecticut",
         univ == "full",
         dim %in% c("sex", "race_x_sex")) |>
  pivot_longer(matches("^earn_q\\d+$"), names_to = c(NA, "quant"), names_sep = "_") |>
  select(univ:occ_group, quant, income = value)

out$full_quant_by_race_x_sex |>
  mutate(quant_num = parse_number(quant)) |>
  mutate(bracket = 50 - abs(50 - quant_num)) |>
  make_quants()
```

![](wage_gap_eda_files/figure-commonmark/unnamed-chunk-19-1.png)

Full-time working Black & Latina women’s 80th percentile incomes are
lower than white & Asian men’s medians

``` r
wages |>
  filter(name == "Connecticut",
         (univ == "full" & dim %in% c("sex", "race_x_sex")) | (univ == "all" & dim %in% c("total", "time"))) |>
  mutate(ratio_80_20 = earn_q80 / earn_q20) |>
  select(univ, sex, race, earn_q20, earn_q80, ratio_80_20) |>
  arrange(desc(ratio_80_20)) |>
  print(n = Inf)
```

    # A tibble: 15 × 6
       univ  sex   race       earn_q20 earn_q80 ratio_80_20
       <fct> <fct> <fct>         <dbl>    <dbl>       <dbl>
     1 all   Total Total          7600    48749        6.41
     2 all   Total Total         22105   100483        4.55
     3 full  Women API           37764   127171        3.37
     4 full  Men   API           46629   150000        3.22
     5 full  Women Other race    32448   103857        3.20
     6 full  Men   Total         41868   130837        3.12
     7 all   Total Total         38684   115136        2.98
     8 full  Men   White         49400   146537        2.97
     9 full  Men   Other race    39600   117158        2.96
    10 full  Women Total         35479   100000        2.82
    11 full  Men   Latino        28737    80922        2.82
    12 full  Women Latino        25421    68884        2.71
    13 full  Men   Black         33158    88421        2.67
    14 full  Women White         40271   105976        2.63
    15 full  Women Black         30211    77455        2.56

ratios to median men’s pay are higher at lower levels except for API
women, i.e. gaps are wider at higher incomes, but not huge differences.

``` r
distros |>
  filter(sex != "Total") |>
  # filter(race == "Total") |>
  group_by(name, sex, race) |>
  mutate(share = count / sum(count)) |>
  ggplot(aes(x = lower, y = share, fill = sex)) +
  geom_col(position = position_identity(), alpha = 0.4) +
  # scale_x_log10() +
  facet_grid(rows = vars(race)) +
  scale_fill_manual(values = pal, aesthetics = c("fill", "color"))
```

![](wage_gap_eda_files/figure-commonmark/unnamed-chunk-21-1.png)

lol I spent a bunch of time figuring this out and it’s kinda useless

## output

``` r
out_tbls <- out |>
  map(function(df) {
    if (unique(df$univ) == "full") {
      df |> filter(name == "Connecticut")
    } else {
      df
    }
  })

saveRDS(out_tbls, here::here("output_data/wage_gap_tbls.rds"))
jsonlite::write_json(out_tbls, here::here("output_data/wage_gap_tbls.json"))
```
