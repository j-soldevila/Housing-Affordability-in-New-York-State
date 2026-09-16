state_housing_affordablity_data
================
Jorge R. Soldevila Irizarry
2026-09-15

# This scripts downloads and calculates Housing Costs for Units with a Mortgage and without a Mortgage for Both Hispanics and Total Population in NYS.

## The data used for the analysis comes from table B25140 and B25140I, S20506, S207, and DP04 from the US Census Bureau.

## Total Population

### Cost Burden

``` r
# To obtain data from the U.S. Census we use tidycensus and the get_acs() function.

st_burden <- get_acs(
  geography = "state",
  state = "NY",
  year = 2024,
  survey = "acs5",
  table = "B25140",
  geometry = TRUE,
  output = "wide"
) %>%
  select(-ends_with("M"))
```

### Display Burden data

### Monthly Costs for Units with a Mortgage

``` r
st_cost_wth_mort <- get_acs(
  geography = "state",
  state = "NY",
  year = 2024,
  survey = "acs5",
  table = "S2506",
  geometry = TRUE,
  output = "wide"
) %>%
  select(-ends_with("M"))
```

### Monthly Costs for Units without a Mortgage

``` r
st_cost_wth_no_mort <- get_acs(
  geography = "state",
  state = "NY",
  year = 2024,
  survey = "acs5",
  table = "S2507",
  geometry = TRUE,
  output = "wide"
) %>%
  select(-ends_with("M"))
```

### Housing Characteristics

``` r
st_hous_charc <- get_acs(
  geography = "state",
  state = "NY",
  year = 2024,
  survey = "acs5",
  table = "DP04",
  geometry = TRUE,
  output = "wide"
) %>%
  select(-ends_with("M"))
```

## Hispanic Population

``` r
hisp_st_burden <- get_acs(
  geography = "state",
  state = "NY",
  year = 2024,
  survey = "acs5",
  table = "B25140I",
  geometry = TRUE,
  output = "wide"
) %>%
  select(-ends_with("M"))
```
