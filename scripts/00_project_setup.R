# 00_project_setup.R

# 1. Load Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidycensus,sf,glue,here,leaflet)

#===============================================================================
# Housing Affordability Function
#===============================================================================

build_housing_affordability <- function(
    geography,
    state,
    year,
    survey = "acs5"
) {
  
  #--------------------------------------------------------------------------
  # Total Population Burden
  #--------------------------------------------------------------------------
  
  burden <- tidycensus::get_acs(
    geography = geography,
    state = state,
    year = year,
    survey = survey,
    table = "B25140",
    geometry = TRUE,
    output = "wide"
  ) %>%
    dplyr::mutate(
      mort_30pct   = round(B25140_003E / B25140_002E * 100, 2),
      mort_50pct   = round(B25140_004E / B25140_002E * 100, 2),
      nomort_30pct = round(B25140_007E / B25140_006E * 100, 2),
      nomort_50pct = round(B25140_008E / B25140_006E * 100, 2),
      rent_30pct   = round(B25140_011E / B25140_010E * 100, 2),
      rent_50pct   = round(B25140_012E / B25140_010E * 100, 2),
      tot_30pct    = round(
        (B25140_003E + B25140_007E + B25140_011E) /
          B25140_001E * 100,2),
      tot_50pct    = round(
        (B25140_004E + B25140_008E + B25140_012E) /
          B25140_001E * 100,2)
    ) %>%
    dplyr::select(
      GEOID,
      NAME,
      mort_30pct,
      mort_50pct,
      nomort_30pct,
      nomort_50pct,
      rent_30pct,
      rent_50pct,
      tot_30pct,
      tot_50pct,
      geometry
    )
  
  #--------------------------------------------------------------------------
  # Mortgage Costs
  #--------------------------------------------------------------------------
  
  cost_mort <- tidycensus::get_acs(
    geography = geography,
    state = state,
    year = year,
    survey = survey,
    table = "S2506",
    geometry = FALSE,
    output = "wide"
  ) %>%
    dplyr::mutate(
      ratio_less4 = round(S2506_C01_027E/S2506_C01_001E*100,2),
      ratio_over4 = round(S2506_C01_028E/S2506_C01_001E*100,2)
    ) %>%
    dplyr::select(
      GEOID,
      S2506_C01_001E, # Total Owner Occupied Housing Units with Mortgage
      S2506_C01_009E, # Owner Occupied Housing Units with Mortgage Median Home Value
      S2506_C01_024E, # Owner Occupied Housing Units with Mortgage Median Household Income
      S2506_C01_027E, # Home Value to Household Income Ratio 3 to 3.9
      S2506_C01_028E, # Home Value to Household Income Ratio 4 or >
      S2506_C01_040E, # Owner Occupied Housing Units with Mortgage Monthly Housing Costs in Median Dollars
      S2506_C02_066E,  # Owner Occupied Housing Units with Mortgage Median Real Estate Taxes
      ratio_less4,
      ratio_over4
    )
  
  #--------------------------------------------------------------------------
  # No Mortgage Costs
  #--------------------------------------------------------------------------
  
  cost_nomort <- tidycensus::get_acs(
    geography = geography,
    state = state,
    year = year,
    survey = survey,
    table = "S2507",
    geometry = FALSE,
    output = "wide"
  ) %>%
    dplyr::mutate(
      ratio_less4 = round(S2507_C01_022E/S2507_C01_001E*100,2),
      ratio_over4 = round(S2507_C01_023E/S2507_C01_001E*100,2)
    ) %>%
    dplyr::select(
      GEOID,
      S2507_C01_001E, # Total Owner Occupied Housing Units without Mortgage
      S2507_C01_010E, # Owner Occupied Housing Units without Mortgage Median Home Value
      S2507_C01_019E, # Owner Occupied Housing Units without Mortgage Median Household Income
      S2507_C01_022E, # Home Value to Household Income Ratio 3 to 3.9
      S2507_C01_023E, # Home Value to Household Income Ratio 4 or >
      S2507_C01_032E, # Owner Occupied Housing Units without Mortgage Monthly Housing Costs in Median Dollars
      S2507_C02_058E,  # Owner Occupied Housing Units without Mortgage Median Real Estate Taxes
      ratio_less4,
      ratio_over4
    )
  
  #--------------------------------------------------------------------------
  # Housing Characteristics
  #--------------------------------------------------------------------------
  
  hous_char <- tidycensus::get_acs(
    geography = geography,
    state = state,
    year = year,
    survey = survey,
    table = "DP04",
    geometry = FALSE,
    output = "wide"
  ) %>%
    dplyr::select(
      GEOID,
      DP04_0001E, # Total Housing Units
      DP04_0002E, # Occupied Housing Units
      DP04_0003E, # Vacant Housing Units
      DP04_0004E, # Homeowner Vacancy Rate
      DP04_0005E, # Renter Vacancy Rate
      DP04_0046E, # Total Owner Occupied Housing Units
      DP04_0047E, # Total Renter Occupied Housing Units
      DP04_0101E, # Median SMOC with Mortgage
      DP04_0109E, # Median SMOC without Mortgage
      DP04_0134E   # Median Gross Rent
    )
  
  #--------------------------------------------------------------------------
  # Hispanic Cost Burden
  #--------------------------------------------------------------------------
  
  hisp_burden <- tidycensus::get_acs(
    geography = geography,
    state = state,
    year = year,
    survey = survey,
    table = "B25140I",
    geometry = FALSE,
    output = "wide"
  ) %>%
    dplyr::mutate(
      hisp_mort_30pct   = round(B25140I_003E / B25140I_002E * 100, 2),
      hisp_mort_50pct   = round(B25140I_004E / B25140I_002E * 100, 2),
      hisp_nomort_30pct = round(B25140I_007E / B25140I_006E * 100, 2),
      hisp_nomort_50pct = round(B25140I_008E / B25140I_006E * 100, 2),
      hisp_rent_30pct   = round(B25140I_011E / B25140I_010E * 100, 2),
      hisp_rent_50pct   = round(B25140I_012E / B25140I_010E * 100, 2),
      hisp_tot_30pct    = round(
        (B25140I_003E + B25140I_007E + B25140I_011E) /
          B25140I_001E * 100,2),
      hisp_tot_50pct    = round(
        (B25140I_004E + B25140I_008E + B25140I_012E) /
          B25140I_001E * 100,2)
    ) %>%
    dplyr::select(
      GEOID,
      starts_with("hisp_")
    )
  
  #--------------------------------------------------------------------------
  # Hispanic Majority / Plurality
  #--------------------------------------------------------------------------
  
  hisp_calc <- tidycensus::get_acs(
    geography = geography,
    state = state,
    year = year,
    survey = survey,
    table = "DP05",
    geometry = FALSE,
    output = "wide"
  ) %>%
    dplyr::mutate(
      pct_hisp = DP05_0090E / DP05_0089E * 100,
      pct_wht  = DP05_0096E / DP05_0089E * 100,
      pct_blk  = DP05_0097E / DP05_0089E * 100,
      pct_aian = DP05_0098E / DP05_0089E * 100,
      pct_asi  = DP05_0099E / DP05_0089E * 100,
      pct_oth  = DP05_0101E / DP05_0089E * 100,
      
      hisp_maj = dplyr::case_when(
        pct_hisp > 50 ~ "majority",
        TRUE ~ "not majority"
      ),
      
      largest_other = pmax(
        pct_wht,
        pct_blk,
        pct_aian,
        pct_asi,
        pct_oth,
        na.rm = TRUE
      ),
      
      hisp_plurality = dplyr::case_when(
        pct_hisp > largest_other ~ "largest group",
        TRUE ~ "not largest group"
      ),
      
      largest_group = dplyr::case_when(
        pct_hisp >= pmax(
          pct_wht,
          pct_blk,
          pct_aian,
          pct_asi,
          pct_oth,
          na.rm = TRUE
        ) ~ "Hispanic",
        
        pct_wht >= pmax(
          pct_blk,
          pct_aian,
          pct_asi,
          pct_oth,
          na.rm = TRUE
        ) ~ "White",
        
        pct_blk >= pmax(
          pct_aian,
          pct_asi,
          pct_oth,
          na.rm = TRUE
        ) ~ "Black",
        
        pct_aian >= pmax(
          pct_asi,
          pct_oth,
          na.rm = TRUE
        ) ~ "AIAN",
        
        pct_asi >= pct_oth ~ "Asian",
        
        TRUE ~ "Other"
      )
    ) %>%
    dplyr::select(
      GEOID,
      DP05_0089E,
      pct_hisp,
      pct_wht,
      pct_blk,
      hisp_maj,
      hisp_plurality,
      largest_group
    )
  
  #--------------------------------------------------------------------------
  # Create Final Output
  #--------------------------------------------------------------------------
  
  data_list <- list(
    burden,
    cost_mort,
    cost_nomort,
    hous_char,
    hisp_burden,
    hisp_calc
  )
  
  final_df <- data_list |>
    purrr::map(sf::st_drop_geometry) |>
    purrr::reduce(dplyr::left_join, by = "GEOID")
  
  final_sf <- burden |>
    dplyr::select(GEOID, NAME, geometry) |>
    dplyr::left_join(final_df, by = c("GEOID", "NAME")) |>
    sf::st_as_sf()
  
  return(final_sf)
  
}


#===============================================================================
# Export Function
#===============================================================================

export_housing_affordability <- function(
    data,
    geography,
    state,
    year
) {
  
  output_folder <- here::here("output")
  
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  
  file_name <- paste0(
    geography,
    "_",
    state,
    "_",
    year,
    "_housing_affordability.gpkg"
  )
  
  sf::st_write(
    data,
    dsn = file.path(output_folder, file_name),
    delete_dsn = TRUE
  )
  
}


#===============================================================================
# Interactive Leaflet Map Function
#===============================================================================

interactive_housing_map <- function(
    data,
    variable,
    title,
    palette = "magma",
    reverse_palette = TRUE,
    basemap = "CartoDB.Positron"
) {
  
  #---------------------------------------------------------------------------
  # Validation
  #---------------------------------------------------------------------------
  
  if (!inherits(data, "sf")) {
    stop("data must be an sf object.")
  }
  
  if (!variable %in% names(data)) {
    stop(
      paste0(
        variable,
        " is not a variable in the supplied data."
      )
    )
  }
  
  #---------------------------------------------------------------------------
  # Color Palette
  #---------------------------------------------------------------------------
  
  pal <- leaflet::colorNumeric(
    palette = viridisLite::viridis(
      n = 256,
      option = palette,
      direction = ifelse(reverse_palette, -1, 1)
    ),
    domain = data[[variable]],
    na.color = "#D3D3D3"
  )

  #---------------------------------------------------------------------------
  # Popup Labels
  #---------------------------------------------------------------------------
  
  data <- data %>%
    dplyr::mutate(
      map_value = .data[[variable]],
      popup_text = paste0(
        "<strong>", NAME, "</strong>",
        "<br>",
        variable,
        ": ",
        round(map_value, 2),
        "%"
      )
    )
  #---------------------------------------------------------------------------
  # Build Map
  #---------------------------------------------------------------------------
  
  leaflet::leaflet(data) %>%
    
    leaflet::addProviderTiles(
      provider = basemap
    ) %>%
    
    leaflet::addPolygons(
      fillColor = ~pal(map_value),
      fillOpacity = 0.8,
      color = "white",
      weight = 1,
      opacity = 1,
      popup = ~popup_text,
      
      highlightOptions = leaflet::highlightOptions(
        weight = 3,
        color = "black",
        bringToFront = TRUE
      )
    ) %>%
    
    leaflet::addLegend(
      position = "bottomright",
      pal = pal,
      values = ~map_value,
      title = title,
      opacity = 1
    )
}
  