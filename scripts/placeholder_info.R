ggplot(
  con_burden) +
  geom_sf(aes(fill = mort_30pct)) +
  ggtitle(label = "Owned Units with a mortgage with Housing Costs Over 30% of Household Income") +
  theme_void() +
  scale_fill_viridis_c(option = "magma", 
                       direction = -1)

ggplot(
  con_burden) +
  geom_sf(aes(fill = mort_50pct)) +
  ggtitle(label = "Owned Units with a mortgage with Housing Costs Over 50% of Household Income") +
  theme_void() +
  scale_fill_viridis_c(option = "magma", direction = -1)

ggplot(
  con_burden) +
  geom_sf(aes(fill = nomort_30pct)) +
  ggtitle(label = "Owned Units without a mortgage with Housing Costs Over 30% of Household Income") +
  theme_void() +
  scale_fill_viridis_c(option = "magma", direction = -1)

ggplot(
  con_burden) +
  geom_sf(aes(fill = nomort_50pct)) +
  ggtitle(label = "Owned Units without a mortgage with Housing Costs Over 50% of Household Income") +
  theme_void() +
  scale_fill_viridis_c(option = "magma", direction = -1)

ggplot(
  con_burden) +
  geom_sf(aes(fill = rent_30pct)) +
  ggtitle(label = "Rented Units with Housing Costs Over 30% of Household Income") +
  theme_void() +
  scale_fill_viridis_c(option = "magma", direction = -1)

ggplot(
  con_burden) +
  geom_sf(aes(fill = mort_50pct)) +
  ggtitle(label = "Rented Units with Housing Costs Over 50% of Household Income") +
  theme_void() +
  scale_fill_viridis_c(option = "magma", direction = -1)