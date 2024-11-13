library(dplyr)
library(readxl)
library(ggplot2)

# Load postcode-IMD lookup
pc_imds <- read.csv(
  file.path("C:", "Users", "TMPCDDES", "OneDrive - Birmingham City Council",
            "Documents", "Documents", "Other", "General data", 
            "West Midlands postcodes.csv")
  ) %>%
  mutate(IMD_rank = Index.of.Multiple.Deprivation) %>%
  select(Postcode, IMD_rank)

# Load BAP postcodes
bap_imds <- read_excel("../data/BAP Postcodes Q2.xlsx") %>%
  # Join postcode IMD ranks
  left_join(pc_imds, by = join_by(Postcode)) %>%
  # Calculate IMD deciles
  mutate(
    IMD_quintile = floor(5 * IMD_rank/32844 + 1)
  ) 

# Calculate percentage in each decile
bap_imd_percs <- bap_imds %>%
  # Remove 5 invalid postcodes
  filter(!is.na(IMD_rank)) %>%
  group_by(
    IMD_quintile
  ) %>%
  summarise(
    n_BAP = n()
  ) %>%
  mutate(
    perc_BAP = round(100 * n_BAP / sum(n_BAP),1)
  )

################################################################
#          Calculate Birmingham total IMD Percentages          #
################################################################

# Load LSOA11 IMD
all_LSOA_IMDs <- read.csv("../data/imd2019lsoa.csv") %>%
  # Filter for Birmingham IDAOPI
  filter(
    Measurement == "Decile ",
    Indices.of.Deprivation == "a. Index of Multiple Deprivation (IMD)"
  ) %>%
  mutate(
    # Calculate IMD quintile
    IMD_quintile = floor((Value+1)/2),
    LSOA11 = FeatureCode
  ) %>%
  select(LSOA11, IMD_quintile)

# lsoa11 to lsoa21 lookup
birmingham_lsoa11_to_lsoa21 <- read.csv("../data/LSOA11_to_LSOA21_to_LA_Lookup.csv") %>%
  filter(LAD22NM == "Birmingham") %>%
  mutate(
    LSOA11 = LSOA11CD, 
    LSOA21 = LSOA21CD) %>%
  select(LSOA11, LSOA21)

# Load census population in each LSOA
LSOA21_pops <- readxl::read_excel("../data/census-brum-oa-lsoa.xlsx") %>%
  # Update column names
  mutate(LSOA21 = `Lower layer Super Output Areas Code`) %>%
  # Sum age groups (using existing census data for speed)
  group_by(LSOA21) %>%
  summarise(Observation = sum(Observation)) 


birmingham_imd_percs <- LSOA21_pops %>%
  left_join(
    birmingham_lsoa11_to_lsoa21,
    by = join_by(LSOA21)
    ) %>%
  left_join(
    all_LSOA_IMDs,
    by = join_by(LSOA11)
  ) %>%
  group_by(IMD_quintile) %>%
  summarise(
    n_Birmingham = sum(Observation)
  ) %>%
  mutate(
    perc_Birmingham = round(100 * n_Birmingham / sum(n_Birmingham),1)
  )

#########################################################
#                 Combine data and save                 #
#########################################################

output_data <- bap_imd_percs %>%
  mutate(
    Group = "Be Active Plus",
    perc = perc_BAP,
    n = n_BAP
  ) %>%
  select(Group, IMD_quintile, perc, n) %>%
  rbind(
    birmingham_imd_percs %>%
      mutate(
        Group = "Birmingham Average",
        perc = perc_Birmingham,
        n = n_Birmingham
      ) %>%
      select(Group, IMD_quintile, perc, n)
  ) %>%
  group_by(
    Group
  ) %>%
  mutate(
    Denominator = sum(n),
    p_hat = perc/100,
    Z = qnorm(0.975),
    CI95Lower = 100 * (p_hat + Z^2/(2*Denominator) - 
                        Z * sqrt((p_hat*(1-p_hat)/Denominator))),
    CI95Upper = 100 * (p_hat + Z^2/(2*Denominator) + 
                         Z * sqrt((p_hat*(1-p_hat)/Denominator)))
  ) %>%
  select(
    -c(Denominator, p_hat, Z)
  )

# Save data
writexl::write_xlsx(output_data, "../output/BAP-IMD-dist.xlsx")

#########################################################
#        Plot BAP - Birmingham IMD Comparison           #
#########################################################

plot <- ggplot(output_data, 
       aes(x = IMD_quintile, y = perc, fill = Group)
       ) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(ymin = CI95Lower, ymax = CI95Upper),
    position=position_dodge(.9),
    width = 0.2) +
  theme_bw() +
  labs(
    x = "IMD Quintile",
    y = "Population Percentage",
    fill = "",
    title = "IMD Distribution of GP-referred Be Active Plus Clients"
  ) +                                           
  theme(
    legend.position.inside = T,
    legend.position = c(0.84, 0.9),
    legend.background = element_blank(),
    )

# Show plot
plot

# Save plot
ggsave("../output/BAP-IMD-dist.png", width=6, height = 4)
  

  