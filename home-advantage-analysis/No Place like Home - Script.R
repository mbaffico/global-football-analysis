# Load the raw data
football_matches <- read.csv("C:/Users/night/OneDrive/Documents/Matches.csv")
# Since the date is currently formatted as a string variable, I'll modify it to a date data type:
football_matches$MatchDate <- as.Date(football_matches$MatchDate)
# I will proceed to remove white spaces to clean the data.
football_matches$HomeTeam <- trimws(football_matches$HomeTeam)
# Cleaning the data to remove unwanted columns and filtering for matches after 2010
football_matches_trimmed <- football_matches %>% 
  select(Division, MatchDate, HomeTeam, AwayTeam, FTHome, FTAway, FTResult, HTHome, 
         HTAway, HTResult, HomeShots, HomeCorners, AwayShots, AwayCorners, HomeTarget, AwayTarget, 
         HomeElo, AwayElo, HomeFouls, AwayFouls, HomeYellow, AwayYellow, HomeRed, AwayRed) %>% 
  filter(MatchDate >= "2010-01-01")
# I will add a country column to aggregate leagues for countries that have over 1 league.
football_matches_new <- football_matches_trimmed %>% 
  mutate(
    Country = case_when(
      Division %in% c("E0", "E1", "E2", "E3", "EC") ~ "England",
      Division %in% c("ARG") ~ "Argentina",
      Division %in% c("SP1", "SP2") ~ "Spain",
      Division %in% c("AUT") ~ "Austria",
      Division %in% c("B1") ~ "Belgium",
      Division %in% c("BRA") ~ "Brazil",
      Division %in% c("CHN") ~ "China",
      Division %in% c("D1", "D2") ~ "Germany",
      Division %in% c("DEN") ~ "Denmark",
      Division %in% c("F1", "F2") ~ "France",
      Division %in% c("FIN") ~ "Finland",
      Division %in% c("G1") ~ "Greece",
      Division %in% c("I1", "I2") ~ "Italy",
      Division %in% c("IRL") ~ "Ireland",
      Division %in% c("JAP") ~ "Japan",
      Division %in% c("MEX") ~ "Mexico",
      Division %in% c("N1") ~ "Netherlands",
      Division %in% c("NOR") ~ "Norway",
      Division %in% c("P1") ~ "Portugal",
      Division %in% c("POL") ~ "Poland",
      Division %in% c("ROM") ~ "Romania",
      Division %in% c("RUS") ~ "Russia",
      Division %in% c("SC0", "SC1", "SC2", "SC3") ~ "Scotland",
      Division %in% c("SUI") ~ "Switzerland",
      Division %in% c("SWE") ~ "Sweden",
      Division %in% c("T1") ~ "Turkey",
      Division %in% c("USA") ~ "United States"
    )
  )
# Let's determine which countries had the largest number of home wins with their respective percentages.
home_wins_overview <- football_matches_new %>% 
  group_by(Country) %>% 
  summarise(
    total_matches = n(),
    home_wins = sum(FTResult == "H"),
    home_win_pct = round((home_wins / total_matches) * 100, 1)
  ) %>% 
  arrange(desc(home_win_pct))
home_wins_overview %>% 
  print(n = 27)

# Let's graph it out on a map to dimension the impact.
# First, let's create a world map to place it in.
world <- ne_countries(scale = "medium", returnclass = "sf")
home_wins_map <- home_wins_overview %>% ## Since the map doesn't recognize England and Scotland, I have to use UK.
  mutate(Country = case_when(
    Country %in% c("England", "Scotland") ~ "United Kingdom",
    TRUE ~ Country
  ))

world_map <- world %>%
  left_join(home_wins_map, by = c("name_ciawf" = "Country"))

# I'll define a color palette for the map
rose_palette <- c(
  "#fff1e0",  # light cream
  "#f7c6d0",  # soft pink
  "#e88abf",  # magenta
  "#b04aa1",  # deep purple
  "#3b2c6e"   # navy blue
)

ggplot(world_map) +
  geom_sf(aes(fill = home_win_pct), color = "gray", size = 0.1) +
  scale_fill_gradientn(colors = rose_palette, na.value = "gray90") +
  labs(
    title = "Home Win % By Country",
    fill = "Home Win %",
    caption = "Source: Football-Data.co.uk"
  ) +
  theme_minimal()

# Analyze now how adding draws to the equation changes things
home_undefeated_overview <- football_matches_new %>% 
  group_by(Country) %>% 
  summarise(
    total_matches = n(),
    home_wins = sum(FTResult == "H"),
    draws = sum(FTResult == "D"),
    undefeated_pct = round(((home_wins + draws) / total_matches) * 100, 1)
  ) %>% 
  arrange(desc(undefeated_pct))
home_undefeated_overview %>% 
  print(n = 27)

# Let's map the undefeated data to see how the map changes

home_undefeated_map <- home_undefeated_overview %>%
  mutate(
    Country = case_when(
      Country %in% c("England", "Scotland") ~ "United Kingdom",
      TRUE ~ Country
    )
  )

world_map_undefeated <- world %>% 
  left_join(home_undefeated_map, by = c("name_ciawf" = "Country"))

ggplot(world_map_undefeated) +
  geom_sf(aes(fill = undefeated_pct), color = "gray", size = 0.1) +
  scale_fill_gradientn(colors = rose_palette, na.value = "gray90") +
  labs(
    title = "Home Undefeated % by Country",
    subtitle = "Wins and Draws on Home Turf",
    fill = "Undefeated %",
    caption = "Source: Football-Data.co.uk"
  ) +
  theme_minimal()

## Let's zoom in to identify how results vary by leagues

league_names <- football_matches_new %>% 
  rename(League = Division) %>% # Rename column
  mutate(                       # Renaming the leagues so that they're more intuitive
    League = case_when(
      League == "BRA" ~ "Serie A (BRA 1)",
      League == "USA" ~ "MLS (USA 1)",
      League == "SP2" ~ "LaLiga 2 (ESP 2)",
      League == "I2" ~ "Serie B (ITA 2)",
      League == "ARG" ~ "Primera Division (ARG 1)",
      League == "F2" ~ "Ligue 2 (FRA 2)",
      League == "G1" ~ "Super League (GRC 1)",
      League == "SP1" ~ "LaLiga (ESP 1)",
      League == "MEX" ~ "Liga MX (MEX 1)",
      League == "NOR" ~ "Eliteserien (NOR 1)",
      League == "T1" ~ "Super Lig (TUR 1)",
      League == "D2" ~ "2. Bundesliga (DEU 2)",
      League == "F1" ~ "Ligue 1 (FRA 1)",
      League == "POL" ~ "Ekstraklasa (POL 1)",
      League == "ROM" ~ "SuperLiga (ROU 1)",
      League == "CHN" ~ "Super League (CHN 1)",
      League == "E0" ~ "Premier League (ENG 1)",
      League == "B1" ~ "Pro League (BEL 1)",
      League == "N1" ~ "Eredivisie (NLD 1)",
      League == "I1" ~ "Serie A (ITA 1)",
      League == "FIN" ~ "Veikkausliiga (FIN 1)",
      League == "RUS" ~ "Russian Premier League (RUS 1)",
      League == "D1" ~ "Bundesliga (DEU 1)",
      League == "E2" ~ "League One (ENG 3)",
      League == "DEN" ~ "Superliga (DEN 1)",
      League == "E1" ~ "EFL Championship (ENG 2)",
      League == "E3" ~ "League Two (ENG 4)",
      League == "SUI" ~ "Swiss Super League (SUI 1)",
      League == "P1" ~ "Liga Portugal (POR 1)",
      League == "SWE" ~ "Allsvenskan (SWE 1)",
      League == "EC" ~ "National League (ENG 5)",
      League == "SC1" ~ "Scottish Championship (SCO 2)",
      League == "IRL" ~ "League of Ireland Premier Division (IRE 1)",
      League == "AUT" ~ "Bundesliga (AUT 1)",
      League == "SC0" ~ "Scottish Premiership (SCO 1)",
      League == "SC3" ~ "Scottish League Two (SCO 4)",
      League == "SC2" ~ "Scottish League One (SCO 3)",
      League == "JAP" ~ "JFL (JPN 1)",
      TRUE ~ League             # Fallback
    ))

# Create a summary for home results per league
league_overview <- league_names %>% 
  group_by(League) %>%          
  summarise(            # Create the summary table
    total_matches = n(),
    home_wins = sum(FTResult == "H", na.rm = TRUE),
    home_undefeated = sum(FTResult %in% c("H","D"), na.rm = TRUE),
    win_pct = round((home_wins / total_matches) * 100, 1),
    undefeated_pct = round((home_undefeated / total_matches) * 100, 1)
  ) %>% 
  arrange(desc(undefeated_pct))

league_overview %>% 
  print(n = 38)

# Make a dot plot for top 10 leagues
league_overview %>% 
  slice_head(n = 10) %>% 
  ggplot(aes(x = reorder(League, undefeated_pct), y = undefeated_pct, fill = League)) + 
    geom_dotplot(
      binaxis = "y",
      dotsize = 1.5
    ) +
    scale_y_continuous(limits = c(65, 80)) +  # Adjust based on range 
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.y = element_text(
        margin = margin(t = 0, r = 10, b = 0, l = 0)),
      legend.position = "none"
      ) +
    labs(
      title = "Top 10 Strongest Home Leagues",
      subtitle = "League Performance Based on Wins and Draws at Home",
      x = "League",
      y = "Undefeated %",
      caption = "Source: Football-Data.co.uk"
    )

# Map the difference between first division and second division
league_overview %>% 
  mutate(                                    # Map leagues to country
    Country = case_when(
      League == "LaLiga (ESP 1)" ~ "Spain",
      League == "LaLiga 2 (ESP 2)" ~ "Spain",
      League == "Ligue 1 (FRA 1)" ~ "France",
      League == "Ligue 2 (FRA 2)" ~ "France",
      League == "Serie A (ITA 1)" ~ "Italy",
      League == "Serie B (ITA 2)" ~ "Italy",
      League == "Premier League (ENG 1)" ~ "England",
      League == "EFL Championship (ENG 2)" ~ "England",
      League == "Bundesliga (DEU 1)" ~ "Germany",
      League == "2. Bundesliga (DEU 2)" ~ "Germany",
      League == "Scottish Premiership (SCO 1)" ~ "Scotland",
      League == "Scottish Championship (SCO 2)" ~ "Scotland"
      ),
    Division = case_when(                    # Split divisions into 2 buckets for intuitiveness
      League %in% c("LaLiga 2 (ESP 2)", "Ligue 2 (FRA 2)", "Serie B (ITA 2)",
                      "EFL Championship (ENG 2)", "2. Bundesliga (DEU 2)",
                      "Scottish Championship (SCO 2)") ~ "Second Division",
      League %in% c("LaLiga (ESP 1)", "Ligue 1 (FRA 1)", "Serie A (ITA 1)",
                      "Premier League (ENG 1)", "Bundesliga (DEU 1)",
                      "Scottish Premiership (SCO 1)") ~ "First Division"
    ) 
  ) %>% 
  filter(!is.na(Country), !is.na(Division)) %>%    # Filter our NA values
  ggplot(aes(x = Division, y = undefeated_pct, group = Country)) +
    geom_line(aes(color = Country), linewidth = 1.2) +
    geom_point(aes(color = Country, size = 2.5)) +
    geom_text_repel(aes(label = League, hjust = ifelse(Division == "First Division", 1.1, -0.1)), 
                    size = 3, 
                    box.padding = 0.3,
                    direction = "y") +
    labs(
      title = "Climbing the Slope of Home Advantage",
      subtitle = "Do Second Divisions Outshine Their First Tier Counterparts?",
      x = NULL,
      y = "Undefeated %",
      caption = "Source: Football-Data.co.uk"
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
    ) +
    guides(size = "none")

# Let's compare Home vs. Away performance in leagues
league_win_pct <- league_names %>% 
  group_by(League) %>% 
  summarise(
    total_matches = n(),
    home_win_pct = round((sum(FTResult == "H", na.rm = TRUE) / total_matches) * 100),
    home_goals = round(mean(FTHome, na.rm = TRUE), 1),
    away_win_pct = round((sum(FTResult == "A", na.rm = TRUE) / total_matches) * 100),
    away_goals = round(mean(FTAway, na.rm = TRUE), 1)
  ) %>% 
  print(n = 38)

# Plot it into a visual for comparison
league_win_pct_plot <- league_win_pct %>% 
  pivot_longer(cols = c(home_win_pct, away_win_pct), 
               names_to = "Match_Type", 
               values_to = "Win_Pct")

ggplot(league_win_pct_plot, aes(x = League, y = Win_Pct, fill = Match_Type)) +
  geom_col(position = "stack") +
  coord_flip() +
  labs(
    title = "The Strength of Home",
    subtitle = "Home and Away Win % per League",
    fill = "Match Type",
    y = "Win Percentage"
  ) +
  theme(axis.text.y = element_text(size = 6))
  
# Let's analyze how home advantage varies for individual teams
team_overview <- league_names %>% 
  group_by(HomeTeam) %>%
  filter(n() > 38) %>%         # This filter will ensure that teams with less than ~1 season worth's of games will be removed
  reframe(
    League = paste(unique(League), collapse = ", "),
    total_matches = n(),
    home_wins = sum(FTResult == "H", na.rm = TRUE),
    undefeated_matches = sum(FTResult %in% c("H", "D"), na.rm = TRUE),
    undefeated_pct = round((undefeated_matches / total_matches) * 100, 1),
    avg_home_goals = mean(FTHome)
  ) %>% 
  arrange(desc(undefeated_pct))

# Identify which leagues have complete match stats for a season.
football_matches_trimmed %>% 
  filter(MatchDate > "2023-01-01") %>% 
  group_by(Division) %>% 
  summarise(
    missing_scores = sum(is.na(FTHome) + is.na(FTAway)),
    missing_shots = sum(is.na(HomeShots) + is.na(AwayShots)),
    missing_corners = sum(is.na(HomeCorners) + is.na(AwayCorners))
  ) %>% 
  arrange(desc(missing_shots)) %>% 
  print(n = 38)

# Let's use the premier league as a a first example
prem_league_home <- football_matches_trimmed %>% 
  filter(between(MatchDate, as.Date("2023-08-11"), as.Date("2024-05-19")), Division == "E0") %>% 
  mutate(
    Team = HomeTeam) %>% 
  group_by(Team) %>% 
  summarise(
    home_win_pct = round((sum(FTResult == "H") / n()) * 100, 1),
    avg_home_goals = round(mean(FTHome), 1),
    avg_goals_conceded_home = round(mean(FTAway), 1),
    avg_shots_home = round(mean(HomeShots), 1),
    avg_shots_conceded_home = round(mean(AwayShots), 1),
    avg_corners_home = round(mean(HomeCorners), 1)
  )

prem_league_away <- football_matches_trimmed %>% 
  filter(between(MatchDate, as.Date("2023-08-11"), as.Date("2024-05-19")), Division == "E0") %>% 
  mutate(
    Team = AwayTeam) %>% 
  group_by(Team) %>% 
  summarise(
    away_win_pct = round((sum(FTResult == "A") / n()) * 100, 1),
    avg_away_goals = round(mean(FTAway), 1),
    avg_goals_conceded_away = round(mean(FTHome), 1),
    avg_shots_away = round(mean(AwayShots), 1),
    avg_shots_conceded_away = round(mean(HomeShots), 1),
    avg_corners_away = round(mean(AwayCorners), 1)
  )

prem_league_teams <- prem_league_home %>%  # This combines home and away stats
  inner_join(prem_league_away, by = "Team") %>% 
  select(Team, home_win_pct, away_win_pct, avg_home_goals, avg_away_goals, 
         avg_goals_conceded_home, avg_goals_conceded_away, avg_shots_home, 
         avg_shots_away, avg_shots_conceded_home, avg_shots_conceded_away, 
         avg_corners_home, avg_corners_away)

#Expand the analysis to LaLiga and Serie A
la_liga_home <- football_matches_trimmed %>% 
  filter(between(MatchDate, as.Date("2023-08-11"), as.Date("2024-05-26")), Division == "SP1") %>% 
  mutate(
    Team = HomeTeam) %>% 
  group_by(Team) %>% 
  summarise(
    home_win_pct = round((sum(FTResult == "H") / n()) * 100, 1),
    avg_home_goals = round(mean(FTHome), 1),
    avg_goals_conceded_home = round(mean(FTAway), 1),
    avg_shots_home = round(mean(HomeShots), 1),
    avg_shots_conceded_home = round(mean(AwayShots), 1),
    avg_corners_home = round(mean(HomeCorners), 1)
  )

la_liga_away <- football_matches_trimmed %>% 
  filter(between(MatchDate, as.Date("2023-08-11"), as.Date("2024-05-26")), Division == "SP1") %>% 
  mutate(
    Team = AwayTeam) %>% 
  group_by(Team) %>% 
  summarise(
    away_win_pct = round((sum(FTResult == "A") / n()) * 100, 1),
    avg_away_goals = round(mean(FTAway), 1),
    avg_goals_conceded_away = round(mean(FTHome), 1),
    avg_shots_away = round(mean(AwayShots), 1),
    avg_shots_conceded_away = round(mean(HomeShots), 1),
    avg_corners_away = round(mean(AwayCorners), 1)
  )

la_liga_teams <- la_liga_home %>% 
  inner_join(la_liga_away, by = "Team") %>% 
  select(Team, home_win_pct, away_win_pct, avg_home_goals, avg_away_goals, 
         avg_goals_conceded_home, avg_goals_conceded_away, avg_shots_home, 
         avg_shots_away, avg_shots_conceded_home, avg_shots_conceded_away, 
         avg_corners_home, avg_corners_away)

serie_a_home <- football_matches_trimmed %>% 
  filter(between(MatchDate, as.Date("2023-08-20"), as.Date("2024-05-26")), Division == "I1") %>% 
  mutate(
    Team = HomeTeam) %>% 
  group_by(Team) %>% 
  summarise(
    home_win_pct = round((sum(FTResult == "H") / n()) * 100, 1),
    avg_home_goals = round(mean(FTHome), 1),
    avg_goals_conceded_home = round(mean(FTAway), 1),
    avg_shots_home = round(mean(HomeShots), 1),
    avg_shots_conceded_home = round(mean(AwayShots), 1),
    avg_corners_home = round(mean(HomeCorners), 1)
  )

serie_a_away <- football_matches_trimmed %>% 
  filter(between(MatchDate, as.Date("2023-08-20"), as.Date("2024-05-26")), Division == "I1") %>% 
  mutate(
    Team = AwayTeam) %>% 
  group_by(Team) %>% 
  summarise(
    away_win_pct = round((sum(FTResult == "A") / n()) * 100, 1),
    avg_away_goals = round(mean(FTAway), 1),
    avg_goals_conceded_away = round(mean(FTHome), 1),
    avg_shots_away = round(mean(AwayShots), 1),
    avg_shots_conceded_away = round(mean(HomeShots), 1),
    avg_corners_away = round(mean(AwayCorners), 1)
  )

serie_a_teams <- serie_a_home %>% 
  inner_join(serie_a_away, by = "Team") %>% 
  select(Team, home_win_pct, away_win_pct, avg_home_goals, avg_away_goals, 
         avg_goals_conceded_home, avg_goals_conceded_away, avg_shots_home, 
         avg_shots_away, avg_shots_conceded_home, avg_shots_conceded_away, 
         avg_corners_home, avg_corners_away)

# Now, I want to incorporate team Elo ratings.
#First determine data quality for Elo accross leagues.
football_matches_trimmed %>% 
  group_by(Division) %>% 
  summarise(
    missing_home_elo = sum(is.na(HomeElo)),
    missing_away_elo = sum(is.na(AwayElo))
  ) %>% 
  arrange(desc(missing_home_elo), desc(missing_away_elo)) %>% 
  print(n = 38)

# Create new dataset for the big 5 leagues with consistent Elo
football_matches_elo <- football_matches_trimmed %>% 
  filter(Division %in% c("E0", "D1", "I1", "SP1", "F1"), !is.na(AwayElo), !is.na(FTResult)) %>% 
  mutate(
    League = case_when(
      Division == "E0"  ~ "Premier League",
      Division == "D1"  ~ "Bundesliga",
      Division == "I1"  ~ "Serie A",
      Division == "SP1" ~ "LaLiga",
      Division == "F1" ~ "Ligue 1"
    )
  ) %>% 
  select(-Division)

football_matches_elo %>%
  ggplot(aes(x = AwayElo, y = as.numeric(FTResult == "H") * 100, color = League)) +
  geom_smooth(method = "loess", se = FALSE) + # Use line to represent the binary trend
  facet_wrap(~ League) +
  labs(
    title = "Home Win Rate vs Away Team Elo",
    x = "Away Team Elo",
    y = "Home Win Rate (%)",
    caption = "Source: Football-Data.co.uk"
  )

# This code calculates and plots the difference in Elo and the win rate
football_matches_elo %>%
  mutate(home_elo_diff = HomeElo - AwayElo) %>%
  group_by(home_elo_diff, League) %>%
  summarise(
    avg_win_rate = mean(FTResult == "H") * 100,
    .groups = "drop"
  ) %>%
  ggplot(aes(x = home_elo_diff, y = avg_win_rate, color = League)) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ League) +
  labs(
    title = "Win Rate vs Elo Difference",
    subtitle = "How the difference between Home and Away Elo influences match outcomes",
    x = "Home Elo − Away Elo",
    y = "Average Home Win Rate (%)",
    caption = "Source: Football-Data.co.uk"
  )

# Let's view the Elo distributions between leagues.
# Since the French and Spanish 2nd divisions have a decent amount of Elo data
# missing, I will shorten the time period to one season.

league_team_elo <- football_matches_trimmed %>% 
  filter(Division %in% c("E0", "D1", "I1", "SP1", "F1", "E1", "D2", "I2", "SP2", "F2"), 
         !is.na(AwayElo), 
         !is.na(FTResult)) %>% 
  mutate(
    League = case_when(
      Division == "E0"  ~ "Premier League",
      Division == "D1"  ~ "Bundesliga",
      Division == "I1"  ~ "Serie A",
      Division == "SP1" ~ "LaLiga",
      Division == "F1" ~ "Ligue 1",
      Division == "E1" ~ "EFL Championship",
      Division == "D2" ~ "2. Bundesliga",
      Division == "I2" ~ "Serie B",
      Division == "SP2" ~ "LaLiga 2",
      Division == "F2" ~ "Ligue 2"
    ),
    Country = case_when(
      League %in% c("Premier League", "EFL Championship") ~ "England",
      League %in% c("Bundesliga", "2. Bundesliga") ~ "Germany",
      League %in% c("Serie A", "Serie B") ~ "Italy",
      League %in% c("LaLiga", "LaLiga 2") ~ "Spain",
      League %in% c("Ligue 1", "Ligue 2") ~ "France"
    )
  ) %>% 
  select(-Division)

avg_home_elo <- league_team_elo %>%
  filter(between(MatchDate, as.Date("2023-07-28"), as.Date("2024-05-31"))) %>%
  group_by(HomeTeam, League, Country) %>%
  summarise(mean_home_elo = round(mean(HomeElo)), .groups = "drop")

avg_away_elo <- league_team_elo %>%
  filter(between(MatchDate, as.Date("2023-08-01"), as.Date("2024-05-31"))) %>%
  group_by(AwayTeam, League, Country) %>%
  summarise(mean_away_elo = round(mean(AwayElo)), .groups = "drop")

avg_team_elo <- full_join(
  avg_home_elo %>% rename(Team = HomeTeam),
  avg_away_elo %>% rename(Team = AwayTeam),
  by = c("Team", "League", "Country")
) %>%
  mutate(mean_team_elo = round(
    rowMeans(select(., mean_home_elo, mean_away_elo), na.rm = TRUE)
  ))

ggplot(avg_team_elo, aes(x = "", y = mean_team_elo)) +
  geom_boxplot(aes(fill = League)) +
  coord_flip() +
  facet_wrap(~ Country, scales = "free_x") +
  labs(
    title = "Team Elo Distribution (Grouped by Country)",
    x = "",
    y = "Mean Team Elo",
    caption = "Source: Football-Data.co.uk"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# COVID-19 Study
# Reuse League Code but filter by specific seasons

league_names_COVID <- league_names %>%
  mutate(
    Season = case_when(
      MatchDate >= as.Date("2018-07-15") & MatchDate <= as.Date("2019-06-01") ~ "2018/19",
      MatchDate >= as.Date("2020-07-15") & MatchDate <= as.Date("2021-06-01") ~ "2020/21",
      MatchDate >= as.Date("2022-07-15") & MatchDate <= as.Date("2023-06-01") ~ "2022/23",
      TRUE ~ NA_character_  # handle dates that don't match any range
    )
  ) %>%
  filter(!is.na(Season))

COVID_overview <- league_names_COVID %>% 
  group_by(League, Season) %>% 
  summarise(            # Create the summary table
    total_matches = n(),
    home_wins = sum(FTResult == "H", na.rm = TRUE),
    win_pct = round((home_wins / total_matches) * 100, 1),
  )

COVID_overview %>% 
  filter(League %in% c("Premier League (ENG 1)",
                       "EFL Championship (ENG 2)",
                       "Bundesliga (DEU 1)",
                       "2. Bundesliga (DEU 2)",
                       "Serie A (ITA 1)",
                       "Serie B (ITA 2)",
                       "LaLiga (ESP 1)",
                       "LaLiga 2 (ESP 2)",
                       "Ligue 1 (FRA 1)",
                       "Ligue 2 (FRA 2)",
                       "Liga MX (MEX 1)",
                       "Primera Division (ARG 1)",
                       "Serie A (BRA 1)",
                       "MLS (USA 1)")) %>% 
  mutate(
    region_tier = case_when(     # Bucket into regions for a clear facet
      League %in% c("Premier League (ENG 1)", "Bundesliga (DEU 1)", "Serie A (ITA 1)", 
                    "LaLiga (ESP 1)", "Ligue 1 (FRA 1)") ~ "Europe First Division",
      League %in% c("EFL Championship (ENG 2)", "2. Bundesliga (DEU 2)",
                    "Serie B (ITA 2)", "LaLiga 2 (ESP 2)", "Ligue 2 (FRA 2)") ~ "Europe Second Division",
      League %in% c("Liga MX (MEX 1)", "Primera Division (ARG 1)", "Serie A (BRA 1)",
                    "MLS (USA 1)") ~ "Americas First Division")
  ) %>% 
  ggplot(aes(x = Season, y = win_pct, group = League)) +
  geom_line(aes(color = League), size = 0.6) +
  facet_wrap(~ region_tier) +
  geom_point(aes(color = League), size = 1.5) +
  geom_text_repel(   # Add labels for leagues
    data = . %>% filter(Season == "2022/23"),
    aes(label = League, color = League),
    size = 2.5,
    nudge_x = 0.5
  ) +
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.3))) +
  labs(
    title = "Home Win Percentage Across Seasons by League",
    x = "Season",
    y = "Home Win %",
    color = "League"
  ) +
  theme(panel.grid.minor = element_blank()) +
  guides(color = "none")

# Exploring referee bias
# Below I will mean foul statistics for referees across leagues
match_fouls <- football_matches_trimmed %>% 
  filter(between(MatchDate, as.Date("2015-07-15"), as.Date("2025-05-31")),
         Division %in% c("E0", "I1", "D1", "SP1", "F1")) %>% 
  select(Division, MatchDate, HomeFouls, AwayFouls, HomeYellow, AwayYellow, HomeRed, AwayRed)

match_fouls %>% 
  rename("League" = Division) %>% 
  group_by(League) %>% 
  mutate(League = case_when(
    League == "E0" ~ "Premier League",
    League == "I1" ~ "Serie A",
    League == "D1" ~ "Bundesliga",
    League == "SP1" ~ "LaLiga",
    League == "F1" ~ "Ligue 1"),
    across(c(HomeFouls, AwayFouls, HomeYellow, AwayYellow, HomeRed, AwayRed), 
           ~    replace_na(., 0))) %>%
  summarise(
    foul_diff = mean(HomeFouls) - mean(AwayFouls),
    avg_home_fouls = mean(HomeFouls),
    avg_away_fouls = mean(AwayFouls),
    avg_home_yellows = mean(HomeYellow),
    avg_away_yellows = mean(AwayYellow),
    avg_home_reds = mean(HomeRed),
    avg_away_reds = mean(AwayRed)
  ) %>% 
  arrange(desc(foul_diff)) %>% 
  kable(col.names = c("League", "Foul Difference (Home - Away)", "Avg Home Fouls",
        "Avg Away Fouls", "Avg Home Yellows", "Avg Away Yellows",
        "Avg Home Reds", "Avg Away Reds"))


match_fouls %>% 
  rename("League" = Division) %>%
  mutate(
    time_period = case_when(    # Create time periods to analyze COVID impact
      MatchDate < as.Date("2020-03-01") ~ "Pre-COVID",
      between(MatchDate, as.Date("2020-03-01"), as.Date("2021-05-31")) ~ "COVID",
      MatchDate > as.Date("2021-07-15") ~ "Post-COVID"
    ),
    # Order the time periods to be chronological instead of alphabetic
    time_period = factor(time_period, levels = c("Pre-COVID", "COVID", "Post-COVID")),
    League = case_when(
      League == "E0" ~ "Premier League",
      League == "I1" ~ "Serie A",
      League == "D1" ~ "Bundesliga",
      League == "SP1" ~ "LaLiga",
      League == "F1" ~ "Ligue 1"
    ),
    # For fouls and cards given, na values should be 0
    across(c(HomeFouls, AwayFouls, HomeYellow, AwayYellow, HomeRed, AwayRed), ~replace_na(., 0))
  ) %>%
  group_by(League, time_period) %>%
  summarise(
    foul_diff = mean(HomeFouls) - mean(AwayFouls),
    yellow_diff = mean(HomeYellow) - mean(AwayYellow),
    red_diff = mean(HomeRed) - mean(AwayRed),
  ) %>%
  pivot_longer(cols = ends_with("_diff"), names_to = "metric", values_to = "difference") %>%
  ggplot(aes(x = time_period, y = League, fill = difference)) +
  geom_tile(color = "white") +
  facet_wrap(~ metric, labeller = labeller(    # Re-label facet headings
    metric = c(
      foul_diff = "Foul Difference",
      yellow_diff = "Yellow Card\nDifference",
      red_diff = "Red Card \nDifference"
    ))
    )+
  scale_fill_gradientn(     # I made a custom scale since red cards per game are very few
    colours = c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#f7f7f7", 
                "#92c5de", "#4393c3", "#2166ac", "#053061"),
    values = scales::rescale(c(-0.5, -0.1, -0.05, 0, 0.05, 0.1, 0.5)),
    limits = c(-0.5, 0.5),
    oob = scales::squish,
    guide = guide_colorbar(
      barwidth = unit(1, "cm"),
      barheight = unit(2, "cm"),
      title.position = "top",   
      title.hjust = 0.5,         
      label.position = "right"   
    )
  ) +
  labs(
    title = "Card & Foul Differences by League and COVID Period",
    subtitle = "Positive values favor away teams; negative values favor home teams",
    x = NULL,
    y = NULL,
    fill = "Difference"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    strip.text = element_text(face = "bold", size = 11, margin = margin(t = 5, b = 5)),
  )

# Create a logistical regression to determine what are the most impactful items
# in having the home team win

# Map a relative dominance score for the regression
dominance_vars <- football_matches_trimmed %>%
  filter(Division %in% c("E0", "I1", "SP1", "D1", "F1", "SC0"), !is.na(HomeShots), !is.na(HomeTarget)) %>%
  mutate(
    shot_diff = HomeShots - AwayShots,
    target_diff = HomeTarget - AwayTarget,
    corner_diff = HomeCorners - AwayCorners
  ) %>%
  select(shot_diff, target_diff, corner_diff) %>%
  scale()

dominance_fa <- fa(dominance_vars, nfactors = 1, rotate = "none", fm = "ml")
dominance_scores <- dominance_fa$scores[, 1]  # Latent dominance factor

home_df <- football_matches_trimmed %>%
  filter(Division %in% c("E0", "I1", "SP1", "D1", "F1", "SC0"), !is.na(HomeShots), !is.na(HomeTarget)) %>%
  rename(league = Division) %>%
  mutate(
    has_crowd = ifelse(between(MatchDate, as.Date("2020-03-01"), as.Date("2021-07-15")), 0, 1),
    across(c(HomeFouls, AwayFouls, HomeYellow, AwayYellow, HomeRed, AwayRed, HomeCorners, AwayCorners), 
           ~    replace_na(., 0)),
    referee_bias_index = (HomeFouls - AwayFouls) + 2 * (HomeYellow - AwayYellow) + 
      5 * (HomeRed - AwayRed),
    relative_dominance = dominance_scores,
    team = HomeTeam,
    venue = "Home",
    team_win = ifelse(FTResult == "H", 1, 0),
    ht_goal_diff = HTHome - HTAway,
    elo_diff = HomeElo - AwayElo
  )

away_df <- football_matches_trimmed %>%
  filter(Division %in% c("E0", "I1", "SP1", "D1", "F1", "SC0"), !is.na(HomeShots), !is.na(HomeTarget)) %>%
  rename(league = Division) %>%
  mutate(
    has_crowd = ifelse(between(MatchDate, as.Date("2020-03-01"), as.Date("2021-07-15")), 0, 1),
    across(c(HomeFouls, AwayFouls, HomeYellow, AwayYellow, HomeRed, AwayRed, HomeCorners, AwayCorners), 
           ~    replace_na(., 0)),
    referee_bias_index = (HomeFouls - AwayFouls) + 2 * (HomeYellow - AwayYellow) + 
      5 * (HomeRed - AwayRed),
    relative_dominance = dominance_scores,  # Same score; match-level context
    team = AwayTeam,
    venue = "Away",
    team_win = ifelse(FTResult == "A", 1, 0),
    ht_goal_diff = HTHome - HTAway,
    elo_diff = HomeElo - AwayElo
  )

regression_df <- bind_rows(home_df, away_df) %>%
  mutate(
    venue = factor(venue, levels = c("Away", "Home")),
    has_crowd = factor(has_crowd, levels = c(0, 1), labels = c("No", "Yes"))
  )

HA_regression <- glm(team_win ~ has_crowd + venue + venue:has_crowd + referee_bias_index + relative_dominance +
                       ht_goal_diff + elo_diff, 
                     data = regression_df,
                     family = binomial)
  