library(tidyverse)
library(modelr)
library(broom)

df_wide <- read_csv("unicef-u5mr.csv")   # Task 1 (Look at Data)
glimpse(df_wide)
names(df_wide)[1:6]

df <- df_wide %>% #Task 2 (Tidy)
  pivot_longer(starts_with("U5MR"),
               names_to = "Year",
               names_prefix = "U5MR.",
               values_to = "U5MR") %>%
  mutate(Year = as.numeric(Year))

glimpse(df)

p1 <- ggplot(df, aes(x = Year, y = U5MR, group = CountryName)) + #Task 3 (Plot)
  geom_line(alpha = 0.5) +
  facet_wrap(~Continent)

p1

ggsave("THOMPSON_Plot_1.png", plot = p1) #Task 4 (Save as LASTNAME_Plot_1.png)

df_mean <- df %>% #Task 5 (Plot)
  group_by(Continent, Year) %>%
  summarize(mean_U5MR = mean(U5MR, na.rm = TRUE), .groups = "drop")

p2 <- ggplot(df_mean, aes(x = Year, y = mean_U5MR, color = Continent)) +
  geom_line()

p2

ggsave("THOMPSON_Plot_2.png", plot = p2) #Task 6 (Save as LASTNAME_Plot_2.png)

mod1 <- lm(U5MR ~ Year, data = df) #Task 7 (Build Models)
mod2 <- lm(U5MR ~ Year + Continent, data = df)
mod3 <- lm(U5MR ~ Year * Continent, data = df)

mods <- list(mod1 = mod1, mod2 = mod2, mod3 = mod3) #Task 8 (Compare Models)

# Fit statistics
map_dfr(mods, glance, .id = "model") %>%
  select(model, adj.r.squared, AIC, BIC)

# Prediction error
map_dbl(mods, ~ rmse(.x, df))

# Formal test of whether added terms help
anova(mod1, mod2, mod3) #I chose mod3 because it scored best on adjusted R-squared, AIC, and BIC, even after those measures account for it having more terms than mod1 and mod2.

df_pred <- df %>% #Task 9 (Plot)
  gather_predictions(mod1, mod2, mod3)

head(df_pred)

ggplot(df_pred, aes(x = Year, y = pred, color = Continent, group = CountryName)) +
  geom_line() +
  facet_wrap(~model)
