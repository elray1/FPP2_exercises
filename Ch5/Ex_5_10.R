library(fpp2)
library(dplyr)

# Set Up
daily20 <- head(elecdaily,20)
daily20

daily20_df <- daily20 %>%
  as.data.frame()

# a.
ggplot(daily20_df, aes(x=Temperature, y=Demand)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

fit <- tslm(Demand ~ Temperature, data=daily20)

# b.
checkresiduals(fit)

daily20_df <- daily20 %>%
  as.data.frame() %>%
  mutate(
    index = row_number(),
    resid = residuals(fit),
    lag1_resid = lag(resid)
  )

acf(daily20_df$resid)

ggplot(data = daily20_df, mapping = aes(x = index, y = resid)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = daily20_df, mapping = aes(x = lag1_resid, y = resid)) +
  geom_point() +
  geom_smooth(method = "lm")

# c.
daily20_df %>%
  slice(-1) %>%
  select(lag1_resid, resid) %>%
  cor()

# d.
forecast(fit, newdata=data.frame(Temperature=c(15,35)))

# e.
elecdaily_df <- elecdaily %>%
  as.data.frame()

ggplot(elecdaily_df, aes(x=Temperature, y=Demand)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

# Additional exploration
head(elecdaily_df)

elecdaily_df <- elecdaily_df %>%
  mutate(
    WorkDay = as.factor(WorkDay)
  )

ggplot(elecdaily_df, aes(x=Temperature, y=Demand, color=WorkDay, shape=WorkDay)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

