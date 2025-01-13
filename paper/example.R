library(easystats)

model <- lm(Petal.Width ~ Petal.Length * Species, data = iris)

parameters::parameters(model)


estimate_relation(model, by=c("Petal.Length", "Species"), length=100) |>
  plot()


estimate_means(model, by="Species")

estimate_contrasts(model, contrast="Species")

estimate_slopes(model, trend = "Petal.Length", by="Species")

estimate_contrasts(model, contrast="Petal.Length", by="Species", backend="marginaleffectss")
# estimate_contrasts(model, contrast="Petal.Length", by="Species")

