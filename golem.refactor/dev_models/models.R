library(recipes)
library(parsnip)

# Create recipe
preproc_recipe <- recipes::recipe(
    logLastSoldPrice ~
        city +
        bathrooms +
        finishedSqFt +
        yearBuilt +
        cpipctchange +
        season,
    data = modl_data_fnl
) |>
    recipes::step_string2factor(recipes::all_nominal()) |>
    recipes::prep()

# Apply computations to new data
modl_data_preproc_full <- recipes::bake(
    preproc_recipe,
    modl_data_fnl
)

# Define xgb model
xgb_modl_fnl <- parsnip::boost_tree(
    mode = "regression",
    trees = 1000
) |>
    parsnip::set_engine(
        engine = "xgboost",
        objective = "reg:squarederror"
    ) |>
    parsnip::fit(
        logLastSoldPrice ~ .,
        data = modl_data_preproc_full
    )

# Save xgb model
saveRDS(xgb_modl_fnl, "inst/models/xgb.RDS")

# Make an example prediction using xgb model
newdata <- tibble::tibble(
    city = "Andover",
    bathrooms = 2,
    finishedSqFt = 1800,
    yearBuilt = 2000,
    season = "Spring",
    cpipctchange = 0.0336
)

predict(xgb_modl_fnl, newdata)

# Define lm model
lm_modl <- parsnip::linear_reg(mode = "regression") |>
    parsnip::set_engine(engine = "lm") |>
    parsnip::fit(
        logLastSoldPrice ~ .,
        data = modl_data_preproc_full
    )

# Save lm model
saveRDS(lm_modl, "inst/models/lm.RDS")

# Make an example prediction using lm model
predict(lm_modl, newdata)
