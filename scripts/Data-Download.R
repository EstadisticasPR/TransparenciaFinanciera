library(jsonlite)

# The url is a CKAN API link to datos.estadisticas.pr
x <- fromJSON("https://datos.estadisticas.pr/api/action/datastore_search?resource_id=824d2a93-7998-4f37-8fe9-9e110913ec5e&limit=500000000")

# Exctract the records from the HTTPS response
df <- x$result$records

# This column is not necessary for the dataset
df['_id'] <- NULL

# Match names to the names firstly used by the SQL DB
colnames(df) <- c("fiscal_year_period", "account", "name", "fiscal_year", "date", "amount",  "department")

# Save file, without the row indexes
write.csv(df, "../data/transparencia.csv", row.names = FALSE)