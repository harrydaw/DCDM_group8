df = read.csv("data/merged/all_clean_data_mutated.csv")
df = df[df$analysis_id != "0+45557890005784i", ]
write.csv(df, "data/merged/all_clean_data_mutated.csv", row.names = FALSE, na = "")