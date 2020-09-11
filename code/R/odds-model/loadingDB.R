##########################################
#Dataset
##########################################

con <- dbConnect(SQLite(), dbname="../../../datasets/european-football-db/database.sqlite")
dbListTables(con)
league <- tbl_df(dbGetQuery(con,"SELECT * FROM League"))
team   <- tbl_df(dbGetQuery(con,"SELECT * FROM Team"))
match  <- tbl_df(dbGetQuery(con,"SELECT * FROM Match"))
players <- tbl_df(dbGetQuery(con, "SELECT * FROM Player"))
player_attrs <- tbl_df(dbGetQuery(con, "SELECT * FROM Player_Attributes"))
countries <- tbl_df(dbGetQuery(con, "SELECT * FROM Country")) #not relevant at all
team_attrs <- tbl_df(dbGetQuery(con, "SELECT * FROM Team_Attributes"))

tables <- list(league, team, match, players, player_attrs, countries, team_attrs)


