box::use(
  here,
  dplyr,
  readr,
  tidyr,
  recipes,
  parsnip,
  workflows,
  broom,
  tibble,
  stringr,
  wehoop
)

# Returns the game schedule based on a season
#' @export
get_schedule <- function(season) {
  schedule <- wehoop$load_wbb_schedule(season)
  schedule_df <- as.data.frame(schedule)
  # write.csv(schedule_df, file = "wbb_schedule.csv", row.names = FALSE)
  return(schedule_df)
}

# Returns the boxscores of each game and merges the team/opponent data on game_date and game_id
#     This leaves you with two entries for every game with <team, opp> -> <Clemson vs USC>  and  <USC vs Clemson>
#' @export
get_boxscores <- function(season) {
  boxscores <- as.data.frame(wehoop$load_wbb_team_box(season))
  cols_to_drop <- c(
    'season_type', 'game_date_time', 'team_uid', 'team_slug', 'team_location',
    'team_name', 'team_abbreviation', 'team_short_display_name', 'team_color', 'team_alternate_color', 'team_logo',
    'opponent_team_id', 'opponent_team_uid', 'opponent_team_slug', 'opponent_team_location', 'opponent_team_name',
    'opponent_team_abbreviation', 'opponent_team_display_name', 'opponent_team_short_display_name',
    'opponent_team_color', 'opponent_team_alternate_color', 'opponent_team_logo', 'opponent_team_score'
  )

  boxscores <- boxscores[, !names(boxscores) %in% cols_to_drop]
  boxscores <- merge(boxscores, boxscores, by = c('game_date', 'game_id'), suffixes = c("", "_opponent"))
  boxscores <- boxscores[boxscores$team_id != boxscores$team_id_opponent, ]
  return(boxscores)
}


# Returns a list of all future opponents based on a given date and a season (**this function is not really needed)
#' @export
get_future_opps <- function(team, date, season) {
  schedule_df <- get_schedule(season)
  future_games <- schedule_df[schedule_df$game_date > date, ]

  team_games <- future_games[(future_games$home_display_name == team) | (future_games$away_display_name == team), ]

  opp_dict <- list()
  opp_dict[[team]] <- list()

  for (i in 1:nrow(team_games)) {
    game <- team_games[i, ]
    if (game$home_display_name == team) {
      opponent <- game$away_display_name
    } else {
      opponent <- game$home_display_name
    }
    opp_dict[[team]][[i]] <- list(date = game$game_date, opponent = opponent)
  }

  return(opp_dict)
}
# # This is just for reference if needed
# futureOpps <- get_future_opps(team, date, season)
# for (team_name in names(futureOpps)) {
#   cat("Team:", team_name, "\n")
#   for (game in futureOpps[[team_name]]) {
#     cat("Date:", game$date, ", Opponent:", game$opponent, "\n")
#   }
# }
# futureOpp <- futureOpps[[names(futureOpps)[1]]][[7]]$opponent

# Returns the list of previous opponents of a given team
#' @export
get_past_opps <- function(boxscores, futureOpp, date) {
  date <- as.Date(date, "%m/%d/%Y")

  past_opps <- boxscores[boxscores$team_display_name == futureOpp & boxscores$game_date < date, ]

  return(past_opps)
}

# Returns a df with all of the calcuated statstics based on a fitlered boxscore df
#' @export
get_game_stats <- function(df) {
  calculated_df <- data.frame(
    teamRslt = as.numeric(df$team_score > df$team_score_opponent),
    teamMargin = as.numeric(df$team_score - df$team_score_opponent),
    teamName = df$team_display_name,
    oppName = df$team_display_name_opponent,
    gmDate = as.character(df$game_date),
    gmSite = df$team_home_away,
    teamFGA = df$field_goals_attempted,
    teamFGM = df$field_goals_made,
    team3FGA = df$three_point_field_goals_attempted,
    team3FGM = df$three_point_field_goals_made,
    teamFTA = df$free_throws_attempted,
    teamFTM = df$free_throws_made,
    teamORB = df$offensive_rebounds,
    teamDRB = df$defensive_rebounds,
    teamPF = df$fouls,
    teamAssts = df$assists,
    teamTOs = df$turnovers,
    team2FGA = df$field_goals_attempted - df$three_point_field_goals_attempted,
    team2FGM = df$field_goals_made - df$three_point_field_goals_made,
    team2FGPercent = (df$field_goals_made - df$three_point_field_goals_made) / (df$field_goals_attempted - df$three_point_field_goals_attempted),
    team3FGPercent = df$three_point_field_goals_made / df$three_point_field_goals_attempted,
    teamFTPercent = df$free_throws_made / df$free_throws_attempted,
    teamPoss = df$field_goals_attempted + 0.5 * df$free_throws_attempted - df$offensive_rebounds + df$turnovers,
    teamPoints = df$team_score,
    teamPPShot = df$team_score / df$field_goals_attempted,
    teamPPPoss = df$team_score / (df$field_goals_attempted + 0.5 * df$free_throws_attempted - df$offensive_rebounds + df$turnovers),
    teamPercentFGA2 = (df$field_goals_attempted - df$three_point_field_goals_attempted) / df$field_goals_attempted,
    teamPercentFGA3 = df$three_point_field_goals_attempted / df$field_goals_attempted,
    teamPercentPTS2 = ((df$field_goals_made - df$three_point_field_goals_made) * 2) / df$team_score,
    teamPercentPTS3 = (df$three_point_field_goals_made * 3) / df$team_score,
    teamPercentPTS1 = df$free_throws_made / df$team_score,
    teamTOPercent = df$turnovers / (df$field_goals_attempted + 0.5 * df$free_throws_attempted + df$assists + df$turnovers),
    teamAsstFGPercent = df$assists / df$field_goals_made,
    teamFGAPerPoss = df$field_goals_attempted / (df$field_goals_attempted + 0.5 * df$free_throws_attempted - df$offensive_rebounds + df$turnovers),
    teamDRebPercent = df$defensive_rebounds / (df$defensive_rebounds + df$defensive_rebounds_opponent),
    teamORebPercent = df$offensive_rebounds / (df$offensive_rebounds + df$defensive_rebounds_opponent),
    teamPPShot1 = (df$team_score - df$three_point_field_goals_made * 3) / df$field_goals_attempted,
    teamPPShot2 = df$team_score / df$three_point_field_goals_attempted,
    teamPPShot3 = df$team_score / df$free_throws_attempted,

    oppFGA = df$field_goals_attempted_opponent,
    oppFGM = df$field_goals_made_opponent,
    opp3FGA = df$three_point_field_goals_attempted_opponent,
    opp3FGM = df$three_point_field_goals_made_opponent,
    oppFTA = df$free_throws_attempted_opponent,
    oppFTM = df$free_throws_made_opponent,
    oppORB = df$offensive_rebounds_opponent,
    oppDRB = df$defensive_rebounds_opponent,
    oppPF = df$fouls_opponent,
    oppAssts = df$assists_opponent,
    oppTOs = df$turnovers_opponent,
    opp2FGA = df$field_goals_attempted_opponent - df$three_point_field_goals_attempted_opponent,
    opp2FGM = df$field_goals_made_opponent - df$three_point_field_goals_made_opponent,
    opp2FGPercent = (df$field_goals_made_opponent - df$three_point_field_goals_made_opponent) / (df$field_goals_attempted_opponent - df$three_point_field_goals_attempted_opponent),
    opp3FGPercent = df$three_point_field_goals_made_opponent / df$three_point_field_goals_attempted_opponent,
    oppFTPercent = df$free_throws_made_opponent / df$free_throws_attempted_opponent,
    oppPoss = df$field_goals_attempted_opponent + 0.5 * df$free_throws_attempted_opponent - df$offensive_rebounds_opponent + df$turnovers_opponent,
    oppPoints = df$team_score_opponent,
    oppPPShot = df$team_score_opponent / df$field_goals_attempted_opponent,
    oppPPPoss = df$team_score_opponent / (df$field_goals_attempted_opponent + 0.5 * df$free_throws_attempted_opponent - df$offensive_rebounds_opponent + df$turnovers_opponent),
    oppPercentFGA2 = (df$field_goals_attempted_opponent - df$three_point_field_goals_attempted_opponent) / df$field_goals_attempted_opponent,
    oppPercentFGA3 = df$three_point_field_goals_attempted_opponent / df$field_goals_attempted_opponent,
    oppPercentPTS2 = ((df$field_goals_made_opponent - df$three_point_field_goals_made_opponent) * 2) / df$team_score_opponent,
    oppPercentPTS3 = (df$three_point_field_goals_made_opponent * 3) / df$team_score_opponent,
    oppPercentPTS1 = df$free_throws_made_opponent / df$team_score_opponent,
    oppTOPercent = df$turnovers_opponent / (df$field_goals_attempted_opponent + 0.5 * df$free_throws_attempted_opponent + df$assists + df$turnovers_opponent),
    oppAsstFGPercent = df$assists_opponent / df$field_goals_made_opponent,
    oppFGAPerPoss = df$field_goals_attempted_opponent / (df$field_goals_attempted_opponent + 0.5 * df$free_throws_attempted_opponent - df$offensive_rebounds_opponent + df$turnovers_opponent),
    oppDRebPercent = df$defensive_rebounds_opponent / (df$defensive_rebounds_opponent + df$defensive_rebounds),
    oppORebPercent = df$offensive_rebounds_opponent / (df$offensive_rebounds_opponent + df$defensive_rebounds),
    oppPPShot1 = (df$team_score_opponent - df$three_point_field_goals_made_opponent * 3) / df$field_goals_attempted_opponent,
    oppPPShot2 = df$team_score_opponent / df$three_point_field_goals_attempted_opponent,
    oppPPShot3 = df$team_score_opponent / df$free_throws_attempted_opponent
  )
  return(calculated_df)
}
