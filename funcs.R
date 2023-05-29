createGameDatatable <- function(game_lines) {
  # Combine the character vectors into a single matrix
  game_matrix <- do.call(rbind, game_lines)
  
  # Create a data.table with the game data
  game_dt <- data.table(
    event = str_match(game_matrix[, 1], '\\[Event "([^"]*)"\\]')[, 2],
    site = str_match(game_matrix[, 2], '\\[Site "([^"]*)"\\]')[, 2],
    white = str_match(game_matrix[, 3], '\\[White "([^"]*)"\\]')[, 2],
    black = str_match(game_matrix[, 4], '\\[Black "([^"]*)"\\]')[, 2],
    result = str_match(game_matrix[, 5], '\\[Result "([^"]*)"\\]')[, 2],
    date = str_match(game_matrix[, 6], '\\[UTCDate "([^"]*)"\\]')[, 2],
    time = str_match(game_matrix[, 7], '\\[UTCTime "([^"]*)"\\]')[, 2],
    white_elo = str_match(game_matrix[, 8], '\\[WhiteElo "([^"]*)"\\]')[, 2],
    black_elo = str_match(game_matrix[, 9], '\\[BlackElo "([^"]*)"\\]')[, 2],
    white_rating_diff = str_match(game_matrix[, 10], '\\[WhiteRatingDiff "([^"]*)"\\]')[, 2],
    black_rating_diff = str_match(game_matrix[, 11], '\\[BlackRatingDiff "([^"]*)"\\]')[, 2],
    eco = str_match(game_matrix[, 12], '\\[ECO "([^"]*)"\\]')[, 2],
    opening = str_match(game_matrix[, 13], '\\[Opening "([^"]*)"\\]')[, 2],
    time_control = str_match(game_matrix[, 14], '\\[TimeControl "([^"]*)"\\]')[, 2],
    termination = str_match(game_matrix[, 15], '\\[Termination "([^"]*)"\\]')[, 2],
    game = game_matrix[, 17]
  )
  
  return(game_dt)
}

gamify <- function(file_lines, delimiter){
  
  game_lines <- split(file_lines, cumsum(grepl(delimiter, file_lines)))
  
  return(game_lines)
}

calculatePlayerStats <- function(player) {
  player_games <- games_dt[white == player | black == player]

  winsAsWhite <- player_games[result == "1-0" & white == player]
  winsAsBlack <- player_games[result == "0-1" & black == player]
  lossesAsWhite <- player_games[result == "0-1" & white == player]
  lossesAsBlack <- player_games[result == "1-0" & black == player]
  drawsAsWhite <- player_games[result == "1/2-1/2" & white == player]
  drawsAsBlack <- player_games[result == "1/2-1/2" & black == player]

  player_stats <- data.frame(
    player = player,
    elo = mean(c(as.numeric(player_games$white_elo), as.numeric(player_games$black_elo))) %>% round(),
    winsAsWhite = nrow(winsAsWhite),
    winsAsBlack = nrow(winsAsBlack),
    lossesAsWhite = nrow(lossesAsWhite),
    lossesAsBlack = nrow(lossesAsBlack),
    drawsAsWhite = nrow(drawsAsWhite),
    drawsAsBlack = nrow(drawsAsBlack),
    winPercentage = round((nrow(winsAsWhite) + nrow(winsAsBlack)) / nrow(player_games), 2),
    lossPercentage = round((nrow(lossesAsWhite) + nrow(lossesAsBlack)) / nrow(player_games), 2),
    drawPercentage = round((nrow(drawsAsWhite) + nrow(drawsAsBlack)) / nrow(player_games), 2)
  )
  return(player_stats)
}

calculateStats <- function(game_dt){
    # Calculate percentages for each player
    player_stats <- data.table(
      player = c(game_dt$white, game_dt$black)
    ) %>% 
      unique() %>% 
      mutate(
        winPercentage = map_dbl(player, ~ round((nrow(game_dt[result == "1-0" & white == .x]) + nrow(game_dt[result == "0-1" & black == .x])) / nrow(game_dt[white == .x | black == .x]), 2)),
        lossPercentage = map_dbl(player, ~ round((nrow(game_dt[result == "0-1" & white == .x]) + nrow(game_dt[result == "1-0" & black == .x])) / nrow(game_dt[white == .x | black == .x]), 2)),
        drawPercentage = map_dbl(player, ~ round((nrow(game_dt[result == "1/2-1/2" & white == .x]) + nrow(game_dt[result == "1/2-1/2" & black == .x])) / nrow(game_dt[white == .x | black == .x]), 2))
      )

    return(player_stats)
}

count_lower_elo <- function(x, threshold = 800) {
  sum(x <= threshold)
}

find_elo <- function(games_dt){
  setDT(games_dt)
  elo <- games_dt[, .(elo = mean(c(as.numeric(white_elo), as.numeric(black_elo)))), by = .(player = fcoalesce(white, black))]
  elo[, elo := round(elo)]
  return(elo)
}