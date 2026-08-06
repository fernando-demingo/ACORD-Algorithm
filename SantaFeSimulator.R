get_santa_fe_map <- function() {
  m <- matrix(0, 32, 32)
  # Standard Santa Fe Trail fragments
  trail <- list(
    c(1,2), c(1,3), c(1,4), c(1,5),
    c(2,5), c(3,5), c(4,5), c(5,5), c(6,5),
    c(6,6), c(6,7), c(6,8), c(6,9),
    c(7,9), c(8,9), c(9,9), c(9,10), c(9,11), c(9,12),
    c(10,12), c(11,12), c(12,12), c(13,12), c(14,12), c(15,12),
    c(15,13), c(15,14), c(15,15), c(15,16),
    c(14,16), c(13,16), c(12,16), c(11,16), c(10,16),
    c(10,17), c(10,18), c(10,19), c(10,20)
  )
  for(p in trail) {
    if(p[1] <= 32 && p[2] <= 32) m[p[1], p[2]] <- 1
  }
  m
}

simulate_santa_fe <- function(code_tokens, max_steps = 400) {
  m <- get_santa_fe_map()
  food_total <- sum(m)
  food_collected <- 0
  steps <- 0
  pos <- c(1, 1)
  dir <- 1 # 1: Right, 2: Down, 3: Left, 4: Up
  dx <- c(0, 1, 0, -1)
  dy <- c(1, 0, -1, 0)

  tokens <- unlist(code_tokens)
  if(length(tokens) == 0) return(0)

  # Recursive interpreter with step limit
  interpret <- function(start_idx) {
    i <- start_idx
    while(i <= length(tokens) && steps < max_steps) {
      t <- tokens[i]
      if (t == "move") {
        steps <<- steps + 1
        pos <<- ((pos + c(dx[dir], dy[dir]) - 1) %% 32) + 1
        if (m[pos[1], pos[2]] == 1) {
          food_collected <<- food_collected + 1
          m[pos[1], pos[2]] <<- 0
        }
      } else if (t == "left") {
        steps <<- steps + 1
        dir <<- ((dir - 2) %% 4) + 1
      } else if (t == "right") {
        steps <<- steps + 1
        dir <<- (dir %% 4) + 1
      } else if (t == "ifFoodAhead") {
        steps <<- steps + 1
        ahead <- ((pos + c(dx[dir], dy[dir]) - 1) %% 32) + 1
        has_food <- m[ahead[1], ahead[2]] == 1

        # Simple brace matching for demonstration
        # ifFoodAhead { T } else { F }
        # We find indices of { and }
        braces <- which(tokens == "{" | tokens == "}")
        if (length(braces) >= 4) {
          # Very rough block index finding
          t_start <- braces[1] + 1
          t_end <- braces[2] - 1
          f_start <- braces[3] + 1
          f_end <- braces[4] - 1

          if (has_food) {
            # Execute True block
            for (j in t_start:t_end) if(steps < max_steps) interpret_single(tokens[j])
          } else {
            # Execute False block
            for (j in f_start:f_end) if(steps < max_steps) interpret_single(tokens[j])
          }
          i <- braces[4] # Skip the whole if structure
        }
      }
      i <- i + 1
    }
  }

  interpret_single <- function(t) {
    if (t == "move") {
      steps <<- steps + 1
      pos <<- ((pos + c(dx[dir], dy[dir]) - 1) %% 32) + 1
      if (m[pos[1], pos[2]] == 1) {
        food_collected <<- food_collected + 1
        m[pos[1], pos[2]] <<- 0
      }
    } else if (t == "left") {
      steps <<- steps + 1
      dir <<- ((dir - 2) %% 4) + 1
    } else if (t == "right") {
      steps <<- steps + 1
      dir <<- (dir %% 4) + 1
    }
  }

  interpret(1)
  return(food_collected)
}
