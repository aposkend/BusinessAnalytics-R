#Question 1
simple_shooting_simulation <- function(num_shots, hit_rate){
  hit_count <- rbinom(num_shots, size=1, prob=hit_rate)
  total_hits <- sum(hit_count)
  actual_hit_rate <- (total_hits/num_shots)
  return(list(total_hits, actual_hit_rate))
}

result <- simple_shooting_simulation(100, 0.75)
print(result)

#ANS: result

#Question 2
basketball_series <- function(team1_name="Team 1", team2_name="Team 2"){
  
  comp_count <- 0 #比賽場次
  team1_win <- 0 #隊伍1獲勝次數
  team2_win <- 0 #隊伍2獲勝次數
  
  #只要兩隊都沒獲勝4次以上(含)，就持續比賽
  while(team1_win < 4 && team2_win < 4){
    comp_count <- comp_count + 1 #更新比賽場次
    
    #從Uniform分佈中在80~120之間取1個數作為分數
    #因為Uniform分佈是連續的機率分佈
    #因此得到的分數要經過round變成整數
    team1_score <- round(runif(1, 80, 120)) 
    team2_score <- round(runif(1, 80, 120))
    
    #印出結果
    print(sprintf("------第 %s 場比賽------", comp_count))
    print(sprintf("隊伍1: %s 得分為: %s", team1_name, team1_score))
    print(sprintf("隊伍2: %s 得分為: %s", team2_name, team2_score))
    
    #判斷本次誰獲勝
    if(team1_score > team2_score){
      team1_win <- team1_win + 1
      print(sprintf("本次由隊伍1: %s 獲勝, 累計勝場為: %s", team1_name, team1_win))
    }else if(team2_score > team1_score){
      team2_win <- team2_win + 1
      print(sprintf("本次由隊伍2: %s 獲勝, 累計勝場為: %s", team2_name, team2_win))
    }else{
      print(sprintf("平手 雙方皆不加分"))
    }
  }
  
  #印出最終結果
  print("------最終結果------")
  print(sprintf("隊伍1: %s 共獲得 %s 勝場", team1_name, team1_win))
  print(sprintf("隊伍2: %s 共獲得 %s 勝場", team2_name, team2_win))
  
  #判斷最後誰獲勝
  if(team1_win > team2_win){
    print(sprintf("最終比賽結果為： %s 獲勝", team1_name))
  }else if(team1_win < team2_win){
    print(sprintf("最終比賽結果為： %s 獲勝", team2_name))
  }else{
    print(sprintf("最終比賽結果為： 平手"))
  }
}

basketball_series(team1_name = "A班", team2_name = "B班")

