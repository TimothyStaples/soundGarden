setwd("/home/timothy/Dropbox/Tim/Post-doc/Research projects/soundGarden")

snakeN = 20
repN = 4
soundN = 4

# set up a data-frame with each row being one trial for one snake,
# and randomly drawing the order of these sounds into our trial blocks
test <- do.call("rbind", lapply(1:snakeN, function(n){
  
  data.frame(snake = rep(n, repN * soundN),
             sound = rep((1:soundN)-1, repN),
             trialBlock = sample(1:(repN * soundN)))
  
  
}))

# now split this data into each trial block, randomize the order so snakes are
# in different positions within the trial, and allocate a random speaker corner
# for the trial block
trial <- do.call("rbind", lapply(split(test, f=test$trial), function(x){
  
  # randomize snake order
  x$orderInTrial <- sample(1:nrow(x))
  
  # allocate a random corner
  x$corner <- sample(1:4, 1)
  return(x)
}))

# add snake names using ID numbers
snakeId <- read.csv("snakeId.csv")

soundGardenTrial <- merge(trial, snakeId,
                          by.x="snake", by.y="snakNum", sort=FALSE)

# resort by trial
soundGardenTrial <- soundGardenTrial[order(soundGardenTrial$trialBlock,
                                           soundGardenTrial$orderInTrial),]

write.csv(soundGardenTrial, "./soundGardenTrial.csv")
