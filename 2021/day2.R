# Calulate the submarines position

d <- read.table("day2.txt")
colnames(d) <- c("command", "value")

forward <- d$command %in% "forward"
down <- d$command %in% "down"
up <- d$command %in% "up"

depth <- sum(d$value[down]) - sum(d$value[up])
horisontal <- sum(d$value[forward])

depth * horisontal # answer: 2070300


# Include variable 'aim'. All observations are relient on the previous
aim <- 0
horisontal <- 0
depth <- 0

for (i in 1:nrow(d)) {
  command <- d$command[i]
  value <- d$value[i]

  if (command=="forward") {horisontal <- horisontal + value; depth <- depth + value * aim}
  if (command=="up") {aim <- aim - value}
  if (command=="down") {aim <- aim + value}
}

horisontal * depth
