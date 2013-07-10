# Initialize network
lab.ego.net <- network.initialize(1000, directed = F)
summary(lab.ego.net)

# Setup the actual network we have collected real data on
lab.ego.deg <- c(60, 140, 450, 350)
sum(lab.ego.deg)

# Obtain the theoretical number of edges
lab.ego.edges <- sum(0*350+1*450+2*140+3*60)/2

# Create a model of the network object using edges as the only input
lab.ego.fit <- ergm(lab.ego.net ~ edges, target.stats = lab.ego.edges)
summary(lab.ego.fit)

# Run a simulation of a new network based off of the model
lab.ego.sim1 <- simulate(lab.ego.fit)
summary(lab.ego.sim1)

# View the results
plot(lab.ego.sim1, vertex.cex = 0.65)
