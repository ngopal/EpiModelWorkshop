# Tutorial
library(statnet)
data(samplk)

samp <- list()
samp[[1]] <- samplk1
samp[[2]] <- samplk2
samp[[3]] <- samplk3

plot(samplk1)

samp.fit <- stergm(samp,
                   formation= ~edges+mutual+cyclicalties+transitiveties,
                   dissolution = ~edges+mutual+cyclicalties+transitiveties,
                   estimate = "CMLE",
                   times=1:3
)

summary(samp.fit)

samp.fit.2 <- stergm(samp,
                     formation= ~edges+mutual+cyclicalties+transitiveties,
                     dissolution = ~edges+mutual+cyclicalties+transitiveties,
                     estimate = "CMLE",
                     times=1:2
)

data(florentine)
plot(flobusiness)

theta.diss <- log(9)
X11()

stergm.fit.1 <- stergm(flobusiness,
                       formation= ~edges+gwesp(0,fixed=T),
                       dissolution = ~offset(edges),
                       targets="formation",
                       offset.coef.diss = theta.diss,
                       estimate = "EGMME",
                       control=control.stergm(SA.plot.progress=TRUE)
)

dev.off()

mcmc.diagnostics(stergm.fit.1)

stergm.fit.1
names(stergm.fit.1)

stergm.sim.1 <- simulate.stergm(stergm.fit.1, nsim=1,
                                time.slices = 1000)

stergm.sim.1

net429 <- network.collapse(stergm.sim.1,at=429)
net429
plot(network.collapse(stergm.sim.1,at=882))

summary(flobusiness~edges+gwesp(0,fixed=T))
colMeans(attributes(stergm.sim.1)$stats)
plot(attributes(stergm.sim.1)$stats)
plot(as.matrix(attributes(stergm.sim.1)$stats))

stergm.sim.1.dm <- as.data.frame(stergm.sim.1)
names(stergm.sim.1.dm)
mean(stergm.sim.1.dm$duration)

get.edge.activity(stergm.sim.1, 25)
get.edge.activity(stergm.sim.1, 25, as.spellList=T)

require(ndtv) || install.packages('ndtv', repos="http://statnet.csde.washington.edu")
stergm.sim.50 <- simulate.stergm(stergm.fit.1, nsim=1,
                                 time.slices = 50)

render.par=list(tween.frames=5,show.time=T,
                show.stats="~edges+gwesp(0,fixed=T)")

wealthsize = log(get.vertex.attribute(flobusiness, "wealth")) * 2/3

render.animation(stergm.sim.50,render.par=render.par,
                 edge.col="darkgray",displaylabels=T,
                 label.cex=.8,label.col="blue",
                 vertex.cex=wealthsize)

ani.replay()

# If I get the video codec then I can save the animation as an mp4 or gif

theta.diss.100 <- log(99)
fit1 <- ergm(flobusiness ~ edges+gwesp(0,fixed=T))
theta.form <- fit1$coef
theta.form[1] <- theta.form[1] - theta.diss.100

stergm.sim.2 <- simulate(flobusiness,
                         formation=~edges+gwesp(0,fixed=T),
                         dissolution=~edges,
                         monitor="all",
                         coef.form=theta.form,
                         coef.diss=theta.diss.100,
                         time.slices=10000)


summary(flobusiness~edges+gwesp(0,fixed=T))
colMeans(attributes(stergm.sim.2)$stats)
stergm.sim.dm.2 <- as.data.frame(stergm.sim.2)
mean(stergm.sim.dm.2$duration)
plot(attributes(stergm.sim.2)$stats)


msm.net <- network.initialize(500, directed=F)
msm.net %v% 'race'<- c(rep(0,250),rep(1,250))
msm.net
msm.form.formula <- ~edges+nodematch('race')+degree(0)+concurrent
msm.target.stats <- c(225,187,180,90) #values are proportional to known, e.g. 500*0.18 = 90
msm.diss.formula <- ~offset(edges)+offset(nodematch("race"))
msm.theta.diss <- c(2.944, -0.747)

set.seed(0)

msm.fit <- stergm(msm.net,
                  formation= msm.form.formula,
                  dissolution= msm.diss.formula,
                  targets="formation",
                  target.stats= msm.target.stats,
                  offset.coef.diss = msm.theta.diss,
                  estimate = "EGMME",
                  control=control.stergm(SA.plot.progress=TRUE,
                                         SA.init.gain=0.005)
)

mcmc.diagnostics(msm.fit)
summary(msm.fit)


# Lab
n <- 500
msm.net <- network.initialize(n, directed=F)
formula <- msm.net ~ edges + degree(0:1)
constraints <- ~bd(maxout=3)

# Degree 0 are isolates, Degree 1 are folks with 1 connection
degdist.m1 <- c(0.35, 0.45, 0.14, 0.06)
edges.m1 <- n*(degdist.m1 %*% 0:3)/2
degct.m1 <- n*degdist.m1
target.stats.m1 <- c(edges.m1, degct.m1[1:2])

msm.m1 <- ergm(formula, 
               target.stats = target.stats.m1, 
               constraints = constraints)

msm.net.m1 <- simulate(msm.m1)

# We are using theta to correct for the next and previous step. It's the edges that
# make a major difference in this model so we only care about correcting for the edges
# the log(25) comes from log(D-1) where D is the duration of the sample (26 months)
theta.diss.m1 <- log(25)
theta.form.m1 <- msm.m1$coef
theta.form.m1[1] <- theta.form.m1[1] - theta.diss.m1

sim.stergm.m1 <- simulate(msm.net.m1,
                          formation = ~edges+degree(0:1),
                          dissolution = ~edges,
                          monitor = "all",
                          coef.form = theta.form.m1,
                          coef.diss = theta.diss.m1,
                          constraints = constraints,
                          time.slices = 100)

colMeans(attributes(sim.stergm.m1)$stats)

library(ndtv)
slice.par=list(start = 0, 
               end = 25, 
               interval = 1, 
               aggregate.dur = 1, 
               rule = 'any')
compute.animation(sim.stergm.m1, slice.par = slice.par)

render.par = list(tween.frames = 10, 
                  show.time = T, 
                  show.stats = "~degree(0:3)")
plot.par=list(mar = c(1,0,0,0), 
              mgp = c(0,0,0))
render.animation(sim.stergm.m1, 
                 render.par = render.par, 
                 plot.par = plot.par,
                 vertex.cex = 0.9, 
                 vertex.col = 'firebrick', 
                 edge.col = 'darkgrey', 
                 vertex.border = 'lightgrey',
                 displaylabels = FALSE)



