# Q1
# install.packages('actuar'); library('actuar');
ntrials = 1000
set.seed(1)
shift = .3
roos_rt = rinvgauss(ntrials, 0.769)
eduardo_rt = rinvgauss(ntrials, 0.896)
yasemin_rt = rinvgauss(ntrials, 0.938)

layout(1:3)
hist(roos_rt, breaks = 20)  # Typical method of plotting choice-RTs
hist(eduardo_rt, breaks = 20)
hist(yasemin_rt, breaks = 20, xlab = 'Choice * Response Time (sec)')


# Q2
mydata = read.csv('living-experiment-responses.csv')

## Subject 1
subject1 <- subset(mydata, Subject == 249833)
# get proportion of correct choices for living things
subject1_TP <- nrow(subset(subject1, Accuracy == 1 & Word == 'living')) / 50 # 1.0
# get proportion of incorrect choices for nonliving things
subject1_FN <- nrow(subset(subject1, Accuracy == 0 & Word == 'nonliving')) / 50 # 0.24

dprime_est1 = (1 / sqrt(2)) * (qnorm(0.99) - qnorm(subject1_FN))
dprime_est1 # 2.144

## Subject 2
subject2 <- subset(mydata, Subject == 249903)
subject2_TP <- nrow(subset(subject2, Accuracy == 1 & Word == 'living')) / 50 # 0.94
subject2_FN <- nrow(subset(subject2, Accuracy == 0 & Word == 'nonliving')) / 50 # 0.12

dprime_est2 = (1 / sqrt(2)) * (qnorm(subject2_TP) - qnorm(subject2_FN))
dprime_est2 # 1.930


# Q3
# response times, accuracies and condition
rts1 = mydata$RT[mydata$Subject == 249833]/1000  #Convert to seconds
accuracy1 = mydata$Accuracy[mydata$Subject == 249833]
condition1 = mydata$Word[mydata$Subject == 249833]

set.seed(1)
replications = 1000
trial_index  = seq(1,length(accuracy1))
est_dprimes = c()
for(i in 1:replications){
  bootstrap_index = sample(trial_index, replace=T)  # Resample
  acc_boot1 = accuracy1[bootstrap_index]
  cond_boot1 = condition1[bootstrap_index]
  answer_word_given_word = mean((acc_boot1 == 1) & (cond_boot1 == "living"))
  answer_word_given_nonword = mean((acc_boot1 == 0) & (cond_boot1 == "nonliving"))
  dprime_est = (1 / sqrt(2)) * (qnorm(answer_word_given_word) - qnorm(answer_word_given_nonword))
  est_dprimes[i] = dprime_est
}
bootstrap_samps <- sort(est_dprimes)
c(bootstrap_samps[0.025*replications], bootstrap_samps[0.975*replications])

replications = 3000
trial_index  = seq(1,length(accuracy1))
est_dprimes = c()
for(i in 1:replications){
  bootstrap_index = sample(trial_index, replace=T)  # Resample
  acc_boot1 = accuracy1[bootstrap_index]
  cond_boot1 = condition1[bootstrap_index]
  answer_word_given_word = mean((acc_boot1 == 1) & (cond_boot1 == "living"))
  answer_word_given_nonword = mean((acc_boot1 == 0) & (cond_boot1 == "nonliving"))
  dprime_est = (1 / sqrt(2)) * (qnorm(answer_word_given_word) - qnorm(answer_word_given_nonword))
  est_dprimes[i] = dprime_est
}
bootstrap_samps <- sort(est_dprimes)
c(bootstrap_samps[0.025*replications], bootstrap_samps[0.975*replications])


# Q5
ntrials = 50
pChooseBlue_BlueCorrect = .819
pChooseYellow_YellowCorrect = .885
set.seed(1)
BlueCorrectSamp = rbinom(ntrials, 1, pChooseBlue_BlueCorrect)
YellowCorrectSamp = rbinom(ntrials, 1, pChooseYellow_YellowCorrect)

layout(1:2)
hist(BlueCorrectSamp, ylim=c(0,ntrials), xlab='Blue (2.35 cpd) was correct')
hist(YellowCorrectSamp, ylim=c(0,ntrials), xlab='Yellow (2.65 cpd) was correct')

# d' calculation
pestChooseBlue_BlueCor = mean(BlueCorrectSamp)
pestChooseBlue_YellowCor = 1 - mean(YellowCorrectSamp)
dprime_est = (1 / sqrt(2)) * (qnorm(pestChooseBlue_BlueCor) - qnorm(pestChooseBlue_YellowCor))
dprime_est


# Q7
# Random walk
set.seed(14379791)
nwalks = 50
nsteps = 100
step_length = .01
time = seq(0, 1, length.out = nsteps)
random_walks = matrix(0, nrow = nsteps, ncol = nwalks)
for (w in 1:nwalks)
{
  this_random_walk = 0.5
  for (s in 2:nsteps)
  {
    evidence_units = rnorm(1, 0.01, 0.01)
    this_random_walk[s] = this_random_walk[s - 1] + evidence_units
  }
  random_walks[, w] = this_random_walk
}
x11()
matplot(time, random_walks, type = 'l')


# Q8
# Simulating Drift Diffusion Model (DDM)
set.seed(14379791)
ntrials = 200
nsteps = 300
step_length = .01
time = seq(0, nsteps * step_length, length.out = nsteps)
boundary = 1.2
drift = rnorm(ntrials, 1.5, .5)
ndt = 0.2
dc = 1
random_walks = matrix(0, nrow = nsteps, ncol = ntrials)
rts = rep(0, ntrials)
correct = rep(1, ntrials)

for (n in 1:ntrials)
{
  random_walks[1, n] = 0.5 * boundary  # relative start point = 0.5
  for (s in 2:nsteps)
  {
    random_walks[s, n] = random_walks[s - 1, n] + rnorm(1, drift[n] * step_length, dc *
                                                          sqrt(step_length))
    if (random_walks[s, n] >= boundary)
    {
      random_walks[-(1:(s - 1)), n] = boundary
      rts[n] = s * step_length + ndt
      correct[n] = 1
      break
    } else if (random_walks[s, n] <= 0) {
      random_walks[-(1:(s - 1)), n] = 0
      rts[n] = s * step_length + ndt
      correct[n] = 0
      break
    } else if (s == nsteps) {
      correct[n] = NA
      rts[n] = NA
      break
    }
  }
}
x11()
matplot(time, random_walks, type = 'l')
x11()
hist((correct * 2 - 1) * rts, breaks = 20)
mean(correct, na.rm = TRUE)
mean(rts, na.rm = TRUE)

# Q9

# data for accuracy
acc_living <- rbinom(100, 1, 0.8)
acc_living

acc_nonliving <- rbinom(100, 1, 0.7)
acc_nonliving


# data for response times
rt_living <- rinvgauss(100, mean = 0.91)
rt_living

rt_nonliving <- rinvgauss(100, mean = 0.75)
rt_nonliving

data <- data.frame(
  Subject = 249833,
  Accuracy = c(acc_living, acc_nonliving),
  Word = rep(c("living", "nonliving"), each = 100),
  RT = c(rt_living, rt_nonliving)
)
data


# Q10
# Fitting DDMs to data (using EZ Diffusion)
# simple EZ-diffusion model based on Wagenmakers et al, 2007
ezdiff = function(rt, correct, s=1.0)
{
  if (length(rt) <= 0) stop('length(rt) <= 0')
  if (length(rt) != length(correct)) stop('RT and correct unequal lengths')
  if (max(correct, na.rm=TRUE) > 1) stop('Correct values larger than 1')
  if (min(correct, na.rm=TRUE) < 0) stop('Correct values smaller than 0')
  pc = mean(correct, na.rm=TRUE)
  print(pc)
  if (pc <= 0) stop('Mean accuracy less than or equal to 0')
  # subtract or add 1/2 an error to prevent division by zero
  if (pc == 1.0) {pc=1 - 1/(2*length(correct))}
  if (pc == 0.5) {pc=0.5 + 1/(2*length(correct))}
  MRT = mean(rt[correct == 1], na.rm=TRUE)
  VRT = var(rt[correct == 1], na.rm=TRUE)
  if (VRT <= 0) stop('RT variance less than or equal to 0')
  r=(qlogis(pc)*(((pc^2)*qlogis(pc)) - pc*qlogis(pc) + pc - 0.5))/VRT
  drift=sign(pc-0.5)*s*(r)^0.25
  boundary=(s^2 * qlogis(pc))/drift
  y=(-1*drift*boundary)/(s^2)
  MDT=(boundary/(2*drift))*((1-exp(y))/(1+exp(y)))
  ndt=MRT-MDT
  return(list(boundary, drift, ndt))
}

# Subject 1
rts1 = mydata$RT[mydata$Subject == 249833] / 1000
accuracy1 = mydata$Accuracy[mydata$Subject == 249833]
accuracy1

est_params1 = ezdiff(rts1, accuracy1)
est_params1


# Subject 2
rts2 = mydata$RT[mydata$Subject == 250252] / 1000
accuracy2 = mydata$Accuracy[mydata$Subject == 250252]
accuracy2

est_params2 = ezdiff(rts2, accuracy2)
est_params2


# Q9
## answer
set.seed(14379791)
replications = 1000
subject1 <- subset(mydata, Subject == 249833)

# subject 1:
trial_index_sub1 <- seq(1, nrow(subject1))
est_drift_rate_sub1 <- c()
est_boundry_sub1 <- c()

for (i in 1:replications) {
  bootstrap_index = sample(trial_index_sub1, replace = T)  # Resample
  rt = subject1$RT[bootstrap_index] / 1000
  acc = subject1$Accuracy[bootstrap_index]
  parameter_est = unlist(ezdiff(rt, acc))
  est_boundry_sub1[i] = parameter_est[1]
  est_drift_rate_sub1[i] = parameter_est[2]
}

est_drift_rate_sub1 <- sort(est_drift_rate_sub1)
est_boundry_sub1 <- sort(est_boundry_sub1)

c(est_drift_rate_sub1[0.025 * replications], est_drift_rate_sub1[0.975 * replications])
c(est_boundry_sub1[0.025 * replications], est_boundry_sub1[0.975 * replications])

# subject 2:
set.seed(14379791)
replications = 1000
subject2 <- subset(mydata, Subject == 250252)

trial_index_sub2 <- seq(1, nrow(subject2))
est_drift_rate_sub2 <- c()
est_boundry_sub2 <- c()

for (i in 1:replications) {
  bootstrap_index = sample(trial_index_sub2, replace = T)  # Resample
  rt = subject2$RT[bootstrap_index] / 1000
  acc = subject2$Accuracy[bootstrap_index]
  parameter_est = unlist(ezdiff(rt, acc))
  est_boundry_sub2[i] = parameter_est[1]
  est_drift_rate_sub2[i] = parameter_est[2]
}

est_drift_rate_sub2 <- sort(est_drift_rate_sub2)
est_boundry_sub2 <- sort(est_boundry_sub2)
c(est_drift_rate_sub2[0.025 * replications], est_drift_rate_sub2[0.975 * replications])
c(est_boundry_sub2[0.025 * replications], est_boundry_sub2[0.975 * replications])

