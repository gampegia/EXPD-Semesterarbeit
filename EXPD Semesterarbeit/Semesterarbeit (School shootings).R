setwd("C:\\Users\\jonas\\OneDrive\\Dokumente\\GitHub\\EXPD-Semesterarbeit\\EXPD Semesterarbeit")
#setwd("C:\\Users\\glm87\\Documents\\GITHUB\\EXPD-Semesterarbeit\\EXPD Semesterarbeit")
# install.packages("ggplot2")
library(ggplot2)
library(readr)
dat <- read_csv("school-shootings-data.csv")
View(dat)

# Age of Shooter
vec_shooter_age <- c(dat$age_shooter1,dat$age_shooter2) 
vec_shooter_age <- na.exclude(vec_shooter_age)

hist(vec_shooter_age, col = "lightblue", xlab = "Age [Years]",
     main = "Age of school shooters", ylab = "Density",
     breaks = c(seq(0, 80, by = 1)),
     las = 1, freq = FALSE)
axis(side=1, at=seq(0, 100, by=10))
median_age_shooter <- median(vec_shooter_age,na.rm = TRUE)
abline(v = median_age_shooter, col = "red", lty = 1, lwd = 2)
legend("top", legend = paste("Median =", median_age_shooter), col = "red", lty = 1, lwd = 2)

# Number schoolshootings
hist(dat$year,
     breaks = c(seq(1999, 2023)),
     main = "Numbers of school shootings in the US since 1999",
     xaxt = "n",
     xlab = "Years",
     col = "lightblue")
axis(side = 1, at = seq(1999, 2023, by = 1), las = 2)

meanshootings <- length(dat$year)/length(unique(dat$year))

# Median
mediantable <- table(dat$year)
medianshootings <- median(c(mediantable))

# Add Median and Legend to plot
abline(h = medianshootings, col = "red", lty = 1, lwd = 2)
legend("top", legend = paste("Median Shootings: ", round(medianshootings, 2)), col = "red", lty = 1, lwd = 2)


# W8

# Diagramm 1

plot(x = dat$age_shooter1,
     y = dat$casualties,
     col = adjustcolor("black", alpha = 0.2),
     pch = 19,
     main = "Shooter Age / Number of casualties",
     ylab = "Number of casualties",
     xlab = "Age of the shooter",
     log = "y",
     )
abline(v = median_age_shooter, col = "red", lwd = 2)
legend("topright", legend = paste("Median age of shooter: ", round(median_age_shooter, 2)), col = "red", lty = 1, lwd = 2)

# DIAGRAMM 2

diag_shooting_type <- ggplot(dat, aes(x = casualties, y = shooting_type)) +
  geom_boxplot()
diag_shooting_type + labs(x = "Casualties" , y = "Shooting Type" , title ="Casulties per shooting Type"  )


#------------------------------------------------
# Abgabe 4
# Multivariate Grafik 1

# install.packages("ggmosaic", dependencies = T)
library(ggmosaic)

library(readr)
US_partys <- read_csv("US partys.csv")

US_partys$dominant_party <- ifelse(US_partys$`Republican/lean Rep.` > US_partys$`Democrat/lean Dem.`, "Republican", "Democratic")
names(US_partys)[names(US_partys) == "State"] <- "state"


dat <- merge(dat, US_partys[,c("state", "dominant_party")],
                     by = "state",
                     all.x = TRUE)

party_colors <- c("Democratic" = "blue", "Republican" = "red")

ggplot(data = dat, aes(x = injured, y = killed, color = dominant_party, shape = school_type)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2)) +
  scale_color_manual(values = party_colors) +
  ggtitle("Injuries and Killings by Party Affiliation and School Type")



ggplot(data = dat, aes(x = dominant_party, fill = dominant_party)) +
  geom_bar() +
  scale_fill_manual(values = party_colors) +
  ggtitle("Injuries and Killings by Party Affiliation and School Type")




# Abgabe 4
# Multivariate Grafik 2
# install.packages("ggmosaic", dependencies = T)
library(ggmosaic)
library(readr)
guns <- read_csv("gun_pc_23.csv")
dat <- merge(dat, guns[,c("state", "gpc")],
             by = "state",
             all.x = TRUE)


US_partys <- read_csv("US partys.csv")

US_partys$dominant_party <- ifelse(US_partys$`Republican/lean Rep.` > US_partys$`Democrat/lean Dem.`, "Republican", "Democratic")
names(US_partys)[names(US_partys) == "State"] <- "state"


dat <- merge(dat, US_partys[,c("state", "dominant_party")],
             by = "state",
             all.x = TRUE)

party_colors <- c("Democratic" = "blue", "Republican" = "red")

ggplot(data = dat, aes(x = gpc, y = killed, color = dominant_party)) +
  geom_point(position = position_jitter(width = 0.2, height = 0.2)) +
  scale_color_manual(values = party_colors) +
  ggtitle("Gun Ownership per Capita vs. Killings by Party Affiliation")


ggplot(data = dat, aes(x = dominant_party, fill = dominant_party)) +
  geom_bar() +
  scale_fill_manual(values = party_colors) +
    ggtitle("Gun Ownership per Capita vs. Killings by Party Affiliation")





