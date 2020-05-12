require("minerva")

sink("result_byR.txt")  # console �̏o�͓��e��ۑ�(path�͓K�X�ύX)

PrintIndicators <- function(x, y){
  corr <- cor(x, y)
  mine.vals <- mine(x, y)
  
  print(paste("[���`���ւ̎w�W] Pearson Corr = ",
              sprintf("%.3f", corr)
              ),
              quote = F
        )
  print(paste("[���`�Ƃ͌���Ȃ����ւ̎w�W] MIC = ",
              sprintf("%.3f", mine.vals$MIC)
              ),
              quote = F
        )
  print(paste("[�P���֌W���Ȃ��������ǂ����̎w�W] MAS = ",
              sprintf("%.3f", mine.vals$MAS)
              ),
              quote = F
        )
  print(paste("[��̓I�Ȋ֐������肻�����ǂ����̎w�W] MEV = ",
              sprintf("%.3f", mine.vals$MEV)
              ),
              quote = F
        )
  print(paste("[���G���̎w�W] MCN = ",
              sprintf("%.3f", mine.vals$MCN)
              ),
              quote = F
        )
  print(paste("[����`���̎w�W] MIC-R2 = ",
              sprintf("%.3f", mine.vals$`MIC-R2`)
              ),
              quote = F
        )
  print(paste("GMIC = ",
              sprintf("%.3f", mine.vals$GMIC)
              ),
              quote = F
        )
  print(paste("TIC = ",
              sprintf("%.3f", mine.vals$TIC)
              ),
              quote = F
        )
  
  plot(x, y, col = 4, main = "scatter plot of (x, y)", cex.main = 1.3)
}

###############################################################################
# y = x + noise
###############################################################################
set.seed(1)
N <- 100
x <- seq(-10, 10, length = N)
noise <- runif(N, min = -1, max = 1)
y <- x + noise

print("y = x + noise")
PrintIndicators(x, y)

###############################################################################
# y = x^2 + noise
###############################################################################
set.seed(2)
N <- 100
x <- seq(-10, 10, length = N)
noise <- runif(N, min = -10, max = 10)
y <- x**2 + noise

print("y = x^2 + noise")
PrintIndicators(x, y)

###############################################################################
# y = exp(x) + noise
###############################################################################
set.seed(3)
N <- 100
x <- seq(-10, 10, length = N)
noise <- runif(N, min = -1000, max = 1000)
y <- exp(x) + noise

print("y = exp(x) + noise")
PrintIndicators(x, y)

###############################################################################
# y = sin(x) + noise
###############################################################################
set.seed(4)
N <- 100
x <- seq(-10, 10, length = N)
noise <- runif(N, min = -1, max = 1)
y <- sin(x) + noise

print("y = sin(x) + noise")
PrintIndicators(x, y)

###############################################################################
# y = x �� y = -x ��2�̒���
###############################################################################
set.seed(5)
N <- 200
x <- seq(-10, 10, length = N)
y <- c()
noise <- runif(N, min = -1, max = 1)
for (i in 1:N){
  y[i] <- ifelse(i %% 2 == 0, x[i], -x[i])
}
y <- y + noise

print("y = x & y = -x")
PrintIndicators(x, y)

###############################################################################
# x^2 + y^2 = 1
###############################################################################
set.seed(6)
N <- 200
x <- seq(-1, 1, length = N)
y <- c()
noise <- runif(N, min = -0.2, max = 0.2)
for (i in 1:N){
  y[i] <- ifelse(i %% 2 == 0, sqrt(1 - x[i]**2), -sqrt(1 - x[i]**2))
}
y <- y + noise

print("x^2 + y^2 = 1")
PrintIndicators(x, y)

###############################################################################
# y = x^2 �� y = -x^2 ��2�̋Ȑ�
###############################################################################
set.seed(7)
N <- 200
x <- seq(-1, 1, length = N)
y <- c()
noise <- runif(N, min = -0.2, max = 0.2)
for (i in 1:N){
  y[i] <- ifelse(i %% 2 == 0, x[i]**2, -x[i]**2)
}
y <- y + noise

print("y = x^2 & y = -x^2")
PrintIndicators(x, y)

###############################################################################
# y = x - [x]
###############################################################################
set.seed(8)
N <- 100
x <- seq(0, 5, length = N)
noise <- runif(N, min = -0.05, max = 0.05)
y <- x - floor(x) + noise

print("y = x - [x]")
PrintIndicators(x, y)

###############################################################################
# y = x^2 �� z = sin(x) �Ƃ��� y �� z �̑���
###############################################################################
set.seed(9)
N <- 200
x <- seq(-1, 1, length = N)
noise <- runif(N, min = -0.3, max = 0.3)
y <- x**2 + noise
z <- sin(x) + noise

print("y = x^2, z = sin(x), caluc (y, z)")
PrintIndicators(y, z)

###############################################################################
# AR(1) ���f�� y_t = 0.5 y_{t-1} + epsilon_t
###############################################################################
set.seed(10)
N <- 100
x <- seq(-1, 1, length = N)
y <- rep(0, N)  # ������
phi <- 0.5

for (i in 1:N){
  y[i] <- ifelse(i > 2, phi * y[i-1] + rnorm(n = 1, mean = 0, sd = 1), y[1])
}

print("AR(1) model y_t = 0.5 y_{t-1} + epsilon_t")
PrintIndicators(x, y)

# �Q�l
plot(x, y, col = 4, type = "l")

###############################################################################
# y = noise
###############################################################################
set.seed(11)
N <- 100
x <- seq(-10, 10, length = N)
noise <- runif(N, min = -1, max = 1)
y <- noise

print("y = noise")
PrintIndicators(x, y)

###############################################################################
# sink �̐ݒ������ 
###############################################################################

sink()

###############################################################################
# END
###############################################################################