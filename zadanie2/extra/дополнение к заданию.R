my.seed <- 1486372882    # ядро
n.all <- 60              # наблюдений всего
train.percent <- 0.85    # доля обучающей выборки
res.sd <- 1              # стандартное отклонение случайного шума задания 1
res.sd.1 <- 2              # стандартное отклонение случайного шума задания 2
x.min <- 5               # границы изменения X: нижняя
x.max <- 105             #  и верхняя

# фактические значения x
set.seed(my.seed)
x <- runif(x.min, x.max, n = n.all)

# случайный шум
set.seed(my.seed)
res <- rnorm(mean = 0, sd = res.sd, n = n.all)
res.1 <- rnorm(mean = 0, sd = res.sd.1, n = n.all)

# отбираем наблюдения в обучающую выборку
set.seed(my.seed)
inTrain <- sample(seq_along(x), size = train.percent*n.all)

# истинная функция взаимосвязи
y.func <- function(x) {12 - 0.05 * x}

# для графика истинной взаимосвязи
x.line <- seq(x.min, x.max, length = n.all)
y.line <- y.func(x.line)

# фактические значения y (с шумом)
y <- y.func(x) + res
y.1 <- y.func(x) + res.1
# Создаём векторы с данными для построения графиков ############################

# наблюдения на обучающей выборке для модели 1
x.train <- x[inTrain]
y.train <- y[inTrain]

# наблюдения на тестовой выборке для модели 1
x.test <- x[-inTrain]
y.test <- y[-inTrain]

# наблюдения на обучающей выборке для модели 2
x.1.train <- x[inTrain]
y.1.train <- y.1[inTrain]

# наблюдения на тестовой выборке для модели 2
x.1.test <- x[-inTrain]
y.1.test <- y.1[-inTrain]


#MSE двух моделей-----

# строим модель из задания 1

mod <- smooth.spline(x = x.train, y = y.train, df = 2)

# модельные значения для расчёта ошибок
y.model.train <- predict(mod, data.frame(x = x.train))$y[, 1]
y.model.test <- predict(mod, data.frame(x = x.test))$y[, 1]

# считаем средний квадрат ошибки на обечающей и тестовой выборке
MSE <- c(sum((y.train - y.model.train)^2) / length(x.train),
         sum((y.test - y.model.test)^2) / length(x.test))


# строим модель из задания 2
mod.1 <- smooth.spline(x = x.1.train, y = y.1.train, df = 2)

# модельные значения для расчёта ошибок
y.model.train.1 <- predict(mod.1, data.frame(x = x.1.train))$y[, 1]
y.model.test.1 <- predict(mod.1, data.frame(x = x.1.test))$y[, 1]

# считаем средний квадрат ошибки на обечающей и тестовой выборке
MSE.1 <- c(sum((y.1.train - y.model.train.1)^2) / length(x.1.train),
         sum((y.1.test - y.model.test.1)^2) / length(x.1.test))
model <- c(1, 2)
MSE.train <- c(0.9624485, 4.344858)
MSE.test <- c(1.3304612, 1.232344)
MSE.table <- data.frame(Model=model,MSE.TRAIN=MSE.train, MSE.TEST=MSE.test )

# степени свободы у наименьшей ошибки на тестовой выборке
min.MSE.test <- min(MSE.table$MSE.TEST)
# степени свободы у наименьшей ошибки на тестовой выборке
min.MSE.table <- min(MSE.table$MSE.TRAIN)
