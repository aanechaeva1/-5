#renv::init() # инициализация виртуального окружения
#renv::install() # установка библиотеки из CRAN
#renv::snapshot() # делаем снимок версий библиотек в нашем виртуальном окружении
# фиксируем этот список в .lock-файле для возможности восстановления
# renv::restore() # команда отктиться к предыдушему удачному обновления библиотек

# ------------------- 
# Лабораторная работа №5:
# Написание классов S3, S4.

# Для примера посмотрим на регрессионный анализ с использованием ф-ии lm()
?lm
x <- c(1:3)
y <- c(1,3,8)
lmout <- lm(y~x)
class(lmout)
lmout # это объект класса lm

# напишем класс S3
j <- list(
  name="Joe",
  salary=55000,
  union=T
)
class(j) <- "employee"

attributes(j)

j # вызвали метод print по умлочанию к этому классу

# -------------Напишем собственный метод для нашего класса j
print.employee <- function(wrkr){
  cat(wrkr$name, "\n")
  cat("Зарплата", wrkr$salary, "\n")
  cat("Является ли членом профсоюза?", wrkr$union, "\n")
}

# теперь любой вызов print() для объекта класса employee теперь должен обозначаться print.employee()
methods(,"employee")
print.employee

j


# ----------- Наследование
# Смысл наследования - в создании новых объектов, как специализированных версий объектов-родителей

k <- list(
  name = "Kate",
  salary = 68000,
  union = F,
  hrsthismonth = 2 # новый класс содержит одну доп переменную по сравнению с родительским
)
class(k) <- c("hrlyemployee", "employee") # эта доп переменная указывается первой
k


# Задание №1 - создать класс Домашнее животное с подклассами (кошка, собака, корова), у которых есть общие методы (наследуемые) и собственные.
cat <- list(
  type="cat",
  name="Eva",
  cuteness = 10
)
class(cat) <- "pets"

class(p) <- c("cat","dog", "cow")
print.pets <- function(pets){
  cat(pets$name, "\n")
  cat("Type", pets$type/ "\n")
  cat("Насколько милый?", pets$cuteness, "\n")
}
methods(,"pets")
print.pets

cow <- list(
  type ="cow",
  name ="Burenka",
  cuteness = 10,
  milk = "nice"
)
class(cow) <- c("milk", "pets")

dog <- list(
  type ="dog",
  name ="Bobik",
  cuteness = 7,
  jocundity = "very"
)
class(dog) <- c("jocundity", "pets")



# S4 класс - более безопасный с точки зрения типов данных и ошибок в орфографии.
# S4  класс задается немного по-другому
setClass("employee",
         representation(
           name="character",
           salary="numeric",
           union="logical")
)
joe <- new("employee", name="Joe", salary=55000, union=T)
joe

joe@name
joe@salary
joe@union

slot(joe, "salary") <- 88000
joe@salary

# обобщенный метод для класса S4
setMethod("show", "employee",
          function(object){
            inorout <- ifelse(object@union, "is", "is not")
            cat(object@name, "has a salary of", object@salary,
                "and", inorout, "in the union", "\n")
          }
  )
joe



# Машины
setClass("cars",
         slots = list(),
         contains = "environment")
setGeneric("ride",
           function(object) 
           {
             standardGeneric("ride")
           }
)
setGeneric("bibikat",
           function(object) 
           {
             standardGeneric("bibikat")
           }
)
setMethod("ride",
          signature(object = "cars"),
          function(object) 
          {
            cat("I drive\n")
          }
)

setMethod("bibikat",
          signature(object = "cars"),
          function(object) 
          {
            cat("bi-bi\n")
          }
)
setClass("Дизельный_автомобиль",
         slots = list(),
         contains = "cars")

setMethod("ride",
          signature(object = "Дизельный_автомобиль"),
          function(object) 
          {
            cat("I drive on diesel fuel\n")
          }
)

setClass("Бензиновый_автомобиль",
         slots = list(),
         contains = "cars")

setMethod("ride",
          signature(object = "Бензиновый_автомобиль"),
          function(object) 
          {
            cat("I drive on gasoline\n")
          }
)

setClass("Электромобиль",
         slots = list(),
         contains = "cars")

setMethod("ride",
          signature(object = "Электромобиль"),
          function(object) 
          {
            cat("I drive on electricity\n")
          }
)

start_cars <- function() {
  выбор <- readline(prompt = "Какой класс автомобилей вас интересует? (Дизельный, Бензиновый, Электрический): ")
  
  if (tolower(выбор) == "дизельный") {
    cars <- new("Дизельный_автомобиль")
  } else if (tolower(выбор) == "бензиновый") {
    cars <- new("Бензиновый_автомобиль")
  } else if (tolower(выбор) == "электрический") {
    cars <- new("Электромобиль")
  } else {
    cat("Неверный выбор!\n")
    return
  }
  
  cat("Родительские методы:\n")
  ride(cars)
  bibikat(cars)
  cat("Собственные методы:\n")
  if (class(cars) == "Дизельный_автомобиль") 
  {
    cat("Едет на солярке\n")
  } 
  else if (class(cars) == "Бензиновый_автомобиль") 
  {
    cat("Едет на бензине\n")
  } 
  else if (class(cars) == "Электромобиль") 
  {
    cat("Едет на электричестве\n")
  }
}

start_cars()