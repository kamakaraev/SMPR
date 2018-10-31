
Задачи классификации
=====================
Метрические алгоритмы классификации
-----------------------------------
Метрический алгоритм - алгоритм классификации основанный на оценке сходства объектов.Для формализации понятия сходств, вводится *функция расстояния* в простанстве объектов X, то есть, чем меньше расстояние, тем больше объекты похожи друг на друга. При этом метрические алгоритмы основываются на гипотезе компактности, которая говорит, что *схожим объектам соответствуют схожие ответы*.

Алгоритм k ближайших соседей (KNN)
----------------------------------
Имеется некоторая выборка Xl, состоящая из объектов x(i), i = 1, ..., l(выборка ирисов Фишера). **Алгоритм KNN** относит классифицируемый объект *z* к тому классу , к которому относится большинство из k его ближайших соседей.
```diff
kNN <- function(xl, z, k)
{
  ## Сортируем выборку согласно классифицируемого объекта
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  
  ## Получаем классы первых k соседей
  classes <- orderedXl[1:k, n + 1]
  
  ## Составляем таблицу встречаемости каждого класса
  counts <- table(classes)
  ## Находим класс, который доминирует среди первых k соседей
  class <- names(which.max(counts))
  
  return (class)
}
```

**Результат работа**
![knn](https://user-images.githubusercontent.com/43620917/47822651-e94f4a80-dd75-11e8-879f-0825d5ff65d8.png)




 
  
 
  
  
  1.1 1NN 
  
Алгоритм 1NN относит классифицируемый объект <b>u ∈ Xℓ</b> к тому классу, которому принадлежит ближайший к нему обучающий объект.
 Единственное достоинство этого алгоритма — простота реализации.
Недостатки:
  Неустойчивость к погрешностям. Если среди обучающих объектов есть выброс — объект, находящийся в окружении объектов чужого класса, то не только он сам будет классифицирован неверно, но те окружающие его объекты, для которых он окажется ближайшим, также будут классифицированы неверно.
 Отсутствие параметров, которые можно было бы настраивать по выборке. Алгоритм полностью зависит от того, насколько удачно выбрана метрика ρ.
 В результате — низкое качество классификации.

 
 Код: задана некоторая выборка Xl, состоящая из объектов. Затем подбирается метрика, в данном случае используется евклидово расстояние.
  
  
 Далее сортируем объекты в порядке увеличения расстояния  относительно заданой точке и и тогда заданая точка будет принадлежать классу элемента стоящего первый в отсортированном списке
  
   
 



