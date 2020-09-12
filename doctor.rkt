; заготовка "Доктора". Август 2019
#lang scheme/base
; В учебных целях используется базовая версия Scheme

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop name '())
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name history)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply user-response history)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name (cons user-response history))
             )
       )
      )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response history)
      (case (if (null? history) (random 2)
                (random 3)) ; с равной вероятностью выбирается один из трёх способов построения ответа 
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge))  ; 2й способ
          ((2) (history-answer history)) ; 3й способ
      )
)
			
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
        (append (pick-random '((you seem to think that)
                               (you feel that)
                               (why do you believe that)
                               (why do you say that)
                               (what made you regret that)
                               (its interesting that)
                               (why do you think that))
                )
                (change-person user-response)
        )
 )

; случайный выбор одного из элементов списка lst
(define (pick-random lst)
  (list-ref lst (random (length lst)))
)

; замена лица во фразе			
(define (change-person phrase)
        ;(many-replace                   ; начальный вариант
        ;(many-replace-iter              ; итеративный вариант
        (many-replace-map               ; вариант с функцией высшего порядка
                        '((am are)
                        (are am)
                        (i you)
                        (me you)
                        (mine yours)
                        (my your)
						(myself yourself)
                        (you i)
                        (your my)
                        (yours mine)
						(yourself myself))
                      phrase)
 )
  
; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) lst)
              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                (car lst) ; иначе в начале ответа помещается начало списка без изменений
                            )
                            (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                        )
                     )
               )
         )
  )

; то же самое с помощью итеративного процесса
(define (many-replace-iter replacement-pairs lst)
        (let iter-replacer ((f lst) (l '()))  ; на каждой итерации добавляет в список из второго аргумента изменённое, если нужно, слово
          (cond ((null? f) (reverse l))       ; второй аргумент аккумулирует изменённую фразу в обратном порядке, перед выдачей - переворачиваем
                (else (iter-replacer (cdr f)
                                     (let* ((word (car f)) (pat-rep (assoc word replacement-pairs)))
                                       (cons (if pat-rep (cadr pat-rep)
                                                 word
                                                 )
                                             l
                                       )
                       )
                      )
                )
          )
        )
  )

; то же самое с помощью функции высшего порядка
(define (many-replace-map replacement-pairs lst) 
  (map (lambda (word)
         (let ((pat-rep (assoc word replacement-pairs)))
           (if pat-rep (cadr pat-rep)
               word
               )
           )
         )
       lst
       )
  )

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge)
       (pick-random '((please go on)
                       (many people have the same sorts of feelings)
                       (many of my patients have told me the same thing)
                       (please continue)
                       (every human went through the same situation)
                       (please do not stop)
                       (what was the reason of saying this))
         )
)

; 3й способ генерации ответной реплики -- возврат к предыдущей реплике пациентаы
(define (history-answer history)
  (append '(earlier you said that)
          (let ((num (random (length history))))
            (change-person (list-ref history num))
            )
          )
  )