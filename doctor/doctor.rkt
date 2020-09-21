; заготовка "Доктора". Август 2019
#lang scheme/base
; В учебных целях используется базовая версия Scheme

; функция, спрашивающая имя следующего пациента
(define (ask-patient-name)
 (begin
  (println '(next!))
  (println '(who are you?))
  (print '**)
  (car (read))
 ) 
)

; основная функция, запускающая "Доктора"
; параметр stop-word -- стоп-слово, при вводе которого в качестве имени пациента доктор завершает приём
; параметр max-patients -- максимальное количество пациентов, которое принимает доктор
(define (visit-doctor stop-word max-patients)
  (if (= max-patients 0) (print '(its time to go home))
      (let ((name(ask-patient-name)))
         (if (eq? name stop-word) (print '(its time to go home))
             (begin (printf "Hello, ~a!\n" name)
                    (print '(what seems to be the trouble?))
                    (doctor-driver-loop name '())
                    (visit-doctor stop-word (- max-patients 1)))
             )
         )
      )
)

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
; параметр history -- список предыдущих реплик пациента
(define (doctor-driver-loop name history)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (printf "(see you next week)\n"))
            (else (print (reply user-response history)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name (cons user-response history))
             )
       )
      )
)

; генерация ответной реплики по user-response -- реплике от пользователя 
(define (reply user-response history)
  (let ((history? (if (null? history) 0 1)) (patterns? (if (check-patterns user-response) 2 0)))
    (case (+ history? patterns?)
      ((0) (case (random 2)
             ((0) (qualifier-answer user-response)) 
             ((1) (hedge)) 
             )
           )
      ((1) (case (random 3)
             ((0) (qualifier-answer user-response)) 
             ((1) (hedge))  
             ((2) (history-answer history)) 
             )
           )
      ((2) (case (random 3)
             ((0) (qualifier-answer user-response)) 
             ((1) (hedge))  
             ((2) (pattern-reply user-response)) 
             )
           )
      ((3) (case (random 4)
             ((0) (qualifier-answer user-response)) 
             ((1) (hedge))  
             ((2) (history-answer history))
             ((3) (pattern-reply user-response)) 
             )
           )
      )
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
        (let iter-replacer ((raw lst) (res '()))  ; на каждой итерации добавляет в список из второго аргумента изменённое, если нужно, слово
          (cond ((null? raw) (reverse res))       ; второй аргумент аккумулирует изменённую фразу в обратном порядке, перед выдачей - переворачиваем
                (else (iter-replacer (cdr raw)
                                     (let* ((word (car raw)) (pat-rep (assoc word replacement-pairs)))
                                       (cons (if pat-rep (cadr pat-rep)
                                                 word
                                                 )
                                             res
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
          (change-person (pick-random history))
          )     
  )

; 4й способ генерации ответной реплики -- ключевые слова
(define patterns '( 
                   ( ; начало данных 1й группы
                    (depressed suicide exams university) ; список ключевых слов 1й группы
                    ( ; список шаблонов для составления ответных реплик 1й группы 
                     (when you feel depressed, go out for ice cream)
                     (depression is a disease that can be treated)
                     )
                    ) ; завершение данных 1й группы
                   ( ; начало данных 2й группы ...
                    (mother father parents brother sister uncle ant grandma grandpa)
                    (
                     (tell me more about your * , i want to know all about your *)
                     (why do you feel that way about your * ?)
                     )
                    )
                   (
                    (university scheme lections)
                    (
                     (your education is important)
                     (how many time do you spend to learning ?)
                     )
                    )
                   )
  )

; получение всех ключевых слов списком
(define (get-key-words)
  (foldl (lambda (group cur-keys)
           (foldl cons cur-keys (car group)))
         '()
         patterns)
  )

; функция-предикат
(define (check-patterns phrase)
  (let* ((lst (get-key-words))
         (res (ormap (lambda (word) (member word lst)) phrase))) ; проверяем, есть ли хотя бы одно совпадение с ключевым словом
    (not (not res))                                              ; хотим в качестве ответа получать либо #t, либо #f
    )
  )

; получение всех ключевых слов, содержащихся в фразе
(define (get-used-words phrase)
  (let ((key-words (get-key-words)))
    (filter (lambda (word) (member word key-words))
            phrase
     )
    )
  )

; получение всех доступных реплик по слову
(define (get-patterns word)
  (foldl (lambda (next-patterns result) (foldl cons result next-patterns))
         '()
         (map cadr (filter (lambda (group)                                 ; получение подходящих шаблонов в виде 
                             (member word (car group))                     ; ((<шаблоны группы 1>) (<шаблоны группы 2>))
                             )
                           patterns
                           )
              )
         )
  )

; функция построения ответа
(define (pattern-reply phrase)
  (let* ((key-word (pick-random (get-used-words phrase))) (new-phrase (pick-random (get-patterns key-word))))
    (many-replace-map (list (list '* key-word)) new-phrase)
    )
  )