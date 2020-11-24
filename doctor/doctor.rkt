; заготовка "Доктора". Август 2019
#lang scheme/base
; В учебных целях используется базовая версия Scheme
(require scheme/string)
(require racket/format)
(require "graph.rkt")
; основная функция, запускающая "Доктора"
; параметр stop-word -- стоп-слово, при вводе которого в качестве имени пациента доктор завершает приём
; параметр max-patients -- максимальное количество пациентов, которое принимает доктор
(define (visit-doctor stop-word max-patients)
  (if (= max-patients 0) (printf "its time to go home")
      (let ((name(ask-patient-name)))
         (if (equal? name (~a stop-word)) (printf "its time to go home")
             (begin (printf "Hello, ~a!\n" name)
                    (printf "what seems to be the trouble?\n")
                    (doctor-driver-loop name '())
                    (visit-doctor stop-word (- max-patients 1)))
             )
         )
      )
)

; функция, спрашивающая имя следующего пациента
(define (ask-patient-name)
 (begin
  (printf "next!\n")
  (printf "who are you?\n")
  (print '**)
  (car (filter non-empty-string? (string-split (read-line) #px"\\s*\\b\\s*")))
 ) 
)


; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
; параметр history -- список предыдущих реплик пациента
(define (doctor-driver-loop name history)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read-response (read-line))))
      (cond 
	    ((equal? (car user-response) '("goodbye")) ; если первое предложение - '(goodbye), то прощаемся
             (printf "Goodbye, ~a!\n" name)
             (printf "see you next week\n"))
            (else (printf (make-answer (reply user-response history strategies))) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name (append user-response history))
             )
       )
      )
)

(define (help)
  (print (make-answer (car (read-response (read-line))))))

(define (read-response str)
  (map (lambda (sent) (filter non-empty-string? (string-split sent #px"\\s*\\b\\s*")))
       (string-split str #px"\\.|\\?|!")
   )
  )

(define (make-answer str)
  (let form ((cur str) (res '()))
    (if (null? cur) (string-join (reverse res))
        (if (regexp-match-exact? #px"\\W+" (car cur)) (form (cdr cur) (cons (string-append (car res) (car cur)) (cdr res)))
            (form (cdr cur) (cons (car cur) res))
         )
        )
    )
  )

; генерация ответной реплики по user-response -- реплике от пользователя
; history -- история реплик, strategies -- используемые стратегии
(define (reply user-response history strategies)
  (let ((valid (filter (lambda (strategy) ((strategy-predicate strategy) user-response history '())) strategies)))
    ((pick-random-with-weight (map cdr valid)) user-response history '())
    )
)

; случайный выбор одного из элементов списка lst
(define (pick-random lst)
  (list-ref lst (random (length lst)))
)

; случайный выбор одного из элементов списка weighted-list с весами
; ((вес1 элемент1) ...)
(define (pick-random-with-weight weighted-list)
  (let ((full-weight (foldl (lambda (elem res) (+ (car elem) res)) 0 weighted-list))) 
    (let choose ((remaining (random full-weight)) (tail weighted-list))
      (if (> (caar tail) remaining) (cadar tail)
          (choose (- remaining (caar tail)) (cdr tail))
       )
      )
    )
  )

; замена лица во фразе			
(define (change-person phrase)
        ;(many-replace                   ; начальный вариант
        ;(many-replace-iter              ; итеративный вариант
        (many-replace-map               ; вариант с функцией высшего порядка
                        '(("am" "are")
                        ("are" "am")
                        ("i" "you")
                        ("me" "you")
                        ("mine" "yours")
                        ("my" "your")
						("myself" "yourself")
                        ("you" "i")
                        ("your" "my")
                        ("yours" "mine")
						("yourself" "myself"))
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

; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
; структура стратегии
(define qualifier-strategy (list (lambda (user-response history others) #t)
                                   3
                                   (lambda (user-response history others) (qualifier-answer user-response)))
  )

(define (qualifier-answer user-response)
        (append (pick-random '(("you seem to think that")
                               ("you feel that")
                               ("why do you believe that")
                               ("why do you say that")
                               ("what made you regret that")
                               ("its interesting that")
                               ("why do you think that"))
                )
                (change-person (car user-response))
        )
 )

; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
; структура стратегии
(define hedge-strategy (list (lambda (user-response history others) #t)
                             1
                             (lambda (user-response history others) (hedge)))
  )

(define (hedge)
       (pick-random '(("please go on")
                       ("many people have the same sorts of feelings")
                       ("many of my patients have told me the same thing")
                       ("please continue")
                       ("every human went through the same situation")
                       ("please do not stop")
                       ("what was the reason of saying this"))
         )
)

; 3й способ генерации ответной реплики -- возврат к предыдущей реплике пациента
; структура стратегии
(define history-strategy (list (lambda (user-response history others) (not (null? history))) 
                               2
                               (lambda (user-response history others) (history-answer history)))
  )

(define (history-answer history)
  (cons "earlier you said that"         
          (change-person (pick-random history))
          )
  )

; 4й способ генерации ответной реплики -- ключевые слова
; структура стратегии
(define keywords-strategy (list (lambda (user-response history others) (check-patterns user-response))
                                2
                                (lambda (user-response history others) (pattern-reply user-response)))
  )

; ключевые слова в виде ((<ключевые слова> <шаблоны>) ... )
(define patterns '( 
                   ( ; начало данных 1й группы
                    ("depressed" "suicide" "exams" "university" "depression") ; список ключевых слов 1й группы
                    ( ; список шаблонов для составления ответных реплик 1й группы 
                     ("when you feel depressed, go out for ice cream")
                     ("depression is a disease that can be treated")
                     ("you should say more about this")
                     )
                    ) ; завершение данных 1й группы
                   ( ; начало данных 2й группы ...
                    ("mother" "father" "parents" "brother" "sister" "uncle" "ant" "grandma" "grandpa")
                    (
                     ("tell me more about your" * ", i want to know all about your" *)
                     ("why do you feel that way about your" * "?")
                     ("family is important for everyone")
                     ("is everything ok with your" *)
                     )
                    )
                   (
                    ("university" "scheme" "lections" "lessons")
                    (
                     ("your education is important")
                     ("how many time do you spend to learning ?")
                     ("you should put more effort to" *)
                     )
                    )
                   (
                    ("love" "affection" "passion" "tenderness" "softness")
                    (
                     ("Feeling" * "is great")
                     ("Sometimes such feelings can be painfull")
                     ("Make" * "not war")
                     )
                    )
                   (
                    ("good" "bad" "nice" "normal" "ok")
                    (
                     ("you say general words , be more detailed please")
                     ("why do you say , that it is" *)
                     )
                    )
                   )
  )

; функция-предикат
(define (check-patterns phrase)
  (let ((res (ormap (lambda (word) (member word all-key-words)) phrase))) ; проверяем, есть ли хотя бы одно совпадение с ключевым словом
    (not (not res))                                              ; хотим в качестве ответа получать либо #t, либо #f
    )
  )

; получение всех ключевых слов списком
(define all-key-words
  (foldl (lambda (group cur-keys)
           (foldl cons cur-keys (car group)))
         '()
         patterns)
  )

; получение всех ключевых слов, содержащихся в фразе
(define (get-used-words phrase)
  (filter (lambda (word) (member word all-key-words))
          phrase
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

; 5й способ генерации ответа - автогенерация
(define generation-strategy (list (lambda (user-response history others) #t)
                                  5
                                  (lambda (user-response history others) (mixed-generation user-response)))
  )

; структура стратегий - список стратегий
; каждая стратегия - список из трёх элементов: функция-предикат (применима ли стратегия),
; вес (натуральное число, чем больше вес, тем выше вероятность), тело (функция, возвращающая ответную реплику) 
(define strategies (list qualifier-strategy hedge-strategy history-strategy keywords-strategy generation-strategy))
(define (strategy-predicate strategy) (list-ref strategy 0))
(define (strategy-weight strategy) (list-ref strategy 1))
(define (strategy-body strategy) (list-ref strategy 2))
