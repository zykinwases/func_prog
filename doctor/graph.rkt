#lang scheme/base

(require scheme/string)

(define input "train/art2.txt")
(provide (all-defined-out))

;таблица н-1грамм, с которых начинается предложения 
(define forward-freq (make-hash))
;взвешенный граф н-1грамм и следующих слов
(define forward-ngramm (make-hash))
;таблица н-1грамм, которыми заканчиваются предложения
(define reverse-freq (make-hash))
;взвешенный граф н-1грамм и предыдущих слов
(define reverse-ngramm (make-hash))
;длина н-грамм
(define n 5)

; формирует все графы по входному файлу
(define (train-graphs file)
  (let ((in (open-input-file file)))
    (let reader ((line (read-line in)))
      (if (eof-object? line) (println "Everything is ready")
          (begin (let ((sentences (parse-line line)))
                   (map add-to-frw-freq sentences)
                   (map add-to-frw-ngramms sentences)
                   (map add-to-rev-freq sentences)
                   (map add-to-rev-ngramms sentences)
                   )
                 (reader (read-line in))
                 )
          )
      )
    )
  )
  
; читает одну строку из входного файла и разбивает её на предложения
(define (parse-line line)
  (map (lambda (sent)
         (reverse (cons "." (reverse (filter non-empty-string? (string-split sent #px"\\s*\\b\\s*"))))))
       (string-split (string-replace (string-downcase line)
                                     #px"[^a-z\\.\\?!;,:'\\s\\-]+"      
                                     ""
                                     #:all? #t)
                     #px"\\.|\\?|!")
       )
  )

; прямой режим
; добавление предложения в таблицу частот
(define (add-to-frw-freq sent)
  (let ((n-1-gramm (make-n-gramm sent (sub1 n))))
    (if (hash-has-key? forward-freq n-1-gramm)
        (hash-set! forward-freq n-1-gramm (add1 (hash-ref forward-freq n-1-gramm)))
        (hash-set! forward-freq n-1-gramm 1)
        )
   )
  )

; добавление предложения в граф следования
(define (add-to-frw-ngramms sent)
  (let loop ((rev-n-gramm (make-n-gramm sent n)) (rest sent))
    (begin
      (let ((n-1-gramm (cdr rev-n-gramm)) (last-word (car rev-n-gramm)))
        (if (hash-has-key? forward-ngramm n-1-gramm)
            (let ((next-words-hash (hash-ref forward-ngramm n-1-gramm)))
              (if (hash-has-key? next-words-hash last-word)
                  (hash-set! next-words-hash last-word (add1 (hash-ref next-words-hash last-word)))
                  (hash-set! next-words-hash last-word 1)
                  )
              )
            (hash-set! forward-ngramm n-1-gramm (make-hash (list (cons last-word 1))))
            )
        )
      (if (> (length rest) n) (loop (make-n-gramm (cdr rest) n) (cdr rest))
          (void)
          )
      )
    )
  )

; обратный режим
; добавление предложения в таблицу частот
(define (add-to-rev-freq sent)
  (let ((n-1-gramm (make-n-gramm (cdr (reverse sent)) (sub1 n))))
    (if (hash-has-key? reverse-freq n-1-gramm)
        (hash-set! reverse-freq n-1-gramm (add1 (hash-ref reverse-freq n-1-gramm)))
        (hash-set! reverse-freq n-1-gramm 1)
        )
   )
  )

; добавление предложения в граф следования
(define (add-to-rev-ngramms sent)
  (let ((rev-sent (cdr (reverse (cons "." sent)))))
    (let loop ((rev-n-gramm (make-n-gramm rev-sent n)) (rest rev-sent))
      (begin
        (let ((n-1-gramm (cdr rev-n-gramm)) (last-word (car rev-n-gramm)))
          (if (hash-has-key? reverse-ngramm n-1-gramm)
              (let ((next-words-hash (hash-ref reverse-ngramm n-1-gramm)))
                (if (hash-has-key? next-words-hash last-word)
                    (hash-set! next-words-hash last-word (add1 (hash-ref next-words-hash last-word)))
                    (hash-set! next-words-hash last-word 1)
                    )
                )
              (hash-set! reverse-ngramm n-1-gramm (make-hash (list (cons last-word 1))))
              )
          )
        (if (> (length rest) n) (loop (make-n-gramm (cdr rest) n) (cdr rest))
            (void)
            )
        )
      )
    )
  )

;перевёрнутая н-грамма
(define (make-n-gramm sent n)
  (let loop ((i n) (cur sent) (res '()))
    (if (or (= i 0) (null? cur)) res
        (loop (sub1 i) (cdr cur) (cons (car cur) res))
     )
   )
  )

; получение списка всех н-грамм предложения (н - параметр)
(define (make-all-ngramms sent n)
  (let loop ((rest sent) (res '()))
    (if (<= (length rest) n) (cons rest res)
        (loop (cdr rest) (cons (make-n-gramm rest n) res))
     )
   )
  )

; взвешенный выбор случайного элемента из списка (элемент . вес)
(define (pick-random-from-hash hash-list)
  (let ((full-weight (foldl (lambda (elem res) (+ (cdr elem) res)) 0 hash-list))) 
    (let choose ((remaining (random full-weight)) (tail hash-list))
      (if (> (cdar tail) remaining) (caar tail)
          (choose (- remaining (cdar tail)) (cdr tail))
       )
      )
    )
  )

; прямой способ генерации по изначально заданной н-1грамме (в обратном порядке следования слов) 
(define (forward-generation initial)
    (let gener ((cur-n-1gramm initial) (res initial) (iter 50))
      (if (or (eq? (car res) ".") (not (hash-has-key? forward-ngramm cur-n-1gramm)) (= iter 0))
          (reverse res)
          (let ((new-word (pick-random-from-hash (hash->list (hash-ref forward-ngramm cur-n-1gramm)))))
            (gener (cons new-word (reverse (cdr (reverse cur-n-1gramm)))) (cons new-word res) (sub1 iter))
             )
       )
      )
  )

; обратный способ генерации по изначально заданной н-1грамме
(define (reverse-generation initial)
    (let gener ((cur-n-1gramm initial) (res initial) (iter 50))
      (if (or (eq? (car res) ".") (not (hash-has-key? reverse-ngramm cur-n-1gramm)) (= iter 0))
          (cdr res)
          (let ((new-word (pick-random-from-hash (hash->list (hash-ref reverse-ngramm cur-n-1gramm)))))
            (gener (cons new-word (reverse (cdr (reverse cur-n-1gramm)))) (cons new-word res) (sub1 iter))
             )
       )
      )
  )

; поиск всех н-1 грамм в графах, в которые включаются слова из списка
(define (find-n-1gramms lst)
   (let loop ((res '()) (rest-lst lst) (rest-elem (car lst)) (keys (append (hash-keys forward-ngramm) (map reverse (hash-keys reverse-ngramm))))) 
         (let ((kek (filter (lambda (x) (member (car rest-elem) x)) keys)))
           (if (null? (cdr rest-lst)) res
               (if (null? kek) (loop res (cdr rest-lst) (cadr rest-lst) (append (hash-keys forward-ngramm) (map reverse (hash-keys reverse-ngramm))))
                   (if (null? (cdr rest-elem)) (loop (append res kek) (cdr rest-lst) (cadr rest-lst) (append (hash-keys forward-ngramm) (map reverse (hash-keys reverse-ngramm))))
                       (loop res rest-lst (cdr rest-elem) kek)
                       )
                   )
               )
           )
     )
  )

; смешанный способ генерации 
(define (mixed-generation sentence)
  (let loop ((extendable-n-1gramms (filter (lambda (ngramm) (or (hash-has-key? forward-ngramm ngramm) (hash-has-key? reverse-ngramm (reverse ngramm))))
                                                                (find-n-1gramms (make-all-ngramms sentence (sub1 n))))) (k (sub1 n)))
; цикл, производящий смешанную генерацию для полученных н-1грамм, которые получаются "расширением" к-грамм из основного предложения
    (if (null? extendable-n-1gramms)
        (if (> k 1) (loop (filter (lambda (ngramm) (or (hash-has-key? forward-ngramm ngramm) (hash-has-key? reverse-ngramm (reverse ngramm))))
                                                                (find-n-1gramms (make-all-ngramms sentence (sub1 k)))) (sub1 k)) ;если генерировать ответ не на что среди к-грамм предложения, пытаемся сгенерировать ответ для к-1грамм
            (forward-generation (pick-random-from-hash (hash->list forward-freq)))) ;не получилось ни для каких к - случайная прямая генерация
        (let ((n-1gramm (list-ref extendable-n-1gramms (random (length extendable-n-1gramms)))))
          (append (if (hash-has-key? reverse-ngramm (reverse n-1gramm))
                      (reverse-generation (reverse n-1gramm))
                      (reverse n-1gramm))
                  (if (hash-has-key? forward-ngramm n-1gramm)
                      (let without-first-i ((sent (forward-generation n-1gramm)) (i (sub1 n))) ;удаляем повторный вывод н-1граммы
                        (if (= i 0) sent
                            (without-first-i (cdr sent) (sub1 i))
                            )
                        )
                      (reverse n-1gramm))
                  )
          )
        )
   )
  )

(train-graphs input)