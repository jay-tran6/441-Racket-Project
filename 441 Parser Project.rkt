#lang racket

(require parser-tools/lex)
(require parser-tools/yacc)
(define regex-ident #rx"^[A-Za-z](?>[A-Za-z]|[0-9])*")
(define regex-number #rx"^[0-9]+(?>\\.[0-9]+)?(?>e[+-]?[0-9]+)?")
(define regex-op #rx"^[-+*/^]")
(define regex-equal #rx"^(:=)")
(define regex-ws #rx"^ *")
(define regex-keyword #rx"^let")
(define regex-end #rx"^\\$+[\\$$]")
(define regex-fname #rx"^(read | write)")
(define regex-readwrite #rx"^(read | write)")
(define regex-windows #rx"^(\r\n)")
(define regex-unix #rx"^(\n)")


;reading out txt file
(define input-sample
  (with-input-from-file "file01.txt"
    (lambda () (string-split (port->string (current-input-port))))))
(displayln input-sample)


(define (tokenizer instr) ;scanner
  (let loop ([i 0])
    (let* ([str-len (string-length instr)]
           [next-pos 0]
           [start (cdar (regexp-match-positions regex-ws instr i))])

      (define (match-reg regex)
        (let ([v (regexp-match-positions regex instr start)])
          (if (equal? v #f)
              (set! next-pos #f)
              (set! next-pos (cdar v)))
          next-pos))

      (define (classify type)
        (let ([val (substring instr start next-pos)])
          val))

      (define (at-end)
        (or (= str-len next-pos)
            (let ([c (string-ref instr next-pos)])
              (not (or (char-numeric? c) (char-alphabetic? c))))))

      (let ([token
             (cond [(= start str-len)'()]
                   [(match-reg regex-equal) (classify 'equal)]                   
                   [(match-reg regex-end) (classify 'end)]
                   [(match-reg regex-windows) (classify 'windows)]
                   [(match-reg regex-unix) (classify 'unix)]
                   
                   [(and (match-reg regex-keyword) (at-end))
                    (classify 'fname)]
                   [(and (match-reg regex-fname) (at-end))
                    (classify 'keyword)]
                   
                   [(match-reg regex-ident) (classify 'ident)]
                   [(match-reg regex-number) (classify 'number)]
                   [(match-reg regex-op) (classify 'op)]
                   [(equal? #\( (string-ref instr start))
                    (set! next-pos (add1 start))
                    "("]
                   [(equal? #\) (string-ref instr start))
                    (set! next-pos (add1 start))
                    ")"]
                   
                   [else #f])])
        (cond [(equal? token '()) '()]
              [token (cons token (loop next-pos))]
              [else (error (format "Invalid token, line: " start))])))))


(define inp-token null)
(define tkn-list '())
(define line-cnt 0)


(define (parser process) ;at least thats the plan to parse
  (set! tkn-list (tokenizer (file->string parser process)))
  (cond
    [(or (regexp-match? regex-readwrite inp-token) (regexp-match? inp-token regex-ident) (equal? "$$" inp-token)) 
     (stmt-list)
     (match-tkn "$$")
     "accept"]
    [else (error "Syntax error, line: "line-cnt)]))


(define (match-tkn)
  (cond
    [(equal? inp-token)]
    [else (error "Syntax error, line: "line-cnt)]))


(define (match-reg-tkn)
  (cond
    [(regexp-match? inp-token)]
    [else (error "Syntax error, line: "line-cnt)]))


(define (addition-op)
  (cond    
    [(equal? "+" inp-token)
     (match-tkn "+")]
    [(equal? "-" inp-token)
     (match-tkn "-")]
    [else (error "Syntax error, line: "line-cnt)]))


(define (multiply-op)
  (cond  
    [(equal? "*" inp-token)
     (match-tkn "*")]
    [(equal? "/" inp-token)
     (match-tkn "/")]
    [else (error "Syntax error, line: "line-cnt)]))


(define (stmt)
  (cond    
    [(equal? "read" inp-token)
     (match-tkn "read")
     (match-reg-tkn regex-ident)]
    [(equal? "write" inp-token)
     (match-tkn "write")]
    [(regexp-match? regex-ident inp-token)
     (match-reg-tkn regex-ident)
     (match-tkn ":=")]
    [(or (equal? "\r\n" inp-token) (equal? "\n" inp-token))]
    [else (error "Syntax error, line: "line-cnt)]))


(define (stmt-list)
    (cond   
      [(regexp-match? regex-readwrite inp-token)
       (stmt)
       (stmt-list)]
      [(regexp-match? regex-ident inp-token)
       (stmt)
       (stmt-list)]
      [(equal? "$$"  inp-token) #t]
      [(or (equal? "\r\n" inp-token) (equal? "\n" inp-token))
       (stmt)
       (stmt-list)]
      [else (error "Syntax error, line: "line-cnt)]))

;(tokenizer(input-sample))
;(parser(input-sample))