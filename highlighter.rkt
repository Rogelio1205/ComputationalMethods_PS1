#lang racket

;; We declare all tokens, words, and delimeters

(struct token (type value) #:transparent)

(define KEYWORDS
  (set "and" "as" "assert" "async" "await"
       "break" "case" "class" "continue"
       "def" "del" "elif" "else" "except"
       "False" "finally" "for" "from"
       "global" "if" "import" "in" "is"
       "lambda" "match" "None" "nonlocal"
       "not" "or" "pass" "raise" "return"
       "True" "try" "while" "with" "yield"))

(define delimeters
  (set "(" ")" "[" "]" "{" "}" "," ":" "." ";"))

(define (classify-word word)
  (if (set-member? KEYWORDS word)
      (token 'keyword word)
      (token 'identifier word)))

;; Character helpers

(define (digit? c)
  (char<=? #\0 c #\9))

(define (letter? c)
  (or (char<=? #\a c #\z)
      (char<=? #\A c #\Z)
      (char=? c #\_)))

(define (word-char? c)
  (or (letter? c) (digit? c)))

(define (whitespace? c)
  (or (char=? c #\space)
      (char=? c #\newline)
      (char=? c #\tab)
      (char=? c #\return)))

(define (operator-start? c)
  (member c '(#\+ #\- #\* #\/ #\% #\= #\! #\< #\>)))

;; Read functions, this will return true if the input code have certain conditions
(define (read-while pred chars)
  (let loop ([remaining chars] [acc '()])
    (if (or (null? remaining)
            (not (pred (car remaining))))
        (values (list->string (reverse acc)) remaining)
        (loop (cdr remaining) (cons (car remaining) acc)))))

(define (read-comment chars)
  (read-while (lambda (c) (not (char=? c #\newline))) chars))

(define (read-string chars quote-char)
  (let loop ([remaining chars] [acc (list quote-char)])
    (cond
      [(null? remaining)
       (values (list->string (reverse acc)) remaining)]
      [(char=? (car remaining) #\\)
       (if (null? (cdr remaining))
           (values (list->string (reverse (cons #\\ acc))) '())
           (loop (cddr remaining)
                 (cons (cadr remaining) (cons #\\ acc))))]
      [(char=? (car remaining) quote-char)
       (values (list->string (reverse (cons quote-char acc)))
               (cdr remaining))]
      [else
       (loop (cdr remaining) (cons (car remaining) acc))])))

(define (read-triple-string chars quote-char)
  (let loop ([remaining chars] [acc (list quote-char quote-char quote-char)])
    (cond
      [(null? remaining)
       (values (list->string (reverse acc)) remaining)]
      [(and (>= (length remaining) 3)
            (char=? (car remaining) quote-char)
            (char=? (cadr remaining) quote-char)
            (char=? (caddr remaining) quote-char))
       (values (list->string (reverse (append (list quote-char quote-char quote-char) acc)))
               (cdddr remaining))]
      [else
       (loop (cdr remaining) (cons (car remaining) acc))])))

(define (read-number chars)
  (let-values ([(int-part rest1) (read-while digit? chars)])
    (if (and (not (null? rest1))
             (char=? (car rest1) #\.)
             (not (null? (cdr rest1)))
             (digit? (cadr rest1)))
        (let-values ([(dec-part rest2) (read-while digit? (cdr rest1))])
          (values (string-append int-part "." dec-part) rest2 'float))
        (values int-part rest1 'integer))))

(define (read-operator chars)
  (let ([c1 (car chars)]
        [rest (cdr chars)])
    (if (null? rest)
        (values (string c1) rest)
        (let* ([c2   (car rest)]
               [two  (string c1 c2)]
               [two-char-ops '("==" "!=" "<=" ">=" "+=" "-=" "*=" "/=" "%=" "**" "//")])
          (if (member two two-char-ops)
              (values two (cdr rest))
              (values (string c1) rest))))))
;; Principal Scanner. It will scan the text and will call the previous functions depending on the text

(define (tokenize chars)
  (cond
    [(null? chars) '()]

    [(whitespace? (car chars))
     (let-values ([(val rest) (read-while whitespace? chars)])
       (cons (token 'whitespace val) (tokenize rest)))]

    [(char=? (car chars) #\#)
     (let-values ([(val rest) (read-comment chars)])
       (cons (token 'comment val) (tokenize rest)))]

    [(and (>= (length chars) 3)
          (char=? (car chars) #\")
          (char=? (cadr chars) #\")
          (char=? (caddr chars) #\"))
     (let-values ([(val rest) (read-triple-string (cdddr chars) #\")])
       (cons (token 'string val) (tokenize rest)))]

    [(and (>= (length chars) 3)
          (char=? (car chars) #\')
          (char=? (cadr chars) #\')
          (char=? (caddr chars) #\'))
     (let-values ([(val rest) (read-triple-string (cdddr chars) #\')])
       (cons (token 'string val) (tokenize rest)))]

    [(char=? (car chars) #\")
     (let-values ([(val rest) (read-string (cdr chars) #\")])
       (cons (token 'string val) (tokenize rest)))]

    [(char=? (car chars) #\')
     (let-values ([(val rest) (read-string (cdr chars) #\')])
       (cons (token 'string val) (tokenize rest)))]

    [(digit? (car chars))
     (let-values ([(val rest type) (read-number chars)])
       (cons (token type val) (tokenize rest)))]

    [(letter? (car chars))
     (let-values ([(val rest) (read-while word-char? chars)])
       (cons (classify-word val) (tokenize rest)))]

    [(operator-start? (car chars))
     (let-values ([(val rest) (read-operator chars)])
       (cons (token 'operator val) (tokenize rest)))]

    [(set-member? delimeters (string (car chars)))
     (cons (token 'delimiter (string (car chars)))
           (tokenize (cdr chars)))]

    [else
     (tokenize (cdr chars))]))

(define (tokenize-string input)
  (tokenize (string->list input)))

; DEBUG, change the file to make tests, comment it when the HTML and CSS is done.

(define (read-file path)
  (call-with-input-file path
    (lambda (port)
      (port->string port))))

(define (print-tokens tokens)
  (for-each (lambda (tok)
              (printf "[~a] ~s\n"
                      (token-type tok)
                      (token-value tok)))
            tokens))
;; Convert text to HTML-safe string
(define (escape-html s)
  (let loop ([chars (string->list s)] [acc '()])
    (if (null? chars)
        (apply string-append (reverse acc))
        (let ([c (car chars)])
          (cond
            [(char=? c #\&) (loop (cdr chars) (cons "&amp;" acc))]
            [(char=? c #\<) (loop (cdr chars) (cons "&lt;" acc))]
            [(char=? c #\>) (loop (cdr chars) (cons "&gt;" acc))]
            [else (loop (cdr chars) (cons (string c) acc))])))))

(define (token->-html tok)
  (let* ([t (symbol->string (token-type tok))]
         [v (escape-html (token-value tok))])
    (if (string-ci=? t "whitespace")
        v
        (format "<span class=\"~a\">~a</span>" t v))))

(define CSS
  "pre.code {background:#1e1b2b;color:#fff;padding:1rem;white-space:pre;overflow:auto;font-family:monospace;}
.keyword {color:#c792ea;font-weight:bold}
.identifier {color:#ffffff}
.integer {color:#f78c6c}
.float {color:#f78c6c}
.string {color:#c3e88d}
.comment {color:#7f848e;font-style:italic}
.operator {color:#89ddff}
.delimiter {color:#ff9d41}
.whitespace {color:inherit}")


(define (tokens->html tokens)
  (apply string-append (map token->-html tokens)))

(define (write-html output-path html)
  (call-with-output-file output-path
    (lambda (o) (display html o))
    #:exists 'replace))

(define (make-html-page body)
  (format "<!doctype html>\n<html lang=\"en\">\n  <head>\n    <meta charset=\"utf-8\">\n    <meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">\n    <title>Highlighted Source</title>\n    <style>~a</style>\n  </head>\n  <body>\n    <pre class=\"code\">~a</pre>\n  </body>\n</html>" CSS body))

(define (main)
  (let* ([args (current-command-line-arguments)]
         [input (if (zero? (vector-length args)) "prueba.py" (vector-ref args 0))]
         [source (read-file input)]
         [tokens (tokenize-string source)]
         [body (tokens->html tokens)]
         [html (make-html-page body)]
         [out (if (regexp-match #rx"\\.[^./\\]+$" input)
                  (regexp-replace #rx"\\.[^./\\]+$" input ".html")
                  (string-append input ".html"))])
    (write-html out html)
    (printf "Wrote ~a (~a tokens)\n" out (length tokens))))

(when (equal? (current-directory) (current-directory))
  (main))