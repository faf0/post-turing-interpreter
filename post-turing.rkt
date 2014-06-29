#lang eopl

; *** Post-Turing Program Interpreter ***
; The program is a solution for the PhD qualification exam in Programming
; Languages from the Fall of 2007 at Stevens Institute of Technology.
; Adriana Compagnoni posed the exam which is available under
; https://web.stevens.edu/compsci/graduate/phd/qualsfiles/pl-f07.pdf
; The solution is based on the code given in the appendix of the exam.
(define copyx '(
                < A > RIGHT
                  IF B GOTO
                  END
                  < B > PRINT 5
                  < C > RIGHT
                  IF 1 GOTO
                  C
                  < D > RIGHT
                  IF 1 GOTO
                  D
                  PRINT 1
                  < E > LEFT
                  IF 1 GOTO
                  E
                  IF B GOTO
                  E
                  PRINT 1
                  RIGHT
                  IF B GOTO
                  F
                  IF 1 GOTO
                  B
                  IF B GOTO
                  B
                  < F > LEFT
                  IF 1 GOTO
                  F
                  ))

(define-datatype sym sym?
  (blank)
  (num (number number?)))

(define-datatype label label?
  (lab (l symbol?))
  (nl))

(define-datatype instr instr?
  (PRINT (l label?) (s sym?))
  (LEFT (l label?))
  (RIGHT (l label?))
  (IF (l label?) (s sym?) (goto label?)))

(define-datatype program program?
  (prog (p (list-of instr?)) (next number?)))

(define-datatype tape tape?
  (tp (left (list-of sym?))
      (head sym?)
      (right (list-of sym?))))

(define parse-instrs
  (lambda (l)
    (if (null? l)
        '()
        (if (eqv? (car l) '<)
            ; it is a labeled instruction
            (let ((instr-name (cadddr l)))
              (cond
                ((eqv? instr-name 'PRINT)
                 ; it is a labeled PRINT
                 (cons (PRINT (lab(cadr l))
                              (let ((symbol (car (cddddr l))))
                                (if (number? symbol)(num symbol)(blank))))
                       (parse-instrs (cdr (cddddr l)))))
                ((eqv? instr-name 'RIGHT)
                 (cons (RIGHT (lab (cadr l))) (parse-instrs (cddddr l))))
                ((eqv? instr-name 'LEFT)
                 (cons (LEFT (lab (cadr l))) (parse-instrs (cddddr l))))
                ((eqv? instr-name 'IF)
                 (cons (IF (lab(cadr l))
                           (let ((symbol (car (cddddr l))))
                             (if (number? symbol)
                                 (num symbol)
                                 (blank)))
                           (lab (car (cdr (cdr (cddddr l))))))
                       (parse-instrs (cdddr (cddddr l)))))
                (else (eopl:error 'parse-instrs "unrecognized labeled instruction ~s" instr-name))
                ))
            ; it is an unlabeled instruction
            (let ((instr-name (car l)))
              (cond
                ((eqv? instr-name 'PRINT)
                 ; it is an unlabeled PRINT
                 (cons (PRINT (nl) (let ((symbol (cadr l)))
                                     (if (number? symbol)
                                         (num symbol)
                                         blank)))
                       (parse-instrs (cddr l))))
                ((eqv? instr-name 'RIGHT)
                 (cons (RIGHT (nl)) (parse-instrs (cdr l))))
                ((eqv? instr-name 'LEFT)
                 (cons (LEFT (nl)) (parse-instrs (cdr l))))
                ((eqv? instr-name 'IF)
                 (cons (IF (nl)
                           (let ((symbol (car (cdr l))))
                             (if (number? symbol)
                                 (num symbol)
                                 (blank)))
                           (lab (cadddr l)))
                       (parse-instrs (cddddr l))))
                (else (eopl:error 'parse-instrs "unrecognized unlabeled instruction ~s" instr-name))
                ))
            ))))

(define LEFTproc
  (lambda (t)
    (cases tape t
      (tp (left head right)
          (if (null? left)
              (tp '() (blank) (cons head right))
              (tp (cdr left) (car left) (cons head right)))))))

(define RIGHTproc
  (lambda (t)
    (cases tape t
      (tp (left head right)
          (if (null? right)
              (tp (cons head left) (blank) '())
              (tp (cons head left) (car right) (cdr right)))))))

(define PRINTproc
  (lambda (t s)
    (cases tape t
      (tp (left head right)
          (tp left s right)))))

(define sym-to-symbol
  (lambda (s)
    (cases sym s
      (blank{} 'B)
      (num(n) n))))

(define symbol-to-sym
  (lambda (s)
    (if (eqv? s 'B) (blank)
        (if (number? s)
            (num s)
            (eopl:error 'symbol-to-sym "~s is not a valid tape symbol" s)))))

(define symlist-to-list
  (lambda (sl)
    (if (null? sl) '() (cons (sym-to-symbol (car sl)) (symlist-to-list (cdr sl))))))

(define symlist1
  (list (blank) (num 3) (num 4) (num 5)))

(define list-to-symlist
  (lambda (l)
    (if (null? l) '()
        (if (eqv? (car l) 'B)
            (cons (blank) (list-to-symlist (cdr l)))
            (cons (num (car l)) (list-to-symlist (cdr l)))
            ))))

(define input-to-tape
  (lambda (l)
    (if (null? l)
        (tp '() (blank) '())
        (tp '{} (symbol-to-sym (car l)) (list-to-symlist (cdr l))))))

(define unparse
  (lambda (t)
    (cases tape t
      (tp (left head right)
          (append (reverse (symlist-to-list left))
                  (cons '< (cons (sym-to-symbol head) (cons '> (symlist-to-list right)))))
          ))
    ))
    
; SELF-WRITTEN

(define lookup-instr
  (lambda (l n)
    (let ((ndec (- n 1)))
      (if (> n (length l))
          (eopl:error "lookup-instr: Instruction not found")
          (list-ref l ndec)))))

(define lookup-lab-instr
  (lambda (instrs lb n end)
    (lookup-lab-instr-off instrs lb n end 0)))

(define lookup-lab-instr-off
  (lambda (instrs lb n end off)
    (if (>= off (length instrs))
        end
        (let ((cinstr (list-ref instrs off)))
          (cases instr cinstr
            (PRINT (clb s)
                   (check-label instrs lb n end off clb))
            (LEFT (clb)
                  (check-label instrs lb n end off clb))
            (RIGHT (clb)
                   (check-label instrs lb n end off clb))
            (IF (clb s goto)
                (check-label instrs lb n end off clb))
          )))))

(define check-label
  (lambda (instrs lb n end off clb)
    (if (equal? clb lb)
        (+ off n)
        (lookup-lab-instr-off instrs lb n end (+ off 1)))))

; tape x program -> tape
(define run
  (lambda (it p)
    (cases program p
      (prog (instrs n) (run-i it instrs n)))))

(define run-i
  (lambda (it instrs n)
    (if (> n (length instrs))
        it
        (let ((cinstr (list-ref instrs (- n 1))))
          (cases instr cinstr
            (PRINT (lb s)
                   (run-i (PRINTproc it s) instrs (+ n 1)))
            (LEFT (lb)
                  (run-i (LEFTproc it) instrs (+ n 1)))
            (RIGHT (lb)
                   (run-i (RIGHTproc it) instrs (+ n 1)))
            (IF (lb s gotol)
                (cases tape it
                  (tp (left head right)
                      (if (equal? head s)
                          (let* ((end (+ 1 (length instrs)))
                                 (next-n (lookup-lab-instr instrs gotol 1 end)))
                            (if (= next-n end)
                                it
                                (run-i it instrs next-n)))
                          (run-i it instrs (+ n 1))))))
            )))))
