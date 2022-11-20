(defun TODO (thing)
  (error "Unimplemented: ~A" thing))


(defun find-binding (variable bindings)
  "Looking up binding for varianble.

  When binding exists, return:
  (VALUES binding t)

  When binding does not exists, return:
  (VALUES VARIABLE nil)"
  (let ((cons (assoc variable bindings)))
    (if cons
        (values (cdr cons) t)
        (values variable nil))))



;; Simplify a symbolic arithmetic expression
;;
;; Examples:
;; ---------
;;
;; (partial-eval 1 nil) => 1
;; (partial-eval a '((a . 2))) => 2
;; (partial-eval '(+ 1 2) nil) => 3
;; (partial-eval '(+ a 2) '((a . 2))) => 4
;; (partial-eval '(* 1 a (+ x y)) '((a . 3) (x . 0))) => '(* 3 Y)
;;
(defun partial-eval (exp bindings)
  (labels ((minus-args (args)
             (map 'list (lambda (a)
                          (if (numberp a)
                              (- a)
                              `(- ,a)))
                  args))
           ;; Simplify an associative and commutative function
           (assoc-comm (lisp-function identity args)
             (list
              ;; collect numbers
              (reduce (lambda (n elt)
                             (if (numberp elt)
                                 (funcall lisp-function n elt)
                                 n))
                           args
                           :initial-value identity)
              ;; collect symbolic expressions
              (reverse (reduce (lambda (exps a)
                                 (if (numberp a)
                                     exps
                                     (cons a exps)))
                               args
                               :initial-value nil))))
           (recurse (exp)
             (etypecase exp
               (number exp)
               (symbol
                (values (find-binding exp bindings)))
               (cons
                (destructuring-bind (op &rest args) exp
                  (let ((args (map 'list #'recurse args)))
                    (case op
                      (+ (destructuring-bind (n vars) (assoc-comm #'+ 0 args)
                           (TODO 'partial-eval-+)))
                      (*
                       (TODO 'partial-eval-*))
                      (- (cond
                           ((null args)
                            (error "No arguments to minus"))
                           ((null (cdr args))
                            (destructuring-bind (a) args
                              (if (numberp a)
                                  (- a)
                                  `(- ,a))))
                           (t
                            (TODO 'partial-eval--))))
                      (/ (cond
                           ((null args)
                            (error "No arguments to div"))
                           ((null (cdr args))
                            (recurse `(/ 1 ,(car args))))
                           ((null (cddr args))
                            (TODO 'partial-eval-/))
                           (t
                            (recurse `(/ ,(car args)
                                         (* ,@(cdr args)))))))
                      (expt
                       (destructuring-bind (base power) args
                         (cond
                           ((and (numberp base) (numberp power))
                            (expt base power))
                           ((or (and (numberp power) (zerop power))
                                (and (numberp base) (= 1 base)))
                            1)
                           ((and (numberp power) (= 1 power))
                            base)
                           (t `(expt ,base ,power)))))
                      (otherwise ; a function call
                       (cons op args)))))))))
  (recurse exp)))



(defun deriv (function argument)
  "Return the derivative for known functions.
Return NIL for unknown functions."
  (case function
    (cos `(- (sin ,argument)))
    (sin `(cos ,argument))
    (ln `(/ 1 ,argument))
    (exp `(exp ,argument))
    (otherwise nil)))


;; Symbolically differentiate EXP by VAR.
;;
;; Examples:
;; ---------
;;
;; (diff-eval 't 't) => 1
;; (diff-eval '(cos (expt t 2)) t) => (* (* 2 T) (- (SIN (EXPT T 2))))
;; (diff-eval '(* (f t) (exp t)) 't) => (+ (* (F T) (EXP T)) (* (DIFF (F T) T) (EXP T)))


(defun diff-eval (exp var)
  "Symbolically differentiate EXP by VAR."
  (labels ((constp (exp)
             (and (or (numberp exp)
                      (symbolp exp))
                  (not (eq exp var))))
           (add (a b) (list '+ a b))
           (mul (a b) (list '* a b))
           (diff-mul (a b)
             (TODO 'diff-mul))
           (diff-div (a b)
             (TODO 'diff-div))
           (diff-chain (fun arg)
             ;; Just handle simple, unary functions
             (if (constp arg)
                 0
                 (TODO 'diff-chain)))
           (unsupported (exp)
             (error "Unsupported expression: `~A'" exp))
           (recurse (exp)
             (cond
               ((constp exp)
                0)
               ((eq exp var)
                1)
               (t
                (recurse-op exp))))
           (recurse-op (exp)
             (destructuring-bind (op &rest args) exp
               (case op
                 (+
                  (cons '+ (map 'list #'recurse args)))
                 (-
                  (TODO 'diff--))
                 (*
                  (cond
                    ((null args) 0)
                    ((null (cdr args)) (recurse (car args)))
                    ((null (cddr args))
                     ;; chain rule
                     (diff-mul (first args) (second args)))
                    (t (diff-mul (first args) `(* ,@(cdr args))))))
                 (/
                  (cond
                    ((null args)
                     (error "invalid number of arguments to /"))
                    ((null (cdr args))
                     (recurse `(/ 1 ,(car args))))
                    ((null (cddr args))
                     ;; chain rule
                     (diff-div (first args) (second args)))
                    (t (diff-div (first args) `(* ,@(cdr args))))))
                 (expt
                  (destructuring-bind (base power) args
                    (cond
                      ((and (numberp power) (= 0 power))
                       0)
                      ((constp power)
                       `(* ,power
                           (expt ,base ,(1- power))
                           ,(recurse base)))
                      (t
                       (unsupported exp)))))
                 (otherwise
                  ;; function call
                  (cond
                    ((null args)
                     (unsupported exp))
                    ((null (cdr args))
                     (diff-chain op (car args)))
                    (t
                     (unsupported exp))))))))
    ;; Run it
    (let ((d (recurse exp)))
      (partial-eval d nil))))
