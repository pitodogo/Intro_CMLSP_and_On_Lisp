(defun make-adder (n) #'(lambda (x) (+ x n)))

(setq add2 (make-adder 2)
      add10 (make-adder 10))

(funcall add2 2)

(defun make-adderb (n)
  #'(lambda (x &optional change)
      (if change
	  (setq n x)
	(+ x n))))


(setq addx (make-adderb 1))

(defun make-dmbs (db)
  (list
   #'(lambda (key)
       (cdr (assoc key db)))
   #'(lambda (key val)
       (push (cons key val) db)
       key)
   #'(lambda (key)
       (setf db (delete key db :key #'car))
       key)))


;; recursion

(defun our-member (obj lst)
  if null lst
     nil
     if eql (car lst) obj
        lst
        our-member obj (cdr lst))

(format t "~A plus ~A equals ~A.~%" 2 3 (+ 2 3))

(defun askem (string)
  (format t "~A" string)
  (read))

(askem "How old are you? ")

(defun ask-number ()
  (format t "Please enter a number. ")
  (let ((val (read)))
    (if (numberp val)
	val
      (ask-number))))

(defparameter glob 99)

(setf lst '(c a r a t))
(remove 'a list)


(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(show-squares 2 5)


(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

					; function is regular objects

(funcall #'+ 1 2 3 4 5)
(apply #'+ 1 2 '(3 4 5))


(defun our-listp (x)
  (or (null x) (consp x)))

(equal (cons 'a nil) (cons 'a nil))
(eql (cons 'a nil) (cons 'a nil))

(defun out-equal (x y)
  (or eql x y)
  (and (consp x)
       (consp y)
       (our-equal (car x) (car y))
       (our-equal (cdr x) (cdr y))))


(defun our-copy-list (lst)
  (if (atom lst)
      lst
    (cons (car lst) (our-copy-list (cdr lst)))))

(append '(a b) '(c d) '(e))


(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
    elt))

(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
    x))


(defun compr (elt n lst)
  (if (null lst)
      (list (n-elts elt n))
    (let ((next (car lst)))
      (if (eql next elt)
	  (compr elt (+ n 1) (cdr lst))
	(cons (n-elts elt n)
	      (compr next 1 (cdr lst)))))))


(defun uncompress (lst)
  (if (null lst)
      nil
    (let ((elt (car lst))
	  (rest (uncompress (cdr lst))))
      (if (consp elt)
	  (append (apply #'list-of elt)
	      rest)
      (cons elt rest)))))

(defun list-of (n elt)
  (if (zerop n)
      nil
    (cons elt (list-of (- n 1) elt))))

     
	     
(nth 0 '(a b c))
(nthcdr 2 '(a b c))
  

(defun our-nthcdr (n lst)
  (if (zerop n)
      lst
    (our-nthcdr (- n 1) (cdr lst))))

(defun our-copy-tree (tr)
  (if (atom tr)
      tr
    (cons (our-copy-tree (car tr))
	  (our-copy-tree (cdr tr)))))

(substitute 'y 'x '(and (intergerp x) (zerop (mod x 2))))
(subst 'y 'x '(and (intergerp x) (zerop (mod x 2))))

(defun our-subst (new old tree)
  (if (eql tree old)
      new
    (if (atom tree)
	tree
      (cons (our-subst new old (car tree))
	    (our-subst new old (cdr tree))))))


		       


	  

(teast)
