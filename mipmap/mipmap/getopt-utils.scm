(define-module (mipmap getopt-utils)
  #:use-module (ice-9 getopt-long)
  #:use-module (srfi srfi-1)
  #:export (option-ref-list))

(define (option-ref-list options key default)
  "Return list of matching values in alist OPTIONS using KEY, a symbol; or DEFAULT if not found."
  (let ((vals (fold (lambda (x l)
                      (if (eq? key (car x))
                          (cons (cdr x) l)
                          l))
                    '() options)))
    (if (null? vals) default vals)))

