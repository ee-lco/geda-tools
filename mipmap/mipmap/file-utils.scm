(define-module (mipmap file-utils)
  #:export (open-input-file-from-path read-file))

(define (open-input-file-from-path filename paths)
(or-map
  (lambda (path)
    (catch #t
      (lambda () (open-input-file (string-append path (if (string-suffix? "/" path) "" "/") filename)))
      (lambda (key . args) #f)))
  paths))

(define (read-file file)
  (let ((line (read-line file)))
    (if (eof-object? line)
      '()
      (cons line (read-file file)))))

