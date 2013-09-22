(use-modules
  (geda page)
  (geda object)
  (geda attrib)
  (ice-9 getopt-long)
  (ice-9 regex)
  (srfi srfi-1)
  (mipmap gaf-utils)
  (mipmap getopt-utils))

(define pages #f)
(define variant #f)

(define (process-page page-name)
  (let ((page (load-page! page-name)))
    (map
      (lambda (component)
        (map
          (lambda (attrib)
            (let ((match (string-match "^[[]([^]]*)[]][[:space:]]*(.*)$" (attrib-value attrib))))
              (if match
                (if (find (lambda (var) (string=? var variant)) (string-split (match:substring match 1) #\,))
                  (set-attrib-value! attrib (match:substring match 2))
                  (begin
                    (detach-attribs! component attrib)
                    (page-remove! page attrib))))))
          (object-attribs component)))
      (filter component? (page-contents page)))
    (page-save! page)))

(define (main args)
  (let* ((option-spec '((variant (single-char #\v) (value #t))))
         (options (getopt-long (cdr args) option-spec)))
    (set! variant (option-ref options 'variant "ALL"))
    (display stdin)
    (let ((pages (option-ref options '() '())))
      (map process-page pages))))

(main (command-line))

