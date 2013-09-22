(use-modules
  (geda page)
  (geda object)
  (geda attrib)
  (ice-9 getopt-long)
  (ice-9 rdelim)
  (ice-9 regex)
  (ice-9 string-fun)
  (srfi srfi-1)
  (mipmap file-utils)
  (mipmap gaf-utils)
  (mipmap getopt-utils))

(define pages '())
(define devmap-paths '())

(define (open-devmap-file filename)
  (or (open-input-file-from-path filename devmap-paths) (throw 'file-not-found filename)))

(define (read-devmap file)
  (let ((line (read-line file)))
    (if (eof-object? line)
      '()
      (cons line (read-devmap file)))))

(define (parse-devmap cmds)
  (filter-map
    (lambda (cmd)
      (let ((match (string-match "^([A-Za-z0-9_-]+)[[:space:]]+(.*)$" cmd)))
        (if match
          (cond ((string=? (match:substring match 1) "attr") (list component-add-attrib (match:substring match 2)))
                ((string=? (match:substring match 1) "delattr") (list component-del-attrib (match:substring match 2)))
                (else (throw 'syntax-error cmd)))
          (if (string-null? cmd) #f (throw 'syntax-error cmd)))))
    cmds))

(define (apply-devmap page component devmap-attrib)
;  (let ((match (string-match "^([^:]*):(.*)$" (attrib-value devmap-attrib))))
;    (if match
;      (separate-fields-discarding-char "," (match:substring match 1))
;    ; check if current variant is in comma-seperated list 'match 1'
;    ; use entire attrib-value
  (map
    (lambda (cmd)
      (apply (car cmd) (list page component devmap-attrib (cadr cmd))))
    (parse-devmap (read-devmap (open-devmap-file (attrib-value devmap-attrib))))))

(define (apply-pinmap page component pinmap-attrib)
  (let ((match (string-match "^([^=]+)=(.*)$" (attrib-value pinmap-attrib))))
    (if (not match) (throw 'syntax-error (text-string pinmap-attrib)))
    (let ((oldpin (match:substring match 1))
          (newpin (match:substring match 2)))
      (map
        (lambda (pin)
          (map
            (lambda (pinnumber)
              (if (string=? (attrib-value pinnumber) oldpin)
                 (set-attrib-value! pinnumber newpin)))
            (object-filter-attribs pin "pinnumber")))
        (filter pin? (component-contents component))))))

(define (process-page page-name)
  (let ((page (load-page! page-name)))
    (map
      (lambda (component)
        (map
          (lambda (devmap)
            (apply-devmap page component devmap))
          (object-filter-attribs component "devmap"))
        (map
          (lambda (pinmap)
            (apply-pinmap page component pinmap))
          (object-filter-attribs component "pinmap")))
      (filter component? (page-contents page)))
    (page-save! page)))

(define (main args)
  (let* ((option-spec '((lib     (single-char #\L) (value #t))))
         (options (getopt-long args option-spec)))
    (set! devmap-paths (option-ref-list options 'lib '()))
    (let ((pages (option-ref options '() '())))
      (map process-page pages))))

(main (command-line))

