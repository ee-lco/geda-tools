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

(define devmap-paths '())

(define (open-devmap-file filename)
  (or (open-input-file-from-path filename devmap-paths) (throw 'file-not-found filename)))

(define (read-devmap file)
  (let ((line (read-line file)))
    (if (eof-object? line)
      '()
      (cons line (read-devmap file)))))

(define (parse-devmap component lines)
  (map
    (lambda (line)
      (let ((match (string-match "^([A-Za-z0-9_-]+)[[:space:]]+(.*)$" line)))
        (if match
          (let ((cmd (match:substring match 1))
                (arg (match:substring match 2)))
            (cond ((string=? cmd "attr") (component-add-attrib! component arg))
                  ((string=? cmd "delattr") (map remove-attrib! (object-filter-attribs component arg)))
                  (else (throw 'syntax-error line))))
          (if (not (string-null? line)) (throw 'syntax-error cmd)))))
    lines))

(define (process-attrib component devmap-attrib)
  (parse-devmap component (read-devmap (open-devmap-file (attrib-value devmap-attrib)))))

; Performs devmapping on all component attributes on <page>
(define (process-page page)
  (map
    (lambda (component)
      (map
        (lambda (devmap-attrib) (process-attrib component devmap-attrib))
        (object-filter-attribs component "devmap")))
    (filter component? (page-contents page))))

(define (main args)
  (let* ((option-spec '((lib     (single-char #\L) (value #t))))
         (options (getopt-long args option-spec))
         (page (string->page "STDIN" (read-delimited "" (current-input-port)))))
    (set! devmap-paths (option-ref-list options 'lib '()))
    (process-page page)
    (display (page->string page))))

(main (command-line))

