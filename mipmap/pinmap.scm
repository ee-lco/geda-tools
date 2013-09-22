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
          (lambda (pinmap)
            (apply-pinmap page component pinmap))
          (object-filter-attribs component "pinmap")))
      (filter component? (page-contents page)))
    (page-save! page)))

(define (main args)
  (let* ((option-spec '())
         (options (getopt-long args option-spec)))
    (let ((pages (option-ref options '() '())))
      (map process-page pages))))

(main (command-line))

