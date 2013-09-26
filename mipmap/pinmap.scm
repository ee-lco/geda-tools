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

(define (process-attrib component pinmap-attrib)
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

; Performs pinmapping on all component attributes on <page>
(define (process-page page)
  (map
    (lambda (component)
      (map
        (lambda (pinmap)
          (process-attrib component pinmap))
        (object-filter-attribs component "pinmap")))
    (filter component? (page-contents page))))

(define (main args)
  (let* ((option-spec '())
         (options (getopt-long args option-spec))
         (page (string->page "STDIN" (read-delimited "" (current-input-port)))))
    (process-page page)
    (display (page->string page))))

(main (command-line))

