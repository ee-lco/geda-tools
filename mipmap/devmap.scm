(use-modules
  (geda page)
  (geda object)
  (geda attrib)
  (ice-9 getopt-long)
  (ice-9 rdelim)
  (ice-9 regex)
  (ice-9 string-fun)
  (srfi srfi-1)
  (mipmap gaf-utils))

(define (option-ref-list options key default)
  "Return list of matching values in alist OPTIONS using KEY, a symbol; or DEFAULT if not found."
  (let ((vals (fold (lambda (x l)
                      (if (eq? key (car x))
                          (cons (cdr x) l)
                          l))
                    '() options)))
    (if (null? vals) default vals)))

(define pages '())
(define devmap-paths '())

(define (open-devmap-file name)
  (let ((
    file
    (or-map
      (lambda (path)
        (catch #t
          (lambda () (open-input-file (string-append path (if (string-suffix? "/" path) "" "/") name)))
          (lambda (key . args) #f)))
      devmap-paths)))
    (if (not file) (throw 'file-not-found-ex name))
    file))


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

;(define has-devmap?
;    (lambda (object)
;        (not (null? (filter
;            (lambda (attrib) (string=? (attrib-name attrib) "devmap"))
;            (object-attribs object))))))

;(define page (load-page! in_schem_file))

;(display (page-contents page))
;(display (filter component? (page-contents page)))
;(display (filter has-devmap? (filter component? (page-contents page))))
;(define build-devmap-path
;    (lambda (path name)
;(define find-devmap
;    (lambda (devmap-name)
;(define apply-devmap
;    (lambda (page component devmap-attrib)
;        (map
;            (lambda (path)
;                (display (string-append path (if (string-suffix? "/" path) "" "/") (attrib-value devmap-attrib) ".dev" "\n")))
;            devmap-path)))
;;        (display (string-append (basename (page-filename page)) " " (component-basename component) " " (attrib-value devmap-attrib) "\n"))))

;(map
;    (lambda (page-filename)
;        (define page (load-page! page-filename))
;        (map
;            (lambda (component)
;                (map
;                    (lambda (attrib)
;                        (if (string=? (attrib-name attrib) "devmap")
;                            (apply-devmap page component attrib)))
;                    (object-attribs component)))
;            (filter component? (page-contents page)))
;        (page-save-as! page (string-append page-filename "~")))
;    pages)

;(page-save-as! page out_schem_file)


;(define find-devmap
;    (lambda (object)
;        (filter
;            (lambda (attrib) (string=? (attrib-name attrib) "devmap"))
;            (object-attribs object))))

;(define has-devmap?
;    (lambda (object)
;        (not (null? (filter
;            (lambda (attrib) (string=? (attrib-name attrib) "devmap"))
;            (object-attribs object))))))

;(define ot
;    (lambda (o)
;        (if (component? o) (display (string-append (component-basename o) "\n"))
;        (if (text? o) (set-text-string! o "eelco")))))

;(display (map component-basename (filter has-devmap? (filter component? (page-contents page1)))))

;(define add-attr
;    (lambda (page component devmap-attr attr-string)
;        (let ((devmap-attr-info (text-info devmap-attr))
;              (attr (make-text (list-ref devmap-attr-info 0) (list-ref devmap-attr-info 1) (list-ref devmap-attr-info 2) attr-string (list-ref devmap-attr-info 4) (list-ref devmap-attr-info 5) (list-ref devmap-attr-info 6) (list-ref devmap-attr-info 7))))
;            (page-append! page attr)
;            (attach-attribs! component attr))))

;(define apply-devmap-cmd
;    (lambda (page component devmap-attr cmd)
;        (if (string=? (car cmd) "attr") (add-attr page component devmap-attr (cdr cmd))
;        (if (string=? (car cmd) "del")  (del-attr page component devmap-attr (cdr cmd))))))

;(add-attr (car (filter has-devmap? (filter component? (page-contents page1)))) "pietje=gek")
;(map
;    (lambda (obj)
;        (if (object-component obj) (display (string-append (component-basename obj) "\n"))))
;    (page-contents page1))

;(display (page->string page1) (open-output-file "testschem.sch"))
;(page-save-as! page1 out_schem_file)

