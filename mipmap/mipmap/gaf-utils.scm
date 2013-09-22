(define-module (mipmap gaf-utils)
  #:use-module (geda page)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (ice-9 rdelim)
  #:export (load-page! page-save! page-save-as!)
  #:export (object-filter-attribs)
  #:export (component-add-attrib component-del-attrib)
  #:export (remove-attrib!))

; Creates a gEDA <page> from the file <filename>
(define (load-page! filename)
  (string->page filename (read-delimited "" (open-input-file filename))))

; Saves <page> to the file specified by its associated <filename>
(define (page-save! page)
  (display (page->string page) (open-output-file (page-filename page))))

; Saves <page> to the file <filename>
(define (page-save-as! page filename)
  (display (page->string page) (open-output-file filename)))

; Returns a list of all attributes attached to <object> that have
; attrib-name <name> and (optionally) attrib-value <value=opts[0]>.
(define (object-filter-attribs object name . opts)
  (let ((value (if (null? opts) #f (car opts))))
    (filter
      (lambda (attrib)
        (and
          (string=? (attrib-name attrib) name)
          (if (null? value) #t (string=? (attrib-value attrib) value))))
      (object-attribs object))))

; Deprecated
(define (component-add-attrib page component devmap-attrib attrib-string)
  (let ((attrib (apply make-text (text-info devmap-attrib))))
    (set-text-string! attrib attrib-string)
    (page-append! page attrib)
    (attach-attribs! component attrib)))

; Deprecated
(define (component-del-attrib page component devmap-attrib attrib-string)
  (map
    (lambda (attrib)
      (detach-attribs! component attrib)
      (page-remove! page attrib))
    (filter
      (lambda (attrib)
        (string=? (attrib-name attrib) attrib-string))
      (object-attribs component))))

; Removes <attrib> from its associated page.
(define (remove-attrib! attrib)
  (let ((component (attrib-attachment attrib))
        (page (object-page attrib)))
    (if component (detach-attribs! component attrib))
    (if page (page-remove! page attrib))))

