(define-module (mipmap gaf-utils)
  #:use-module (geda page)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (ice-9 rdelim)
  #:export (load-page! page-save! page-save-as!)
  #:export (object-filter-attribs)
  #:export (component-add-attrib! remove-attrib!))

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
          (if value (string=? (attrib-value attrib) value) #t)))
      (object-attribs object))))

; Adds an attribute with the string <attrib-name>=<attrib-value> to <component>
(define (component-add-attrib! component attrib-string)
  (let ((attrib (make-text (component-position component) 'lower-left 0 attrib-string 10 #f 'both 5))
        (page (object-page component)))
    (page-append! page attrib)
    (attach-attribs! component attrib)))

; Removes <attrib> from its associated page.
(define (remove-attrib! attrib)
  (let ((component (attrib-attachment attrib))
        (page (object-page attrib)))
    (if component (detach-attribs! component attrib))
    (if page (page-remove! page attrib))))

