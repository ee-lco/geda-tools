(define-module (mipmap gaf-utils)
  #:use-module (geda page)
  #:use-module (geda object)
  #:use-module (geda attrib)
  #:use-module (ice-9 rdelim)
  #:export (load-page! page-save! page-save-as!)
  #:export (object-filter-attribs)
  #:export (component-add-attrib component-del-attrib))

(define (load-page! filename)
  (string->page filename (read-delimited "" (open-input-file filename))))
(define (page-save! page)
  (display (page->string page) (open-output-file (page-filename page))))
(define (page-save-as! page filename)
  (display (page->string page) (open-output-file filename)))

(define (object-filter-attribs component name)
  (filter
    (lambda (attrib) (string=? (attrib-name attrib) name))
    (object-attribs component)))

(define (component-add-attrib page component devmap-attrib attrib-string)
  (let ((attrib (apply make-text (text-info devmap-attrib))))
    (set-text-string! attrib attrib-string)
    (page-append! page attrib)
    (attach-attribs! component attrib)))

(define (component-del-attrib page component devmap-attrib attrib-string)
  (map
    (lambda (attrib)
      (detach-attribs! component attrib)
      (page-remove! page attrib))
    (filter
      (lambda (attrib)
        (string=? (attrib-name attrib) attrib-string))
      (object-attribs component))))

