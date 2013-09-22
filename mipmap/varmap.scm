(use-modules
  (geda page)
  (geda object)
  (geda attrib)
  (ice-9 getopt-long)
  (ice-9 rdelim)
  (ice-9 regex)
  (srfi srfi-1)
  (mipmap gaf-utils)
  (mipmap getopt-utils))

; Performs varmapping on the specified attrib <var-attrib>
(define (process-attrib component var-attrib variant)
  ; Check if the attrib-value matches the var-attrib format:
  ; [<var-spec>] <new-attrib-value>
  ; with <var-spec> being a comma separated list of variants
  (let ((match (string-match "^[[]([^]]*)[]][[:space:]]*(.*)$" (attrib-value var-attrib))))
    (if match
      ; Check if <var-spec> contains <variant>
      (if (find (lambda (var) (string=? var variant)) (string-split (match:substring match 1) #\,))
        (begin
          ; Check if a <target-attrib> exists. If so, assign <new-attrib-value> to <target-attrib>
          ; and delete <var-attrib>, otherwise assign <new-attrib-value> to <var-attrib>
          ; A <target-attrib> should have the same attrib-name as <var-attrib> and
          ; attrib-value "*"
          (let ((target-attrib (object-filter-attribs component (attrib-name var-attrib) "*")))
            (if (null? target-attrib)
              (set-attrib-value! var-attrib (match:substring match 2))
              (begin
                (set-attrib-value! (car target-attrib) (match:substring match 2))
                (remove-attrib! var-attrib)))))
        ; No match, delete <var-attrib>
        (remove-attrib! var-attrib)))))

; Performs varmapping on all component attributes on <page>
(define (process-page page variant)
  (map
    (lambda (component)
      (map 
        (lambda (var-attrib) (process-attrib component var-attrib variant))
        (object-attribs component)))
    (filter component? (page-contents page))))

; @todo simplify argument handling, only one argument <variant> is needed.
(define (main args)
  (let* ((option-spec '((variant (single-char #\v) (value #t))))
         (options (getopt-long args option-spec))
         (variant (option-ref options 'variant "ALL"))
         (page (string->page "STDIN" (read-delimited "" (current-input-port)))))
    (process-page page variant)
    (display (page->string page))))

(main (command-line))

