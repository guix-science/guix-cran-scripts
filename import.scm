;;; Copyright © 2022-2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2022 Lars-Dominik Braun <lars@6xq.net>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;;; for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program. If not, see <https://www.gnu.org/licenses/>.

(use-modules
  (statprof)
  (htmlprag)
  (sxml xpath)
  (gnu packages)
  (guix packages)
  (guix import cran)
  (guix import utils)
  (guix read-print)
  (guix diagnostics)
  (guix modules)
  (guix discovery)
  ((guix http-client) #:select (http-fetch))
  ((guix build utils) #:select (mkdir-p with-directory-excursion invoke delete-file-recursively))
  ((guix store) #:select (with-store add-indirect-root))
  ((guix download) #:select (download-to-store))
  ((guix memoization) #:select (mlambdaq))
  (json)
  (rnrs bytevectors)
  (srfi srfi-1)
  (srfi srfi-26)
  (srfi srfi-71)
  (ice-9 match)
  (ice-9 threads)
  (ice-9 sandbox)
  (ice-9 ftw)
  ((ice-9 popen) #:select (open-input-pipe close-pipe))
  ((ice-9 rdelim) #:select (read-string))
  (web client))

(define %bioc-channel-url "https://github.com/guix-science/guix-bioc.git")
(define %cran-channel-url "https://github.com/guix-science/guix-cran.git")

;; Cache current time, so it’s available without overhead.
(define %now (current-time))
(define %cran-url "https://cloud.r-project.org/web/packages/")
(define %bioconductor-version
  (@@ (guix import cran) %bioconductor-version))
(define %bioconductor-url
  (string-append "https://bioconductor.org/packages/json/"
                 %bioconductor-version "/bioc/packages.js"))
(define %bioconductor-annotation-url
  (string-append "https://bioconductor.org/packages/json/"
                 %bioconductor-version "/data/annotation/packages.js"))
(define %bioconductor-experiment-url
  (string-append "https://bioconductor.org/packages/json/"
                 %bioconductor-version "/data/experiment/packages.js"))

(define download
  (@@ (guix import cran) download))

(define* (download-source urls #:key method (ref '()))
  "Download CRAN source tarball and store to cache. The cache never
expires."
  ;; CRAN urls have unique basename.
  (let* ((url (match urls
                ((url . rest) url)
                (url url)))
         (cache-path (string-append "cache/contents/" (basename url))))
    (unless (access? cache-path R_OK)
      (format #t "Fetching ~a into cache~%" url)
      (with-store store
        (let ((store-path (download url #:method method #:ref ref)))
          (symlink store-path cache-path)
          (add-indirect-root store (canonicalize-path cache-path)))))
    cache-path))

(define (cache-fresh? file)
  "Consider the cached file stale after 23 hours."
  (let ((file-stat (false-if-exception (lstat file))))
    (and file-stat
         (< (- %now (* 23 60 60))
            (stat:mtime file-stat)))))

(define* (cached-fetch-description repository name #:optional version)
  "Back FETCH-DESCRIPTION with a cache."
  (let ((cache-path (string-append "cache/description/" name)))
    (let ((result
           (if (cache-fresh? cache-path)
               (call-with-input-file cache-path read)
               (let ((contents
                      ((@@ (guix import cran) fetch-description)
                       repository name version download-source)))
                 (call-with-output-file cache-path
                   (lambda (port) (write contents port)))
                 contents))))
      (or result
          (error (format #false "No DESCRIPTION for `~a'" name))))))

;; TODO: memoize these
(define (all-cran-packages)
  (let ((response body
                  (http-get
                   (string-append %cran-url "available_packages_by_name.html"))))
    ((sxpath '(* * table * td a span *text*))
     (html->sxml body))))

(define (bioc-packages url)
  "Return the names of all Bioconductor packages"
  (let* ((json (let ((response body (http-get url)))
                 (let* ((text (utf8->string body))
                        (data-index (string-index text #\{))
                        (json-string (string-drop text data-index)))
                   (call-with-input-string json-string
                     (lambda (port)
                       (json->scm port #:concatenated #true))))))
         (content (vector->list (assoc-ref json "content"))))
    ;; Some Bioconductor packages have no description file and no
    ;; source tarball.  Remove these from the list.  As a side effect
    ;; we warm up the cache.
    (let* ((names (map (compose car vector->list) content)))
      (filter-map (lambda (name)
                    (and (false-if-exception
                          (cached-fetch-description 'bioconductor name))
                         name))
                  names))))

(define (all-bioc-packages)
  (append (bioc-packages %bioconductor-url)
          (bioc-packages %bioconductor-annotation-url)
          (bioc-packages %bioconductor-experiment-url)))

(define all-r-packages
  (fold-packages
   (lambda (pkg res)
     (if (eq? (package-build-system pkg)
              (@ (guix build-system r) r-build-system))
         (cons pkg res)
         res))
   (list)))

(define cran-guix-name (cut guix-name "r-" <>))

(define (upstream-name pkg)
  "Return the upstream CRAN name of the package PKG."
  (or (and=>
       (package-properties pkg)
       (lambda (prop) (assoc-ref prop 'upstream-name)))
      (string-drop (package-name pkg) 2)))

;; CRAN package names not in Guix yet. Packages names are case-insensitive.
(define (missing all)
  (let ((existing (map upstream-name all-r-packages)))
    (lset-difference string-ci=? all existing)))

(define (package-sexp->name package)
  "Extract the value of the NAME field from the package S-expression
PACKAGE."
  (match package
    ((and expr ('package fields ...))
     (match expr
       ((path *** ('name name))
         name)))
    (x
     (pk 'nope))))

(define (package-sexp->propagated-inputs package)
  "Extract the values of the PROPAGATED-INPUTS field from the package
S-expression PACKAGE as a list."
  (match package
    ((and expr ('package fields ...))
     (match expr
       ((path *** ('propagated-inputs ('list sym ...)))
         sym)
       (_ '())))
    (x
     (pk 'nope))))

(define (package-sexp->inputs package)
  "Extract the value of the INPUTS field from the package S-expression
PACKAGE as a list."
  (match package
    ((and expr ('package fields ...))
     (match expr
       ((path *** ('inputs ('list sym ...)))
         sym)
       (_ '())))
    (x
     (pk 'nope))))

(define (package-sexp->native-inputs package)
  "Extract the value of the NATIVE-INPUTS field from the package
S-expression PACKAGE as a list."
  (match package
    ((and expr ('package fields ...))
     (match expr
       ((path *** ('native-inputs ('list sym ...)))
         sym)
       (_ '())))
    (x
     (pk 'nope))))

(define (replace-package-sexp-field package field replacement)
  "Replace PACKAGE’s FIELD with REPLACEMENT."
  (match package
    ((and expr ('package fields ...))
     `(package ,@(map (lambda (x) (if (eq? (car x) field) `(,(car x) (list ,@replacement)) x)) fields)))
    (x
     (pk 'nope))))

;; Required to turn the cran importer’s symbol names into module imports.
(define known-variable-definition
  (mlambdaq (symbol) ((@@ (guix ui) known-variable-definition) symbol)))

(define (package-path output-dir channel-name)
  (string-append output-dir "/" channel-name "/packages"))

(define (write-module path module-sexp packages-sexp)
  (call-with-output-file path
    (lambda (port)
      (pretty-print-with-comments port module-sexp)
      (format port "\n\n")
      (for-each
        (lambda (p)
          (pretty-print-with-comments port p)
          (format port "\n\n"))
        packages-sexp))))

(define (delete-nonexistent-variables inputs all-r-names)
  "Delete non-existent variables from INPUTS."

  (fold (lambda (input accum)
    (match (known-variable-definition input)
      ;; Assumes all packages can be imported correctly i.e., all-r-names is correct.
      (#f (if (member (symbol->string input) all-r-names)
              (cons input accum)
              (begin
                (format (current-error-port) "Deleting variable ~a: Does not exist.~%" input)
                accum)))
      ((? module? module)
       (let ((pkg (module-ref module input)))
         ;; Packages like pkg-config use DEFINE-SYNTAX, so PACKAGE? would return
         ;; #f. But packages can never be procedures.
         (if (procedure? pkg)
           (begin
             (format (current-error-port) "Deleting variable ~a: Not a package.~%" input)
             accum)
           (cons input accum))))))
   '() inputs))

(define (add-license:-prefix symbol)
  "Add license: prefix to SYMBOL."
  (string->symbol (string-append "license:" (symbol->string symbol))))
    
(define (import-package upstream-name type)
  "Import package UPSTREAM-NAME from upstream repository TYPE, fix
inputs and return imports/package definition."
  (format #t "Importing package ~a from ~a…~%"
          upstream-name (case type
                          ((bioc) "Bioconductor")
                          ((cran) "CRAN")))
  (let* ((all-r-names
          (map cran-guix-name
               (case type
                 ((bioc)
                  (append (all-cran-packages)
                          (all-bioc-packages)))
                 ((cran)
                  (all-cran-packages)))))
         (package-sexp (cran->guix-package upstream-name
                                           #:repo type
                                           #:license-prefix add-license:-prefix
                                           #:fetch-description cached-fetch-description
                                           #:download-source download-source))
         (_ (unless package-sexp (error (format #false "`~a' is empty" upstream-name))))
         (guix-name (package-sexp->name package-sexp))

         ;; Rewrite inputs, deleting non-existent variables.
         (propagated-inputs (delete-nonexistent-variables
                             (package-sexp->propagated-inputs package-sexp)
                             all-r-names))
         (inputs (delete-nonexistent-variables
                  (package-sexp->inputs package-sexp)
                  all-r-names))
         (native-inputs (delete-nonexistent-variables
                         (package-sexp->native-inputs package-sexp)
                         all-r-names))
         (fixed-package-sexp (replace-package-sexp-field
                               (replace-package-sexp-field
                                 (replace-package-sexp-field package-sexp
                                   'propagated-inputs propagated-inputs)
                                 'inputs inputs)
                               'native-inputs native-inputs))

         (module-args-sexp
          `((#:use-module (guix packages))
            (#:use-module (guix download))
            (#:use-module (guix build-system r))
            (#:use-module ((guix licenses) #:prefix license:))
            ,@(fold (lambda (input accum)
                  (match (known-variable-definition input)
                    ;; New package → variable is defined in this module.
                    (#f accum)
                    ;; Existing package → import corresponding module.
                    ((? module? module) (cons `(#:use-module ,(module-name module)) accum))))
                 '() (append inputs propagated-inputs native-inputs))))
         (package-def-sexp `(define-public ,(string->symbol guix-name) ,fixed-package-sexp)))
    (list guix-name module-args-sexp package-def-sexp)))

(define (package->module package)
  "Find the module which the PACKAGE is defined in."
  (map string->symbol
    (string-split (string-drop-right
      (location-file (package-location package)) 4) #\/)))

(define (import-existing package)
  (format #t "Referencing package ~a from Guix proper…~%" (package-name package))
  (let* ((guix-name (package-name package))
         (module (package->module package))

         (module-args-sexp `((#:use-module (,module #:prefix guix:))))
         (package-def-sexp `(define-public ,(string->symbol guix-name)
                              ,(string->symbol (string-append "guix:" guix-name)))))
    (list guix-name module-args-sexp package-def-sexp)))

(define (channel-file type)
  "Return a description of the channel for the given repository TYPE.
This is to be written to the .guix-channel file."
  (case type
    ((bioc)
     `(channel
       (version 0)
       (url ,%bioc-channel-url)
       (dependencies
        (channel
         (name guix-cran)
         (url ,%cran-channel-url)))))
    ((cran)
     `(channel
       (version 0)
       (url ,%cran-channel-url)))))

(define (write-channel-file output-dir type)
  "Write .guix-channel to indicate output-dir is a valid Guix channel."
  (call-with-output-file (string-append output-dir "/.guix-channel")
    (lambda (port)
      (pretty-print-with-comments port (channel-file type)))))

(define (group sorted-items key)
  "Group all items in list SORTED-ITEMS by KEY and return a list of
((key-a . items-a) (key-b . items-b) …)"
  (fold
    (lambda (item accum)
      (let ((item-key (key item)))
        (match accum
          ('() (list (list item-key (list item))))
          (((last-key (items ...)) rest ...)
             (if (eq? last-key item-key)
               (cons (list last-key (cons item items)) rest)
               (cons (list item-key (list item)) (cons (list last-key items) rest)))))))
    '() sorted-items))

(define (create-channel output-dir channel-name missing type)
  "Create channel containing all MISSING packages not in Guix proper yet."

  (when (zero? (length missing))
    ;; Something must be wrong.
    (raise-exception 'no-missing-packages))

  (delete-file-recursively (package-path output-dir channel-name))
  (mkdir-p (package-path output-dir channel-name))

  ;(define existing-sexps (map import-existing all-r-packages))
  (define existing-sexps '())
  
  (define new-sexps
    (filter-map (lambda (name)
                  (false-if-exception
                   (import-package name type)))
                missing))

  (define all-sexps
    (sort (append existing-sexps new-sexps)
          (lambda (a b) (string<? (car a) (car b)))))

  ;; Group packages by name. It seems that having multiple smaller files
  ;; is slightly faster than having a big one with all packages.
  (define grouped-sexps
    (group all-sexps
           (match-lambda
            ((name imports package)
             (string->symbol (string-downcase (substring name 2 3)))))))

  (define (write-group-module key items)
    (let* ((all-imports (apply append
                               (delete-duplicates
                                 (append-map
                                   (match-lambda ((name imports package) imports))
                                   items))))
           (local-imports (apply append
                                 (map (match-lambda
                                        ((group-key (items ...))
                                          (if (eq? key group-key)
                                            '()
                                            `(#:use-module (,(string->symbol channel-name) packages ,group-key)))))
                                      grouped-sexps)))
           (all-packages (map (match-lambda ((name imports package) package)) items))
           (module-sexp `(define-module (,(string->symbol channel-name) packages ,key)
                           ,@all-imports
                           ,@local-imports))
           (path (string-append (package-path output-dir channel-name) "/" (symbol->string key) ".scm")))
      (write-module path module-sexp all-packages)))

  (for-each (match-lambda ((key (items ...)) (write-group-module key items))) grouped-sexps)
  (write-channel-file output-dir type))

(define (guix-channels)
  "Get Guix channel list sexp."
  (let* ((port (open-input-pipe "guix describe -f channels"))
         (channels (read port)))
    (close-pipe port)
    channels))

(define (validate-channel output-dir channel-name)
  "Ensure the channel in OUTPUT-DIR can be pulled from. If not, reset to
previous commit."
  (let* ((guix-channels (guix-channels))
         (channels-scm `(cons (channel
                                (name ',(string->symbol channel-name))
                                (url ,(canonicalize-path output-dir)))
                              ;; Re-use the exact Guix commit, so packages are the same.
                              ,guix-channels))
         (channels-port (mkstemp "/tmp/channels.scm.XXXXXXX"))
         (channels-path (port-filename channels-port)))
    (pretty-print-with-comments channels-port channels-scm)
    (close-port channels-port)

    ;; Commit changes.
    (format #t "Committing changes in ~a~%" output-dir)
    (with-directory-excursion output-dir
      ;; guile git does not provide commit, so use shell tools.
      (invoke "git" "add" ".")
      (invoke "git" "commit" "-m" (string-append "Automated CRAN import "
                                                 (strftime "%F" (gmtime %now))
                                                 "\n\n"
                                                 "Compatible with the following channels:\n\n"
                                                 (object->string* guix-channels 0))))

    ;; Try pulling from the channel, revert to previous state if it fails.
    (with-exception-handler
      (lambda (e)
        (format #t "`guix pull` failed: ~a~%" e)
        (with-directory-excursion output-dir
          (invoke "git" "reset" "HEAD^"))
        (exit 2))
      (lambda ()
        (let ((profile-directory (string-append (mkdtemp "/tmp/profile.XXXXXXX") "/profile")))
          (format #t "Trying to pull from ~a to ~a~%" channels-path profile-directory)
          (invoke "guix" "pull" "-C" channels-path  "-p" profile-directory)
          (delete-file-recursively profile-directory)))
      #:unwind? #t)
    (delete-file channels-path)))

(define* (main output-dir channel-name #:optional (type 'cran))
  "Entry point."
  (let* ((packages (case type
                     ((bioc) (all-bioc-packages))
                     ((cran) (all-cran-packages))))
         (missing-packages
          (missing packages)))
    (create-channel output-dir channel-name missing-packages type)
    (validate-channel output-dir channel-name)))

(setvbuf (current-output-port) 'line)
(match (program-arguments)
  ((_ output-dir channel-name . type)
   (let ((type* (match type
                  (("bioc") 'bioc)
                  (_ 'cran))))
     (main output-dir channel-name type*)))
  (_ (format #t "Usage: guix repl import.scm <output-dir> <channel-name> [bioc|cran]~%")))

