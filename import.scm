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
  (guix i18n)
  (guix http-client)
  (guix sets)
  ((guix build utils) #:select (alist-cons-after mkdir-p with-directory-excursion invoke delete-file-recursively))
  ((guix build-system r) #:select (bioconductor-uri))
  ((guix store) #:select (with-store add-indirect-root))
  ((guix download) #:select (download-to-store))
  ((guix memoization) #:select (mlambdaq memoize))
  (rnrs bytevectors)
  (srfi srfi-1)
  (srfi srfi-26)
  (srfi srfi-34)
  (srfi srfi-35)
  (srfi srfi-71)
  (ice-9 match)
  (ice-9 threads)
  (ice-9 sandbox)
  (ice-9 ftw)
  ((ice-9 popen) #:select (open-input-pipe close-pipe))
  ((ice-9 rdelim) #:select (read-string read-line))
  (web client)
  (web response)
  (web uri))

(define %bioc-channel-url "https://github.com/guix-science/guix-bioc.git")
(define %cran-channel-url "https://github.com/guix-science/guix-cran.git")

;; Cache current time, so it’s available without overhead.
(define %now (current-time))
(define %cran-url "https://cloud.r-project.org/web/packages/")
(define %bioconductor-version
  (@@ (guix import cran) %bioconductor-version))

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

;; We use this to avoid downloading Bioconductor data packages with
;; excessively large source tarballs.
(define (source-size-too-big? url)
  "Check the size of the download behind URL and abort if it exceeds
the threshold.  Return the size if the source is too big."
  (let* ((cache-path (string-append "cache/contents/" (basename url)))
         (largest-size (* 1024 1024 1024)) ;1 GiB
         (size (if (access? cache-path R_OK)
                   (stat:size (stat cache-path))
                   (let ((response body (http-head url)))
                     (assoc-ref (response-headers response)
                                'content-length)))))
    (and (> size largest-size)
         (format (current-error-port) "Rejecting `~a' due to size: ~a bytes~%"
                 url size)
         size)))

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

(define all-cran-packages
  (memoize
   (lambda ()
     (let ((response body
                     (http-get
                      (string-append %cran-url "available_packages_by_name.html"))))
       ((sxpath '(* * table * td a span *text*))
        (html->sxml body))))))

;; XXX taken from (guix import cran)
(define* (bioconductor-packages-list-url #:optional type)
  (string-append "https://bioconductor.org/packages/"
                 %bioconductor-version
                 (match type
                   ('annotation "/data/annotation")
                   ('experiment "/data/experiment")
                   (_ "/bioc"))
                 "/src/contrib/PACKAGES"))

;; XXX adapted from (guix import cran)
(define bioconductor-packages-list
  (memoize
   (lambda (type)
     "Return the latest version of package NAME for the current bioconductor
release."
     (let ((url (string->uri (bioconductor-packages-list-url type))))
       (guard (c ((http-get-error? c)
                  (warning (G_ "failed to retrieve list of packages \
from ~a: ~a (~a)~%")
                           (uri->string (http-get-error-uri c))
                           (http-get-error-code c)
                           (http-get-error-reason c))
                  #f))
         ;; Split the big list on empty lines, then turn each chunk into an
         ;; alist of attributes.
         (map (lambda (chunk)
                (description->alist (string-join chunk "\n")))
              (let* ((port  (http-fetch/cached url))
                     (lines (read-lines port)))
                (close-port port)
                (chunk-lines lines))))))))

(define* (bioconductor-name->url name #:optional type)
  "Return the source URLs for the Bioconductor package NAME of the
given TYPE."
  (and=> (find (lambda (meta)
                 (string=? (assoc-ref meta "Package") name))
               (bioconductor-packages-list type))
         (lambda (entry)
           (bioconductor-uri name (assoc-ref entry "Version") type))))

(define* (bioc-packages #:optional type)
  "Return the names of all Bioconductor packages of the given TYPE."
  ;; Some Bioconductor packages have no description file and no
  ;; source tarball.  Remove these from the list.  As a side effect
  ;; we warm up the cache.
  ;; Also remove source files that are too big.
  (let* ((packages-info (bioconductor-packages-list type))
         (names (map (lambda (entry) (assoc-ref entry "Package"))
                     packages-info)))
    (filter-map (lambda (i name)
                  (let ((url (and=> (bioconductor-name->url name type) first)))
                    (format (current-error-port)
                            "Validating sources of Bioconductor package [~a/~a]: ~a...~%" i (length names) name)
                    (and (not (source-size-too-big? url))
                         (false-if-exception
                          (cached-fetch-description 'bioconductor name))
                         name)))
                (iota (length names) 1)
                names)))

(define all-bioc-packages
  (memoize
   (lambda ()
     (append (bioc-packages)
             (bioc-packages 'annotation)
             (bioc-packages 'experiment)))))

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
      (#f (if (set-contains? all-r-names (symbol->string input))
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

(define all-r-names
  (memoize
   (lambda (type)
     (list->set
      (map cran-guix-name
           (case type
             ((bioc)
              (append (all-cran-packages)
                      (all-bioc-packages)))
             ((cran)
              (all-cran-packages))))))))

(define (disable-tests package)
  (match package
    ((and expr ('package fields ...))
     (if (assoc-ref fields 'arguments)
         ;; Add test item
         `(package ,@(map (lambda (x)
                            (if (eq? (car x) 'arguments)
                                `(,(car x)
                                  (list #:tests? #false
                                        ,@(cdadr x)))
                                x))
                          fields))
         ;; Replace whole field.
         `(package
            ,@(alist-cons-after 'build-system 'arguments
                                '((list #:tests? #false))
                                fields))))
    (x
     (pk 'nope))))

(define (import-package upstream-name type)
  "Import package UPSTREAM-NAME from upstream repository TYPE, fix
inputs and return imports/package definition."
  (format #t "Importing package ~a from ~a…~%"
          upstream-name (case type
                          ((bioc) "Bioconductor")
                          ((cran) "CRAN")))
  (let* ((package-sexp (cran->guix-package upstream-name
                                           ;; XXX: annoying discrepancy
                                           #:repo (case type
                                                    ((bioc) 'bioconductor)
                                                    ((cran) 'cran))
                                           #:license-prefix add-license:-prefix
                                           #:fetch-description cached-fetch-description
                                           #:download-source download-source))
         (_ (unless package-sexp (error (format #false "`~a' is empty" upstream-name))))
         (guix-name (package-sexp->name package-sexp))

         ;; Rewrite inputs, deleting non-existent variables.
         (propagated-inputs (delete-nonexistent-variables
                             (package-sexp->propagated-inputs package-sexp)
                             (all-r-names type)))
         (inputs (delete-nonexistent-variables
                  (package-sexp->inputs package-sexp)
                  (all-r-names type)))
         (native-inputs (delete-nonexistent-variables
                         (package-sexp->native-inputs package-sexp)
                         (all-r-names type)))
         ;; We don't want to build vignettes or run tests because that
         ;; usually requires inputs that are not listed in the
         ;; DESCRIPTION file.
         (fixed-package-sexp (disable-tests
                              (replace-package-sexp-field
                               (replace-package-sexp-field
                                (replace-package-sexp-field
                                 package-sexp
                                 'propagated-inputs propagated-inputs)
                                'inputs inputs)
                               'native-inputs native-inputs)))

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
   (setenv "GUIX_CRAN_IGNORE_VIGNETTE_INPUTS" "t")
   (setenv "GUIX_CRAN_IGNORE_TEST_INPUTS" "t")
   (let ((type* (match type
                  (("bioc") 'bioc)
                  (_ 'cran))))
     (main output-dir channel-name type*)))
  (_ (format #t "Usage: guix repl import.scm <output-dir> <channel-name> [bioc|cran]~%")))

