;; note on type:
;; - s case-insensitive
;; - S case-sensitive

;;; GET DATA FROM FILE

(defvar elfeed-score-org-input-file
  (expand-file-name "~/org/projects/elfeed-score-org/scores.org")
  "Org file with scores.")

(defvar elfeed-score-org-input-file-TEST
  "~/org/projects/elfeed-score-org/test-scores.org"
  "Org file with scores.")

(defvar elfeed-score-org-rule-type-property (intern ":SECTION")
  "Name of the property that identifies the type of rule.
Each type of rule has its own sublist in the score file.")

(defvar elfeed-score-org-value-property        (intern ":VALUE"))
(defvar elfeed-score-org-titlevalue-property   (intern ":TITLEVALUE"))
(defvar elfeed-score-org-contentvalue-property (intern ":CONTENTVALUE"))
(defvar elfeed-score-org-string-type-property  (intern ":STRINGTYPE"))
(defvar elfeed-score-org-attr-property         (intern ":ATTR"))
(defvar elfeed-score-org-tags-property         (intern ":FEEDTAGS"))
(defvar elfeed-score-org-feeds-property        (intern ":FEEDS"))
(defvar elfeed-score-org-text-property         (intern ":raw-value"))

(defvar elfeed-score-org-sections-variables
  '(title (:section :text :value :type :feeds)
          feed (:section :text :value :type :tags :attr)
          title-or-content (:section :text :title-value :content-value :type)
          tag (:section :text :value)
          link (:section :text :value :type :feeds))
  "Plist with sections, and related variables to capture.")

(defun elfeed-score-org-parse-headline (hl)
  "Return `entry' plist with all properties from org headline HL."
  (let* ((section (plist-get (car (cdr hl))
                             elfeed-score-org-rule-type-property))
         (expected-vars (elfeed-score-org-variables-by-section section)))
    (flatten-list
     (apply
      #'list
      (seq-map
       (lambda (var)
         (pcase var
           (:section
            (list var (plist-get (car (cdr hl))
                                 elfeed-score-org-rule-type-property)))
           (:text
            (list var (plist-get (car (cdr hl))
                                 elfeed-score-org-text-property)))
           (:value
            (list var (plist-get (car (cdr hl))
                                 elfeed-score-org-value-property)))
           (:type
            (list var (plist-get (car (cdr hl))
                                 elfeed-score-org-string-type-property)))
           (:title-value
            (list var (plist-get (car (cdr hl))
                                 elfeed-score-org-titlevalue-property)))
           (:content-value
            (list var (plist-get (car (cdr hl))
                                 elfeed-score-org-contentvalue-property)))
           (:attr
            (list var (if-let (val (plist-get (car (cdr hl))
                                              elfeed-score-org-attr-property)) val "t")))
           (:feeds
            (list var (plist-get (car (cdr hl))
                                 elfeed-score-org-feeds-property)))
           (:tags
            (list var (if-let (val (plist-get (car (cdr hl))
                                              elfeed-score-org-tags-property)) val "")))))
       expected-vars)))))

(defun elfeed-score-org-parse-input (filename)
  "Parse org FILENAME and return list of entries.
Entry is a plist (:section section :text text etc.)"
  (let* ((entries nil)
         (filter
          (lambda (h)
            (if (org-element-property
                 elfeed-score-org-rule-type-property h) h nil)))
         (ast (with-temp-buffer
                (insert-file-contents filename)
                (org-element-parse-buffer)))
         (headlines (org-element-map ast 'headline filter)))
    (seq-map (lambda (hl) (push (elfeed-score-org-parse-headline hl) entries))
             headlines)
    (reverse entries)))

(defun elfeed-score-org-filter-entries (entries section)
  "Given SECTION, return entries of that type from list ENTRIES."
  (seq-filter
   (lambda (entry) (equal (symbol-name section) (plist-get entry :section)))
   entries))

(defun elfeed-score-org-variables-by-section (section)
  "Get the infos for SECTION."
  (plist-get elfeed-score-org-sections-variables
             (if (stringp section) (intern section) section)))

(defun elfeed-score-org-get-value (entry variable)
  "Return value of VARIABLE from ENTRY."
  (plist-get entry variable))

(defun elfeed-score-org-plist-variables (plist)
  "Return all variables included in PLIST."
  (seq-filter (lambda (x) x)
              (seq-map-indexed (lambda (elt idx)
                                 (if (cl-oddp idx) elt)) plist)))

(defun elfeed-score-org-plist-keys (plist)
  "Return all keys of PLIST."
  (seq-filter (lambda (x) x)
              (seq-map-indexed (lambda (elt idx)
                                 (if (cl-evenp idx) elt)) plist)))

(defun elfeed-score-org-check-completeness (entry)
  "Raise error if ENTRY does not include all variables expected for
its section type."
  (let* ((section (elfeed-score-org-get-value entry :section))
         (expected-variables (elfeed-score-org-variables-by-section section))
         (entry-variables (elfeed-score-org-plist-keys entry))
         (diff (seq-difference expected-variables entry-variables)))
    (if diff (error "Missing variable %s from entry: %s" diff entry))))

(defun elfeed-score-org-format-variable-value (entry var)
  "Given ENTRY and VARIABLE, format the corresponding value to be
inserted in output text. As example, text is quoted, and tags contain
the flag."
  (let ((section (elfeed-score-org-get-value entry :section))
        (val (elfeed-score-org-get-value entry var)))
    (if (and (string= section "tag") (equal var :text))
        (format ":tags (t . %s) " val)
      (pcase var
        (:section "")
        ((or :text :link)
         (format "%s \"%s\" " var val))
        (:tags
         (if (not (or (not val) (equal val "")))
             (format ":tags (t . %s) " val) ""))
        (:feeds
         (if (not (or (not val) (equal val "")))
             (format " :feeds (t . ((t s \"%s\")))" val) ""))
        ((or :value :title-value :content-value :attr :type)
         (format "%s %s " var val))
        ))))

(defun elfeed-score-org-create-line (entry section)
  "Given ENTRY, return formatted string LINE."
  (elfeed-score-org-check-completeness entry)
  (let* ((section (elfeed-score-org-get-value entry :section))
         (variables (elfeed-score-org-variables-by-section section)))
    (concat "  ("
            (apply #'concat
                   (seq-map
                    (lambda (var)
                      (format "%s" (elfeed-score-org-format-variable-value
                                    entry var)))
                    variables))
            ")")))

(defun elfeed-score-org-create-lines (entries section)
  "Create lines (strings) for score file SECTION.
You can pass all INFOS, and they will be filtered for SECTION."
  (let ((lines nil)
        (entries (elfeed-score-org-filter-entries entries section)))
    (push (format " (\"%s\"" (symbol-name section)) lines)
    (dolist (entry entries)
      (push (elfeed-score-org-create-line entry section) lines))
    (push "  )" lines)
    (reverse lines)))

(defun elfeed-score-org-create-buffer-file (filename)
  "Create temp buffer from FILENAME and write to file."
  (let ((entries (elfeed-score-org-parse-input filename))
        (sections (elfeed-score-org-plist-keys
                   elfeed-score-org-sections-variables)))
    (save-excursion
      (with-output-to-temp-buffer "*Result*"
        (goto-char (point-min))
        (princ ";;; Elfeed score file     -*- lisp -*-\n(\n")
        (dolist (section sections)
          (dolist (line (elfeed-score-org-create-lines entries section))
            (princ (format "%s\n" line))))
        (princ  " (mark -2500)\n (\"adjust-tags\")\n")
        (princ ")")
        (pop-to-buffer "*Result*")
        (when (file-writable-p elfeed-score-org-output-file)
          (let ((backup-name (concat (make-backup-file-name
                                      elfeed-score-org-output-file)
                                     (format-time-string "%Y%m%dT%H%M%S"))))
            (copy-file elfeed-score-org-output-file backup-name))
          (write-region (point-min)
                        (point-max)
                        elfeed-score-org-output-file))))))


(defun elfeed-score-org-hook ()
  "Run elfeed-score-org after saving the score file."
  (when (string= (buffer-file-name) elfeed-score-org-input-file)
    (elfeed-score-org-create-buffer-file elfeed-score-org-input-file)))

(defun elfeed-score-org-run ()
  (interactive)
  (elfeed-score-org-create-buffer-file elfeed-score-org-input-file))

(bind-key "C-c C-j" #'elfeed-score-org-run 'emacs-lisp-mode-map)
;; (elfeed-score-org-setup)

;;; ORG CAPTURE
(defun elfeed-score-org-capture-template (type)
  (concat "%i* %^{text}\n"
          ":PROPERTIES:\n"
          ":SECTION: " (symbol-name type) "\n"
          (if (equal type 'title-or-content)
              (concat ":CONTENTVALUE: %^{content val}\n"
                      ":TITLEVALUE: %^{title val}\n")
            ":VALUE: %^{value}\n")
          ":STRINGTYPE: %^{string type: `s'= insensitive, `S'= sensitive|s}\n"
          ":ENTERED:  %U\n"
          ":END:"))

(defun elfeed-score-org-setup ()
  "Add hook to generate score file when saving the score file,
and setup org-capture templates for score entry."
  (add-hook 'after-save-hook 'elfeed-score-org-hook)
  (let ((group-template '("s" "Score file entry"))
        (entry-template
         '(("sf" "Score feed" entry
            (id "9d5d1d1f-6ae6-4610-a4b6-dbcf321104d0")
            (function (lambda ()
                        (elfeed-score-org-capture-template 'feed))))
           ("st" "Score title" entry
            (id "9d5d1d1f-6ae6-4610-a4b6-dbcf321104d0")
            (function (lambda ()
                        (elfeed-score-org-capture-template 'title))))
           ("sc" "Score title-or-content" entry
            (id "9d5d1d1f-6ae6-4610-a4b6-dbcf321104d0")
            (function (lambda ()
                        (elfeed-score-org-capture-template
                         'title-or-content))))
           ("sl" "Score link" entry
            (id "9d5d1d1f-6ae6-4610-a4b6-dbcf321104d0")
            (function (lambda ()
                        (elfeed-score-org-capture-template 'link))))
           ("st" "Score tag" entry
            (id "9d5d1d1f-6ae6-4610-a4b6-dbcf321104d0")
            (function (lambda ()
                        (elfeed-score-org-capture-template 'tag)))))))
    (if (not (member group-template org-capture-templates))
        (setq org-capture-templates
              (append org-capture-templates (list group-template))))
    (if (not (member entry-template org-capture-templates))
        (setq org-capture-templates
              (append org-capture-templates entry-template)))))

;;; OUTPUT DATA
(defvar elfeed-score-org-output-file "~/org/areas/emacs/elfeed.score"
  "Output file with scores, format elfeed-score.")

;;; TESTS
(ert-deftest tags ()
  (should
   (equal
    (elfeed-score-org-create-line
     '(:section "title" :text "macOS" :value "-200" :type "S" :feeds "") 'tag)
    "  (:text \"macOS\" :value -200 :type S )")))

(ert-deftest parse-input-file ()
  (should (equal
           (elfeed-score-org-parse-input elfeed-score-org-input-file-TEST)
           '((:section "title-or-content" :text "air show" :title-value "+400" :content-value "+100" :type "s")
            (:section "feed" :text "Il Post" :value "+400" :type "s" :tags "" :attr "t")
            (:section "link" :text "https://www.ilpost.it/episodes/" :value "-600" :type "S" :feeds)
            (:section "link" :text "offerte-lavoro" :value "+400" :type "s" :feeds "varesenews")
            (:section "tag" :text "science" :value "+10")
            (:section "tag" :text "sport" :value "-500")
            (:section "tag" :text "emacs" :value "-100")
            (:section "title" :text "WSJ" :value "+200" :type "S" :feeds "")
            (:section "title" :text "Germany" :value "+100" :type "s" :feeds "Il Post")
            (:section "title" :text "macOS" :value "-200" :type "S" :feeds "Hacker News")
            (:section "feed" :text "VareseNews" :value "-600" :type "S" :tags "" :attr "t")))))

(ert-deftest entry-complete ()
  (should-not (elfeed-score-org-check-completeness
               '(:section title :text "example text" :value 100 :type s))))
