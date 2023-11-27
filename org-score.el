;; note on type:
;; - s case-insensitive
;; - S case-sensitive

;;; GET DATA FROM FILE

(defvar org-score-input-file "~/org/projects/elfeed-score-org/scores.org"
  "Org file with scores.")

(defvar org-score-input-file-TEST
  "~/org/projects/elfeed-score-org/test-scores.org"
  "Org file with scores.")

(defvar org-score-rule-type-property (intern ":SECTION")
  "Name of the property that identifies the type of rule.
Each type of rule has its own sublist in the score file.")

(defvar org-score-value-property        (intern ":VALUE"))
(defvar org-score-titlevalue-property   (intern ":TITLEVALUE"))
(defvar org-score-contentvalue-property (intern ":CONTENTVALUE"))
(defvar org-score-string-type-property  (intern ":STRINGTYPE"))
(defvar org-score-attr-property         (intern ":ATTR"))
(defvar org-score-tags-property         (intern ":TAGS"))
(defvar org-score-feeds-property        (intern ":FEEDS"))
(defvar org-score-text-property         (intern ":raw-value"))

(defvar org-score-sections-variables
  '(title (:section :text :value :type :feeds)
          feed (:section :text :value :type :tags :attr)
          title-or-content (:section :text :title-value :content-value :type)
          tag (:section :text :value)
          link (:section :text :value :type :feeds))
  "Plist with sections, and related variables to capture.")

(defun org-score-parse-headline (hl)
  "Return `entry' plist with all properties from org headline HL."
  (let* ((section (plist-get (car (cdr hl)) org-score-rule-type-property))
         (expected-vars (org-score-variables-by-section section)))
    (flatten-list
     (apply
      #'list
      (seq-map
       (lambda (var)
         (pcase var
           (:section
            (list var (plist-get (car (cdr hl))
                                 org-score-rule-type-property)))
           (:text
            (list var (plist-get (car (cdr hl)) org-score-text-property)))
           (:value
            (list var (plist-get (car (cdr hl))
                                 org-score-value-property)))
           (:type
            (list var (plist-get (car (cdr hl))
                                 org-score-string-type-property)))
           (:title-value
            (list var (plist-get (car (cdr hl))
                                 org-score-titlevalue-property)))
           (:content-value
            (list var (plist-get (car (cdr hl))
                                 org-score-contentvalue-property)))
           (:attr
            (list var (if-let (val (plist-get (car (cdr hl))
                                 org-score-attr-property)) val "t")))
           (:feeds
            (list var (plist-get (car (cdr hl))
                                 org-score-feeds-property)))
           (:tags
            (list var (if-let (val (plist-get (car (cdr hl))
                                 org-score-tags-property)) val "")))))
       expected-vars)))))

(defun org-score-parse-input (filename)
  "Parse org FILENAME and return list of entries.
Entry is a plist (:section section :text text etc.)"
  (let* ((entries nil)
         (filter
          (lambda (h)
            (if (org-element-property org-score-rule-type-property h) h nil)))
         (ast (with-temp-buffer
                (insert-file-contents filename)
                (org-element-parse-buffer)))
         (headlines (org-element-map ast 'headline filter)))
    (seq-map (lambda (hl) (push (org-score-parse-headline hl) entries))
             headlines)
    (reverse entries)))

(defun org-score-filter-entries (entries section)
  "Given SECTION, return entries of that type from list ENTRIES."
  (seq-filter
   (lambda (entry) (equal (symbol-name section) (plist-get entry :section)))
   entries))

(defun org-score-variables-by-section (section)
  "Get the infos for SECTION."
  (plist-get org-score-sections-variables
             (if (stringp section) (intern section) section)))

(defun org-score-get-value (entry variable)
  "Return value of VARIABLE from ENTRY."
  (plist-get entry variable))

(defun org-score-plist-variables (plist)
  "Return all variables included in PLIST."
  (seq-filter (lambda (x) x)
              (seq-map-indexed (lambda (elt idx)
                                 (if (cl-oddp idx) elt)) plist)))

(defun org-score-plist-keys (plist)
  "Return all keys of PLIST."
  (seq-filter (lambda (x) x)
              (seq-map-indexed (lambda (elt idx)
                                 (if (cl-evenp idx) elt)) plist)))

(defun org-score-check-completeness (entry)
  "Raise error if ENTRY does not include all variables expected for
its section type."
  (let* ((section (org-score-get-value entry :section))
         (expected-variables (org-score-variables-by-section section))
         (entry-variables (org-score-plist-keys entry))
         (diff (seq-difference expected-variables entry-variables)))
    (if diff (error "Missing variable %s from entry: %s" diff entry))))

(defun org-score-format-variable-value (entry var)
  "Given ENTRY and VARIABLE, format the corresponding value to be
inserted in output text. As example, text is quoted, and tags contain
the flag."
  (let ((section (org-score-get-value entry :section))
        (val (org-score-get-value entry var)))
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

(defun org-score-create-line (entry section)
  "Given ENTRY, return formatted string LINE."
  (org-score-check-completeness entry)
  (let* ((section (org-score-get-value entry :section))
         (variables (org-score-variables-by-section section)))
    (concat "  ("
            (apply #'concat
                   (seq-map
                    (lambda (var)
                      (format "%s" (org-score-format-variable-value entry var)))
                    variables))
            ")")))

(defun org-score-create-lines (entries section)
  "Create lines (strings) for score file SECTION.
You can pass all INFOS, and they will be filtered for SECTION."
  (let ((lines nil)
        (entries (org-score-filter-entries entries section)))
    (push (format " (\"%s\"" (symbol-name section)) lines)
    (dolist (entry entries)
      (push (org-score-create-line entry section) lines))
    (push "  )" lines)
    (reverse lines)))

(defun org-score-create-buffer-file (filename)
  "Create temp buffer from FILENAME and write to file."
  (let ((entries (org-score-parse-input filename))
        (sections (org-score-plist-keys org-score-sections-variables)))
    (save-excursion
      (with-output-to-temp-buffer "*Result*"
        (goto-char (point-min))
        (princ ";;; Elfeed score file     -*- lisp -*-\n(\n")
        (dolist (section sections)
          (dolist (line (org-score-create-lines entries section))
            (princ (format "%s\n" line))))
        (princ  " (mark -2500)\n (\"adjust-tags\")\n")
        (princ ")")
        (pop-to-buffer "*Result*")
        (when (file-writable-p org-score-output-file)
          (write-region (point-min)
                        (point-max)
                        org-score-output-file))))))

(defun org-score-run ()
  (interactive)
  (org-score-create-buffer-file org-score-input-file))

(bind-key "C-c C-j" #'org-score-run 'emacs-lisp-mode-map)
;; (org-score-setup)

;;; ORG CAPTURE
(defun org-score-capture-template (type)
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

(defun org-score-setup ()
  "Setup org-capture templates for score entry."
  (let ((group-template '("s" "Score file entry"))
        (entry-template '(("sf" "Score feed" entry
                           (id "9d5d1d1f-6ae6-4610-a4b6-dbcf321104d0")
                           (function (lambda ()
                                       (org-score-capture-template 'feed))))
                          ("st" "Score title" entry
                           (id "9d5d1d1f-6ae6-4610-a4b6-dbcf321104d0")
                           (function (lambda ()
                                       (org-score-capture-template 'title))))
                          ("sc" "Score title-or-content" entry
                           (id "9d5d1d1f-6ae6-4610-a4b6-dbcf321104d0")
                           (function (lambda ()
                                       (org-score-capture-template
                                        'title-or-content))))
                          ("sl" "Score link" entry
                           (id "9d5d1d1f-6ae6-4610-a4b6-dbcf321104d0")
                           (function (lambda ()
                                       (org-score-capture-template 'link))))
                          ("st" "Score tag" entry
                           (id "9d5d1d1f-6ae6-4610-a4b6-dbcf321104d0")
                           (function (lambda ()
                                       (org-score-capture-template 'tag)))))))
    (if (not (member group-template org-capture-templates))
        (setq org-capture-templates
              (append org-capture-templates (list group-template))))
    (if (not (member entry-template org-capture-templates))
        (setq org-capture-templates
              (append org-capture-templates entry-template)))))

;;; OUTPUT DATA
(defvar org-score-output-file "~/org/areas/emacs/elfeed.score"
  "Output file with scores, format elfeed-score.")

;;; TESTS
(ert-deftest tags ()
  (should
   (equal
    (org-score-create-line
     '(:section "title" :text "macOS" :value "-200" :type "S") 'tag)
    "  (:tags (t .  macOS) :value -200)")))

(ert-deftest parse-input-file ()
  (should (equal
           (take 3 (org-score-parse-input org-score-input-file-TEST))
           ((:section "title" :text "macOS" :value "-200" :type "S")
            (:section "title" :text "Germany" :value "+100" :type "s")
            (:section "title" :text "WSJ" :value "+200" :type "S"))))
  (should (equal
           (org-score-parse-input org-score-input-file-TEST)
           '((:text "air show" :title-value "+400" :content-value "+100" :type "s")
             (:section "feed" :text "Il Post" :value "+400" :type "s" :tags "")
             (:text "https://www.ilpost.it/episodes/" :value "-600" :type "S" :feeds)
             (:text "offerte-lavoro" :value "+400" :type "s" :feeds "varesenews")
             (:text "science" :value "+10")
             (:text "sport" :value "-500")
             (:text "emacs" :value "-100")
             (:section "title" :text "WSJ" :value "+200" :type "S")
             (:section "title" :text "Germany" :value "+100" :type "s")
             (:section "title" :text "macOS" :value "-200" :type "S")
             (:section "feed" :text "VareseNews" :value "-600" :type "S" :tags "sport")))))

(ert-deftest entry-complete ()
  (should-not (org-score-check-completeness
               '(:section title :text "example text" :value 100 :type s))))
