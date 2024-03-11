
(defvar elfeed-score-org--test-file
  (expand-file-name "~/org/temp/test-scores.org")
  "Test file for ert-tests")

(defun elfeed-score-org--fixture-test (body)
  "Initialize temporary file for testing."
  (load-file "~/org/projects/elfeed-score-org/elfeed-score-org.el")
  (unwind-protect
      (progn 
        (with-temp-file elfeed-score-org--test-file
          (insert "
* scores
** air show
:PROPERTIES:
:SECTION:  title-or-content
:FEEDS:
:TITLEVALUE:     +400
:CONTENTVALUE:   +100
:STRINGTYPE:     s
:END:
** Il Post
:PROPERTIES:
:SECTION:  feed
:TAGS:
:VALUE:    +400
:STRINGTYPE:     s
:ATTR:     t
:END:
** https://www.ilpost.it/episodes/
:PROPERTIES:
:SECTION:  link
:VALUE:    -600
:STRINGTYPE:     S
:ATTR:     t
:END:
** offerte-lavoro
:PROPERTIES:
:SECTION:  link
:VALUE:    +400
:STRINGTYPE:     s
:ATTR:     t
:FEEDS:    varesenews
:END:
** science
:PROPERTIES:
:VALUE:    +10
:SECTION:  tag
:END:
** sport
:PROPERTIES:
:SECTION:  tag
:VALUE:    -500
:END:
** emacs
:PROPERTIES:
:SECTION:  tag
:VALUE:    -100
:END:
** WSJ
:PROPERTIES:
:FEEDS:
:SECTION:  title
:VALUE:    +200
:STRINGTYPE:     S
:END:
** Germany
:PROPERTIES:
:FEEDS:  Il Post
:SECTION:  title
:VALUE:    +100
:STRINGTYPE:     s
:END:
** macOS
:PROPERTIES:
:SECTION:  title
:FEEDS:     Hacker News
:VALUE:    -200
:STRINGTYPE:     S
:END:
** VareseNews
:PROPERTIES:
:SECTION:  feed
:TAGS:     sport
:VALUE:    -600
:STRINGTYPE:     S
:ATTR:     t
:END: "))
        (funcall body))
    (delete-file elfeed-score-org--test-file)))



(ert-deftest parse-input-file ()
  (elfeed-score-org--fixture-test
   (lambda ()
     (should (equal
              (elfeed-score-org-parse-input elfeed-score-org--test-file)
              '((:section "title-or-content" :text "air show"
                          :title-value "+400" :content-value "+100"
                          :type "s" :tags "")
                (:section "feed" :text "Il Post" :value "+400" :type "s"
                          :tags "" :attr "t")
                (:section "link" :text "https://www.ilpost.it/episodes/"
                          :value "-600" :type "S" :feeds)
                (:section "link" :text "offerte-lavoro" :value "+400"
                          :type "s" :feeds "varesenews")
                (:section "tag" :text "science" :value "+10")
                (:section "tag" :text "sport" :value "-500")
                (:section "tag" :text "emacs" :value "-100")
                (:section "title" :text "WSJ" :value "+200" :type "S" :feeds "")
                (:section "title" :text "Germany" :value "+100" :type "s"
                          :feeds "Il Post")
                (:section "title" :text "macOS" :value "-200" :type "S"
                          :feeds "Hacker News")
                (:section "feed" :text "VareseNews" :value "-600" :type "S"
                          :tags "" :attr "t")))))))

(ert-deftest tags ()
  (should
   (equal
    (elfeed-score-org-create-line
     '(:section "title" :text "macOS" :value "-200" :type "S" :feeds "") 'tag)
    "  (:text \"macOS\" :value -200 :type S )")))

(ert-deftest entry-complete ()
  (should-not (elfeed-score-org-check-completeness
               '(:section title :text "example text" :value 100 :type s))))
