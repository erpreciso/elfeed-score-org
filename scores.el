;;; Elfeed score file     -*- lisp -*-
 ("tag"
  (:tags (t . emacs     ) :value -100)
  (:tags (t . sport     ) :value -500)
  (:tags (t . science   ) :value +10)
  )
 ("title"
  (:text macOS                :value -200 :type S)
  (:text Germany              :value +100 :type s)
  (:text WSJ                  :value +200 :type S)
  )
 ("title-or-content"
  (:text air show             :title-value +400 :content-value +100 :type s)
  )
 ("feed"
  (:text VareseNews           :value -600 :type S :attr t :tags (t . sport))
  (:text Il Post              :value +400 :type s :attr t)
  )
