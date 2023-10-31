;;;; cl-feed2fedi.asd

(asdf:defsystem #:cl-feed2fedi
  :description "Describe cl-feed2fedi here"
  :author "Vee Satayamas <vsatayamas@gmail.com>"
  :license  "AGPL-3.0"
  :version "0.0.1"
  :serial t
  :depends-on (#:drakma #:tooter #:cl-rocksdb #:cl-feedparser)
  :components ((:file "package")
               (:file "cl-feed2fedi")))
