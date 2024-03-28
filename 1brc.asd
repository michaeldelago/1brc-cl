(defsystem "1brc"
  :version "0.1.0"
  :author "Mike Delago"
  :license "MIT"
  :depends-on (:alexandria
                :parse-number
                :serapeum)
  :components ((:module "src"
                        :components
                        ((:file "1brc"))))
  :description "Processing 1 Billion Rows"
  :in-order-to ((test-op (test-op "test"))))
