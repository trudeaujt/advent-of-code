(asdf:defsystem "advent-of-code"
  :description "Advent of Code Solutions"
  :version "0.1.0"
  :author "Your Name"
  :license "MIT"
  :components ((:module "helpers"
                 :components
                 ((:file "input")))
               (:module "src/2024"
                 :components
                 ((:file "day01")))))