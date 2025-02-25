#!/usr/bin/env bb

(require '[babashka.fs :as fs]
         '[clojure.string :as str]
         '[babashka.process :refer [shell]])

(spit "prompt.txt" 
      (str/join "\n"
        (concat
          [(str "\n=== Directory Structure ===\n\n"
                (:out (shell {:out :string} "tree .")))
           "\n=== readme.md ===\n"
           (slurp "readme.md")
           "\n=== llm.bb ===\n"
           (slurp "llm.bb") ]
          (for [file (->> (fs/glob "." "**/*")
                         (filter fs/regular-file?)
                         (remove #(or (str/starts-with? (str %) ".")
                                    (= (fs/file-name %) "prompt.txt")))
                         (map str)
                         sort)]
            (str "\n=== " file " ===\n"
                 (slurp file))))))
(shell "open" "prompt.txt")
