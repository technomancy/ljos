(local editor (require :polywell))
(local fmt (require :polywell.lib.fnlfmt))

(local keywords ["def" "defn" "defn-" "defonce"
                 "do" "if" "let" "let*" "var" "fn" "loop" "loop*"
                 "recur" "throw" "try" "catch" "finally" "set!"
                 "new" "monitor-enter" "monitor-exit" "quote"
                 "letfn" "case" "cond" "cond->" "cond->>" "condp"
                 "for" "when" "when-not" "when-first" "when-some"
                 "if-let" "if-not" "if-some" "->" "->>" "as->"
                 "doto" "and" "or" "dosync" "doseq" "dotimes"
                 "dorun" "doall" "ns" "in-ns" "with-open"
                 "binding" "with-redefs" "declare" "true" "false" "nil"])

(set keywords.comment-pattern ";")

{:name "clojure"
 :parent "edit"
 :activate-patterns [".*clj$"]
 :props {:on-change (partial editor.colorize keywords)
         :activate (partial editor.colorize keywords)
         :indentation fmt.indentation}}
