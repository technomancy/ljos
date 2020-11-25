(local completion (require :polywell.completion))
(local editor (require :polywell))

(local keywords ["and" "break" "do" "else" "elseif" "end" "false"
                 "for" "function" "if" "in" "local" "nil" "not" "or"
                 "repeat" "return" "then" "true" "until" "while"])

(set keywords.comment-pattern "[-][-]")

(fn completer [input]
  (completion.for (completion.add-prefixes _G input []) input))

{:name "lua"
 :parent "edit"
 :activate-patterns [".*lua$"]
 :props {:on-change (partial editor.colorize keywords)
         :activate (partial editor.colorize keywords)
         :completer completer}
 :map {"tab" editor.cmd.complete}
 :alt {"/" editor.cmd.complete}}
