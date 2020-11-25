(local editor (require :polywell))
(local fennel (require :polywell.lib.fennel))
(local fmt (require :polywell.lib.fnlfmt))
(local completion (require :polywell.completion))

(local keywords ["let" "set" "tset" "fn" "lambda" "λ" "require" "if" "when"
                 "do" "block" "true" "false" "nil" "values" "pack" "for" "each"
                 "local" "partial" "and" "or" "not" "special" "macro"])

(set keywords.comment-pattern ";")

(fn paren []
  (if (editor.mod-down? :shift)
      (editor.insert ")")
      (do (editor.insert "9")
          (editor.cmd.forward-char))))

(fn bracket []
  (if (editor.mod-down? :shift)
      (editor.insert "}")
      (do (editor.insert "[]")
          (editor.cmd.forward-char))))

(fn quot []
  (if (editor.mod-down? :shift)
      (editor.insert "\"")
      (do (editor.insert "'")
          (editor.cmd.forward-char))))

(fn jump-to-definition [input]
  (if (editor.mod-down? :shift)
      (editor.cmd.end-of-buffer)
      (let [;; is there a way to look this up without eval? ___replLocals___?
            tgt (fennel.eval input)
            {: what : source : linedefined} (and (= (type tgt) :function)
                                                 (debug.getinfo tgt))]
        (if (= :Lua what)
            (do (editor.open (source:sub 2))
                (editor.go-to linedefined))
            (editor.echo (.. "Not found: " input))))))

(fn completer [input]
  (completion.for (completion.add-prefixes _G input []) input))

{:name "fennel"
 :parent "edit"
 :activate-patterns [".*fnl$"]
 :map {"9" paren
       "[" bracket
       "'" quot
       "tab" editor.cmd.complete}
 :alt {"." #(editor.read-line "Function: " jump-to-definition)}
 :props {:on-change (partial editor.colorize keywords)
         :activate (partial editor.colorize keywords)
         :indentation fmt.indentation
         : completer}}
