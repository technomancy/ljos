(local editor (require :polywell))
(local fennel (require :polywell.lib.fennel))
(local view (require :polywell.lib.fennelview))
(local completion (require :polywell.completion))

(fn completer [input]
  (completion.for (completion.add-prefixes _G input []) input))

(fn activate []
  (let [buf (editor.current-buffer-name)
        out (fn [xs]
              (editor.with-output-to
               buf (partial editor.print (table.concat xs " "))))
        options {:readChunk coroutine.yield
                 :onValues out
                 :onError (fn [kind ...] (out [kind "Error:" ...]))
                 :pp view
                 :env (setmetatable {:print editor.print}
                                    {:__index _G})
                 :moduleName "polywell.lib.fennel"}
        coro (coroutine.create fennel.repl)]
    (editor.set-prompt ">> ")
    (editor.print-prompt)
    (assert (coroutine.resume coro options))
    (editor.set-prop :eval coro)))

(fn handle [input]
  (assert (coroutine.resume (editor.get-prop :eval) (.. input "\n"))))

(local enter (editor.handle-input-with handle))

{:name "repl"
 :parent "line"
 :map {"return" enter
       "tab" editor.cmd.complete}
 :ctrl {"m" enter
        "i" editor.cmd.complete}
 :props {: activate : completer}}
