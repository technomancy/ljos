(local editor (require :polywell))
(local lume (require :polywell.lib.lume))

(require :config.line)

(fn jump-to-error []
  (let [(_ point-line) (editor.point)
        line (editor.get-line point-line)
        ;; Pattern matches "<spaces>[string "buffer-name"]:line-number"
        (path line-num) (line:match "%s*%[string \"([^\"]*)\"%]:(%d*)")]
    (when (= (editor.file-type path) :file)
      (editor.open path)
      (editor.go-to (tonumber line-num))
      true)))

(fn print-err [err]
  (editor.print (or (.. "! Compilation error: " err) "Unknown error"))
  (editor.print-prompt)
  (editor.cmd.end-of-buffer))

(fn eval []
  ;; if you're not on the last line, enter just bumps you down.
  (when (not= ((. editor "get-line-number")) ((. editor "get-max-line")))
    (when (not (jump-to-error))
      (editor.cmd.end-of-buffer)))

  (let [input (editor.get-input)]
    (editor.history-push input)
    (editor.cmd.end-of-line)
    (editor.cmd.newline)
    (editor.cmd.no-mark)

    ;; try to compile the input.
    (let [ls (editor.get-prop :loadstring loadstring)]
      (var trace nil)
      (var (chunk err) (ls (.. "return " input))) ; might be an expression?
      (when (and err (not chunk))
        (set (chunk err) (ls input)) ; might be a statement?
        (if (not chunk)
            (print-err err)
            ;; Try running the compiled code in protected mode
            (let [result [(xpcall chunk (fn [e]
                                          (set trace (debug.traceback))
                                          (set err e)))]]
              (if (. result 1)
                  (do ; pretty-print the return values
                    (var (output i) (values (lume.serialize (. result 2)) 3))
                    (while (<= i (# result))
                      (set output (.. output ", " (lume.serialize (. result i))))
                      (set i (+ i 1)))
                    (editor.print output)
                    (editor.print-prompt))
                  (do ; display error and stack trace
                    (editor.print (or (.. "! Evaluation error: " err) "Unknown"))
                    (let [lines (lume.split trace "\n")]
                      (each [i l (pairs lines)]
                        ;; editor infrastructure adds 8 layers of irrelevant gunk
                        (when (< i (- (# lines) 8))
                          (editor.print l))))
                    (editor.print-prompt)))))))))

{:ctrl {:m eval}
 :map {:return eval}
 :name "console"
 :parent "line"}
