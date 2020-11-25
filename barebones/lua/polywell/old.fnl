(local utf8 (require "polywell.lib.utf8"))
(local lume (require "polywell.lib.lume"))
(local colorize (require "polywell.colorize"))
(local completion (require "polywell.completion"))
(local frontend (require "polywell.frontend"))
(local state (require "polywell.state"))

(local dprint (or (and (os.getenv "DEBUG") print) (fn [])))

;; some constants
(local kill-ring-max 32)
(local mark-ring-max 32)

(fn modeline [b]
  (utf8.format " %s  %s  (%s/%s)  %s"
               (if b.needs-save? "*" "-")
               b.path b.point-line (# b.lines) b.mode))

 ;; This is the structure of a buffer table. However, the fields it contains
 ;; should be considered an internal implementation detail of the editor.
 ;; We expose functions to make changes to a buffer, but we don't let user code
 ;; make any changes directly; otherwise we will not be able to change the
 ;; structure of the table without breaking user code.
(fn make-buffer [path lines props]
  {;; dirty is a per-cycle change flag (for undo tracking)
   :dirty false
   :history []
   ;; repl-style modes need input history tracking and a prompt
   :input-history []
   :input-history-max 64
   :input-history-pos 0
   :last-yank nil
   :lines lines
   :mark nil
   :mark-line nil
   :mark-ring []
   :mode "edit"
   :modeline modeline
   ;; needs-save? is an overall change flag.
   :needs-save? false
   :path path
   :point 0
   :point-line 1
   :prompt nil
   ;; arbitrary key/value storage
   :props (or props [])
   :undo-at 0})

(fn behind-minibuffer []
  (. state.windows state.window 2))

(fn get-current-mode [buffer]
  (let [buffer (or buffer state.b)]
    (. state.modes (and buffer buffer.mode))))

(fn set-prop [prop value]
  (tset state.b.props prop value)
  value)

(fn get-prop [prop default buffer mode]
  (let [buffer (or buffer state.b)
        mode (or mode (get-current-mode buffer))]
    (if (and buffer (. buffer.props prop))
        (and buffer (. buffer.props prop))
        (and mode.props (not= (. mode.props prop) nil))
        (. mode.props prop)
        mode.parent
        (get-prop prop default buffer
                  (assert (. state.modes mode.parent)
                          (.. "no mode " mode.parent)))
        default)))

(fn get-main-prop [prop]
  (if (= state.b.path "minibuffer")
      (get-prop prop nil (behind-minibuffer))
      (get-prop prop)))

(fn vary-prop [prop f ...]
  (set-prop prop (f (get-prop prop) ...)))

(fn get-mode-prop [mode-name prop]
  (let [mode (assert (. state.modes mode-name) mode-name)]
    (if mode.props
        (. mode.props prop)
        mode.parent
        (get-mode-prop mode.parent prop))))

(fn run-on-change []
  (let [on-change (get-prop "on-change")]
    (if (= (type on-change) :function)
        (on-change)
        (= (type on-change) :table)
        (each [_ f (pairs on-change)]
          (f)))))

(fn dbg [arg]
  (print "---------------"
         state.b.path state.b.point-line state.b.point
         state.b.mark-line state.b.mark)
  (each [_ line (ipairs state.b.lines)]
    (print line))
  (print "---------------"))

(fn out-of-bounds? [line point]
  (not (and (. state.b.lines line)
            (>= point 0)
            (<= point (# (. state.b.lines line))))))

(fn bounds-check []
  ;; The first one is a normal occurance; the second two should never happen,
  ;; but let's be forgiving instead of asserting.
  (if (> state.b.point-line (# state.b.lines))
      (set state.b.point-line (# state.b.lines))
      (and (# (. state.b.lines state.b.point-line))
           (> state.b.point (# (. state.b.lines state.b.point-line))))
      (set state.b.point (# (. state.b.lines state.b.point-line)))
      (and state.b.mark-line
           state.b.mark
           (out-of-bounds? state.b.mark-line state.b.mark))
      (do
        (dprint "Mark out of bounds!" state.b.mark-line (# state.b.lines))
        (dbg)
        (set (state.b.mark state.b.mark-line) nil)))
  (when (out-of-bounds? state.b.point-line state.b.point)
    (dprint "Point out of bounds!" state.b.point-line state.b.point
            (# state.b.lines) (and (. state.b.lines state.b.point-line)
                                   (# (. state.b.lines state.b.point-line))))
    (dbg)
    (set (state.b.point state.b.point-line) (values 0 1))))

(fn get-buffer [path]
  (lume.match state.buffers #(= $.path path)))

(fn with-current-buffer [nb f ...]
  (let [nb (if (= (type nb) "string")
               (get-buffer nb)
               nb)
        old-b state.b]
    (set state.b nb)
    (let [val (f ...)]
      (set state.b old-b)
      val)))

;; TODO: continue here

(fn region []
  (set state.b.mark (math.min (utf8.len (. state.b.lines state.b.mark-line)) state.b.mark))
  (if (= state.b.point-line state.b.mark-line)
      (do
        (local (start finish) (values (math.min state.b.point state.b.mark) (math.max state.b.point state.b.mark)))
        (local r [(utf8.sub (. state.b.lines state.b.point-line) (+ start 1) finish)])
        (values r state.b.point-line start state.b.point-line finish))
      (or (= state.b.mark nil) (= state.b.mark-line nil))
      (values [] state.b.point-line state.b.point state.b.point-line state.b.point)
      (do
        (var (start-line start finish-line finish) nil)
        (if (< state.b.point-line state.b.mark-line)
            (set (start-line start finish-line finish) (values state.b.point-line state.b.point state.b.mark-line state.b.mark))
            (set (start-line start finish-line finish) (values state.b.mark-line state.b.mark state.b.point-line state.b.point)))
        (local r [(utf8.sub (. state.b.lines start-line) (+ start 1) (- 1))])
        (for [i (+ start-line 1) (- finish-line 1) 1]
          (table.insert r (. state.b.lines i)))
        (table.insert r (utf8.sub (. state.b.lines finish-line) 0 finish))
        (values r start-line start finish-line finish))))

(fn in-prompt [line point line2]
  (when (or (. state "printing-prompt") (not state.b.prompt))
    (lua "return false"))
  (when (and (= (or line2 line) line) (not= line (# state.b.lines)))
    (lua "return false"))
  (when (and (= line (# state.b.lines)) (>= point (utf8.len state.b.prompt)))
    (lua "return false"))
  true)

(fn edit-disallowed [line point line2 point2]
  (when (. state "inhibit-read-only")
    (lua "return false"))
  (local ro (get-prop "read-only" (in-prompt line point line2 point2)))
  (if (= (type ro) "function")
      (ro line point line2 point2)
      ro))

(fn echo [...]
  "This is for feedback within the editor where print wouldn't make sense.
Shows in the minibuffer."
  (tset state "echo-message" (table.concat [...] " "))
  (tset state "echo-message-new" true))

(fn insert [text point-to-end]
  (when (in-prompt state.b.point-line state.b.point)
    (set state.b.point (# state.b.prompt)))
  (when (edit-disallowed state.b.point-line state.b.point)
    (let [___antifnl_rtn_1___ (echo "Read-only.")]
      (lua "return ___antifnl_rtn_1___")))
  (when (out-of-bounds? state.b.point-line state.b.point)
    (dprint "Inserting out of bounds!" state.b.point-line state.b.point)
    (lua "return "))
  (when (= (type text) "string")
    (set-forcibly! text (lume.split text "\n")))
  (set (state.b.dirty state.b.needs-save?) (values true true))
  (set-forcibly! text (lume.map text (fn [s] (utf8.gsub s "\t" "  "))))
  (when (or (not text) (= (# text) 0))
    (lua "return "))
  (local this-line (or (. state.b.lines state.b.point-line) ""))
  (local before (utf8.sub this-line 0 state.b.point))
  (local after (utf8.sub this-line (+ state.b.point 1)))
  (local first-line (. text 1))
  (if (= (# text) 1)
      (do
        (tset state.b.lines state.b.point-line (.. (or before "") (or first-line "") (or after "")))
        (when point-to-end
          (set state.b.point (+ (utf8.len before) (utf8.len first-line)))))
      (do
        (tset state.b.lines state.b.point-line (.. (or before "") (or first-line "")))
        (each [i l (ipairs text)]
          (when (and (> i 1) (< i (# text)))
            (table.insert state.b.lines (- (+ i state.b.point-line) 1) l)))
        (table.insert state.b.lines (- (+ state.b.point-line (# text)) 1) (.. (. text (# text)) (or after "")))
        (when point-to-end
          (set state.b.point (# (. text (# text))))
          (set state.b.point-line (- (+ state.b.point-line (# text)) 1))))))

(fn delete [start-line start finish-line finish]
  (set-forcibly! start-line (math.min start-line finish-line))
  (set-forcibly! finish-line (math.max start-line finish-line))
  (when (= start-line finish-line)
    (set-forcibly! (start finish) (values (math.min start finish) (math.max start finish))))
  (when (edit-disallowed start-line start finish-line finish)
    (lua "return "))
  (when (or (out-of-bounds? start-line start) (out-of-bounds? finish-line finish))
    (dprint "Deleting out of bounds!")
    (lua "return "))
  (set (state.b.dirty state.b.needs-save?) (values true true))
  (if (= start-line finish-line)
      (do
        (local line (. state.b.lines start-line))
        (tset state.b.lines start-line (.. (utf8.sub line 0 start) (utf8.sub line (+ finish 1))))
        (if (and (= state.b.point-line start-line) (<= start state.b.point))
            (set state.b.point start)
            (and (= state.b.point-line start-line) (<= state.b.point finish))
            (set state.b.point (- state.b.point (- finish start)))))
      (do
        (local after (utf8.sub (. state.b.lines finish-line) (+ finish 1) (- 1)))
        (for [i finish-line (+ start-line 1) (- 1)]
          (table.remove state.b.lines i))
        (tset state.b.lines start-line (.. (utf8.sub (. state.b.lines start-line) 0 start) after))
        (if (and (> state.b.point-line start-line) (<= state.b.point-line finish-line))
            (set (state.b.point state.b.point-line) (values start start-line))
            (> state.b.point-line finish-line)
            (set state.b.point-line (- state.b.point-line (- finish-line start-line)))))))

(fn push [ring item max]
  (table.insert ring item)
  (when (> (# ring) max)
    (table.remove ring 1)))

(fn yank []
  (local text (. (. state "kill-ring") (# (. state "kill-ring"))))
  (when text
    (set state.b.last-yank [state.b.point-line state.b.point (- (+ state.b.point-line (# text)) 1) (utf8.len (. text (# text)))])
    (insert text true)))

(fn system-yank []
  ;; don't crash in headless mode
  (match (frontend.get-clipboard)
    text (insert (lume.split text "\n") true)))

(fn is-beginning-of-buffer []
  (bounds-check)
  (and (= state.b.point 0) (= state.b.point-line 1)))

(fn is-end-of-buffer []
  (bounds-check)
  (and (= state.b.point (# (. state.b.lines state.b.point-line))) (= state.b.point-line (# state.b.lines))))

(fn forward-char [n]
  (let [n (or n 1)] ; bleh; values of n other than 1 or -1 won't work here
    (if (or (and (is-end-of-buffer) (> n 0)) (and (is-beginning-of-buffer) (< n 0)))
        nil
        (and (>= state.b.point (# (. state.b.lines state.b.point-line))) (> n 0))
        (set (state.b.point state.b.point-line) (values 0 (+ state.b.point-line 1)))
        (and (<= state.b.point 0) (< n 0))
        (do
          (set state.b.point (# (. state.b.lines (- state.b.point-line 1))))
          (set state.b.point-line (- state.b.point-line 1)))
        (set state.b.point (+ state.b.point n)))))

(fn point-over []
  (or (utf8.sub (. state.b.lines state.b.point-line) (+ state.b.point 1) (+ state.b.point 1)) " "))

;; state of very limited scope; OK to keep inline
(var (moved-last-point moved-last-line) nil)

(fn point-moved []
  (local (lp ll) (values moved-last-point moved-last-line))
  (set-forcibly! (moved-last-point moved-last-line) (values state.b.point state.b.point-line))
  (not (and (= lp state.b.point) (= ll state.b.point-line))))

(local word-break "[%s%p]")

(fn forward-word [n]
  (set (moved-last-point moved-last-line) (values nil nil))
  (when (utf8.find (point-over) word-break)
    (while (and (point-moved) (utf8.find (point-over) word-break))
      (forward-char n)))
  (forward-char n)
  (while (and (point-moved) (not (utf8.find (point-over) word-break)))
    (forward-char n)))

(fn backward-word []
  (forward-char (- 1))
  (forward-word (- 1))
  (forward-char))

(fn newline [n]
  (local t [""])
  (for [_ 1 (or n 1) 1]
    (table.insert t ""))
  (insert t true))

(fn save-excursion [f]
  (local (old-b p pl) (values state.b state.b.point state.b.point-line))
  (local (m ml) (values state.b.mark state.b.mark-line))
  (local (val err) (pcall f))
  (set state.b old-b)
  (when state.b
    (set (state.b.point state.b.point-line state.b.mark state.b.mark-line)
         (values p pl m ml)))
  (when (not val)
    (error err))
  val)

(fn write [...]
  "Write to the current point in the current buffer."
  (local lines (lume.split (table.concat [...] " ") "\n"))
  (local read-only (. state "inhibit-read-only"))
  (tset state "inhibit-read-only" true)
  (insert lines true)
  (tset state "inhibit-read-only" read-only)
  (values (lume.last lines) (# lines)))

(fn io-write [...]
  "Write to the end of the output buffer right before the prompt."
  (local prev-b state.b)
  (set state.b (assert state.output "no output!"))
  (local (old-point old-point-line old-lines) (values state.b.point state.b.point-line (# state.b.lines)))
  (if (> (# state.b.lines) 1)
      (set (state.b.point state.b.point-line)
           (values (# (. state.b.lines (- (# state.b.lines) 1)))
                   (- (# state.b.lines) 1)))
      (set (state.b.point state.b.point-line) (values 0 1)))
  (local (_ line-count) (write ...))
  (when (= old-point-line old-lines)
    (set (state.b.point state.b.point-line)
         (values old-point (# state.b.lines))))
  (set state.b prev-b)
  (when (= state.b state.output)
    (set state.b.point-line (- (+ old-point-line line-count) 1))))

(fn the-print [...]
  (local (texts read-only) (values [...] (. state "inhibit-read-only")))
  (when (= (. texts 1) nil)
    (lua "return "))
  (tset state "inhibit-read-only" true)
  (tset texts 1 (.. "\n" (. texts 1)))
  (io-write (unpack (lume.map texts tostring)))
  (tset state "inhibit-read-only" read-only))

(fn doubleprint [...]
  (print ...)
  (the-print ...))

(fn with-traceback [f ...]
  (local args [...])
  (xpcall (fn [] (f (unpack args))) (fn [e] (echo e)
                                      (doubleprint e)
                                      (doubleprint (debug.traceback)))))

(fn get-input [tb]
  (set-forcibly! tb (or tb state.b))
  (assert tb.prompt "Buffer does not have a prompt.")
  (utf8.sub (. tb.lines (# tb.lines)) (+ (# tb.prompt) 1)))

(var exit-minibuffer nil) ; mutual recursion

(fn reset-minibuffer-keys [exit]
  (set state.modes.minibuffer.ctrl {:g (partial exit true) :m exit-minibuffer})
  (set state.modes.minibuffer.alt [])
  (set state.modes.minibuffer.ctrl-alt []))

(fn exit-mb [cancel]
  (let [input (get-input)
        callback state.b.callback
        completer (and state.b.props state.b.props.completer)]
    (reset-minibuffer-keys exit-minibuffer)
    (if (and completer (not cancel))
        (let [target (. (completer input) 1)]
          (if (= (utf8.sub (or target "") (- 1) (- 1)) "/")
              (do
                (tset state.b.lines (# state.b.lines) (.. state.b.prompt target))
                (set state.b.point (# (. state.b.lines (# state.b.lines)))))
              (do
                (set state.b (behind-minibuffer))
                (when (or (not cancel) state.b.props.cancelable?)
                  (callback (or target input) cancel)))))
        (do
          (set state.b (behind-minibuffer))
          (when (or (not cancel) (. state.b.props "cancelable?"))
            (callback input cancel))))))

(set exit-minibuffer exit-mb)

(fn delete-backwards []
  (when (not (is-beginning-of-buffer))
    (let [line state.b.point-line
          point state.b.point]
      (var (line2 point2) nil)
      (save-excursion (fn []
                        (forward-char (- 1))
                        (set line2 state.b.point-line)
                        (set point2 state.b.point)))
      (delete line2 point2 line point))))

(fn complete []
  (when state.b.props.completer
    (local completions (state.b.props.completer (get-input)))
    (if (and completions (= (# completions) 1))
        (do
          (tset state.b.lines (# state.b.lines) (.. state.b.prompt (. completions 1)))
          (set state.b.point (# (. state.b.lines (# state.b.lines)))))
        (do
          (local common ((. completion "longest-common-prefix") completions))
          (tset state.b.lines (# state.b.lines) (.. state.b.prompt (or common "")))
          (set state.b.point (# (. state.b.lines (# state.b.lines))))))))

(set state.modes.minibuffer {:map {:backspace delete-backwards
                                   :escape (lume.fn exit-minibuffer true)
                                   :return exit-minibuffer
                                   :tab complete}
                             :name "minibuffer"})

;; This uses a callback; you probably want read-input.
(fn read-line [prompt callback props]
  (let [props (or props [])]
    (when (or (not state.b) (not= state.b.path "minibuffer"))
      (tset props :no-file true)
      (set state.b (make-buffer "minibuffer" [prompt] props)))
    (lume.extend state.b {:mode "minibuffer" :point (# prompt) :block-input? true
                          : prompt : callback})
    (if props.completer
        (let [(last-input completions) (values nil [])]
          (fn state.b.render [mini]
            (local input (get-input mini))
            (when (not= input last-input)
              (set-forcibly! completions (props.completer input))
              (set-forcibly! last-input input))
            (.. (. mini.lines 1) " " (table.concat completions " | "))))
        (set state.b.render (fn [mini] (. mini.lines 1))))
    (when (. props "initial-input")
      (insert (. props "initial-input"))
      (set state.b.point (# (. state.b.lines state.b.point-line))))
    (each [map-name keys (pairs (or props.bind []))]
      (local map (. state.modes.minibuffer map-name))
      (each [key f (pairs keys)]
        (tset map key f)))))

;; good version which runs in a coroutine instead of using callbacks
(fn read-input [prompt props]
  (assert (coroutine.running) "Must call read-input from a coroutine.")
  (read-line prompt (lume.fn coroutine.resume (coroutine.running)) props)
  (coroutine.yield))

(fn activate-mode [mode-name ...]
  (when (not state.b)
    (lua "return "))
  (assert (. state.modes mode-name) (.. mode-name " mode does not exist."))
  (local current-mode (get-current-mode))
  (local new-mode (. state.modes mode-name))
  (when (and current-mode (get-prop "deactivate"))
    ((get-prop "deactivate")))
  (when new-mode.props.cursor
    (frontend.set-cursor new-mode.props.cursor))
  (set state.b.mode mode-name)
  (local read-only (. state "inhibit-read-only"))
  (tset state "inhibit-read-only" true)
  (when (get-prop "activate")
    ((get-prop "activate") ...))
  (tset state "inhibit-read-only" read-only))

(fn auto-activate-mode [path]
  (each [pat mode (pairs (. state "activate-patterns"))]
    (when (: path "find" pat)
      (activate-mode mode)
      (lua "return true"))))

(fn change-buffer [path create-if-missing]
  (var new (get-buffer path))
  (when (and (not new) create-if-missing)
    (set new (make-buffer path)))
  (local old state.b)
  (set state.b (assert new (.. "Buffer not found: " path)))
  (tset (. state.windows state.window) 2 state.b)
  (tset state "last-buffer" old))

(fn relativize-path [path]
  (if (= (: path "sub" 1 1) "/")
      (: (: path "gsub" state.cwd "") "gsub" "^/" "")
      path))

(fn open [path mode no-file props]
  (set-forcibly! path (relativize-path path))
  (tset state "last-buffer" state.b)
  (set state.b (get-buffer path))
  (if state.b
      (when mode
        (activate-mode mode))
      (do
        (if no-file
            (do
              (set state.b (make-buffer path [""] {:no-file no-file}))
              (table.insert state.buffers state.b))
            (= (state.fs.type path) nil)
            (do
              (fn make-file [input]
                (when (or (: (: input "lower") "match" "^y") (= input ""))
                  (state.fs.write path "")
                  (open path mode no-file props)))
              (set state.b (. state "last-buffer"))
              (let [---antifnl-rtn-1--- (read-line "File does not exist; create? [Y/n] " make-file)]
                (lua "return ---antifnl-rtn-1---")))
            (and (not= (state.fs.type path) "file") (not no-file))
            (echo "Tried to open a directory or something.")
            (do
              (local lines (lume.split (state.fs.read path) "\n"))
              (set state.b (make-buffer path lines []))
              (table.insert state.buffers state.b)))
        (each [k v (pairs (or props []))]
          (set-prop k v))
        (local parts (lume.split (. state.b.lines 1) "-*-"))
        (local auto-mode (or mode (and (. parts 2) (lume.trim (. parts 2)))))
        (if auto-mode
            (activate-mode auto-mode)
            (not (auto-activate-mode path))
            (activate-mode "edit"))))
  (change-buffer path)
  (tset (. state.windows state.window) 2 state.b))

{:activate-mode activate-mode
 :beginning-of-buffer? is-beginning-of-buffer
 :buffer-names (fn [] (lume.map state.buffers (fn [bu] bu.path)))
 :change-buffer change-buffer ;; caller is responsible for setting last-buffer

 ;; all end-user commands to be bound to a keystroke or invokable should
 ;; be in this table, but they're getting ported to commands.fnl
 :cmd {:backward-char (lume.fn forward-char (- 1))
       :backward-kill-word (fn []
                             (let [original-point-line state.b.point-line
                                   original-point state.b.point]
                               (backward-word)
                               (delete state.b.point-line state.b.point
                                       original-point-line original-point)))
       :backward-word backward-word
       :beginning-of-buffer (fn [] (set (state.b.point state.b.point-line)
                                        (values 0 1))
                              (values state.b.point state.b.point-line))
       :beginning-of-input (fn [] (if (and (= state.b.point-line (# state.b.lines)) state.b.prompt)
                                      (set state.b.point (# state.b.prompt))
                                      (set state.b.point 0)))
       :beginning-of-line (fn [] (set state.b.point 0))
       :delete-backwards delete-backwards
       :delete-forwards (fn [n] (when (is-end-of-buffer)
                                  (lua "return "))
                          (local (line point) (values state.b.point-line state.b.point))
                          (local (line2 point2) nil)
                          (save-excursion (fn [] (forward-char n)
                                            (set-forcibly! (line2 point2) (values state.b.point-line state.b.point))))
                          (delete line point line2 point2))
       :end-of-buffer (fn [] (set (state.b.point state.b.point-line)
                                  (values (# (. state.b.lines (# state.b.lines)))
                                          (# state.b.lines)))
                        (values state.b.point state.b.point-line))
       :end-of-line (fn [] (set state.b.point (# (. state.b.lines state.b.point-line))))
       :exit-minibuffer exit-minibuffer
       :forward-char forward-char
       :forward-kill-word (fn [] (local (original-point-line original-point) (values state.b.point-line state.b.point))
                            (forward-word)
                            (delete original-point-line original-point state.b.point-line state.b.point))
       :forward-word forward-word
       :jump-to-mark (fn [] (set (state.b.point state.b.point-line)
                                 (values (or state.b.mark state.b.point)
                                         (or state.b.mark-line state.b.point-line)))
                       (when (> (# state.b.mark-ring) 0)
                         (table.insert state.b.mark-ring 1 (table.remove state.b.mark-ring))
                         (set (state.b.mark state.b.mark-line)
                              (unpack (. state.b.mark-ring 1)))))
       :kill-line (fn [] (local remainder (utf8.sub (. state.b.lines state.b.point-line) (+ state.b.point 1)))
                    (if (utf8.find remainder "[^%s]")
                        (do
                          (save-excursion (fn []
                                            (set (state.b.mark state.b.mark-line)
                                                 (values state.b.point state.b.point-line))
                                            (set state.b.point (# (. state.b.lines state.b.point-line)))
                                            (push (. state "kill-ring") (region) kill-ring-max)))
                          (delete state.b.point-line state.b.point state.b.point-line (# (. state.b.lines state.b.point-line))))
                        (< state.b.point-line (# state.b.lines))
                        (delete state.b.point-line state.b.point (+ state.b.point-line 1) 0)))
       :kill-region (fn [] (when (or (= state.b.mark nil) (= state.b.mark-line nil))
                             (lua "return "))
                      (local (_ start-line start finish-line finish) (region))
                      (push (. state "kill-ring") (region) kill-ring-max)
                      (delete start-line start finish-line finish))
       :kill-ring-save (fn [] (when (or (= state.b.mark nil) (= state.b.mark-line nil))
                                (lua "return "))
                         (push (. state "kill-ring") (region) kill-ring-max))
       :mark (fn [] (push state.b.mark-ring [state.b.point state.b.point-line] mark-ring-max)
               (set (state.b.mark state.b.mark-line)
                    (values state.b.point state.b.point-line)))
       :newline newline
       :next-line (fn [] (if (< state.b.point-line (# state.b.lines))
                             (set state.b.point-line (+ state.b.point-line 1))
                             (set state.b.point (# (. state.b.lines state.b.point-line)))))
       :no-mark (fn [] (set (state.b.mark state.b.mark-line state.active-prefix)
                            nil))
       :open-in-split (fn [...]
                        (local (current-b w h) (values state.b (frontend.get-wh)))
                        (open ...)
                        (set state.window 1)
                        (set state.windows [[[10 10 (- (/ w 2) 10) h] current-b]
                                            [[(+ (/ w 2) 10) 10 (/ w 2) h] state.b]]))
       :prev-line (fn [] (if (> state.b.point-line 1)
                             (set state.b.point-line (- state.b.point-line 1))
                             (set state.b.point 0)))
       :quit (fn []
               ((. (. (require "polywell") "cmd") "save"))
               (frontend.quit))
       :system-copy-region (fn [] (when (or (= state.b.mark nil) (= state.b.mark-line nil))
                                    (lua "return "))
                             (frontend.set-clipboard (table.concat (region) "\n")))
       :system-yank system-yank
       :word-wrap (fn [] (while (and (not= (. state.b.lines (- state.b.point-line 1)) "") (> state.b.point-line 1))
                           (set state.b.point-line (- state.b.point-line 1)))
                    (local column (get-prop "wrap-column" 78))
                    (fn join []
                      (set (state.b.point state.b.point-line)
                           (values 0 (+ state.b.point-line 1)))
                      (delete-backwards)
                      (when (not (utf8.find (point-over) word-break))
                        (insert [" "] true)))
                    (fn has-room []
                      (save-excursion (fn [] (local room (- column (# (. state.b.lines state.b.point-line))))
                                        (set (state.b.point state.b.point-line)
                                             (values 0 (+ state.b.point-line 1)))
                                        (forward-word)
                                        (< state.b.point room))))
                    (while (or (and (not= (. state.b.lines (+ state.b.point-line 1)) "") (< state.b.point-line (# state.b.lines))) (> (# (. state.b.lines state.b.point-line)) column))
                      (if (and (< (# (. state.b.lines state.b.point-line)) column) (has-room))
                          (join)
                          (> (# (. state.b.lines state.b.point-line)) column)
                          (do
                            (set state.b.point column)
                            (when (not (utf8.find (point-over) word-break))
                              (backward-word))
                            (newline)
                            (forward-char)
                            (delete-backwards))
                          (set state.b.point-line (+ state.b.point-line 1)))))
       :yank yank
       :yank-pop (fn []
                   (when state.b.last-yank
                     (table.insert (. state "kill-ring") 1
                                   (table.remove (. state "kill-ring")))
                     (delete (unpack state.b.last-yank))
                     (yank)))}
 :colorize (fn [keywords]
             (let [colors (or (. state.colors state.b.mode) state.colors)
                   color (get-prop "colorize" colorize)]
               (set state.b.colored-lines (color keywords colors state.b.lines))))
 :current-buffer-name (fn [] state.b.path)
 :current-mode-name (fn [] state.b.mode)
 :current-split (fn [] state.window)
 :debug dbg
 :delete delete
 :draw (fn []
         (fn actual-draw []
           (when (get-main-prop "under-draw")
             ((get-main-prop "under-draw")))
           (if (get-prop "full-draw")
               ((get-prop "full-draw"))
               (and (= state.b.path "minibuffer") (get-prop "full-draw" nil
                                                            (behind-minibuffer)))
               (do
                 ((get-prop "full-draw" nil (behind-minibuffer)))
                 (frontend.draw state.b [] (. state "echo-message")
                                state.colors get-prop true))
               (do
                 (local buffers-where [])
                 (each [i bufpos (ipairs state.windows)]
                   (tset buffers-where (. bufpos 1) (. bufpos 2))
                   (tset (. bufpos 1) "current" (= i state.window)))
                 (frontend.draw state.b buffers-where
                                (. state "echo-message")
                                state.colors get-prop false)))
           (when (get-main-prop "over-draw")
             ((get-main-prop "over-draw"))))
         (if frontend.wrap
             (frontend.wrap actual-draw)
             (actual-draw)))
 :echo echo
 :end-of-buffer? is-end-of-buffer
 :enforce-max-lines (fn [max-lines] (for [_ 1 (- (# state.b.lines) max-lines) 1]
                                      (table.remove state.b.lines 1)
                                      (when (>= state.b.point-line 1)
                                        (set state.b.point-line (- state.b.point-line 1)))
                                      (when state.b.mark-line
                                        (set state.b.mark-line (- state.b.mark-line 1)))))
 :file-type (fn [path] (state.fs.type path))
 :get-buffer-wh frontend.get-buffer-wh
 :get-input get-input ; probably don't want this; consider handle-with-input
 :get-line (fn [n]
             (when state.b
               (when (not n)
                 (let [___antifnl_rtn_1___ (. state.b.lines state.b.point-line)]
                   (lua "return ___antifnl_rtn_1___")))
               (when (< n 1)
                 (set-forcibly! n (+ (# state.b.lines) n)))
               (. state.b.lines n)))
 :get-line-number (fn [] state.b.point-line)
 :get-lines (fn [] (lume.clone state.b.lines))
 :get-max-line (fn [] (# state.b.lines))
 :get-mode-prop get-mode-prop
 :set-mode-prop (fn [mode-name prop-name value]
                  (let [mode (assert (. state.modes mode-name)
                                     (.. mode-name "not found"))]
                    (tset mode.props prop-name value)))
 :get-prompt (fn [] (or state.b.prompt "> "))
 :get-prop get-prop
 :get-wh frontend.get-wh
 :go-to (fn [line point buffer-name]
          (local buffer (or (get-buffer buffer-name) state.b))
          (when (and (and (= (type point) "number") (>= point 0))
                     (<= point (# (. buffer.lines buffer.point-line))))
            (set buffer.point point))
          (when (and (and (= (type line) "number") (> line 0))
                     (<= line (# buffer.lines)))
            (set buffer.point-line line)))
 :history-push (fn [input] (when (not (: input "match" "%S"))
                             (lua "return "))
                 (set state.b.input-history-pos 0)
                 (table.insert state.b.input-history input)
                 (when (> (# state.b.input-history) state.b.input-history-max)
                   (table.remove state.b.input-history 1)))
 :init (fn [init-buffer-name mode-name contents fs]
         (local buffer (make-buffer init-buffer-name contents {:no-file true}))
         (set state.fs (or fs state.fs))
         (set (buffer.point buffer.point-line)
              (values (# (. contents (math.max (- (# contents) 1) 1)))
                      (# contents)))
         (set (state.b state.last-buffer state.output)
              (values buffer buffer buffer))
         (tset state.windows 1 [[] state.b])
         (table.insert state.buffers buffer)
         (activate-mode mode-name)
         (reset-minibuffer-keys exit-minibuffer)
         (frontend.init))
 :insert insert
 :kill-buffer (fn [buffer-name force]
                (local b (or (and buffer-name (get-buffer buffer-name)) state.b))
                (when (and (get-prop "unkillable" false b) (not force))
                  (let [___antifnl_rtn_1___ (echo "Unkillable buffer")]
                    (lua "return ___antifnl_rtn_1___")))
                (assert (> (# state.buffers) 1) "Can't kill last buffer")
                (lume.remove state.buffers b)
                (if (and (. state "last-buffer") (not= (. state "last-buffer") state.b))
                    (set (state.b state.last-buffer) (. state "last-buffer"))
                    (set state.b (lume.last state.buffers))))
 :last-buffer (fn [] (and (. state "last-buffer") (. (. state "last-buffer") "path")))
 :open open
 :point (fn [] (values state.b.point state.b.point-line))
 :print the-print
 :print-prompt (fn []
                 (local read-only (. state "inhibit-read-only"))
                 (tset state "printing-prompt" true)
                 (tset state "inhibit-read-only" true)
                 (set (state.b.mark state.b.mark-line) nil)
                 (delete (# state.b.lines) 0 (# state.b.lines)
                         (# (. state.b.lines (# state.b.lines))))
                 (write state.b.prompt)
                 (set (state.b.point state.b.point-line)
                      (values (# (. state.b.lines (# state.b.lines)))
                              (# state.b.lines)))
                 (tset state "printing-prompt" false)
                 (tset state "inhibit-read-only" read-only))
 :prompt (fn [] (or state.b.prompt "> "))
 :raw-write write
 :read-input read-input ;; coroutine-using version
 :read-line read-line ;; callback-using version
 :region region
 :save-excursion save-excursion
 :set-line (fn [line number path]
             (local buffer (or (get-buffer path) state.b))
             (tset buffer.lines number line))
 :set-modeline (fn [modeline-function] (set state.b.modeline modeline-function))
 :set-prompt (fn [p] (when (not state.b)
                       (lua "return "))
               (when state.b.prompt
                 (local line (. state.b.lines (# state.b.lines)))
                 (tset state.b.lines (# state.b.lines)
                       (.. p (utf8.sub line (+ (utf8.len state.b.prompt) 1)))))
               (when (= state.b.point-line (# state.b.lines))
                 (set state.b.point (utf8.len p)))
               (set state.b.prompt p))
 :set-prop set-prop
 :set-scale frontend.set-scale
 :set-wh frontend.set-wh
 :mod-down? frontend.mod-down?
 :start (fn [f] (when (= (type f) "function")
                  (set-forcibly! f (coroutine.create f)))
          (table.insert state.coroutines f)
          f)
 :suppress-read-only (fn [f ...]
                       (local read-only (. state "inhibit-read-only"))
                       (tset state "inhibit-read-only" true)
                       (local val (f ...))
                       (tset state "inhibit-read-only" read-only)
                       val)
 :vary-prop vary-prop
 :with-current-buffer with-current-buffer
 :with-output-to (fn [nb f]
                   (let [new-out (if (= (type nb) :string)
                                     (get-buffer nb)
                                     nb)
                         old-out state.output]
                     (set state.output (assert new-out "no output buffer!"))
                     (let [val (f)]
                       (set state.output old-out)
                       val)))
 :with-traceback with-traceback
 :write io-write
 :write-to (fn [buffer-name ...]
             (let [old-out state.output]
               (set state.output (get-buffer buffer-name))
               (io-write ...)
               (set state.output old-out)))

 ;; TODO: these are added to the public API in order to facilitate porting
 ;; functionality out of this module but shouldn't be considered part of the
 ;; stable API
 :internal {:behind behind-minibuffer
            :bounds-check bounds-check
            :run-on-change run-on-change}}

