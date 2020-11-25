(local editor (require :polywell))
(local (socket-ok? socket) (pcall require :socket))
(local bencode (require :config.bencode))
(local lume (require :polywell.lib.lume))

(local active-requests {})
(var counter 0)

(λ send [msg ?conn]
  (set (msg.id counter) (values counter (+ counter 1)))
  (tset active-requests msg.id true)
  (when (not msg.session)
    (set msg.session (editor.get-prop "nrepl-session")))
  (let [conn (or ?conn (assert (editor.get-prop "conn") "Not connected."))
        (ok err) (: conn :send (bencode.encode msg))]
    (when (not ok)
      (editor.echo (.. "Error: " err)))))

(λ send-input [?session]
  (editor.read-line "Input: "
                    (fn [input]
                      (let [input (if (= nil input) "" input)]
                        (send {:op :stdin
                               :session ?session
                               :stdin input})))))

(λ p [out]
  (editor.with-output-to (editor.current-buffer-name)
                         (partial editor.write (.. "\n" out))))

(λ handler [response]
  (when response.new-session
    (editor.echo "Connected.")
    (editor.set-prop :nrepl-session response.new-session))

  (when response.err (p response.err))
  (when response.out (p response.out))
  (when response.value (p response.value))

  (when response.ns
    (editor.set-prop response.ns)
    (editor.set-prompt (.. response.ns "=> ")))

  (when response.status
    (when (lume.find response.status :interrupted)
      (editor.echo "Evaluation interrupted.")
      (tset active-requests response.id nil))
    (when (lume.find response.status :done)
      (tset active-requests response.id nil))
    (when (lume.find response.status :needs-input)
      (send-input response.session))))

(λ receive [conn buffer-name]
  (let [(data err part) (: conn :receive "*a")]
    (if (or data (and part (not= part "")))
        (let [(decodeds d-err) (bencode.decode_all (or data part))]
          (if decodeds
              (each [_ decoded (ipairs decodeds)]
                (editor.with-current-buffer buffer-name handler decoded))
              (p (.. "Decoding error: " d-err (or data part) "\n"))))
        (and err (not= err :timeout))
        (coroutine.yield true)
        ;; else
        (coroutine.yield false)))
  (receive conn buffer-name))

(λ send-eval-input [input]
  (send {:op :eval :code input}))

(λ connect [port]
  (let [(conn err) (socket.connect "localhost" (tonumber port))]
    (if conn
        (let [buffer-name (.. "*nrepl " port "*")]
          (: conn :settimeout 0)
          (send {:op :clone} conn)
          (editor.open buffer-name "nrepl" true)
          (editor.set-prop :conn conn)
          (editor.set-prompt "user=> ")
          (editor.print-prompt)
          (editor.start (partial receive conn buffer-name)))
        (editor.echo err))))

(λ editor.cmd.nrepl []
  (editor.read-line "Connect to nREPL server: " connect))

(λ eval-input []
  (when (not= (editor.get-line-number) (editor.get-max-line))
    (editor.end-of-buffer))
  (let [input (editor.get-input)]
    (editor.history-push input)
    (editor.cmd.end-of-line)
    (editor.cmd.newline)
    (editor.cmd.no-mark)
    (editor.print-prompt)
    (send-eval-input input)))

(λ editor.cmd.nrepl-interrupt []
  (each [id (pairs active-requests)]
    (send {:op :interrupt :interrupt-id id})))

(λ editor.cmd.nrepl-doc []
  (let [code "(require 'clojure.repl) (clojure.repl/doc %s)"]
    (editor.read-line "Describe: "
                      (fn [v] (send {:op :eval :code (: code :format v)})))))

(λ editor.cmd.nrepl-change-ns []
  (let [code "(ns %s)"]
    (editor.read-line "Enter namespace: "
                      (fn [n] (send {:op :eval :code (: code :format n)})))))

(λ editor.cmd.nrepl-reload []
  (let [code "(require '%s :reload)"]
    (editor.read-line "Enter namespace: "
                      (fn [n] (send {:op :eval :code (: code :format n)})))))

{:name "nrepl"
 :parent "line"
 :map {:return eval-input}
 :ctrl {"x" {"i" editor.cmd.nrepl-interrupt
             "d" editor.cmd.nrepl-doc
             "n" editor.cmd.nrepl-change-ns}}}
