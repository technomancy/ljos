;; shell mode; this is nowhere near close to working
(local editor (require :polywell))
(local lume (require :polywell.lib.lume))

(fn print [x]
  (editor.with-output-to "*shell*" (partial editor.print x)))

(fn make-env []
  {:PWD (os.getenv :PWD)
   :HOME (os.getenv :HOME)
   :PATH (os.getenv :PATH)})

(fn getenv [name]
  (let [env (editor.get-prop :env)]
    (if (not env)
        (do (editor.set-prop :env (make-env))
          (getenv name))
        (. env name))))

(fn setenv [name val]
  (let [env (editor.get-prop :env)]
    (if (not env)
        (do (editor.set-prop :env (make-env))
          (getenv name))
        (tset env name val))))

(fn normalize [relative-to dir]
  (if (dir:match "^/")
      dir
      (.. relative-to "/" dir)))

(fn cd [dir]
  (let [target (normalize (getenv "PWD") dir)]
    (if (= (editor.file-type target) "directory")
        (setenv "PWD" target)
        (print (.. target " not found.")))))

(fn on-path? [f path]
  (if (and (f:match "^/") (= :file (editor.file-type f)))
      true
      (let [path (or path (lume.split (getenv :PATH) ":"))
            dir (table.remove path)]
        (if (not dir) false
            (= :file (editor.file-type (.. dir "/" f))) true
            (on-path? f path)))))

(fn pump [p]
  (let [line (p:read "*l")]
    (if line
        (do (print line)
          (coroutine.yield)
          (pump p))
        (editor.set-prop :subprocess nil))))

(fn execute [input]
  ;; TODO: there's no way to affect the processes environment here
  ;; unfortunately the docs for luaposix are terrible.
  (let [p (assert (io.popen input))]
    (editor.set-prop :subprocess p)
    (editor.start (partial pump p))))

(fn echo [args]
  (let [out []]
    (each [_ a (ipairs args)]
      (table.insert out (if (a:match "${.+}")
                            (let [env (a:match "${(.+)}")]
                              (editor.print env a)
                              (. [(a:gsub "${(.+)}" (getenv env))] 1))
                            a)))
    (print (table.concat out " "))))

(fn run [input]
  (if (editor.get-prop :subprocess)
      (editor.echo "...")
      (match (lume.split input)
        ["cd" dir] (cd dir)
        ["cd"] (cd (getenv "HOME"))
        ["echo" & args] (echo args)
        ([e] ? (e:match "(.+)=(.+)")) (setenv (e:match "(.+)=(.+)"))
        ([command & args] ? (on-path? command)) (execute input)
        _ (print (.. input ": command not found")))))

(fn activate []
  (editor.set-prompt "$ ")
  (editor.print-prompt))

(fn editor.cmd.shell []
  (editor.open "*shell*" "shell" true))

(local enter (editor.handle-input-with run))

{:name "shell"
 :parent "line"
 :map {"return" enter}
 :ctrl {"m" enter}
 :props {:activate activate}}
