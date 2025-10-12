(ns spy
  (:require [clojure.walk :as walk]))

(def ^:dynamic *spy-bindings* (atom {}))

(defn should-spy?
  "Predicate to determine if a symbol should be spied upon."
  [sym]
  (and (symbol? sym)
       (not= '& sym)
       (not (re-matches #"map__\d+|&" (str sym)))))

(defn- bound-symbols
  "Return a sequence of all valid symbol nodes inside a binding form (e.g., arg vector)."
  [binding]
  (let [acc (atom [])]
    (walk/postwalk
     (fn [x]
       (when (should-spy? x)
         (swap! acc conj x))
       x)
     binding)
    (distinct @acc)))

(defn- extract-binding-symbols
  "Given a let binding vector, return the collection of symbols bound by it."
  [bvec]
  (->> (partition 2 bvec)
       (mapcat (fn [[binding _expr]] (bound-symbols binding)))
       (distinct)))

(defn inject-spy-defs
  "Walk the form and inject swap! calls into let bindings and function arguments."
  [form]
  (walk/postwalk
   (fn [f]
     (cond
       ;; instrument defn / fn / fn*
       (and (seq? f)
            (#{'fn 'fn* 'defn 'defn-} (first f)))
       (let [name? (when (symbol? (second f)) (second f))
             ;; Handle multi-arity functions correctly
             bodies (if (vector? (if name? (nth f 2) (second f)))
                      [(drop (if name? 2 1) f)]
                      (if name? (drop 2 f) (rest f)))
             new-bodies
             (map (fn [body]
                    (let [args (first body)
                          body-forms (rest body)
                          syms (bound-symbols args)
                          spy-forms (map (fn [s] `(swap! *spy-bindings* assoc '~s ~s)) syms)]
                      ;; Wrap the original body in a `let`.
                      ;; This ensures that destructuring in `args` has already happened
                      ;; before we try to access the symbols to spy on them.
                      `(~args
                        (let [~@(mapcat (fn [s] [s s]) syms)]
                          ~@spy-forms
                          (do ~@body-forms)))))
                  bodies)]
         (if name?
           `(~(first f) ~name? ~@new-bodies)
           `(~(first f) ~@new-bodies)))

       ;; instrument let / let*
       (and (seq? f) (#{'let 'let*} (first f)))
       (let [bvec (second f)
             body (drop 2 f)
             syms (extract-binding-symbols bvec)
             spy-forms (map (fn [sym]
                              `(swap! *spy-bindings* assoc '~sym ~sym))
                            syms)]
         ;; Using let* ensures bindings are available sequentially.
         ;; Placing spy-forms after the binding vector is correct.
         `(let* ~bvec
                ~@spy-forms
                ~@body))

       :else f))
   form))

(defmacro spy [& body]
  (let [expanded (walk/macroexpand-all `(do ~@body))]
    (inject-spy-defs expanded)))

(defn unspy
  "Resets all spy bindings."
  []
  (reset! *spy-bindings* {}))

(defn spy-val
  "Retrieves the value of a spied symbol."
  [sym]
  (get @*spy-bindings* sym))

(defn spy-runtime
  "Dynamically instruments and redefines a var from a string or list.
  Crucially, it redefines the var in its original namespace."
  [var-symbol form-or-str]
  (let [form (if (string? form-or-str)
               (read-string form-or-str)
               form-or-str)
        ;; Resolve the namespace from the target symbol
        target-ns (find-ns (symbol (namespace var-symbol)))
        _ (when-not target-ns
            (throw (IllegalArgumentException.
                    (str "Namespace not found for symbol: " var-symbol))))
        expanded (walk/macroexpand-all form)
        injected (inject-spy-defs expanded)]
    ;; Bind *ns* to the target namespace before calling eval.
    (binding [*ns* target-ns]
      (eval injected))))
