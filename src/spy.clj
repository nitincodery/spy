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
  (let [xform (comp (mapcat (fn [[binding _expr]] (bound-symbols binding)))
                    (distinct))]
    (sequence xform (partition 2 bvec))))

(defn inject-spy-defs
  "Walk the form and inject swap! calls into let bindings and function arguments."
  [form]
  (walk/postwalk
   (fn [f]
     (cond
       ;; instrument defn / fn / fn*
       ;; Matches any function definition form.
       (and (seq? f)
            (#{'fn 'fn* 'defn 'defn-} (first f)))
       (let [;; A defn form might have a name, but a fn form does not.
             name? (when (symbol? (second f)) (second f))
             ;; A function can have multiple arities (bodies). This logic handles both single- and multi-arity functions.
             bodies (if (vector? (if name? (nth f 2) (second f)))
                      [(drop (if name? 2 1) f)]
                      (if name? (drop 2 f) (rest f)))
             ;; We process each arity (body) separately.
             new-bodies
             (map (fn [body]
                    (let [args (first body)
                          body-forms (rest body)
                          ;; Get all the symbols from the argument vector, including those in destructuring.
                          syms (bound-symbols args)
                          ;; For each symbol, create a form that will update our spy atom.
                          spy-forms (map (fn [s] `(swap! *spy-bindings* assoc '~s ~s)) syms)]
                      ;; We reconstruct the function body. We wrap the original body in a `let` block.
                      ;; This is crucial because it ensures that any destructuring in the argument vector
                      ;; has already happened before we try to access the symbols to spy on them.
                      `(~args
                        (let [~@(mapcat (fn [s] [s s]) syms)]
                          ~@spy-forms
                          (do ~@body-forms)))))
                  bodies)]
         ;; Reconstruct the final function form with the new, instrumented bodies.
         (if name?
           `(~(first f) ~name? ~@new-bodies)
           `(~(first f) ~@new-bodies)))

       ;; instrument let / let*
       ;; Matches let and let* forms.
       (and (seq? f) (#{'let 'let*} (first f)))
       (let [bvec (second f)
             body (drop 2 f)
             ;; Get all the symbols from the binding vector.
             syms (extract-binding-symbols bvec)
             ;; For each symbol, create a form that will update our spy atom.
             spy-forms (map (fn [sym]
                              `(swap! *spy-bindings* assoc '~sym ~sym))
                            syms)]
         ;; We use let* to ensure that bindings are available sequentially, which is important for correctness.
         ;; The spy forms are inserted after the binding vector, so they have access to the bound values.
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
