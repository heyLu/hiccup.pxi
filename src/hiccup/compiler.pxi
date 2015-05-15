(ns hiccup.compiler
  "Internal functions for compilation."
  (:require [hiccup.util :refer :all]
            [pixie.string :as str])
  )

(defn as-str [n]
  (if (satisfies? INamed n)
    (name n)
    (str n)))
(def ^:dynamic *html-mode*)
(def defn- defn)

(defn- xml-mode? []
  (#{:xml :xhtml} :xhtml))

(defn- html-mode? []
  (#{:html :xhtml} 
           :html
           ;*html-mode*
           ))

(defn- end-tag []
  (if (xml-mode?) " />" ">"))

(defn- xml-attribute [name value]
  (str " " (as-str name) "=\"" (escape-html value) "\""))

(defn- render-attribute [[name value]]
  (cond
    (true? value)
      (if (xml-mode?)
        (xml-attribute name name)
        (str " " (as-str name)))
    (not value)
      ""
    :else
      (xml-attribute name value)))

(defn- render-attr-map [attrs]
  (apply str
         (map render-attribute (seq attrs))))

;; (def ^{:doc "Regular expression that parses a CSS-style id and class from an element name."
;;        :private true}
;;   re-tag #"([^\s\.#]+)(?:#([^\s\.#]+))?(?:\.([^\s#]+))?")

; re-matches re-tag (str tag)
(defn npos? [n]
  (when n (pos? n)))

(defn splice-tag
  "Split a tag into id and class if present"
  [tag]
  (let [id-idx (str/index-of tag "#")
        class-idx (str/index-of tag ".")
        [tag id class] (cond
                        (and (npos? id-idx) (npos? class-idx)) [(str/substring tag 0 id-idx)
                                                              (str/substring tag (inc id-idx) class-idx)
                                                              (str/substring tag (inc class-idx))]
                        (npos? id-idx) [(str/substring tag 0 id-idx) (str/substring tag (inc id-idx)) nil]
                        (npos? class-idx) [(str/substring tag 0 class-idx) nil (str/substring tag (inc class-idx))]
                        :else [tag nil nil])]
    ['_ tag id class]))

(def ^{:doc "A list of elements that must be rendered without a closing tag."
       :private true}
  void-tags
  #{"area" "base" "br" "col" "command" "embed" "hr" "img" "input" "keygen" "link"
    "meta" "param" "source" "track" "wbr"})

(defn- container-tag?
  "Returns true if the tag has content or is not a void tag. In non-HTML modes,
  all contentless tags are assumed to be void tags."
  [tag content]
  (or content
      (and (html-mode?) (not (void-tags tag)))))

(defn- merge-attributes [{:keys [id class]} map-attrs]
  (->> map-attrs
       (merge (if id {:id id}))
))

(defn normalize-element
  "Ensure an element vector is of the form [tag-name attrs content]."
  [[tag & content]]
  (when (not (or (keyword? tag) (symbol? tag) (string? tag)))
    (throw (str tag " is not a valid element name.")))
  (let [[_ tag id class] (splice-tag (as-str tag))
        tag-attrs        {:id id
                          :class (if class (str/replace class "." " "))}
        map-attrs        (first content)]
    (if (map? map-attrs)
      [tag (merge-attributes tag-attrs map-attrs) (next content)]
      [tag tag-attrs content])))

(defprotocol HtmlRenderer
  (render-html [this]
    "Turn a Clojure data type into a string of HTML."))

(defn- render-element
  "Render an element vector as a HTML element."
  [element]
  (let [[tag attrs content] (normalize-element element)]
    (if (container-tag? tag content)
      (str "<" tag (render-attr-map attrs) ">"
           (render-html content)
           "</" tag ">")
      (str "<" tag (render-attr-map attrs) (end-tag)))))

(extend-protocol HtmlRenderer
  IVector
  (render-html [this]
    (render-element this))
  ISeq
  (render-html [this]
    (apply str (map render-html this)))
  INamed
  (render-html [this]
    (name this))
  Nil
  (render-html [this]
    "")
  IObject
  (render-html [this]
    (str this)))

(defn- unevaluated?
  "True if the expression has not been evaluated."
  [expr]
  (or (symbol? expr)
      (and (seq? expr)
           (not= (first expr) `quote))))

(defn compile-attr-map
  "Returns an unevaluated form that will render the supplied map as HTML
  attributes."
  [attrs]
  (if (some unevaluated? (mapcat identity attrs))
    `(render-attr-map ~attrs)
    (render-attr-map attrs)))

(defn- form-name
  "Get the name of the supplied form."
  [form]
  (if (and (seq? form) (symbol? (first form)))
    (name (first form))))

(declare compile-html)

(defmulti compile-form
  "Pre-compile certain standard forms, where possible."
  {:private true}
  form-name)

(defmethod compile-form "for"
  [[_ bindings body]]
  `(apply str (for ~bindings ~(compile-html body))))

(defmethod compile-form "if"
  [[_ condition & body]]
  `(if ~condition ~@(for [x body] (compile-html x))))

(defmethod compile-form :default
  [expr]
  `(render-html ~expr))

(defn- not-hint?
  "True if x is not hinted to be the supplied type."
  [x type]
  (if-let [hint (-> x meta :tag)]
    (not (instance? (eval hint) type))))

(defn- hint?
  "True if x is hinted to be the supplied type."
  [x type]
  (if-let [hint (-> x meta :tag)]
    (instance? (eval hint) type)))

(defn- literal?
  "True if x is a literal value that can be rendered as-is."
  [x]
  (and (not (unevaluated? x))
       (or (not (or (vector? x) (map? x)))
           (every? literal? (seq x)))))

(defn- not-implicit-map?
  "True if we can infer that x is not a map."
  [x]
  (or (= (form-name x) "for")
      (not (unevaluated? x))
      (not-hint? x PersistentHashMap)))

(defn- element-compile-strategy
  "Returns the compilation strategy to use for a given element."
  [[tag attrs & content :as element]]
  (cond
    (every? literal? element)
      :hiccup.compiler/all-literal                    ; e.g. [:span "foo"]
    (and (literal? tag) (map? attrs))
      :hiccup.compiler/literal-tag-and-attributes     ; e.g. [:span {} x]
    (and (literal? tag) (not-implicit-map? attrs))
      :hiccup.compiler/literal-tag-and-no-attributes  ; e.g. [:span ^String x]
    (literal? tag)
      :hiccup.compiler/literal-tag                    ; e.g. [:span x]
    :else
      :hiccup.compiler/default))                      ; e.g. [x]

(declare compile-seq)

(defmulti compile-element
  "Returns an unevaluated form that will render the supplied vector as a HTML
  element."
  {:private true}
  element-compile-strategy)

(defmethod compile-element :hiccup.compiler/all-literal
  [element]
  (render-element (eval element)))

(defmethod compile-element :hiccup.compiler/literal-tag-and-attributes
  [[tag attrs & content]]
  (let [[tag attrs _] (normalize-element [tag attrs])]
    (if (container-tag? tag content)
      `(str ~(str "<" tag) ~(compile-attr-map attrs) ">"
            ~@(compile-seq content)
            ~(str "</" tag ">"))
      `(str "<" ~tag ~(compile-attr-map attrs) ~(end-tag)))))

(defmethod compile-element :hiccup.compiler/literal-tag-and-no-attributes
  [[tag & content]]
  (compile-element (apply vector tag {} content)))

(defmethod compile-element :hiccup.compiler/literal-tag
  [[tag attrs & content]]
  (let [[tag tag-attrs _] (normalize-element [tag])
        attrs-sym         (gensym "attrs")]
    `(let [~attrs-sym ~attrs]
       (if (map? ~attrs-sym)
         ~(if (container-tag? tag content)
            `(str ~(str "<" tag)
                  (render-attr-map (merge ~tag-attrs ~attrs-sym)) ">"
                  ~@(compile-seq content)
                  ~(str "</" tag ">"))
            `(str ~(str "<" tag)
                  (render-attr-map (merge ~tag-attrs ~attrs-sym))
                  ~(end-tag)))
         ~(if (container-tag? tag attrs)
            `(str ~(str "<" tag (render-attr-map tag-attrs) ">")
                  ~@(compile-seq (cons attrs-sym content))
                  ~(str "</" tag ">"))
            (str "<" tag (render-attr-map tag-attrs) (end-tag)))))))

(defmethod compile-element :default
  [element]
  `(render-element
     [~(first element)
      ~@(for [x (rest element)]
          (if (vector? x)
            (compile-element x)
            x))]))

(defn- compile-seq
  "Compile a sequence of data-structures into HTML."
  [content]
  (for [expr content]
    (cond
     (vector? expr) (compile-element expr)
     (literal? expr) expr
     (hint? expr String) expr
     (hint? expr Number) expr
     (seq? expr) (compile-form expr)
     :else `(render-html ~expr))))

(defn- collapse-strs
  "Collapse nested str expressions into one, where possible."
  [expr]
  (if (seq? expr)
    (cons
     (first expr)
     (mapcat
      #(if (and (seq? %) (symbol? (first %)) (= (first %) (first expr) `str))
         (rest (collapse-strs %))
         (list (collapse-strs %)))
      (rest expr)))
    expr))

(defn compile-html
  "Pre-compile data structures into HTML where possible."
  [& content]
  (collapse-strs `(str ~@(compile-seq content))))
