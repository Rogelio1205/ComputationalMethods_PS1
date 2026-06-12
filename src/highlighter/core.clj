(ns highlighter.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;;constants

(def keywords
  #{"and" "as" "assert" "async" "await" "break" "case" "class" "continue"
    "def" "del" "elif" "else" "except" "False" "finally" "for" "from"
    "global" "if" "import" "in" "is" "lambda" "match" "None" "nonlocal"
    "not" "or" "pass" "raise" "return" "True" "try" "while" "with" "yield"})

(def delimiters
  #{"(" ")" "[" "]" "{" "}" "," ":" "." ";"})

(def two-char-ops
  #{"==" "!=" "<=" ">=" "+=" "-=" "*=" "/=" "%=" "**" "//"})

(def css
  "pre.code {background:#1e1b2b;color:#fff;padding:1rem;white-space:pre;overflow:auto;font-family:monospace;}
.keyword   {color:#c792ea;font-weight:bold}
.identifier{color:#ffffff}
.integer   {color:#f78c6c}
.float     {color:#f78c6c}
.string    {color:#c3e88d}
.comment   {color:#7f848e;font-style:italic}
.operator  {color:#89ddff}
.delimiter {color:#ff9d41}
.whitespace{color:inherit}")

;;character predicates

(defn digit? [c]
  (and (char? c) (Character/isDigit c)))

(defn letter? [c]
  (and (char? c)
       (or (Character/isLetter c)
           (= c \_))))

(defn word-char? [c]
  (or (letter? c) (digit? c)))

(defn whitespace? [c]
  (and (char? c)
       (or (= c \space)
           (= c \newline)
           (= c \tab)
           (= c \return))))

(defn operator-start? [c]
  (contains? #{\+ \- \* \/ \% \= \! \< \>} c))

;;read helpers

(defn read-while
  "Reads characters while pred is true. Returns [string remaining]."
  [pred chars]
  (loop [remaining chars
         acc []]
    (if (or (empty? remaining)
            (not (pred (first remaining))))
      [(apply str acc) remaining]
      (recur (rest remaining)
             (conj acc (first remaining))))))

(defn read-comment
  "Reads from current position to end of line."
  [chars]
  (read-while #(not= % \newline) chars))

(defn read-string-token
  "Reads a single or double quoted string, handling backslash escapes."
  [chars quote-char]
  (loop [remaining chars
         acc [quote-char]]
    (cond
      (empty? remaining)
      [(apply str acc) remaining]

      (= (first remaining) \\)
      (if (empty? (rest remaining))
        [(apply str (conj acc \\)) '()]
        (recur (drop 2 remaining)
               (conj acc \\ (second remaining))))

      (= (first remaining) quote-char)
      [(apply str (conj acc quote-char)) (rest remaining)]

      :else
      (recur (rest remaining)
             (conj acc (first remaining))))))

(defn read-triple-string
  "Reads a triple-quoted string until the matching closing triple-quote."
  [chars quote-char]
  (loop [remaining chars
         acc [quote-char quote-char quote-char]]
    (cond
      (empty? remaining)
      [(apply str acc) remaining]

      (and (>= (count remaining) 3)
           (= (first remaining) quote-char)
           (= (second remaining) quote-char)
           (= (nth remaining 2) quote-char))
      [(apply str (concat acc [quote-char quote-char quote-char]))
       (drop 3 remaining)]

      :else
      (recur (rest remaining)
             (conj acc (first remaining))))))

(defn read-number
  "Reads an integer or float literal. Returns [value remaining type]."
  [chars]
  (let [[int-part rest1] (read-while digit? chars)]
    (if (and (seq rest1)
             (= (first rest1) \.)
             (seq (rest rest1))
             (digit? (second rest1)))
      (let [[dec-part rest2] (read-while digit? (rest rest1))]
        [(str int-part "." dec-part) rest2 :float])
      [int-part rest1 :integer])))

(defn read-operator
  "Reads a one or two character operator."
  [chars]
  (let [c1 (first chars)
        rest-chars (rest chars)]
    (if (empty? rest-chars)
      [(str c1) rest-chars]
      (let [two (str c1 (first rest-chars))]
        (if (contains? two-char-ops two)
          [two (rest rest-chars)]
          [(str c1) rest-chars])))))

;;token classification

(defn classify-word [word]
  (if (contains? keywords word)
    {:type :keyword :value word}
    {:type :identifier :value word}))

;;tokenizer

(defn tokenize
  "Main lexer. Takes a sequence of characters and returns a vector of tokens."
  [chars]
  (loop [remaining chars
         tokens []]
    (if (empty? remaining)
      tokens
      (let [c (first remaining)]
        (cond
          ;;whitespace
          (whitespace? c)
          (let [[val rest-chars] (read-while whitespace? remaining)]
            (recur rest-chars
                   (conj tokens {:type :whitespace :value val})))

          ;;comment
          (= c \#)
          (let [[val rest-chars] (read-comment remaining)]
            (recur rest-chars
                   (conj tokens {:type :comment :value val})))

          ;;triple double-quote string
          (and (>= (count remaining) 3)
               (= c \")
               (= (second remaining) \")
               (= (nth remaining 2) \"))
          (let [[val rest-chars] (read-triple-string (drop 3 remaining) \")]
            (recur rest-chars
                   (conj tokens {:type :string :value val})))

          ;;triple single-quote string
          (and (>= (count remaining) 3)
               (= c \')
               (= (second remaining) \')
               (= (nth remaining 2) \'))
          (let [[val rest-chars] (read-triple-string (drop 3 remaining) \')]
            (recur rest-chars
                   (conj tokens {:type :string :value val})))

          ;;double-quote string
          (= c \")
          (let [[val rest-chars] (read-string-token (rest remaining) \")]
            (recur rest-chars
                   (conj tokens {:type :string :value val})))

          ;;single-quote string
          (= c \')
          (let [[val rest-chars] (read-string-token (rest remaining) \')]
            (recur rest-chars
                   (conj tokens {:type :string :value val})))

          ;;number
          (digit? c)
          (let [[val rest-chars type] (read-number remaining)]
            (recur rest-chars
                   (conj tokens {:type type :value val})))

          ;;identifier or keyword
          (letter? c)
          (let [[val rest-chars] (read-while word-char? remaining)]
            (recur rest-chars
                   (conj tokens (classify-word val))))

          ;;operator
          (operator-start? c)
          (let [[val rest-chars] (read-operator remaining)]
            (recur rest-chars
                   (conj tokens {:type :operator :value val})))

          ;;delimiter
          (contains? delimiters (str c))
          (recur (rest remaining)
                 (conj tokens {:type :delimiter :value (str c)}))

          ;;unknown character 
          :else
          (recur (rest remaining) tokens))))))

(defn tokenize-string [input]
  (tokenize (seq input)))

;;html generation

(defn escape-html [s]
  (-> s
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

(defn token->html [{:keys [type value]}]
  (let [v (escape-html value)]
    (if (= type :whitespace)
      v
      (format "<span class=\"%s\">%s</span>" (name type) v))))

(defn tokens->html [tokens]
  (apply str (map token->html tokens)))

(defn make-html-page [body]
  (format "<!doctype html>
<html lang=\"en\">
  <head>
    <meta charset=\"utf-8\">
    <title>Highlighted Source</title>
    <style>%s</style>
  </head>
  <body>
    <pre class=\"code\">%s</pre>
  </body>
</html>"
          css
          body))

;; ─────────────────────────────────────────────
;; File I/O
;; ─────────────────────────────────────────────

(defn output-path [input-path]
  (str/replace input-path #"\.[^./\\]+$" ".html"))

(defn process-file
  "Reads a Python file, tokenizes it and writes the highlighted HTML output."
  [path]
  (let [source (slurp path)
        tokens (tokenize-string source)
        body (tokens->html tokens)
        html (make-html-page body)
        out (output-path path)]
    (spit out html)
    {:file path
     :tokens (count tokens)
     :output out}))

;;directory processing - Sequential version

(defn get-python-files
  "Returns all .py files inside a directory, including subdirectories."
  [dir-path]
  (->> (file-seq (io/file dir-path))
       (filter #(and (.isFile %)
                     (str/ends-with? (.getName %) ".py")))
       (map #(.getPath %))))

(defn process-directory-sequential
  "Processes all Python files in a directory one by one."
  [dir-path]
  (let [files (get-python-files dir-path)]
    (println (format "Sequential: processing %d files..." (count files)))
    (let [start (System/nanoTime)
          results (mapv process-file files)
          elapsed (/ (- (System/nanoTime) start) 1e9)]
      (doseq [r results]
        (println (format "  wrote %s (%d tokens)" (:output r) (:tokens r))))
      (println (format "Sequential done in %.4f seconds." elapsed))
      {:results results
       :time elapsed})))

;;directory processing - Parallel version

(defn process-directory-parallel
  "Processes all Python files in a directory in parallel using pmap."
  [dir-path]
  (let [files (get-python-files dir-path)]
    (println (format "Parallel: processing %d files..." (count files)))
    (let [start (System/nanoTime)
          results (doall (pmap process-file files))
          elapsed (/ (- (System/nanoTime) start) 1e9)]
      (doseq [r results]
        (println (format "  wrote %s (%d tokens)" (:output r) (:tokens r))))
      (println (format "Parallel done in %.4f seconds." elapsed))
      {:results results
       :time elapsed})))

;;benchmarking

(defn average [xs]
  (/ (reduce + xs) (count xs)))

(defn benchmark
  "Runs both versions N times on the same directory and prints average times and speedup."
  [dir-path n]
  (println (format "\nBenchmarking with %d runs on directory: %s" n dir-path))
  (println "─────────────────────────────────────────")

  ;; Warm up JVM
  (println "\nWarming up JVM...")
  (process-directory-sequential dir-path)
  (process-directory-parallel dir-path)

  ;; Sequential runs
  (println (format "\nRunning sequential version %d times..." n))
  (let [seq-times (mapv
                   (fn [i]
                     (println (format "  Sequential run %d..." (inc i)))
                     (:time (process-directory-sequential dir-path)))
                   (range n))
        seq-avg (average seq-times)]

    ;; Parallel runs
    (println (format "\nRunning parallel version %d times..." n))
    (let [par-times (mapv
                     (fn [i]
                       (println (format "  Parallel run %d..." (inc i)))
                       (:time (process-directory-parallel dir-path)))
                     (range n))
          par-avg (average par-times)
          speedup (/ seq-avg par-avg)]

      (println "\n─────────────────────────────────────────")
      (println "RESULTS")
      (println "─────────────────────────────────────────")
      (println (format "Sequential times (s): %s"
                       (str/join ", " (map #(format "%.4f" %) seq-times))))
      (println (format "Parallel times (s)  : %s"
                       (str/join ", " (map #(format "%.4f" %) par-times))))
      (println (format "Sequential average  : %.4f s" seq-avg))
      (println (format "Parallel average    : %.4f s" par-avg))
      (println (format "Speedup             : %.4fx" speedup))
      (println "─────────────────────────────────────────")

      {:seq-avg seq-avg
       :par-avg par-avg
       :speedup speedup
       :seq-times seq-times
       :par-times par-times})))

;;entry point

(defn -main [& args]
  (cond
    ;;no arguments
    (empty? args)
    (do
      (println "Usage:")
      (println "  lein run seq samples")
      (println "  lein run par samples")
      (println "  lein run bench samples 10"))

    ;;sequential mode
    (= (first args) "seq")
    (if (< (count args) 2)
      (println "Error: please provide a directory path.")
      (process-directory-sequential (second args)))

    ;;parallel mode
    (= (first args) "par")
    (if (< (count args) 2)
      (println "Error: please provide a directory path.")
      (process-directory-parallel (second args)))

    ;;benchmark mode
    (= (first args) "bench")
    (if (< (count args) 2)
      (println "Error: please provide a directory path.")
      (let [dir (second args)
            n (if (>= (count args) 3)
                (Integer/parseInt (nth args 2))
                5)]
        (benchmark dir n)))

    ;;unknown mode
    :else
    (println (format "Unknown mode: %s. Use seq, par, or bench." (first args)))))