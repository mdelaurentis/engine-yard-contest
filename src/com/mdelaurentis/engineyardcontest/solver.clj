(ns com.mdelaurentis.engineyardcontest.solver
  (:import [java.security MessageDigest])
  (:use [clojure.contrib test-is]
        compojure)
  (:gen-class))

(defstruct solution :tweet :score :time)
(defstruct solver :num-tries :solutions :scoring-fn)

(def running (ref false))

(def message-digest (MessageDigest/getInstance "SHA-1"))

(defn sha1 [str]
  (. message-digest (digest (.getBytes str))))

(defn byte-to-bits [byte]
  (let [int-val (Integer/valueOf byte)]
    (if (> 0 int-val)
      (+ int-val 128)
      int-val)))

(defn bytes-to-string [byte-array]
  (let [res (.toString  (BigInteger. byte-array) 2)
        padding (- 160 (count res))]
    (apply str (concat (take padding (repeat 0))
                       res))))

(defn format-hash [hash-str]
  (map str (partition 4 hash-str)))

(sha1 "What you write today will become legacy")
(sha1 "RuBy one eight six rspec mongrel MRI jruby jruby memcached exception reflection utf8E")

(defn compare-hashes [a b]
  (count (filter (fn [[a-bit b-bit]]
                   (println a-bit b-bit)
                   (= a-bit b-bit))
                 (map list 
                      (bytes-to-string a) 
                      (bytes-to-string b)))))

(deftest test-compare-hashes
  (is (= 80 (compare-hashes (sha1 "What you write today will become legacy")
                            (sha1 "RuBy one eight six rspec mongrel MRI jruby jruby memcached exception reflection utf8E")))))

(defn random-tweets [dict]
  "Returns an infinite sequence of random combinations of twelve words
chosen from the given dictionary."
  (let [limit         (count dict)
        rand-word     #(dict (rand-int limit))
        rand-tweet #(apply str (interpose " " (take 12 (repeatedly rand-word))))]
    (repeatedly rand-tweet)))

(defn score-tweet [prob tweet]
  (compare-hashes (sha1 tweet) (:hash prob)))

(defn scoring-fn [phrase]
  (let [target-hash (sha1 phrase)]
    (fn [tweet]
      (compare-hashes (sha1 tweet) target-hash))))


(def the-solver (agent nil))

(defn update [solver tweet]
  (let [solutions (:solutions solver ())
        score     (compare-hashes (sha1 tweet) (:hash solver))
        solver    (inc (:num-tries solver))]
    (if (< score (:score (first solutions)))
      (assoc solver
        :solutions (conj solutions (struct solution tweet score (java.util.Date.))))
      solver)))

(defn make-solver [solver phrase dictionary]
  (struct-map solver
    :phrase phrase
    :hash (sha1 phrase)
    :dictionary dictionary
    :solutions ()
    :num-tries 0))

(defroutes solver-app
  (GET "/"
    (html
     (doctype :html4)
     [:html
      [:head
       [:title "Solver"]
       (include-css "/styles.css")]
      [:body
       (link-to "/configure" "Configure")
       [:h3 "Phrase:"] (:phrase @the-solver)
       [:h3 "Dictionary:"] (:dictionary @the-solver)
       [:h3 "Solutions:"]
       [:table
        [:tr 
         [:td "Phrase"]
         [:td "Score"]
         [:td "Time Found"]]
        (for [s (:solutions @the-solver)]
          [:td 
           [:td (:phrase s)]
           [:td (:score s)]
           [:td (:time s)]])]]]))
  
  (GET "/configure"
       (html
        (doctype :html4)
        [:html
         [:head
          [:title "Solver"]
          (include-css "/styles.css")]
         [:body
          (form-to [:post "/"]
                   [:p (label :phrase "Phrase: ")
                    (text-field :phrase (:phrase @the-solver))]
                   [:p (label :dictionary "Dictionary: ")
                    (text-area :dictionary (:dictionary @the-solver))]
                   (submit-button "Submit"))]]))

  (POST "/"
    (let [phrase (params :phrase)
          dict   (vec (.split (params :dictionary) "\\s+"))
          score  (scoring-fn phrase)
          solve  (fn solve [solver [tweet tweets]]
                   (when @running (send *agent* solve tweets))
                   (update solver tweet))]
      (send the-solver make-solver phrase dict)
      (send the-solver solve))))

(defn -main []
  (run-server {:port 8080} "/*" (servlet solver-app)))

(defservice solver-app)