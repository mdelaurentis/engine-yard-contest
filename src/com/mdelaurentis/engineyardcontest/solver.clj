(ns com.mdelaurentis.engineyardcontest.solver
  (:import [java.security MessageDigest]
           [java.util Date])
  (:use [clojure.contrib test-is]
        compojure)
  (:gen-class 
   :extends javax.servlet.http.HttpServlet))

;; SHA-1 related functions

(defn sha1 [str]
  (. (MessageDigest/getInstance "SHA-1") (digest (.getBytes str))))

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

(defn compare-hashes [a b]
  (count (filter (fn [[a-bit b-bit]]
                   (= a-bit b-bit))
                 (map list 
                      (bytes-to-string a) 
                      (bytes-to-string b)))))

(defstruct st-solution :tweet :score :time)
(defstruct st-solver :id :phrase :dictionary :num-tries :solutions)

(defn make-solver [id phrase dictionary]
  (struct-map st-solver
    :id id
    :phrase phrase
    :dictionary dictionary
    :hash (sha1 phrase)
    :solutions ()
    :num-tries 0))

(defn make-solution [tweet score]
  (struct-map st-solution
    :tweet tweet
    :score score
    :time (Date.)))

(defn make-solvers [n phrase dictionary]
  (for [i (range  n)]
    (make-solver i phrase dictionary)))

(defn best-solution [solver]
  (first (:solutions solver)))

(defn best-tweet [solver]
  (:tweet (best-solution solver)))

(defn best-score [solver]
  (:score (best-solution solver) 161))

(defn random-tweets [dict]
  "Returns an infinite sequence of random combinations of twelve words
chosen from the given dictionary."
  (let [limit      (count dict)
        rand-word  #(dict (rand-int limit))
        rand-tweet #(apply str (interpose " " (take 12 (repeatedly rand-word))))]
    (repeatedly rand-tweet)))

(defn inc-num-tries [solver]
  "Returns solver with :num-tries incremented."
  (assoc solver
    :num-tries (inc (:num-tries solver 0))))

(defn add-tweet [solver tweet]
  (if [(and tweet (string? tweet))]
    (try
     (let [solutions (:solutions solver ())
           hash      (sha1 tweet)
           score     (compare-hashes hash (:hash solver))]
       (assoc solver
         :solutions (if (< score (best-score solver))
                      (conj solutions (make-solution tweet score))
                      solutions)))
     (catch Throwable t
       (printf "Couldn't add tweet '%s' (%s) to solver %s%n"
               tweet (class tweet) solver)
       (.printStackTrace t)
       solver))
    solver))


;;
;; Agents
;;

(defn stopped? [solver]
  (:stopped? solver))

(defn solve [solver tweets]
  (if tweets
    (do
      (when-not (stopped? solver)
        (send *agent* solve (rest tweets)))
      (inc-num-tries (add-tweet solver (first tweets))))
    solver))



(defn stop-solving [solver]
  (assoc solver :stopped? true))

(defn accumulate [manager solvers]
  (assoc
      (reduce add-tweet manager (map best-tweet solvers))
    :num-tries (apply + (map :num-tries solvers))))

