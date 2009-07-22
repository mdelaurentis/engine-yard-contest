(ns com.mdelaurentis.engineyardcontest.solver
  (:import [java.security MessageDigest]
           [java.util Date BitSet]
           [com.mdelaurentis.engineyardcontest Bits])
  (:use [clojure.contrib test-is]
        compojure)
  (:gen-class 
   :extends javax.servlet.http.HttpServlet))

(set! *warn-on-reflection* true)

;; SHA-1 related functions

(defn sha1 [#^String str]
  (. (MessageDigest/getInstance "SHA-1") (digest (.getBytes str))))

(defn hamming-fn [target-hash]
  (let [target-bits (Bits/toBitSet target-hash)]
    (.flip target-bits 0 160)
    (fn [tweet-hash]
      (let [tweet-bits (Bits/toBitSet tweet-hash)]
        (.xor tweet-bits target-bits)
        (- 160 (.cardinality tweet-bits))))))

(defn compare-hashes [target-hash tweet-hash]
  ((hamming-fn target-hash) tweet-hash))

(defstruct st-solution :tweet :score :time)
(defstruct st-solver :id :phrase :dictionary :num-tries :solutions :score-fn)

(defn make-solver [id phrase dictionary]
  (let [hash (sha1 phrase)]
    (struct-map st-solver
      :id id
      :phrase phrase
      :dictionary dictionary
      :hash hash
      :score-fn (hamming-fn hash)
      :solutions ()
      :num-tries 0)))

(defn make-solution [tweet score]
  (struct-map st-solution
    :tweet tweet
    :score score
    :time (System/currentTimeMillis)))

(defn make-solvers [n phrase dictionary]
  (for [i (range  n)]
    (make-solver i phrase dictionary)))

(defn best-solution [solver]
  (first (:solutions solver)))

(defn best-tweet [solver]
  (:tweet (best-solution solver)))

(defn best-score [solver]
  (:score (best-solution solver) 161))

(def chars-for-random-word [\2 \3 \4 \5 \6 \7 \8 \9
                            \a \b \c \d \e \g \h \j \k \l \m \n \p \q \r \v \w \x \y \z
                            \A \B \C \D \E \G \H \J \K \L \M \N \P \Q \R \V \W \X \Y \Z])

(defn random-char []
  (chars-for-random-word (rand-int (count chars-for-random-word))))

(defn random-chars []
  (apply str (for [i (range (inc (rand-int 5)))]
               (random-char))))

(defn random-tweets [dict]
  "Returns an infinite sequence of random combinations of twelve words
chosen from the given dictionary."
  (let [limit      (count dict)
        rand-word  #(dict (rand-int limit))
        rand-tweet #(apply str (interpose " " (concat
                                               (take 12 (repeatedly rand-word))
                                               (list (random-chars)))))]
    (repeatedly rand-tweet)))

(defn inc-num-tries [solver]
  "Returns solver with :num-tries incremented."
  (assoc solver
    :num-tries (inc (:num-tries solver 0))))

(defn score-tweet [solver tweet]
  ((:score-fn solver) (sha1 tweet)))

(defn add-tweet [solver tweet]
  (if [(and tweet (string? tweet))]
    (try
     (let [solutions (:solutions solver ())
           score     (score-tweet solver tweet)]
       (if (< score (best-score solver))
         (assoc solver
           :solutions (conj (:solutions solver ()) (make-solution tweet score)))
         solver))
     (catch Throwable t
       (printf "Couldn't add tweet '%s' (%s) to solver %s: %s%n"
               tweet (class tweet) solver (.getMessage t))
       (.printStackTrace t)
       solver))
    solver))


;;
;; Agents
;;

(defn stopped? [solver]
  (:stopped? solver))

(defn solve 
  ([solver tweets]
     (if tweets
       (do
         (when-not (stopped? solver)
           (send *agent* solve (rest tweets)))
         (inc-num-tries (add-tweet solver (first tweets))))
       solver)))

(defn stop-solving [solver]
  (assoc solver :stopped? true))

(defn accumulate [manager solvers]
  (assoc
      (reduce add-tweet manager (map best-tweet solvers))
    :num-tries (apply + (map :num-tries solvers))))

(System/getProperty "java.rmi.server.hostname")

