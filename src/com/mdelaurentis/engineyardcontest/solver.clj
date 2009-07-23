(ns com.mdelaurentis.engineyardcontest.solver
  (:import [java.security MessageDigest]
           [java.util Date BitSet]
           [com.mdelaurentis.engineyardcontest Bits])
  (:use [clojure.contrib test-is]
        compojure)
  (:gen-class 
   :extends javax.servlet.http.HttpServlet))

(set! *warn-on-reflection* true)

;;;
;;; SHA-1 related functions
;;;

(defn sha1 [#^String str]
  (. (MessageDigest/getInstance "SHA-1") (digest (.getBytes str))))

(defn hamming-fn 
  "Given a target hash (byte array), returns a function that takes
  another hash and returns the hamming distance."
  [target-hash]
  (let [target-bits (Bits/toBitSet target-hash)]
    (.flip target-bits 0 160)
    (fn [tweet-hash]
      (let [tweet-bits (Bits/toBitSet tweet-hash)]
        (.xor tweet-bits target-bits)
        (- 160 (.cardinality tweet-bits))))))

;;;
;;; Functions for getting a random sequence of potential solutions
;;;

(def chars-for-random-word [\2 \3 \4 \5 \6 \7 \8 \9
                            \a \b \c \d \e \g \h \j \k \l \m \n \p \q \r \v \w \x \y \z
                            \A \B \C \D \E \G \H \J \K \L \M \N \P \Q \R \V \W \X \Y \Z])

(defn random-char []
  (chars-for-random-word (rand-int (count chars-for-random-word))))

(defn random-chars 
  "Returns a seq of one to five characters randomly chosen from
  chars-for-random-word." 
  []
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

;;;
;;; Functions for making solvers and solutions
;;;

(defn solver
  "Returns a map that has the state required for generating
  and scoring solutions for the given phrase and dictionary."  
  [id phrase dictionary]
  (let [hash (sha1 phrase)]
    {:id id
     :phrase phrase
     :dictionary dictionary
     :hash hash
     :score-fn (hamming-fn hash)
     :solutions ()
     :num-tries 0}))

(defn solvers 
  "Returns a seq of n solvers for the given phrase and dictionary."
  [n phrase dictionary]
  (for [i (range  n)]
    (solver i phrase dictionary)))

(defn solution 
  "Returns a map representing a single solution to a problem, a :tweet
  with a :score and a :time found."
  [tweet score] {:tweet tweet
   :score score
   :time (System/currentTimeMillis)})

;;;
;;; Functions of solvers
;;;

(defn best-solution [slvr]
  (first (:solutions slvr)))

(defn best-tweet [slvr]
  (:tweet (best-solution slvr)))

(defn best-score [slvr]
  (:score (best-solution slvr) 161))

(defn score-tweet [slvr tweet]
  ((:score-fn slvr) (sha1 tweet)))

(defn inc-num-tries [slvr]
  (assoc slvr
    :num-tries (inc (:num-tries slvr 0))))

(defn add-tweet
  "Compares the given tweet to the current best solution in slvr.  If
  it's better, adds it.  If not, ignores it."
  [slvr tweet]
  (if [(and tweet (string? tweet))]
    (try
     (let [score (score-tweet slvr tweet)]
       (if (< score (best-score slvr))
         (assoc slvr
           :solutions (conj (:solutions slvr ()) (solution tweet score)))
         slvr))
     (catch Throwable t
       (printf "Couldn't add tweet '%s' (%s) to solver %s: %s%n"
               tweet (class tweet) slvr (.getMessage t))
       (.printStackTrace t)
       slvr))
    slvr))


;;
;; Solvers as agents
;;

(defn stopped? [slvr]
  (:stopped? slvr))

(defn solve 
  "Action for evaluating a sequence of tweets on a solver agent.
Sends itself to *agent* until the solver is stopped (by stop-solving)
or tweets runs out."
  ([slvr tweets]
     (if tweets
       (do
         (when-not (stopped? slvr)
           (send *agent* solve (rest tweets)))
         (inc-num-tries (add-tweet slvr (first tweets))))
       slvr)))

(defn stop-solving [slvr]
  (assoc slvr :stopped? true))

(defn accumulate 
  "Update manager with the solutions from each of the given solvers.
  At the end manager's best tweet will be the best tweet from any of
  the given solvers." 
  [manager slvrs]
  (assoc
      (reduce add-tweet manager (map best-tweet slvrs))
    :num-tries (apply + (map :num-tries slvrs))))
