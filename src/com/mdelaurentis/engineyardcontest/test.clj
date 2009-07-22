(ns com.mdelaurentis.engineyardcontest.webapp
  (:use [clojure.contrib test-is]
        [com.mdelaurentis.engineyardcontest solver]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests
;;; 

(deftest test-compare-hashes
  (is (= 0 (compare-hashes (sha1 "foo") (sha1 "foo"))))
  (is (= 80 (compare-hashes (sha1 "What you write today will become legacy")                            (sha1 "RuBy one eight six rspec mongrel MRI jruby jruby memcached exception reflection utf8E"))))
  (is (= 74 (compare-hashes (sha1 "I am not a big believer in fortune telling")
                            (sha1 "Rubinius one eight six active active record memcached exception JRuby DHH TOKYO sdfe3")))))

(deftest test-make-solvers 
  (let [dict  ["asdf" "bar" "foo" "clojure" "engine" "yard" "contest"]
        phrase "this is a test phrase"
        solvers (make-solvers 10 phrase dict)]
    (doseq [s solvers]
      (is (= phrase (:phrase s)))
      (is (= dict (:dictionary s)))
      (is (not (nil? (:hash s)))))))

(deftest test-add-tweet
  (let [dict  ["asdf" "bar" "foo" "clojure" "engine" "yard" "contest"]
        phrase "this is a test phrase"
        solvers (for [s (make-solvers 10 phrase dict)]
                  (reduce add-tweet s (take 100 (random-tweets dict))))]
    (doseq [s solvers]
      (is (> (count (:solutions s)) 0)))))

(deftest test-agents
  (let [dict    ["asdf" "bar" "foo" "clojure" "engine" "yard" "contest"]
        phrase  "this is a test phrase"
        solvers (map agent (make-solvers 10 phrase dict))
        manager (agent (make-solver "manager" phrase dict))]
    (doseq [s solvers]
      (send s solve (random-tweets dict)))
    (dotimes [i 10]
      (send manager accumulate (map deref solvers))
      (Thread/sleep 1000))
    (doseq [s solvers]
      (send s stop-solving))
    (apply await manager solvers)
    (is (= (best-score manager)
           (first (sort (map best-score solvers)))))))

(deftest test-many-tweets
  (let [dict    ["asdf" "bar" "foo" "clojure" "engine" "yard" "contest"]
        phrase  "this is a test phrase"
        solver (make-solver "test" phrase dict)]
    (time (reduce add-tweet solver (take 10000 (random-tweets dict))))))

;(run-tests)
;(shutdown-agents)

;(time (test-agents))

(test-many-tweets)