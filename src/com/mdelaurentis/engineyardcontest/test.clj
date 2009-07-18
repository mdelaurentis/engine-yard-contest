(ns com.mdelaurentis.engineyardcontest.webapp
  (:use [clojure.contrib test-is]
        [com.mdelaurentis.engineyardcontest solver]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests
;;; 

(deftest test-compare-hashes
  (is (= 80 (compare-hashes (sha1 "What you write today will become legacy")
                            (sha1 "RuBy one eight six rspec mongrel MRI jruby jruby memcached exception reflection utf8E")))))

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
      (println "Calling accumulate")
      (send manager accumulate (map deref solvers))
      (Thread/sleep 1000))
    (doseq [s solvers]
      (send s stop-solving))
    (apply await manager solvers)
    (is (= (best-score manager)
           (first (sort (map best-score solvers)))))))

(run-tests)
(shutdown-agents)