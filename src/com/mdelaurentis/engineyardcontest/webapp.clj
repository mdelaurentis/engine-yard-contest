(ns com.mdelaurentis.engineyardcontest.webapp
  (:use [clojure.contrib test-is]
        [com.mdelaurentis.engineyardcontest solver]
        compojure)
  (:gen-class 
   :extends javax.servlet.http.HttpServlet))

(def manager (agent {}))

(defroutes solver-app
  (GET "/"
    (html
     (doctype :html4)
     [:html
      [:head
       [:title "Solver"]
       ;(include-css "/styles.css")
       ]
      [:body
       [:h3 "Phrase:"] (:phrase @manager)
       [:h3 "Dictionary:"] (apply str (:dictionary @manager))
       [:h3 "Num Tried:"] (:num-tries @manager)
       [:h3 "Solutions:"]
       [:table
        [:tr 
         [:td "Phrase"]
         [:td "Score"]
         [:td "Time Found"]]
        (for [s (:solutions @manager)]
          [:tr
           [:td (:tweet s)]
           [:td (:score s)]
           [:td (:time s)]])]]])))


(defservice solver-app)

(defn manage [manager solvers]
  (let [tweets  (map best-tweet solvers)]
    (println "Tweets are" tweets)
    (reduce add-tweet manager tweets)))

(defn make-solver-agents [n phrase dict]
  (for [i n]
    (agent (make-solver i phrase dict))))

(defn -main [phrase-file dict-file num-solvers port cluster-file]
  (let [phrase  (slurp phrase-file)
        dict    (vec (.split (slurp dict-file) "\\s+"))
        solvers (map agent (make-solvers (Integer/valueOf num-solvers) phrase dict))]
    (send manager (fn [m] (make-solver "manager" phrase dict)))

    (dosync (ref-set running true))
    (println "solvers are" solvers)
    (doseq [solver solvers]
      (send solver solve (random-tweets dict)))
    
  (run-server {:port (Integer/valueOf port)} "/*" (servlet solver-app))
  (loop []
    (Thread/sleep 1000)
    (send manager manage solvers)
    (when @running (recur)))))